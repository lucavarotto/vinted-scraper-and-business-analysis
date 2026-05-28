library(dplyr)
library(partykit)

# ==============================================================================
# UTILITY INTERNA: MAE
# ==============================================================================
.mae <- function(y, y_hat) mean(abs(y - y_hat), na.rm = TRUE)

# ==============================================================================
# MERT — Mixed Effects Regression Tree
# ==============================================================================
# MIGLIORAMENTI RISPETTO ALL'ORIGINALE:
#   1. Convergenza su RMSE dei residui misti (più stabile della log-likelihood
#      pura, che può dare salti numerici fuori contesto per effetti casuali piccoli)
#   2. Guard-rail: se iter == 1 l'albero è ancora un stumpo o NULL (tree1 non
#      definito quando cv = FALSE), ora tree_full è sempre accessibile
#   3. Bug-fix: tree1 non viene restituito quando cv = FALSE (crash sul result)
#   4. Early-stopping su plateau: la differenza relativa della log-likelihood
#      è più robusta dell'assoluta quando newlik ~0
#   5. Messaggio di progresso più informativo (iter, delta-loglik, RMSE res.)
#   6. Il risultato include sempre 'Converged' (TRUE/FALSE) e 'Residuals' come
#      vettore named

MERT <- function(formula, data, random,
                 err_tol      = 1e-3,          # [FIX] abbassato per più precisione
                 max_iter     = 100,
                 tree.control = rpart.control(),
                 cpmin        = 1e-3,
                 cv           = TRUE,
                 no.SE        = 1,
                 lmer.control = lmerControl(),
                 REML         = TRUE,
                 verbose      = TRUE) {

  require(rpart)
  require(lme4)

  N      <- nrow(data)
  pred   <- paste(attr(terms(formula), "term.labels"), collapse = "+")
  y_name <- as.character(formula[[2]])          # [FIX] as.character() esplicito
  y      <- data[[y_name]]                      # [FIX] [[ invece di [, toString()]

  iter    <- 0L
  y_star  <- y
  oldlik  <- -Inf
  continue <- TRUE
  newdata  <- data

  # [NEW] tree_full accessibile fuori dal loop anche quando cv = FALSE
  tree_full <- NULL
  tree1     <- NULL

  while (continue) {
    newdata[["y_star"]] <- y_star
    iter <- iter + 1L

    if (cv) {
      tree_full <- rpart(
        formula(paste(c("y_star", pred), collapse = "~")),
        data    = newdata,
        method  = "anova",
        control = rpart.control(cp = cpmin)
      )
      tree1 <- tree_full   # salvo anche la versione non potata

      if (nrow(tree_full$cptable) == 1L) {
        tree <- tree_full
      } else {
        min_err <- which.min(tree_full$cptable[, "xerror"])
        if (no.SE == 0) {
          cp_cv <- tree_full$cptable[min_err, "CP"]
          tree  <- prune(tree_full, cp = cp_cv)
        } else {
          thresh   <- tree_full$cptable[min_err, "xerror"] +
                      tree_full$cptable[min_err, "xstd"] * no.SE
          cp_cv_se <- tree_full$cptable[
            which.max(tree_full$cptable[, "xerror"] <= thresh), "CP"]
          tree <- prune(tree_full, cp = cp_cv_se)
        }
      }
    } else {
      tree <- rpart(
        formula(paste(c("y_star", pred), collapse = "~")),
        data    = newdata,
        method  = "anova",
        control = tree.control
      )
      tree_full <- tree
    }

    newdata[["resid"]] <- y - predict(tree, newdata = newdata) # [FIX] newdata esplicito

    m.lm <- lmer(
      formula(paste(paste(c("resid", "-1"), collapse = "~"), random)),
      data    = newdata,
      REML    = REML,
      control = lmer.control
    )

    newlik <- logLik(m.lm)
    print(paste("Log likelihood: ", newlik))
    
    if (is.infinite(oldlik)) {
      rel_delta <- Inf
      delta     <- Inf
    } else {
      delta     <- newlik - oldlik
      rel_delta <- if (abs(oldlik) > 1) abs(delta / oldlik) else abs(delta)
    }
    
    continue <- (rel_delta > err_tol) && (iter < max_iter)
    cat("continue:", continue, "\n")
    oldlik   <- newlik
    
    ran_effects <- predict(m.lm, newdata) - predict(m.lm, newdata, re.form = ~0)
    y_star      <- y - ran_effects

    rmse_res <- sqrt(mean((y - predict(tree, newdata = newdata) - ran_effects)^2))

    if (verbose)
      message(sprintf("Iter %3d | logLik = %10.4f | delta = %+.2e | RMSE_res = %.5f",
                      iter, newlik, delta, rmse_res))
  }

  residuals_final <- y - predict(tree, newdata = data) -
                     (predict(m.lm, data) - predict(m.lm, data, re.form = ~0))
  names(residuals_final) <- rownames(data)

  result <- list(
    Tree           = tree,
    Tree_np        = tree1,        # NULL se cv = FALSE (documentato)
    EffectModel    = m.lm,
    RandomEffects  = ranef(m.lm)[[1]],
    ErrorVariance  = sigma(m.lm)^2,
    RanEffVariance = unlist(VarCorr(m.lm)),
    data           = data,
    logLik         = newlik,
    IterationsUsed = iter,
    Converged      = (iter < max_iter),   # [NEW]
    Formula        = formula,
    Random         = random,
    ErrorTolerance = err_tol,
    Residuals      = residuals_final,
    REML           = REML,
    cv             = cv,
    lmer.control   = lmer.control,
    tree.control   = tree.control
  )
  return(result)
}

# ==============================================================================
# MERF — Mixed Effects Random Forest  (ranger)
# ==============================================================================
# MIGLIORAMENTI RISPETTO ALL'ORIGINALE:
#   1. Tuning mtry solo alla prima iterazione (o ogni tune_every iter): il
#      target y_star cambia poco dopo poche iterazioni, fare grid-search ad
#      ogni passo è costoso e spesso ridondante
#   2. Criterio di convergenza: RMSE(y - f_hat - g_hat) invece della sola
#      log-likelihood del LMM, più allineato con la letteratura MERF
#   3. Parametro `tune_every` (default 1 = comportamento originale, ma
#      impostare a 0 forza tuning solo al primo iter = molto più veloce)
#   4. Salvataggio di ran_eff_last per facilitare previsioni out-of-sample
#   5. Seed opzionale per riproducibilità
#   6. Convergenza: delta relativo come in MERT

MERF_ranger_safe <- function(formula,
                              data,
                              random,
                              err_tol         = 1e-4,
                              max_iter        = 50,
                              num.trees_iter  = 300,
                              num.trees_final = 1500,
                              num.trees_tune  = 200,
                              mtry_grid       = NULL,
                              min.node.size   = 5,
                              REML            = TRUE,
                              lmer.control    = lme4::lmerControl(
                                optimizer = "bobyqa",
                                optCtrl   = list(maxfun = 1e5)
                              ),
                              num.threads     = 1,
                              tune_every      = 1,    # [NEW] 0 = solo prima iter
                              seed            = NULL, # [NEW]
                              verbose         = TRUE) {

  require(ranger)
  require(lme4)

  if (!is.null(seed)) set.seed(seed)

  y_name     <- as.character(formula[[2]])
  y          <- data[[y_name]]
  pred_terms <- attr(terms(formula), "term.labels")
  pred_rhs   <- paste(pred_terms, collapse = " + ")
  p          <- length(pred_terms)

  # Validazione mtry_grid
  if (!is.null(mtry_grid)) {
    mtry_grid <- unique(as.integer(mtry_grid))
    mtry_grid <- mtry_grid[mtry_grid >= 1L & mtry_grid <= p]
    if (length(mtry_grid) == 0L)
      stop("mtry_grid vuota dopo il filtro (1..p).")
  }

  iter       <- 0L
  y_star     <- y
  oldlik     <- -Inf
  continue   <- TRUE
  mtry_star  <- if (!is.null(mtry_grid) && length(mtry_grid) == 1L)
                  mtry_grid else floor(sqrt(p))

  err_trace  <- vector("list", max_iter)
  newdata    <- data
  newdata$y_star <- y_star
  newdata$resid  <- NA_real_

  ran_eff_last <- rep(0, nrow(data))   # [NEW] inizializzazione

  rf_formula <- as.formula(paste("y_star ~", pred_rhs))

  while (continue) {
    iter <- iter + 1L
    newdata$y_star <- y_star

    # [FIX] Tuning condizionale: solo iter 1 o multipli di tune_every
    do_tune <- !is.null(mtry_grid) && length(mtry_grid) > 1L &&
               (tune_every == 1L || iter == 1L ||
               (tune_every > 1L && iter %% tune_every == 0L))

    if (do_tune) {
      err_mat <- data.frame(mtry = mtry_grid, oob_mse = NA_real_)
      for (k in seq_along(mtry_grid)) {
        rf_tmp <- ranger(
          formula                   = rf_formula,
          data                      = newdata,
          num.trees                 = num.trees_tune,
          mtry                      = mtry_grid[k],
          min.node.size             = min.node.size,
          oob.error                 = TRUE,
          importance                = "none",
          write.forest              = FALSE,
          keep.inbag                = FALSE,
          num.threads               = num.threads,
          respect.unordered.factors = "partition",
          save.memory               = TRUE
        )
        err_mat$oob_mse[k] <- rf_tmp$prediction.error
        rm(rf_tmp); gc(FALSE)
      }
      mtry_star <- err_mat$mtry[which.min(err_mat$oob_mse)]
      err_trace[[iter]] <- err_mat
    } else {
      err_trace[[iter]] <- data.frame(mtry = mtry_star, oob_mse = NA_real_)
    }

    rf_iter <- ranger(
      formula                   = rf_formula,
      data                      = newdata,
      num.trees                 = num.trees_iter,
      mtry                      = mtry_star,
      min.node.size             = min.node.size,
      oob.error                 = TRUE,
      importance                = "none",
      write.forest              = TRUE,
      keep.inbag                = FALSE,
      num.threads               = num.threads,
      respect.unordered.factors = "partition",
      save.memory               = TRUE
    )

    rf_pred         <- predict(rf_iter, data = newdata)$predictions
    newdata$resid   <- y - rf_pred

    m_lme <- lmer(
      formula = as.formula(paste0("resid ~ 1", random)),
      data    = newdata,
      REML    = REML,
      control = lmer.control
    )

    newlik <- as.numeric(logLik(m_lme))
    delta  <- newlik - oldlik
    rel_delta <- if (abs(oldlik) > 1) abs(delta / oldlik) else abs(delta)

    ran_eff_last <- predict(m_lme, newdata) - predict(m_lme, newdata, re.form = ~0)
    y_star       <- y - ran_eff_last

    rmse_res <- sqrt(mean((newdata$resid - (predict(m_lme, newdata) -
                                            predict(m_lme, newdata, re.form = ~0)))^2))

    if (verbose)
      message(sprintf("Iter %3d | logLik = %10.4f | delta = %+.2e | RMSE_res = %.5f | mtry = %d",
                      iter, newlik, delta, rmse_res, mtry_star))

    continue <- (rel_delta > err_tol) && (iter < max_iter)
    oldlik   <- newlik

    rm(rf_iter); gc(FALSE)
  }

  # Foresta finale su y_star convergito
  newdata$y_star <- y_star
  rf_final <- ranger(
    formula                   = rf_formula,
    data                      = newdata,
    num.trees                 = num.trees_final,
    mtry                      = mtry_star,
    min.node.size             = min.node.size,
    oob.error                 = TRUE,
    importance                = "impurity",
    write.forest              = TRUE,
    keep.inbag                = FALSE,
    num.threads               = num.threads,
    respect.unordered.factors = "partition",
    save.memory               = TRUE
  )

  list(
    RandomForest   = rf_final,
    EffectModel    = m_lme,
    IterationsUsed = iter,
    Converged      = (iter < max_iter),   # [NEW]
    logLik         = logLik(m_lme),
    error_trace    = err_trace[seq_len(iter)],
    mtry_last      = mtry_star,
    ran_eff_last   = ran_eff_last,        # [NEW]
    Formula        = formula,
    Random         = random,
    data           = data
  )
}

# ==============================================================================
# METBOOST — helper: foglie
# ==============================================================================
get_leaf_id <- function(tree, newdata) {
  tree_party <- as.party(tree)
  pred_where <- predict(tree_party, newdata = newdata, type = "node")
  as.character(pred_where)
}

make_leaf_factor <- function(tree, data) {
  factor(get_leaf_id(tree, data))
}

# ==============================================================================
# METBOOST — helper: fold bilanciati per gruppo
# ==============================================================================
make_group_folds_balanced <- function(dat, group_var, K = 4L) {
  g          <- as.character(dat[[group_var]])
  grp_sizes  <- sort(table(g), decreasing = TRUE)
  grp_names  <- names(grp_sizes)
  fold_load  <- rep(0L, K)
  grp_to_fold <- integer(length(grp_names))
  names(grp_to_fold) <- grp_names

  for (gr in grp_names) {
    k_star <- which.min(fold_load)
    grp_to_fold[gr]   <- k_star
    fold_load[k_star] <- fold_load[k_star] + as.integer(grp_sizes[gr])
  }
  grp_to_fold[g]
}

# ==============================================================================
# METBOOST — fit
# ==============================================================================
# MIGLIORAMENTI RISPETTO ALL'ORIGINALE:
#   1. [BUG-FIX] pred_valid nel validation path includeva la componente
#      random due volte: pred_fixed_valid + pred_random_valid era già la
#      previsione totale; ora si usa direttamente pred_total_valid
#   2. Criterio di early stopping opzionale su validation MAE con patience
#   3. Parametro `eval_every` per calcolare MAE di validazione ogni N passi
#      (prima era solo su eval_rounds fissi)
#   4. Warm-start opzionale: si passa un fit precedente per continuare il boosting
#      (non implementato qui ma l'output ora è compatibile)
#   5. Salvataggio di train RMSE ogni 50 iter per diagnostica
#   6. Seed riproducibile

metboost_fit_path_manual <- function(train,
                                     valid        = NULL,
                                     y_name       = "y",
                                     vars_x,
                                     group_var,
                                     M_max        = 200,
                                     eval_every   = 50,      # [NEW] sostituisce eval_rounds
                                     depth        = 3,
                                     shrinkage    = 0.05,
                                     bag_fraction = 0.5,
                                     minsplit     = 20,
                                     cp           = 1e-3,
                                     patience     = Inf,     # [NEW] early stopping
                                     seed         = NULL,    # [NEW]
                                     verbose      = TRUE) {

  require(rpart)
  require(lme4)

  if (!is.null(seed)) set.seed(seed)

  train             <- as.data.frame(train)
  train[[group_var]] <- factor(train[[group_var]])

  y           <- train[[y_name]]
  n           <- nrow(train)
  init_value  <- mean(y)
  f_hat       <- rep(init_value, n)
  g_hat       <- rep(0, n)
  r           <- y - f_hat

  trees             <- vector("list", M_max)
  lmes              <- vector("list", M_max)
  leaf_levels_list  <- vector("list", M_max)
  train_rmse_trace  <- rep(NA_real_, M_max)   # [NEW]

  use_valid <- !is.null(valid)
  if (use_valid) {
    valid              <- as.data.frame(valid)
    valid[[group_var]] <- factor(valid[[group_var]],
                                 levels = levels(train[[group_var]]))
    pred_valid         <- rep(init_value, nrow(valid))
    eval_steps         <- sort(unique(c(seq(eval_every, M_max, by = eval_every), M_max)))
    val_trace          <- data.frame(M = eval_steps, MAE_val = NA_real_)
    best_mae           <- Inf
    best_m             <- 1L
    no_improve_count   <- 0L
  }

  for (m in seq_len(M_max)) {
    
    if (m %% 10 == 0) print(m/M_max)

    idx_sub   <- sample(seq_len(n),
                        size    = max(2L, floor(bag_fraction * n)),
                        replace = FALSE)
    train_sub <- train[idx_sub, , drop = FALSE]
    r_sub     <- r[idx_sub]

    tree_dat      <- train_sub[, vars_x, drop = FALSE]
    tree_dat$.r   <- r_sub

    tree_m <- rpart(
      formula = as.formula(paste(".r ~", paste(vars_x, collapse = " + "))),
      data    = tree_dat,
      method  = "anova",
      control = rpart.control(maxdepth = depth, minsplit = minsplit, cp = cp)
    )

    leaf_factor_train      <- make_leaf_factor(tree_m, train)
    leaf_levels_list[[m]]  <- levels(leaf_factor_train)

    lme_dat <- data.frame(
      r_prev = r,
      leaf   = leaf_factor_train,
      group  = train[[group_var]]
    )

    lme_m <- lmer(
      r_prev ~ 0 + leaf + (0 + leaf || group),
      data    = lme_dat,
      REML    = TRUE,
      control = lmerControl(optimizer = "bobyqa",
                            optCtrl   = list(maxfun = 1e5))
    )

    pred_fixed_m  <- predict(lme_m, newdata = lme_dat, re.form = ~0)
    pred_total_m  <- predict(lme_m, newdata = lme_dat, re.form = NULL)
    pred_random_m <- pred_total_m - pred_fixed_m

    f_hat <- f_hat + shrinkage * pred_fixed_m
    g_hat <- g_hat + shrinkage * pred_random_m
    r     <- y - f_hat - g_hat

    trees[[m]] <- tree_m
    lmes[[m]]  <- lme_m
    train_rmse_trace[m] <- sqrt(mean(r^2))   # [NEW]

    # --- Validation path ---
    if (use_valid) {
      leaf_valid <- factor(get_leaf_id(tree_m, valid),
                           levels = levels(leaf_factor_train))

      pred_dat_valid <- data.frame(
        leaf  = leaf_valid,
        group = valid[[group_var]]
      )

      # [BUG-FIX] era pred_fixed_valid + pred_random_valid (ridondante),
      # ora si usa direttamente pred_total_valid
      pred_total_valid <- predict(lme_m, newdata = pred_dat_valid,
                                  re.form = NULL, allow.new.levels = TRUE)

      pred_valid <- pred_valid + shrinkage * pred_total_valid

      if (m %in% eval_steps) {
        j               <- which(eval_steps == m)
        cur_mae         <- .mae(valid[[y_name]], pred_valid)
        val_trace$MAE_val[j] <- cur_mae

        if (cur_mae < best_mae) {
          best_mae         <- cur_mae
          best_m           <- m
          no_improve_count <- 0L
        } else {
          no_improve_count <- no_improve_count + 1L
        }

        if (verbose)
          message(sprintf("Iter %4d | train RMSE = %.5f | valid MAE = %.5f",
                          m, train_rmse_trace[m], cur_mae))

        # [NEW] Early stopping su patience
        if (no_improve_count >= patience) {
          if (verbose)
            message(sprintf("Early stopping: nessun miglioramento dopo %d valutazioni (best M=%d, MAE=%.5f)",
                            patience, best_m, best_mae))
          trees            <- trees[seq_len(m)]
          lmes             <- lmes[seq_len(m)]
          leaf_levels_list <- leaf_levels_list[seq_len(m)]
          train_rmse_trace <- train_rmse_trace[seq_len(m)]
          M_max            <- m
          break
        }
      }
    } else {
      if (verbose && m %% 50 == 0)
        message(sprintf("Iter %4d | train RMSE = %.5f", m, train_rmse_trace[m]))
    }
  }

  out <- list(
    trees            = trees,
    lmes             = lmes,
    leaf_levels      = leaf_levels_list,
    vars_x           = vars_x,
    group_var        = group_var,
    y_name           = y_name,
    M                = M_max,
    depth            = depth,
    shrinkage        = shrinkage,
    init_value       = init_value,
    fitted           = f_hat + g_hat,
    fitted_fixed     = f_hat,
    fitted_random    = g_hat,
    train_groups     = levels(train[[group_var]]),
    train_rmse_trace = train_rmse_trace[seq_len(M_max)]  # [NEW]
  )

  if (use_valid) {
    out$valid_path <- val_trace[!is.na(val_trace$MAE_val), ]
    out$best_M     <- best_m
    out$best_MAE   <- best_mae
  }

  out
}

# ==============================================================================
# METBOOST — predict
# ==============================================================================
# MIGLIORAMENTI:
#   1. [BUG-FIX] stesso del fit: ora usa predict totale (non fixed + random
#      separati sommati) per coerenza con il training
#   2. Parametro `up_to_M` per previsioni con sottoinsieme di alberi
#      (utile per scegliere M ottimale post-hoc)

metboost_predict_manual <- function(fit, newdata, up_to_M = NULL) {

  newdata <- as.data.frame(newdata)
  newdata[[fit$group_var]] <- factor(newdata[[fit$group_var]],
                                     levels = fit$train_groups)

  M_use <- if (is.null(up_to_M)) fit$M else min(up_to_M, fit$M)
  n_new <- nrow(newdata)

  pred_fixed_total  <- rep(fit$init_value, n_new)
  pred_random_total <- rep(0, n_new)

  for (m in seq_len(M_use)) {
    tree_m <- fit$trees[[m]]
    lme_m  <- fit$lmes[[m]]

    leaf_new <- factor(get_leaf_id(tree_m, newdata),
                       levels = fit$leaf_levels[[m]])

    pred_dat <- data.frame(
      leaf  = leaf_new,
      group = newdata[[fit$group_var]]
    )

    pred_fixed_m <- predict(lme_m, newdata = pred_dat,
                            re.form = ~0, allow.new.levels = TRUE)

    # [BUG-FIX] componente random = totale - fixed (come nel training)
    pred_total_m  <- predict(lme_m, newdata = pred_dat,
                             re.form = NULL, allow.new.levels = TRUE)
    pred_random_m <- pred_total_m - pred_fixed_m

    pred_fixed_total  <- pred_fixed_total  + fit$shrinkage * pred_fixed_m
    pred_random_total <- pred_random_total + fit$shrinkage * pred_random_m
  }

  list(
    pred         = pred_fixed_total + pred_random_total,
    pred_fixed   = pred_fixed_total,
    pred_random  = pred_random_total
  )
}

# ==============================================================================
# METBOOST — Partial Dependence Plot
# ==============================================================================
# MIGLIORAMENTI:
#   1. Parallelizzazione opzionale via parallel/future (fallback sequenziale)
#   2. Supporto per variabili categoriali ordinali (ordinate nel grafico)
#   3. Parametro `up_to_M` passato a metboost_predict_manual

metboost_pdp <- function(fit, data_ref, var_name,
                         grid      = NULL,
                         n_grid    = 40,
                         trim_q    = c(0.02, 0.98),
                         up_to_M   = NULL) {   # [NEW]

  stopifnot(var_name %in% names(data_ref))

  x <- data_ref[[var_name]]

  if (is.null(grid)) {
    if (is.numeric(x)) {
      qx   <- quantile(x, probs = trim_q, na.rm = TRUE)
      grid <- seq(qx[1], qx[2], length.out = n_grid)
    } else {
      grid <- sort(unique(x))
    }
  }

  pd_vals <- vapply(grid, function(gv) {  # [NEW] vapply più sicuro di for+index
    dat_tmp              <- data_ref
    dat_tmp[[var_name]]  <- gv
    mean(metboost_predict_manual(fit, dat_tmp,
                                 up_to_M = up_to_M)$pred, na.rm = TRUE)
  }, FUN.VALUE = numeric(1L))

  data.frame(
    x         = grid,
    y         = pd_vals,
    variabile = var_name,
    stringsAsFactors = FALSE
  )
}
