library(dplyr)
library(partykit)
## MERT ----
MERT <- function (formula, data, random, err_tol = 0.001, max_iter = 100,
                  tree.control = rpart.control(), cpmin = 0.001, cv = T, no.SE = 1,
                  lmer.control = lmerControl(), REML = T){
  # random: una stringa del tipo " + (1|id) -> specificazione richiesta da lme4
  require(rpart)
  require(lme4)
  N <- nrow(data)
  pred <- paste(attr(terms(formula), "term.labels"), collapse = "+") # somma delle X
  y_name <- formula[[2]]
  y <- data[, toString(y_name)]
  continue <- TRUE
  iter <- 0
  y_star <- y - rep(0, N) # rimuovo l'effetto casuale che all'inizio è 0
  oldlik <- -Inf
  newdata <- data
  while (continue) {
    newdata[, "y_star"] <- y_star
    iter <- iter + 1
    if (cv) { # valore ottimo tramite cv
      tree1 <- rpart(formula(paste(c("y_star", pred), collapse = "~")),
                     data = newdata, method = "anova", control = rpart.control(cp = cpmin))
      if (nrow(tree1$cptable) == 1) tree <- tree1
      else {
        # posizione dell'errore di convalida minimo
        min_err <- which.min(tree1$cptable[, "xerror"])
        if (no.SE == 0) {
          cp_cv <- tree1$cptable[min_err, "CP"]
          tree <- prune(tree1, cp = cp_cv)
        }
        else {
          xerror_cv <- tree1$cptable[min_err, "xerror"]
          se_xerror_cv <- xerror_cv+tree1$cptable[min_err,"xstd"] * no.SE
          cp_cv_se <- tree1$cptable[which.max(tree1$cptable[,"xerror"] <=
                                                se_xerror_cv), "CP"]
          tree <- prune(tree1, cp = cp_cv_se)
        }
      }
    }
    else { # parametri ottimi già scelti, non serve cv
      tree <- rpart(formula(paste(c("y_star", pred), collapse = "~")),
                    data = newdata, method = "anova", control = tree.control)
    }
    newdata[, "resid"] <- y - predict(tree)
    # stima del modello ad effetti casuali sui residui dell'albero
    m.lm <- lmer(formula(paste(paste(c("resid", "-1"), collapse = "~"), random)),
                 data = newdata, REML = REML, control = lmer.control)
    newlik <- logLik(m.lm) # REML
    print(paste("Log likelihood: ", newlik))
    continue <- (newlik - oldlik > err_tol & iter < max_iter)
    oldlik <- newlik
    # previsione effetti casuali per ogni osservazione
    ran_effects <- predict(m.lm, newdata)-predict(m.lm, newdata, re.form =~0)
    y_star <- y - ran_effects
  }
  residuals <- y - predict(tree) - predict(m.lm) # residui Y-(fisso+random)
  result <- list(Tree = tree, Tree_np = tree1, EffectModel = m.lm,
                 RandomEffects = ranef(m.lm)[[1]], ErrorVariance = sigma(m.lm)^2,
                 RanEffVariance = unlist(VarCorr(m.lm)), data = data,
                 logLik = newlik, IterationsUsed = iter, Formula = formula,
                 Random = random, ErrorTolerance = err_tol, Residuals = residuals,
                 REML = REML, cv = cv, lmer.control = lmer.control,
                 tree.control = tree.control)
  return(result)
}

## MERF -----
MERF_ranger_safe <- function(formula, data, random,
                             err_tol = 1e-3, max_iter = 50,
                             num.trees_iter = 300,
                             num.trees_final = 1500,
                             num.trees_tune  = 200,
                             mtry_grid = NULL,
                             min.node.size = 5,
                             REML = TRUE,
                             lmer.control = lme4::lmerControl(
                               optimizer = "bobyqa",
                               optCtrl = list(maxfun = 1e5)
                             ),
                             num.threads = 1) {
  
  require(ranger)
  require(lme4)
  
  
  y_name <- as.character(formula[[2]])
  y <- data[[y_name]]
  
  pred_terms <- attr(terms(formula), "term.labels")
  pred_rhs   <- paste(pred_terms, collapse = " + ")
  p <- length(pred_terms)
  
  if (!is.null(mtry_grid)) {
    mtry_grid <- unique(as.integer(mtry_grid))
    mtry_grid <- mtry_grid[mtry_grid >= 1 & mtry_grid <= p]
    if (length(mtry_grid) == 0) stop("mtry_grid vuota dopo il filtro (1..p).")
  }
  
  iter <- 0
  y_star <- y
  oldlik <- -Inf
  continue <- TRUE
  
  err_trace <- vector("list", max_iter)
  mtry_star <- NA_integer_
  
  newdata <- data
  newdata$y_star <- y_star
  newdata$resid  <- NA_real_
  
  while (continue) {
    iter <- iter + 1
    newdata$y_star <- y_star
    
    rf_formula <- as.formula(paste("y_star ~", pred_rhs))
    
    if (!is.null(mtry_grid) && length(mtry_grid) > 1) {
      
      err_mat <- data.frame(mtry = mtry_grid, oob_mse = NA_real_)
      
      for (k in seq_along(mtry_grid)) {
        rf_tmp <- ranger(
          formula = rf_formula,
          data = newdata,
          num.trees = num.trees_tune,
          mtry = mtry_grid[k],
          min.node.size = min.node.size,
          oob.error = TRUE,
          importance = "none",
          write.forest = FALSE,  
          keep.inbag = FALSE,
          num.threads = num.threads,
          respect.unordered.factors = "partition",  
          save.memory = TRUE     
        )
        err_mat$oob_mse[k] <- rf_tmp$prediction.error
        rm(rf_tmp); gc(FALSE)
      }
      
      mtry_star <- err_mat$mtry[which.min(err_mat$oob_mse)]
      err_trace[[iter]] <- err_mat
      
    } else {
      if (!is.null(mtry_grid) && length(mtry_grid) == 1) {
        mtry_star <- min(as.integer(mtry_grid), p)
      } else {
        mtry_star <- floor(sqrt(p))  # fallback sensato
      }
      err_trace[[iter]] <- data.frame(mtry = mtry_star, oob_mse = NA_real_)
    }
    
    rf_iter <- ranger(
      formula = rf_formula,
      data = newdata,
      num.trees = num.trees_iter,    
      mtry = mtry_star,
      min.node.size = min.node.size,
      oob.error = TRUE,
      importance = "none",           
      write.forest = TRUE,          
      keep.inbag = FALSE,
      num.threads = num.threads,
      respect.unordered.factors = "partition",
      save.memory = TRUE
    )
    
    rf_pred <- predict(rf_iter, data = newdata)$predictions
    newdata$resid <- y - rf_pred
    
    #lme sui res
    m_lme <- lmer(
      formula = as.formula(paste0("resid ~ 1", random)),
      data = newdata,
      REML = REML,
      control = lmer.control
    )
    
    newlik <- as.numeric(logLik(m_lme))
    continue <- ((newlik - oldlik) > err_tol) && (iter < max_iter)
    oldlik <- newlik
    
    ran_eff <- predict(m_lme, newdata) - predict(m_lme, newdata, re.form = ~0)
    y_star <- y - ran_eff
    
    rm(rf_iter); gc(FALSE)
  }
  
  
  #foresta grande UNA VOLTA SOLA, con y_* finale
  newdata$y_star <- y_star
  rf_formula <- as.formula(paste("y_star ~", pred_rhs))
  
  rf_final <- ranger(
    formula = rf_formula,
    data = newdata,
    num.trees = num.trees_final,   
    mtry = mtry_star,
    min.node.size = min.node.size,
    oob.error = TRUE,
    importance = "impurity",    
    write.forest = TRUE,
    keep.inbag = FALSE,
    num.threads = num.threads,
    respect.unordered.factors = "partition",
    save.memory = TRUE
  )
  
  list(
    RandomForest   = rf_final,
    EffectModel    = m_lme,
    IterationsUsed = iter,
    logLik         = logLik(m_lme),
    error_trace    = err_trace[seq_len(iter)],
    mtry_last      = mtry_star,
    Formula        = formula,
    Random         = random,
    data           = data
  )
}


## METBOOST----
get_leaf_id <- function(tree, newdata) {
  #pred_where <- predict(tree, newdata = newdata, type = "where")
  #as.character(pred_where)
  tree_party <- as.party(tree)
  pred_where <- predict(tree_party, newdata = newdata, type = "node")
  as.character(pred_where)
}



make_leaf_factor <- function(tree, data) {
  leaf_id <- get_leaf_id(tree, data)
  factor(leaf_id)
}

make_group_folds_balanced <- function(dat, group_var, K = 4) {
  g <- as.character(dat[[group_var]])
  grp_sizes <- sort(table(g), decreasing = TRUE)
  grp_names <- names(grp_sizes)
  
  fold_load <- rep(0, K)
  grp_to_fold <- integer(length(grp_names))
  names(grp_to_fold) <- grp_names
  
  for (gr in grp_names) {
    k_star <- which.min(fold_load)
    grp_to_fold[gr] <- k_star
    fold_load[k_star] <- fold_load[k_star] + as.numeric(grp_sizes[gr])
  }
  
  grp_to_fold[g]
}

metboost_fit_path_manual <- function(train,
                                     valid = NULL,
                                     y_name = "y",
                                     vars_x,
                                     group_var,
                                     M_max = 200,
                                     eval_rounds = c(250, 500, 750, 1000),
                                     depth = 3,
                                     shrinkage = 0.05,
                                     bag_fraction = 0.5,
                                     minsplit = 20,
                                     cp = 0.001) {
  
  
  train <- as.data.frame(train)
  train[[group_var]] <- factor(train[[group_var]])
  
  y <- train[[y_name]]
  n <- nrow(train)
  
  init_value <- mean(y)
  
  f_hat <- rep(init_value, n)
  g_hat <- rep(0, n)
  r <- y - f_hat
  
  #liste
  trees <- vector("list", M_max)
  lmes  <- vector("list", M_max)
  leaf_levels_list <- vector("list", M_max)
  
  use_valid <- !is.null(valid)
  if (use_valid) {
    valid <- as.data.frame(valid)
    valid[[group_var]] <- factor(valid[[group_var]], levels = levels(train[[group_var]]))
    pred_valid <- rep(init_value, nrow(valid))
    eval_rounds <- sort(unique(eval_rounds))
    mae_valid <- rep(NA_real_, length(eval_rounds))
  }
  
  for (m in 1:M_max) {
    
    cat(m, "su", M_max, "\n")
    
    idx_sub <- sample(seq_len(n), size = max(2, floor(bag_fraction * n)), replace = FALSE)
    train_sub <- train[idx_sub, , drop = FALSE]
    r_sub <- r[idx_sub]
    
    tree_dat <- train_sub[, vars_x, drop = FALSE]
    tree_dat$.r <- r_sub
    
    tree_formula <- as.formula(
      paste(".r ~", paste(vars_x, collapse = " + "))
    )
    
    tree_m <- rpart(
      formula = tree_formula,
      data = tree_dat,
      method = "anova",
      control = rpart.control(
        maxdepth = depth,
        minsplit = minsplit,
        cp = cp
      )
    )
    numero_split_effettivi <- max(tree_m$cptable[, "nsplit"])
    print(numero_split_effettivi)
    
    leaf_factor_train <- make_leaf_factor(tree_m, train)
    leaf_levels_list[[m]] <- levels(leaf_factor_train)
    
    lme_dat <- data.frame(
      r_prev = r,
      leaf = leaf_factor_train,
      group = train[[group_var]]
    )
    
    lme_m <- lmer(
      r_prev ~ 0 + leaf + (0 + leaf || group),
      data = lme_dat,
      REML = TRUE,
      control = lmerControl(
        optimizer = "bobyqa",
        optCtrl = list(maxfun = 1e6)
      )
    )
    
    pred_fixed_m <- predict(lme_m, newdata = lme_dat, re.form = ~0)
    pred_total_m <- predict(lme_m, newdata = lme_dat, re.form = NULL)
    pred_random_m <- pred_total_m - pred_fixed_m
    
    f_hat <- f_hat + shrinkage * pred_fixed_m
    g_hat <- g_hat + shrinkage * pred_random_m
    r <- y - f_hat - g_hat
    
    trees[[m]] <- tree_m
    lmes[[m]]  <- lme_m
    
    if (use_valid) {
      leaf_valid <- factor(get_leaf_id(tree_m, valid), levels = levels(leaf_factor_train))
      
      pred_dat_valid <- data.frame(
        leaf = leaf_valid,
        group = valid[[group_var]]
      )
      
      pred_fixed_valid <- predict(
        lme_m,
        newdata = pred_dat_valid,
        re.form = ~0,
        allow.new.levels = TRUE
      )
      
      pred_total_valid <- predict(
        lme_m,
        newdata = pred_dat_valid,
        re.form = NULL,
        allow.new.levels = TRUE
      )
      
      pred_random_valid <- pred_total_valid - pred_fixed_valid
      
      pred_valid <- pred_valid + shrinkage * (pred_fixed_valid + pred_random_valid)
      
      if (m %in% eval_rounds) {
        j <- which(eval_rounds == m)
        mae_valid[j] <- mae(valid[[y_name]], pred_valid)
      }
    }
    
    if (m %% 100 == 0) {
      cat("Iterazione", m, "\n")
    }
  }
  
  out <- list(
    trees = trees,
    lmes = lmes,
    leaf_levels = leaf_levels_list,
    vars_x = vars_x,
    group_var = group_var,
    y_name = y_name,
    M = M_max,
    depth = depth,
    shrinkage = shrinkage,
    init_value = init_value,
    fitted = f_hat + g_hat,
    fitted_fixed = f_hat,
    fitted_random = g_hat,
    train_groups = levels(train[[group_var]])
  )
  
  if (use_valid) {
    out$valid_path <- data.frame(
      M = eval_rounds,
      MAE_val = mae_valid
    )
  }
  
  out
}

#previsioni
metboost_predict_manual <- function(fit, newdata) {
  
  newdata <- as.data.frame(newdata)
  newdata[[fit$group_var]] <- factor(newdata[[fit$group_var]], levels = fit$train_groups)
  
  n_new <- nrow(newdata)
  pred_fixed_total  <- rep(fit$init_value, n_new)
  pred_random_total <- rep(0, n_new)
  
  for (m in 1:fit$M) {
    tree_m <- fit$trees[[m]]
    lme_m  <- fit$lmes[[m]]
    
    leaf_new <- get_leaf_id(tree_m, newdata)
    leaf_new <- factor(leaf_new, levels = fit$leaf_levels[[m]])
    
    pred_dat <- data.frame(
      leaf = leaf_new,
      group = newdata[[fit$group_var]]
    )
    
    pred_fixed_m <- predict(
      lme_m,
      newdata = pred_dat,
      re.form = ~0,
      allow.new.levels = TRUE
    )
    
    pred_total_m <- predict(
      lme_m,
      newdata = pred_dat,
      re.form = NULL,
      allow.new.levels = TRUE
    )
    
    pred_random_m <- pred_total_m - pred_fixed_m
    
    pred_fixed_total  <- pred_fixed_total  + fit$shrinkage * pred_fixed_m
    pred_random_total <- pred_random_total + fit$shrinkage * pred_random_m
  }
  
  list(
    pred = pred_fixed_total + pred_random_total,
    pred_fixed = pred_fixed_total,
    pred_random = pred_random_total
  )
}

metboost_pdp <- function(fit, data_ref, var_name,
                         grid = NULL,
                         n_grid = 40,
                         trim_q = c(0.02, 0.98)) {
  
  stopifnot(var_name %in% names(data_ref))
  
  x <- data_ref[[var_name]]
  
  if (is.null(grid)) {
    if (is.numeric(x)) {
      qx <- quantile(x, probs = trim_q, na.rm = TRUE)
      grid <- seq(qx[1], qx[2], length.out = n_grid)
    } else {
      grid <- sort(unique(x))
    }
  }
  
  pd_vals <- numeric(length(grid))
  
  for (i in seq_along(grid)) {
    dat_tmp <- data_ref
    
    dat_tmp[[var_name]] <- grid[i]
    
    pred_tmp <- metboost_predict_manual(fit, dat_tmp)$pred
    pd_vals[i] <- mean(pred_tmp, na.rm = TRUE)
  }
  
  data.frame(
    x = grid,
    y = pd_vals,
    variabile = var_name,
    stringsAsFactors = FALSE
  )
}

