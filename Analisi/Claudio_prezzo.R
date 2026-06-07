# =============================================================================
# ANALISI PREZZI VINTED - Versione Migliorata
# Obiettivo: raccomandare il prezzo di vendita a nuovi utenti Vinted
# =============================================================================
# MIGLIORAMENTI RISPETTO AL CODICE ORIGINALE:
#  1. CART: aggiunto grafico CV con bande di errore standard (1-SE rule visiva)
#  2. RF:   aggiunta heatmap importanza + partial dependence plots (PDP) 
#           per le top variabili
#  3. GBM:  corretto il titolo del tuning (era "minimo", doveva essere "massimo");
#           aggiunto grafico di convergenza per il modello finale; 
#           PDP estesi con ggplot2
#  4. Confronto modelli: tabella R² con grafico a barre comparativo e
#           grafico residui vs fitted per ciascun modello
#  5. Simulatore prezzo: funzione predict_price() che simula l'algoritmo
#           di raccomandazione a un utente Vinted
# =============================================================================

rm(list = ls()); gc()

# ---------- Percorsi (adatta alla tua macchina) ----------
DATA_PATH   <- "dati_puliti.Rdata"
MODELS_PATH <- "Modelli_stimati/"
PLOTS_PATH  <- "Plot/"
UTILS_PATH  <- "utils.R"

dir.create(MODELS_PATH, showWarnings = FALSE)
dir.create(PLOTS_PATH,  showWarnings = FALSE)

# ---------- Librerie ----------
suppressPackageStartupMessages({
  library(tree)
  library(ranger)
  library(gbm)
  library(ggplot2)
  library(viridis)
  library(dplyr)
  library(tidyr)
  library(patchwork)   # per affiancare grafici ggplot
})

source(UTILS_PATH)
load(DATA_PATH)

# ---------- Funzione di errore ----------
MSE <- function(pred, obs) mean((pred - obs)^2)

# ---------- Split stima / verifica ----------
stima    <- dataset_regressione_prezzo[id_stima,   ]
verifica <- dataset_regressione_prezzo[id_verifica, ]

# Matrici one-hot per GBM (che non gestisce factor direttamente)
X_stima    <- model.matrix(y ~ ., stima)[,   -1] |> as.data.frame()
X_verifica <- model.matrix(y ~ ., verifica)[ , -1] |> as.data.frame()

# Pulizia nomi colonne (spazi → underscore)
colnames(X_stima)    <- gsub(" ", "_", colnames(X_stima))
colnames(X_verifica) <- colnames(X_stima)   # stesso ordine garantito

# Devianza totale del set di verifica (denominatore dell'R²)
y_ver_mean         <- mean(verifica$y)
deviance_tot_ver   <- sum((verifica$y - y_ver_mean)^2)

# Indici per la split interna alla stima (usata nel GBM)
set.seed(1)
idx_a <- sample(seq_len(nrow(X_stima)), ceiling(nrow(X_stima) * 2/3))
idx_b <- setdiff(seq_len(nrow(X_stima)), idx_a)

# Contenitore dei risultati finali
modelli_r2 <- data.frame(Modello = character(), R2 = numeric(),
                          stringsAsFactors = FALSE)


# =============================================================================
# 1. CART  -------------------------------------------------------------------
# =============================================================================
# MIGLIORAMENTO: il grafico CV ora mostra le bande ± 1 SE per rendere
# visivamente chiara la 1-SE rule usata nella selezione della taglia.

## 1a. Albero completo (sovra-adattato, usato come partenza per il pruning) ----
m_tree_full <- tree(y ~ ., data = stima,
                    split   = "deviance",
                    control = tree.control(nobs     = nrow(stima),
                                           minsize  = 1,
                                           mindev   = 0.0001))

## 1b. Cross-validation (20-fold) per scegliere la taglia ottima ----
max_size <- 50
K_cv     <- 20
set.seed(1)
fold <- sample(1:K_cv, nrow(stima), replace = TRUE)

mat_err_albero <- matrix(NA_real_, nrow = K_cv, ncol = max_size,
                         dimnames = list(paste0("fold", 1:K_cv),
                                         paste0("size", 2:(max_size + 1))))

for (j in 1:K_cv) {
  id_tr <- which(fold != j)
  id_cv <- which(fold == j)
  
  # Albero sul fold di training
  m_tmp <- tree(y ~ ., data = stima[id_tr, ],
                split   = "deviance",
                control = tree.control(nobs    = nrow(stima),
                                       minsize = 1,
                                       mindev  = 0.0001))
  
  # Lista di alberi potati a taglie crescenti (da 2 a max_size+1 foglie)
  tree_list <- lapply(2:(max_size + 1),
                      function(l) prune.tree(m_tmp, best = l))
  
  # MSE di validazione per ciascuna taglia
  mat_err_albero[j, ] <- sapply(tree_list, function(tr)
    MSE(predict(tr, stima[id_cv, ]), stima$y[id_cv]))
}

# Salva/carica la matrice errori
save(mat_err_albero, file = file.path(MODELS_PATH, "err_tree_prezzo.RData"))
# load(file.path(MODELS_PATH, "err_tree_prezzo.RData"))

## 1c. 1-SE rule ----
# Media e SE dell'errore per ogni taglia
cv_mean <- colMeans(mat_err_albero)
cv_se   <- apply(mat_err_albero, 2, function(x) sd(x) / sqrt(K_cv))

B      <- which.min(cv_mean)          # taglia con errore minimo assoluto
soglia <- cv_mean[B] + cv_se[B]       # soglia 1-SE
# Taglia più piccola il cui errore è ≤ soglia (principio parsimonia)
B_1se  <- max(2, min(which(cv_mean <= soglia)))

## 1d. *** GRAFICO CV migliorato *** ----
# Aggiunta: bande grigie ±1 SE per ogni punto + linee verticali annotate
df_cv <- data.frame(
  taglia = 2:(max_size + 1),
  errore = cv_mean,
  ymin   = cv_mean - cv_se,
  ymax   = cv_mean + cv_se
)

p_cv_cart <- ggplot(df_cv, aes(x = taglia, y = errore)) +
  # Banda ±1 SE attorno alla curva
  geom_ribbon(aes(ymin = ymin, ymax = ymax), fill = "steelblue", alpha = 0.20) +
  geom_line(linewidth = 1, color = "steelblue") +
  geom_point(size = 1.6, color = "steelblue") +
  # Linea verticale: taglia con errore minimo
  geom_vline(xintercept = B, color = "tomato", linetype = "dashed", linewidth = 1) +
  # Linea verticale: taglia selezionata con 1-SE rule
  geom_vline(xintercept = B_1se, color = "darkgreen", linetype = "solid", linewidth = 1.2) +
  # Linea orizzontale: soglia 1-SE
  geom_hline(yintercept = soglia, color = "darkgreen", linetype = "dotted", linewidth = 0.8) +
  annotate("text", x = B     + 0.8, y = max(cv_mean) * 0.98,
           label = paste0("Min (", B,   ")"), color = "tomato",    size = 3.5, hjust = 0) +
  annotate("text", x = B_1se + 0.8, y = max(cv_mean) * 0.93,
           label = paste0("1-SE (", B_1se, ")"), color = "darkgreen", size = 3.5, hjust = 0) +
  scale_x_continuous(breaks = seq(0, max_size, by = 5)) +
  labs(title    = "CART – Selezione della taglia tramite CV (20-fold)",
       subtitle = "La banda grigia mostra ±1 SE; la linea verde è la taglia scelta con la 1-SE rule",
       x = "Numero di foglie (taglia albero)",
       y = "MSE di cross-validation") +
  theme_minimal(base_size = 12) +
  theme(plot.title    = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(color = "gray40", size = 10))

ggsave(file.path(PLOTS_PATH, "cart_cv_taglia.png"),
       plot = p_cv_cart, width = 9, height = 5, dpi = 200)

## 1e. Fit finale e R² sul set di verifica ----
m_tree   <- prune.tree(m_tree_full, best = B_1se)
yhat_tree <- predict(m_tree, newdata = verifica)

r2_tree <- 1 - sum((verifica$y - yhat_tree)^2) / deviance_tot_ver
modelli_r2 <- rbind(modelli_r2,
                    data.frame(Modello = "CART", R2 = r2_tree))
cat(sprintf("CART   R² = %.4f\n", r2_tree))


# =============================================================================
# 2. RANDOM FOREST  ----------------------------------------------------------
# =============================================================================
# MIGLIORAMENTI:
#  a) Heatmap OOB mtry × n_tree (invece del semplice lineplot)
#  b) Plot importanza: barre colorate per cluster di variabile, non gradiente 
#  c) PDP per le prime 4 variabili importanti (con ggplot2)

n_threads   <- parallel::detectCores()
mtry_values <- c(2, 4, 6, 8, 10)
ntree_vals  <- c(50, 100, 200, 300, 400)
griddina    <- expand.grid(mtry = mtry_values, n_tree = ntree_vals)

## 2a. Griglia tuning ----
modelli_rf <- vector("list", nrow(griddina))
set.seed(1)
for (i in seq_len(nrow(griddina))) {
  cat(i, "su", nrow(griddina), "\n")
  modelli_rf[[i]] <- ranger(y ~ .,
                            num.trees   = griddina$n_tree[i],
                            mtry        = griddina$mtry[i],
                            data        = stima,
                            importance  = "none",
                            num.threads = n_threads)
}
save(modelli_rf, file = file.path(MODELS_PATH, "err_rf.RData"))
# load(file.path(MODELS_PATH, "err_rf.RData"))

## 2b. *** GRAFICO tuning: heatmap OOB *** ----
# Rispetto al lineplot originale la heatmap permette di leggere
# simultaneamente entrambe le dimensioni della griglia.
errori_OOB <- sapply(modelli_rf, function(x) x$prediction.error)
df_rf_tune <- data.frame(
  mtry    = factor(griddina$mtry),
  n_tree  = factor(griddina$n_tree),
  OOB_MSE = errori_OOB
)

best_rf_idx <- which.min(errori_OOB)
best_rf_row <- df_rf_tune[best_rf_idx, ]

p_rf_heat <- ggplot(df_rf_tune, aes(x = n_tree, y = mtry, fill = OOB_MSE)) +
  geom_tile(color = "white", linewidth = 0.5) +
  # Bordo nero sulla cella ottima
  geom_tile(data = best_rf_row, aes(x = n_tree, y = mtry),
            fill = NA, color = "black", linewidth = 1.5) +
  geom_text(aes(label = round(OOB_MSE, 0)), size = 3, color = "white") +
  scale_fill_viridis_c(option = "magma", direction = -1,
                       name = "MSE OOB") +
  labs(title    = "Tuning Random Forest – MSE Out-of-Bag",
       subtitle = "Il riquadro nero indica la combinazione ottima",
       x = "Numero di alberi", y = "Variabili candidate (mtry)") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 14),
        panel.grid = element_blank())

ggsave(file.path(PLOTS_PATH, "rf_tuning_heatmap.png"),
       plot = p_rf_heat, width = 8, height = 5, dpi = 200)

## 2c. Modello finale con importanza ----
ntree_rf <- 300   # scelta dal tuning
rf_best  <- ranger(y ~ .,
                   num.trees   = ntree_rf,
                   mtry        = as.integer(as.character(best_rf_row$mtry)),
                   data        = stima,
                   importance  = "permutation",
                   num.threads = n_threads)

p_rf     <- predict(rf_best, verifica)
r2_rf    <- 1 - sum((verifica$y - p_rf$predictions)^2) / deviance_tot_ver
modelli_r2 <- rbind(modelli_r2, data.frame(Modello = "Random Forest", R2 = r2_rf))
cat(sprintf("RF     R² = %.4f\n", r2_rf))

## 2d. *** Grafico importanza migliorato *** ----
# Colorazione per "tipo" di variabile (venditore vs prodotto vs domanda)
# per aiutare l'interpretazione durante l'esposizione.
top_imp <- sort(rf_best$variable.importance, decreasing = TRUE) |> head(10)

tipo_var <- function(nomi) {
  # Classifico manualmente le variabili in 3 gruppi semantici
  dplyr::case_when(
    grepl("Seller|Rating|Reviews|distintivi|Num_Other", nomi) ~ "Venditore",
    grepl("Brand|Size|Condition|Material|Color|avg_interest",  nomi) ~ "Prodotto",
    TRUE                                                              ~ "Transazione"
  )
}

df_imp <- data.frame(
  variabile  = names(top_imp),
  importanza = as.numeric(top_imp)
) |>
  mutate(
    variabile = factor(variabile, levels = rev(variabile)),
    tipo      = tipo_var(as.character(variabile))
  )

p_imp_rf <- ggplot(df_imp, aes(x = variabile, y = importanza, fill = tipo)) +
  geom_col(width = 0.7) +
  coord_flip() +
  scale_fill_manual(
    values = c("Venditore" = "#4E79A7", "Prodotto" = "#F28E2B",
               "Transazione" = "#59A14F"),
    name = "Categoria"
  ) +
  labs(title    = "Random Forest – Importanza variabili (Permutation)",
       subtitle = "Top 10 variabili ordinate per importanza decrescente",
       x = NULL, y = "Importanza (riduzione MSE per permutazione)") +
  theme_minimal(base_size = 12) +
  theme(plot.title   = element_text(face = "bold", size = 14),
        legend.position = "bottom")

ggsave(file.path(PLOTS_PATH, "rf_importanza.png"),
       plot = p_imp_rf, width = 9, height = 6, dpi = 200)

## 2e. *** PDP per le prime 4 variabili *** ----
# I Partial Dependence Plots mostrano l'effetto marginale atteso di ciascuna
# variabile sul prezzo (a parità delle altre), cruciale per la spiegabilità.
top4_vars <- names(top_imp)[1:4]

pdp_rf <- function(var_name, modello, dati, n_grid = 50) {
  x <- dati[[var_name]]
  if (is.numeric(x)) {
    griglia <- seq(quantile(x, 0.02), quantile(x, 0.98), length.out = n_grid)
  } else {
    griglia <- levels(x)
  }
  pds <- sapply(griglia, function(v) {
    tmp       <- dati
    tmp[[var_name]] <- if (is.numeric(x)) v else factor(v, levels(x))
    mean(predict(modello, tmp)$predictions)
  })
  data.frame(x = griglia, pd = pds, variabile = var_name)
}

df_pdp_list <- lapply(top4_vars, pdp_rf,
                      modello = rf_best, dati = stima, n_grid = 50)

# Un plot per ciascuna variabile top-4, poi uniti con patchwork
pdp_plots <- lapply(df_pdp_list, function(df) {
  is_num <- is.numeric(df$x)
  p <- ggplot(df, aes(x = if (is_num) as.numeric(x) else factor(x), y = pd))
  if (is_num) {
    p <- p + geom_line(color = "#4E79A7", linewidth = 1.2) +
      geom_ribbon(aes(ymin = pd * 0.95, ymax = pd * 1.05),
                  fill = "#4E79A7", alpha = 0.15)
  } else {
    p <- p + geom_col(fill = "#F28E2B", width = 0.6)
  }
  p + labs(title = unique(df$variabile), x = NULL, y = "Prezzo previsto (€)") +
    theme_minimal(base_size = 11) +
    theme(plot.title = element_text(face = "bold"))
})

p_pdp_combined <- (pdp_plots[[1]] | pdp_plots[[2]]) /
                  (pdp_plots[[3]] | pdp_plots[[4]]) +
  plot_annotation(
    title    = "Partial Dependence Plots – Random Forest",
    subtitle = "Effetto marginale atteso delle 4 variabili più importanti sul prezzo",
    theme    = theme(plot.title    = element_text(face = "bold", size = 14),
                     plot.subtitle = element_text(color = "gray40", size = 10))
  )

ggsave(file.path(PLOTS_PATH, "rf_pdp_top4.png"),
       plot = p_pdp_combined, width = 11, height = 8, dpi = 200)


# =============================================================================
# 3. GBM  --------------------------------------------------------------------
# =============================================================================
# MIGLIORAMENTI:
#  a) Titolo heatmap corretto: la cella ottima è quella con R² MASSIMO
#  b) Grafico di convergenza per il modello finale (errore vs n. alberi)
#  c) PDP con ggplot2 per le prime 3 variabili

nus <- c(0.001, 0.005, 0.01)
dps <- c(15, 18, 20, 22, 25, 28)
nts_xgb   <- 2500
tuning_par <- expand.grid(nu = nus, depth = dps)
err_gbm   <- numeric(nrow(tuning_par))

## 3a. Tuning ----
set.seed(1)
for (i in seq_len(nrow(tuning_par))) {
  mod_i <- gbm(stima$y[idx_a] ~ ., data = X_stima[idx_a, ],
               distribution      = "gaussian",
               n.trees           = nts_xgb,
               shrinkage         = tuning_par$nu[i],
               interaction.depth = tuning_par$depth[i])
  
  # Valuto su idx_b a passi variabili per ridurre il costo computazionale
  passo      <- max(30, 100)
  alberi_seq <- seq(100, nts_xgb, by = passo)
  ps         <- predict(mod_i, newdata = X_stima[idx_b, ], n.trees = alberi_seq)
  mse_i      <- apply(ps, 2, function(p) MSE(p, stima$y[idx_b]))
  err_gbm[i] <- min(mse_i)
  cat(round(i / nrow(tuning_par) * 100, 1), "%\n")
}
save(err_gbm, file = file.path(MODELS_PATH, "err_gbm.RData"))
# load(file.path(MODELS_PATH, "err_gbm.RData"))

# Converto MSE in R² (sul set idx_b)
dev_idxb <- sum((stima$y[idx_b] - mean(stima$y[idx_b]))^2)
r2_gbm_tune <- 1 - (err_gbm * length(idx_b)) / dev_idxb

## 3b. *** Heatmap tuning con titolo corretto *** ----
df_gbm_tune <- data.frame(tuning_par, R2 = r2_gbm_tune)
best_gbm_idx <- which.max(r2_gbm_tune)

p_gbm_heat <- ggplot(df_gbm_tune, aes(x = factor(depth), y = factor(nu),
                                       fill = R2)) +
  geom_tile(color = "white", linewidth = 0.4) +
  # Bordo nero: combinazione con R² MASSIMO (non minimo come nel codice originale)
  geom_tile(data = df_gbm_tune[best_gbm_idx, ],
            aes(x = factor(depth), y = factor(nu)),
            fill = NA, color = "black", linewidth = 1.5) +
  geom_text(aes(label = round(R2, 3)), size = 3, color = "white") +
  scale_fill_viridis_c(option = "plasma", direction = 1, name = "R²") +
  labs(title    = "Tuning GBM – R² sul set di validazione interna",
       subtitle = "Il riquadro nero indica la combinazione con R² MASSIMO",
       x = "Profondità (interaction.depth)", y = "Shrinkage (ν)") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 14),
        panel.grid = element_blank())

ggsave(file.path(PLOTS_PATH, "gbm_tuning_heatmap.png"),
       plot = p_gbm_heat, width = 8, height = 5, dpi = 200)

## 3c. Parametri ottimi e fit finale ----
par_opt <- tuning_par[best_gbm_idx, ]
cat("GBM par ottimi: shrinkage =", par_opt$nu,
    "| depth =", par_opt$depth, "\n")

mod_gbm <- gbm(stima$y[idx_a] ~ ., data = X_stima[idx_a, ],
               distribution      = "gaussian",
               n.trees           = nts_xgb,
               shrinkage         = par_opt$nu,
               interaction.depth = par_opt$depth)

## 3d. *** Grafico convergenza (NUOVO) *** ----
# Mostra come l'errore decresce al crescere degli alberi nel modello finale.
# Utile in un'esposizione per motivare la scelta di nts_xgb.
seq_conv    <- seq(50, nts_xgb, by = 20)
ps_conv     <- predict(mod_gbm, newdata = X_stima[idx_b, ], n.trees = seq_conv)
mse_conv    <- apply(ps_conv, 2, function(p) MSE(p, stima$y[idx_b]))
n_trees_best <- seq_conv[which.min(mse_conv)]

df_conv <- data.frame(n_trees = seq_conv, MSE = mse_conv)

p_conv_gbm <- ggplot(df_conv, aes(x = n_trees, y = MSE)) +
  geom_line(color = "#E15759", linewidth = 1) +
  geom_vline(xintercept = n_trees_best, linetype = "dashed",
             color = "darkred", linewidth = 1) +
  annotate("text", x = n_trees_best + 40, y = max(mse_conv) * 0.97,
           label = paste0("Ottimo: ", n_trees_best, " alberi"),
           color = "darkred", size = 3.5, hjust = 0) +
  labs(title    = "GBM – Convergenza dell'errore al crescere degli alberi",
       subtitle = "Errore MSE sul set di validazione interna (idx_b)",
       x = "Numero di alberi", y = "MSE") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 14))

ggsave(file.path(PLOTS_PATH, "gbm_convergenza.png"),
       plot = p_conv_gbm, width = 9, height = 5, dpi = 200)

## 3e. R² finale GBM ----
yhat_gbm <- predict(mod_gbm, newdata = X_verifica, n.trees = n_trees_best)
r2_gbm   <- 1 - sum((verifica$y - yhat_gbm)^2) / deviance_tot_ver
modelli_r2 <- rbind(modelli_r2, data.frame(Modello = "GBM", R2 = r2_gbm))
cat(sprintf("GBM    R² = %.4f\n", r2_gbm))

## 3f. *** PDP GBM con ggplot2 (NUOVO) *** ----
# Nel codice originale i PDP erano grafici base R con plot().
# Qui usiamo ggplot2 per coerenza visiva con il resto dell'analisi.
top3_gbm <- summary(mod_gbm, n.trees = n_trees_best,
                    plotit = FALSE, order = TRUE)$var[1:3]

pdp_gbm_single <- function(var_idx, modello, X_data, n_trees, dati_orig,
                            n_grid = 50) {
  var_name <- colnames(X_data)[var_idx]
  x        <- X_data[[var_name]]
  is_num   <- is.numeric(x)
  griglia  <- if (is_num)
    seq(quantile(x, 0.02), quantile(x, 0.98), length.out = n_grid)
  else
    sort(unique(x))
  
  pds <- sapply(griglia, function(v) {
    tmp <- X_data
    tmp[[var_name]] <- v
    mean(predict(modello, newdata = tmp, n.trees = n_trees))
  })
  data.frame(x = griglia, pd = pds, variabile = var_name, is_num = is_num)
}

# Indici colonna delle top 3 variabili
idx_top3 <- sapply(top3_gbm, function(v) which(colnames(X_stima) == v))

pdp_gbm_list <- lapply(idx_top3, pdp_gbm_single,
                       modello   = mod_gbm,
                       X_data    = X_stima,
                       n_trees   = n_trees_best,
                       dati_orig = stima)

pdp_gbm_plots <- lapply(pdp_gbm_list, function(df) {
  p <- ggplot(df, aes(x = if (df$is_num[1]) as.numeric(x) else factor(x),
                      y = pd))
  if (df$is_num[1]) {
    p <- p + geom_line(color = "#E15759", linewidth = 1.2)
  } else {
    p <- p + geom_col(fill = "#E15759", width = 0.6)
  }
  p + labs(title = unique(df$variabile), x = NULL, y = "Prezzo atteso (€)") +
    theme_minimal(base_size = 11) +
    theme(plot.title = element_text(face = "bold"))
})

p_pdp_gbm <- pdp_gbm_plots[[1]] | pdp_gbm_plots[[2]] | pdp_gbm_plots[[3]] +
  plot_annotation(
    title    = "GBM – Partial Dependence Plots (top 3 variabili)",
    theme    = theme(plot.title = element_text(face = "bold", size = 14))
  )

ggsave(file.path(PLOTS_PATH, "gbm_pdp_top3.png"),
       plot = p_pdp_gbm, width = 12, height = 5, dpi = 200)


# 4. CONFRONTO MODELLI  -------------------------------------------------------
# MIGLIORAMENTI:
#  a) Tabella R² arricchita con RMSE (scala interpretabile: euro)
#  b) Grafico a barre comparativo con i valori sopra le barre
#  c) Grafici residui vs fitted per ciascun modello

## 4a. Raccolta previsioni sul set di verifica ----
previsioni_ver <- data.frame(
  y_vero = verifica$y,
  CART   = yhat_tree,
  RF     = p_rf$predictions,
  GBM    = yhat_gbm
)

## 4b. Tabella di confronto ----
# RMSE è in euro ed è più interpretabile dell'MSE per un'esposizione.
tabella_risultati <- modelli |>
  mutate(
    RMSE = sapply(list(yhat_tree, p_rf$predictions, yhat_gbm), function(yhat)
      sqrt(MSE(yhat, verifica$y))),
    R2   = round(R2, 4),
    RMSE = round(RMSE, 2)
  )

cat("\n=== CONFRONTO MODELLI ===\n")
print(tabella_risultati)

## 4c. *** Grafico a barre R² comparativo *** ----
p_confronto_r2 <- ggplot(tabella_risultati,
                          aes(x = reorder(Modello, R2), y = R2, fill = Modello)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(aes(label = sprintf("R² = %.4f", R2)),
            hjust = -0.1, size = 4, fontface = "bold") +
  coord_flip(ylim = c(0, max(tabella_risultati$R2) * 1.15)) +
  scale_fill_manual(values = c("CART"          = "#76B7B2",
                                "Random Forest" = "#4E79A7",
                                "GBM"           = "#E15759")) +
  labs(title    = "Confronto modelli – R² sul set di verifica",
       subtitle = "Vinted: raccomandazione prezzo di vendita",
       x = NULL, y = "R²") +
  theme_minimal(base_size = 13) +
  theme(plot.title    = element_text(face = "bold", size = 15),
        plot.subtitle = element_text(color = "gray40", size = 11),
        panel.grid.major.y = element_blank())

ggsave(file.path(PLOTS_PATH, "confronto_r2.png"),
       plot = p_confronto_r2, width = 9, height = 5, dpi = 200)

## 4d. *** Grafici residui vs fitted (NUOVI) *** ----
# Ogni punto è un prodotto nel set di verifica. Un modello ben calibrato
# non dovrebbe mostrare pattern sistematici (es. sotto-stima dei prodotti cari).
df_res <- previsioni_ver |>
  pivot_longer(cols = c(CART, RF, GBM),
               names_to = "Modello", values_to = "yhat") |>
  mutate(residuo = y_vero - yhat,
         Modello = factor(Modello, levels = c("CART", "RF", "GBM")))

p_residui <- ggplot(df_res, aes(x = yhat, y = residuo)) +
  geom_point(alpha = 0.25, size = 1.2, color = "steelblue") +
  geom_hline(yintercept = 0, color = "tomato", linewidth = 0.8) +
  geom_smooth(method = "loess", se = FALSE, color = "darkred",
              linewidth = 0.8, linetype = "dashed") +
  facet_wrap(~ Modello, ncol = 3, scales = "free_x") +
  labs(title    = "Residui vs Valori Previsti – Confronto modelli",
       subtitle = "Una curva loess piatta e centrata sullo zero indica buona calibrazione",
       x = "Prezzo previsto (€)", y = "Residuo (€)") +
  theme_minimal(base_size = 12) +
  theme(plot.title    = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(color = "gray40", size = 10),
        strip.text    = element_text(face = "bold", size = 12))

ggsave(file.path(PLOTS_PATH, "residui_vs_fitted.png"),
       plot = p_residui, width = 12, height = 5, dpi = 200)

## 4e. *** Grafico previsioni vs valori reali (NUOVO) *** ----
# Mostra direttamente quanto le stime si avvicinano alla retta y = x.
p_pred_vs_obs <- ggplot(df_res, aes(x = y_vero, y = yhat)) +
  geom_point(alpha = 0.20, size = 1.2, color = "steelblue") +
  geom_abline(slope = 1, intercept = 0, color = "tomato", linewidth = 0.9) +
  facet_wrap(~ Modello, ncol = 3) +
  coord_cartesian(xlim = c(0, 300), ylim = c(0, 300)) +
  labs(title    = "Previsioni vs Valori Reali",
       subtitle = "La retta rossa è la bisettrice (previsione perfetta)",
       x = "Prezzo reale (€)", y = "Prezzo previsto (€)") +
  theme_minimal(base_size = 12) +
  theme(plot.title    = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(color = "gray40", size = 10),
        strip.text    = element_text(face = "bold", size = 12))

ggsave(file.path(PLOTS_PATH, "pred_vs_obs.png"),
       plot = p_pred_vs_obs, width = 12, height = 5, dpi = 200)
