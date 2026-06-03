rm(list=ls());gc();
setwd("C:/Users/Utente/OneDrive/Universita/Magistrale/2025-2026/Aziendali/Progetto/Analisi")

load("dati_puliti.Rdata")

source("C:/Users/Utente/OneDrive/Universita/Magistrale/2025-2026/Aziendali/Labs/Lab7/utils.R")

MSE <- function(previsioni, veri_valori){
  mean( (previsioni - veri_valori)^2 )
}

plot_tree <- function(albero, file_name = NULL){
  # Se viene fornito un nome file, attiva il dispositivo corretto
  if(!is.null(file_name)){
    png(file = paste0(file_name, ".png"), width = 1800, height = 1200, res = 200)
  }
  
  # Logica del grafico (immutata)
  plot(albero, col = "gray40", lwd = 1.8)
  text(albero, pretty = 4, digits = 3, cex = 0.8, col = "darkblue", font = 2)
  
  if(!is.null(file_name)) {
    dev.off()
  }
  dev.off()
}

# Regressione sul prezzo ----

stima <- dataset_regressione_prezzo[id_stima,]
verifica <- dataset_regressione_prezzo[id_verifica,]

X_stima <- model.matrix(y~., stima)[,-1] |> as.data.frame()
X_verifica <- model.matrix(y~., verifica)[,-1] |> as.data.frame()

colnames(X_stima) <- colnames(X_stima) |> 
  gsub(pattern=" ", replacement="_")
colnames(X_verifica) <- colnames(X_stima) |> 
  gsub(pattern=" ", replacement="_")

set.seed(1)
idx_a <- sample(1:nrow(X_stima), ceiling(NROW(X_stima)*2/3))
idx_b <- setdiff(1:nrow(X_stima), idx_a)

## CART ----

library(tree)
m_tree_full <- tree(y~., data=stima,
                    split='deviance',
                    control=
                      tree.control(nobs=NROW(stima),
                                   minsize=1,
                                   mindev=0.0001))
sum(m_tree_full$frame$var == "<leaf>")

max_size <- 50
K_cv <- 20
set.seed(1)
fold <- sample(1:K_cv, NROW(stima), replace = T)
matrice_errori_albero = matrix(nrow=K_cv, ncol=max_size)
rownames(matrice_errori_albero) <- paste("fold", 1:K_cv)
for (j in 1:K_cv){
  id_stima = which(fold != j)
  id_verifica = which(fold == j)
  
  m_tree_tmp = tree(y~.,
                    data=stima[id_stima,],
                    split='deviance',
                    control=
                      tree.control(nobs=NROW(stima),
                                   minsize=1,
                                   mindev=0.0001))
  m_tree_tmp
  
  tree_list = lapply(2:(max_size+1), # per noi dopo i 50 split é sovra-adattato
                     # non uso il numero di split m_tree_full perché puó
                     # essere che alcuni alberi siano piú piccoli
                     function(l) prune.tree(m_tree_tmp,
                                            best=l) )
  pred_list = lapply(tree_list,
                     function(x) predict(x,
                                         stima[id_verifica,]))
  lista_errori_CI = lapply(pred_list,
                           function(x) MSE(x, stima$y[id_verifica]))
  
  matrice_errori_albero[j,] = lista_errori_CI |> unlist()
}
save(matrice_errori_albero, file="Modelli_stimati/err_tree_prezzo.RData")
load("Modelli_stimati/err_tree_prezzo.RData")
dim(matrice_errori_albero)

errore_albero_CI = apply(matrice_errori_albero, 2, mean)
se_albero <- apply(matrice_errori_albero, 2, function(x) sd(x) / sqrt(K_cv))

B = which.min(errore_albero_CI)

cv_min <- errore_albero_CI[B]
se_min <- se_albero[B]

soglia_1se <- cv_min + se_min

taglie_valide <- which(errore_albero_CI <= soglia_1se)
B_1se <- max(2, min(taglie_valide))

x11(); plot(errore_albero_CI, type='l', lwd=2)
abline(v=B, col=2, lwd=3)

m_tree = prune.tree(m_tree_full, best=B_1se)

#x11();plot_tree(m_tree, "Plot/albero_prezzo")

yhat_tree <- predict(m_tree, newdata = verifica)
err_tree <-  mean((verifica$y - yhat_tree)^2)

prezzo_medio_verifica <- mean(verifica$y)
devianza_totale_verifica <- sum((verifica$y - prezzo_medio_verifica)^2)
devianza_residua_albero <- sum((verifica$y - yhat_tree)^2)

r2_tree <- 1 - (devianza_residua_albero / devianza_totale_verifica)

modelli_prezzo <- data.frame(albero = r2_tree)
modelli_prezzo

## Random forest -----

n_threads <- parallel::detectCores()
library(ranger)
NCOL(stima) |> sqrt(); NCOL(stima) |> log();
mtry_values <- c(2,4,6,8,10)
num_tree_values <- c(50, 100, 200, 300, 400)
griddina <- expand.grid(mtry=mtry_values,
                        n_tree=num_tree_values)
modelli_rf <- list()
set.seed(1)
for(i in 1:NROW(griddina)){
  cat(i, "su", NROW(griddina), "\n\n")
  modelli_rf[[i]] <- ranger(y~., num.trees = griddina[i,2],
                            mtry=griddina[i,1],
                            data=stima, importance="none",
                            num.threads=n_threads)
}
save(modelli_rf, file="Modelli_stimati/err_rf.RData")
load("Modelli_stimati/err_rf.RData")

errori_OOB <- vapply(modelli_rf, function(x) x$prediction.error, FUN.VALUE = numeric(1))
datini <- data.frame(n_tree = griddina$n_tree,
  mtry = as.factor(griddina$mtry), errore_OOB = errori_OOB)

library(ggplot2)
ggplot(datini, aes(x = n_tree, y = errore_OOB, color = mtry, group = mtry)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) + 
  scale_color_viridis_d(option = "viridis", end = 0.85) + 
  scale_x_continuous(breaks = num_tree_values) + 
  theme_minimal(base_size = 11) + 
  labs(
    title = "Tuning Random Forest: Errore Out-of-Bag",
    x = "Numero di alberi",
    y = "Errore OOB",
    color = "Variabili\ncandidate" 
  ) +
  theme(plot.title = element_text(face = "bold", size = 13),
        panel.grid.minor = element_blank())

ntree_rf <- 300
rf_best <- ranger(y~.,
                  num.trees = ntree_rf, mtry = 4,
                  data=stima, importance="permutation",
                  num.threads=n_threads)

p_rf = predict(rf_best, verifica)
devianza_residua_rf <- sum((verifica$y - p_rf$predictions)^2)

r2_rf <- 1 - (devianza_residua_rf / devianza_totale_verifica)

modelli_prezzo$RF <- r2_rf
modelli_prezzo

### Importanza ----

tmp <- sort(rf_best$variable.importance, decr=T) |> 
  head(10)
dati_rf <- data.frame(nomi = names(tmp), v = tmp)
colnames(dati_rf)

dati_rf$nomi <- factor(dati_rf$nomi, levels = rev(dati_rf$nomi))

ggplot(dati_rf, aes(x = nomi, y = v, fill = v)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Top 20 Variabili per Importanza",
       x = NULL,
       y = "Importanza",
       colour = "Numero di esplicative selezionate") +
  theme_minimal() +
  theme(legend.position = "none")
ggsave("Plot/Importanza_RF_prezzo.png")

## GBM -----

nus <- c(0.001, 0.005, 0.01) # shrinkage
dps <- c(15, 18, 20, 22, 25, 28) # profondità
nts_xgb <- 10000

tuning_par <- expand.grid(nus, dps)
err_gbm <- rep(1, nrow(tuning_par))

library(gbm)
set.seed(123)
# modello di prova per capire se uso un numero corretto di alberi
mod_prova <- gbm(stima$y~., data=X_stima,
                 distribution = "gaussian", n.trees = nts_xgb,
                 shrinkage = tuning_par[1,1],
                 interaction.depth = tuning_par[1,2])

yhat_mp <- predict(mod_prova, newdata = X_verifica, n.trees=1:nts_xgb)
err_pr <- apply(yhat_mp, 2, function(p) mean((verifica$y - p)^2))
plot(err_pr, type="l")
abline(h=min(err_pr), col=2)
abline(v=which.min(err_pr), col=2, lty=2, lwd=2)
abline(v=100, col=3, lty=2, lwd=2)

err_pr[450] / err_pr[100]
err_pr[which.min(err_pr)] / err_pr[100]

nts_xgb <- 2500

### Tuning ----

set.seed(1)
for(i in 1:nrow(tuning_par)){
  mod <- gbm(stima$y[idx_a]~., data=X_stima[idx_a,],
             distribution = "gaussian",
             shrinkage = tuning_par[i,1], n.trees = nts_xgb,
             interaction.depth = tuning_par[i,2])
  passo <- max(30, 1000 * (0.5 - tuning_par[i,1])/(dps/2))
  alberi_pred <- seq(100, nts_xgb, by = passo)
  ps <- predict(mod, newdata = X_stima[idx_b,], n.trees = alberi_pred)
  err_i <- apply(ps, 2, function(p) mean(((stima$y)[idx_b] - p)^2))
  err_gbm[i] <- min(err_i)
  cat(round(i/nrow(tuning_par)*100,2), "%\n")
}
save(err_gbm, file="Modelli_stimati/err_gbm.RData")
load("Modelli_stimati/err_gbm.RData")
prezzo_medio_idxb <- mean(stima$y[idx_b])
devianza_totale_idxb <- sum((stima$y[idx_b] - prezzo_medio_idxb)^2)

err_gbm <- 1 - (err_gbm*length(idx_b)) / devianza_totale_idxb

zmat <- matrix(err_gbm, nrow = length(nus), ncol = length(dps), byrow = TRUE)

par_opt <- tuning_par[which.max(err_gbm),] # un parametro di shrinkage troppo elevato mi fa
par_opt
cbind(tuning_par, err_gbm)

risultati_tuning <- data.frame(tuning_par)
colnames(risultati_tuning) <- c("shrinkage", "depth")
risultati_tuning$err_gbm <- err_gbm  # Aggiungiamo l'errore calcolato nel ciclo

library(ggplot2)
library(viridis)

ggplot(risultati_tuning, aes(x = factor(depth), y = factor(shrinkage))) +
  geom_tile(aes(fill = err_gbm), color = "white", linewidth = 0.2) + 
  geom_tile(data = risultati_tuning[which.max(risultati_tuning$err_gbm), ], 
            aes(x = factor(depth), y = factor(shrinkage)), 
            fill = NA, color = "black", linewidth = 1.2) +
  scale_fill_viridis_c(option = "plasma", direction = 1, name = "R2") + 
  labs(
    title = "Tuning dei Parametri GBM",
    subtitle = "Il riquadro nero indica la combinazione con l'R2 minimo",
    x = "Profondità", 
    y = "Shrinkage"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "gray40", size = 10),
    axis.text = element_text(size = 10),
    panel.grid = element_blank() 
  )
ggsave("Plot/tuning_gbm_prezzo.png")

### Fit finale ----

mod_gbm <- gbm(stima$y[idx_a]~., data=X_stima[idx_a,],
               n.trees = nts_xgb, distribution = "gaussian",
               shrinkage = par_opt[1],
               interaction.depth = par_opt[2])

ps <- predict(mod_gbm, newdata = X_stima[idx_b,],
              n.trees = seq(60, nts_xgb, by=20))
err_i <- apply(ps, 2, function(p) mean((stima$y[idx_b] - p)^2))
n_trees_best <- names(err_i)[which.min(err_i)] |> as.numeric()
n_trees_best

yhat_gbm <- predict(mod_gbm, newdata = X_verifica,
                    n.trees = n_trees_best)
devianza_residua_gbm <- sum((verifica$y - yhat_gbm)^2)

r2_gbm <- 1 - (devianza_residua_gbm / devianza_totale_verifica)

modelli_prezzo$GBM <- r2_gbm
modelli_prezzo

colnames(X_stima)
dim(X_stima)

plot(mod_gbm, i.var=1, n.trees = n_trees_best)
plot(mod_gbm, i.var=2, n.trees = n_trees_best)
plot(mod_gbm, i.var=3, n.trees = n_trees_best)
plot(mod_gbm, i.var=32, n.trees = n_trees_best)
plot(mod_gbm, i.var=34, n.trees = n_trees_best)
plot(mod_gbm, i.var=35, n.trees = n_trees_best)

## Effetti casuali -----

### MERT -----

form <- formula(lm(y~.,
                   data=stima |> dplyr::select(-Brand,
                                               -Condition)))

set.seed(123)
mert_model <- MERT(formula = form , random = " + (1|Brand) + (1|Condition)",
                   data = stima, cv=T)
options(max.print=100)
mert_model
options(max.print=1000)
summary(mert_model$EffectModel)

plot_tree(mert_model$Tree, "Plot/MERT_prezzo")

pred_tree <- predict(mert_model$Tree, newdata = verifica)
pred_ran <- predict(mert_model$EffectModel, newdata = verifica) 
pred_mert <- pred_tree + pred_ran

devianza_residua_mert <- sum((verifica$y - pred_mert)^2)

r2_mert <- 1 - (devianza_residua_mert / devianza_totale_verifica)

modelli_prezzo$mert <- r2_mert
modelli_prezzo

### MERF -----

set.seed(123)
merf_model <- MERF_ranger_safe(formula = form, data = stima,
                               random = " + (1|Brand)",
                               num.trees_final = ntree_rf, # stesso di prima
                               num.threads = n_threads-2,
                               mtry_grid = c(2,4,6,8)
)
merf_model$mtry_last

pred_fix <- predict(merf_model$RandomForest, data = verifica)$predictions
pred_ran <- predict(merf_model$EffectModel, newdata = verifica) - predict(merf_model$EffectModel, newdata = verifica, re.form = ~0)
pred_merf<- pred_fix + pred_ran

devianza_residua_merf <- sum((verifica$y - pred_merf)^2)
r2_merf <- 1 - (devianza_residua_merf / devianza_totale_verifica)

modelli_prezzo$merf <- r2_merf
modelli_prezzo

### Metboost -----

#x_var <- colnames(stima |> dplyr::select(-y, -Brand))
#set.seed(1)
#par_opt # del GBM classico
#met_model <- metboost_fit_path_manual(stima, y_name = "y", vars_x = x_var,
#                                      group_var = "Brand",
#                                      #M_max = nts_xgb,
#                                      M_max = 300,
#                                      shrinkage = 0.01, depth = 4)
# group_var effetti casuali
# M_max numero albero, prima ne avevamo 6000, ora per motivi computazionali scendiamo a 300
# inoltre ad ogni step devo stimare un albero con una certa profondità, più depth
# è elevato più ci metterà, quindi metto 2
# lo shrinkage era ottimo a 0.05, ma sappiamo che deve andare assieme al numero di
# alberi e alla profondità. Quindi, dato che fisso 2 come profondità, una scelta per
# bilanciare è di alzare il parametro di shrinkage

#save(met_model, file="met_model.Rdata")
#load("met_model.Rdata")
#
#met_model$lmes[[10]]@optinfo$conv$lme4$messages
#
#pdp_land_sf <- metboost_pdp(fit = met_model, data_ref = stima,
#                            var_name = "Size", n_grid = 40)
#plot(pdp_land_sf$x, pdp_land_sf$y, type="l", xlab = "Size",
#     ylab = "y", col = "blue")
#pdp_cond <- metboost_pdp(fit = met_model, data_ref = stima,
#                         var_name = "Condition")
#plot(pdp_cond$x, pdp_cond$y, pch=19)
#
#pred_met <- metboost_predict_manual(met_model, verifica)
#err_met  <-  mean((verifica$y - pred_met$pred)^2)
#modelli_prezzo$metboost <- err_met
#modelli_prezzo








# Regressione sui favoriti ----

stima <- dataset_regressione_favoriti[id_stima,]
verifica <- dataset_regressione_favoriti[id_verifica,]

X_stima <- model.matrix(y~., stima)[,-1] |> as.data.frame()
X_verifica <- model.matrix(y~., verifica)[,-1] |> as.data.frame()

set.seed(1)
idx_a <- sample(1:nrow(X_stima), ceiling(NROW(X_stima)*2/3))
idx_b <- setdiff(1:nrow(X_stima), idx_a)
