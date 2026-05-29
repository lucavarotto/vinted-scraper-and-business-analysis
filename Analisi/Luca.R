rm(list=ls());gc();
setwd("C:/Users/Utente/OneDrive/Universita/Magistrale/2025-2026/Aziendali/Progetto/Analisi")

load("dati_puliti.Rdata")

source("C:/Users/Utente/OneDrive/Universita/Magistrale/2025-2026/Aziendali/Labs/Lab7/utils.R")

# Regressione sul prezzo ----

stima <- dataset_regressione_prezzo[id_stima,]
verifica <- dataset_regressione_prezzo[id_verifica,]

X_stima <- model.matrix(y~., stima)[,-1] |> as.data.frame()
X_verifica <- model.matrix(y~., verifica)[,-1] |> as.data.frame()

## GBM -----

nus <- c(0.001, 0.005, 0.01, 0.1) # shrinkage
dps <- c(3, 5, 10, 12, 15, 18) # profondità
nts_xgb <- 1000

tuning_par <- expand.grid(nus, dps)
err <- 1:nrow(tuning_par)

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

nts_xgb <- 600

### Tuning ----

set.seed(1)

idx_a <- sample(1:nrow(X_stima), ceiling(NROW(X_stima)*2/3))
idx_b <- setdiff(1:nrow(X_stima), idx_a)

for(i in 1:nrow(tuning_par)){
  set.seed(i)
  mod <- gbm(stima$y[idx_a]~., data=X_stima[idx_a,],
             distribution = "gaussian",
             shrinkage = tuning_par[i,1], n.trees = nts_xgb,
             interaction.depth = tuning_par[i,2])
  ps <- predict(mod, newdata = X_stima[idx_b,], n.trees = seq(60, nts_xgb, by=20))
  err_i <- apply(ps, 2, function(p) mean(((stima$y)[idx_b] - p)^2))
  err[i] <- min(err_i)
  cat(round(i/nrow(tuning_par)*100,2), "%\n")
}
#save(err, file="Modelli_stimati/err_gbm.RData")
#load("Modelli_stimati/err_gbm.RData")
zmat <- matrix(err, nrow = length(nus), ncol = length(dps), byrow = TRUE)

par_opt <- tuning_par[which.min(err),] # un parametro di shrinkage troppo elevato mi fa
par_opt
cbind(tuning_par, err)

risultati_tuning <- data.frame(tuning_par)
colnames(risultati_tuning) <- c("shrinkage", "depth")
risultati_tuning$err <- err  # Aggiungiamo l'errore calcolato nel ciclo

library(ggplot2)
library(viridis)

ggplot(risultati_tuning, aes(x = factor(depth), y = factor(shrinkage))) +
  geom_tile(aes(fill = err), color = "white", linewidth = 0.2) + 
  geom_tile(data = risultati_tuning[which.min(risultati_tuning$err), ], 
            aes(x = factor(depth), y = factor(shrinkage)), 
            fill = NA, color = "black", linewidth = 1.2) +
  scale_fill_viridis_c(option = "plasma", direction = -1, name = "MSE") + 
  labs(
    title = "Tuning dei Parametri GBM",
    subtitle = "Il riquadro azzurro indica la combinazione con il MSE minimo",
    x = "Profondità (depth)", 
    y = "Shrinkage"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "gray40", size = 10),
    axis.text = element_text(size = 10),
    panel.grid = element_blank() 
  )

### fit finale ----

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
err_gbm  <-  mean((verifica$y - yhat_gbm)^2)

err_gbm / var(verifica$y)

modelli_prezzo <- data.frame(GBM = err_gbm)
modelli_prezzo

plot(mod_gbm, i.var=1, n.trees = n_trees_best)

## CART ----

library(tree)
t1 = tree(y~., data = stima[idx_a,], 
          control = tree.control(nobs = NROW(idx_a),
                                 minsize = 2,
                                 mindev = 0.001))

t1_error = prune.tree(t1, newdata = stima[idx_b,])
plot(t1_error)
cbind(t1_error$dev/1000000, t1_error$size)
J = t1_error$size[which.min(t1_error$dev)]
abline(v = J, col = 2, lwd =2, lty=2)

t1_best = prune.tree(t1, best = J)

x11()
plot(t1_best, col = "gray40", # Grigio scuro, più elegante del nero
     lwd = 1.8) # Linee più spesse
text(t1_best, pretty = 4, # Nomi delle variabili per esteso
     digits = 3, # Arrotondamento a 3 cifre
     cex = 0.8, # Font leggermente più piccolo per evitare sovrapposizioni
     col = "darkblue", # Testo blu scuro ben visibile
     font = 2) # Grassetto

yhat_tree <- predict(t1_best, newdata = verifica)
err_tree <-  mean((verifica$y - yhat_tree)^2)

modelli_prezzo$albero <- err_tree
modelli_prezzo
dev.off()

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
                            data=stima,
                            num.threads=n_threads)
}
#save(modelli_rf, file="Modelli_stimati/err_rf.RData")
#load("Modelli_stimati/err_rf.RData")

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

datini2 <- datini |>
  group_by(n_tree) |>
  summarise(err=mean(errore_OOB))
with(datini2,
     plot(x=n_tree, y=err,
          xlab="numero alberi", ylab="errore OOB",
          main="errore OOB nelle RF"))

ntree <- 300
best_id <- which(griddina$mtry==4 & griddina$n_tree==ntree)
rf_best <- modelli_rf[[ best_id ]]

p_rf = predict(rf_best, verifica)
err_rf <-  mean((verifica$y - p_rf$predictions)^2)

modelli_prezzo$RF <- err_rf
modelli_prezzo

## Effetti casuali -----

### MERT -----

form <- formula(lm(y~., data=stima |> dplyr::select(-Brand)))

set.seed(123)
mert_model <- MERT(formula = form , random = " + (1|Brand)",
                   data = stima, cv=T)
print(mert_model$Tree)
summary(mert_model$EffectModel)

pred_tree <- predict(mert_model$Tree, newdata = verifica)
pred_ran <- predict(mert_model$EffectModel, newdata = verifica) 
pred_mert <- pred_tree + pred_ran
err_mert <- mean((verifica$y - pred_mert)^2)
modelli_prezzo$mert <- err_mert
modelli_prezzo

### MERF -----

set.seed(123)
merf_model <- MERF_ranger_safe(formula = form, data = stima,
                               random = " + (1|Brand)",
                               num.trees_final = ntree, # stesso di prima
                               num.threads = n_threads-2,
                               mtry_grid = c(2,4,6,8,10)
)

pred_fix <- predict(merf_model$RandomForest, data = verifica)$predictions
pred_ran <- predict(merf_model$EffectModel, newdata = verifica) - predict(merf_model$EffectModel, newdata = verifica, re.form = ~0)
pred_merf<- pred_fix + pred_ran
err_merf <-  mean((verifica$y - pred_merf)^2)
modelli_prezzo$merf <- err_merf
modelli_prezzo

### Metboost -----

x_var <- colnames(stima |> dplyr::select(-y, -Brand))
set.seed(1)
par_opt # del GBM classico
met_model <- metboost_fit_path_manual(stima, y_name = "y", vars_x = x_var,
                                      group_var = "Brand",
                                      #M_max = nts_xgb,
                                      M_max = 10,
                                      shrinkage = 0.01, depth = 4)
# group_var effetti casuali
# M_max numero albero, prima ne avevamo 6000, ora per motivi computazionali scendiamo a 300
# inoltre ad ogni step devo stimare un albero con una certa profondità, più depth
# è elevato più ci metterà, quindi metto 2
# lo shrinkage era ottimo a 0.05, ma sappiamo che deve andare assieme al numero di
# alberi e alla profondità. Quindi, dato che fisso 2 come profondità, una scelta per
# bilanciare è di alzare il parametro di shrinkage

#save(met_model, file="met_model.Rdata")
load("met_model.Rdata")

met_model$lmes[[10]]@optinfo$conv$lme4$messages

pdp_land_sf <- metboost_pdp(fit = met_model, data_ref = stima,
                            var_name = "Size", n_grid = 40)
plot(pdp_land_sf$x, pdp_land_sf$y, type="l", xlab = "Size",
     ylab = "y", col = "blue")
pdp_cond <- metboost_pdp(fit = met_model, data_ref = stima,
                         var_name = "Condition")
plot(pdp_cond$x, pdp_cond$y, pch=19)

pred_met <- metboost_predict_manual(met_model, verifica)
err_met  <-  mean((verifica$y - pred_met$pred)^2)
modelli_prezzo$metboost <- err_met
modelli_prezzo








# Regressione sui favoriti ----

stima <- dataset_regressione_favoriti[id_stima,]
verifica <- dataset_regressione_favoriti[id_verifica,]

X_stima <- model.matrix(y~., stima)[,-1] |> as.data.frame()
X_verifica <- model.matrix(y~., verifica)[,-1] |> as.data.frame()

## GBM -----

nus <- c(0.005, 0.01, 0.1) # shrinkage
dps <- c(3, 5, 10, 12, 15, 18) # profondità
nts_xgb <- 1000

tuning_par <- expand.grid(nus, dps)
err <- 1:nrow(tuning_par)

library(gbm)
set.seed(123)
# modello di prova per capire se uso un numero corretto di alberi
mod_prova <- gbm(stima$y~., data=X_stima,
                 distribution = "gaussian", n.trees = nts_xgb,
                 shrinkage = tuning_par[10,1],
                 interaction.depth = tuning_par[10,2])

yhat_mp <- predict(mod_prova, newdata = X_verifica, n.trees=1:nts_xgb)
err_pr <- apply(yhat_mp, 2, function(p) mean((verifica$y - p)^2))
plot(err_pr, type="l")
abline(h=min(err_pr), col=2)
abline(v=which.min(err_pr), col=2, lty=2, lwd=2)
abline(v=100, col=3, lty=2, lwd=2)

err_pr[450] / err_pr[100]
err_pr[which.min(err_pr)] / err_pr[100]

nts_xgb <- 500

### Tuning ----

set.seed(1)

idx_a <- sample(1:nrow(X_stima), ceiling(NROW(X_stima)*2/3))
idx_b <- setdiff(1:nrow(X_stima), idx_a)

for(i in 1:nrow(tuning_par)){
  set.seed(i)
  mod <- gbm(stima$y[idx_a]~., data=X_stima[idx_a,],
             distribution = "gaussian",
             shrinkage = tuning_par[i,1], n.trees = nts_xgb,
             interaction.depth = tuning_par[i,2])
  ps <- predict(mod, newdata = X_stima[idx_b,], n.trees = seq(60, nts_xgb, by=20))
  err_i <- apply(ps, 2, function(p) mean(((stima$y)[idx_b] - p)^2))
  err[i] <- min(err_i)
  cat(round(i/nrow(tuning_par)*100,2), "%\n")
}
#save(err, file="Modelli_stimati/err_gbm_favoriti.RData")
#load("Modelli_stimati/err_gbm_favoriti.RData")
zmat <- matrix(err, nrow = length(nus), ncol = length(dps), byrow = TRUE)

par_opt <- tuning_par[which.min(err),] # un parametro di shrinkage troppo elevato mi fa
par_opt
cbind(tuning_par, err)

risultati_tuning <- data.frame(tuning_par)
colnames(risultati_tuning) <- c("shrinkage", "depth")
risultati_tuning$err <- err # Aggiungiamo l'errore calcolato nel ciclo

library(ggplot2)
library(viridis)

ggplot(risultati_tuning, aes(x = factor(depth), y = factor(shrinkage))) +
  geom_tile(aes(fill = err), color = "white", linewidth = 0.2) + 
  geom_tile(data = risultati_tuning[which.min(risultati_tuning$err), ], 
            aes(x = factor(depth), y = factor(shrinkage)), 
            fill = NA, color = "black", linewidth = 1.2) +
  scale_fill_viridis_c(option = "plasma", direction = -1, name = "MSE") + 
  labs(
    title = "Tuning dei Parametri GBM",
    subtitle = "Il riquadro azzurro indica la combinazione con il MSE minimo",
    x = "Profondità (depth)", 
    y = "Shrinkage"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "gray40", size = 10),
    axis.text = element_text(size = 10),
    panel.grid = element_blank() 
  )

### fit finale ----

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
err_gbm  <-  mean((verifica$y - yhat_gbm)^2)

modelli <- data.frame(GBM = err_gbm)
modelli

plot(mod_gbm, i.var=1, n.trees = n_trees_best)
plot(mod_gbm, i.var=6, n.trees = n_trees_best)
plot(mod_gbm, i.var=10, n.trees = n_trees_best)

## CART ----

library(tree)
t1 = tree(y~., data = stima[idx_a,], 
          control = tree.control(nobs = NROW(idx_a),
                                 minsize = 2,
                                 mindev = 0.001))

t1_error = prune.tree(t1, newdata = stima[idx_b,])
plot(t1_error)
cbind(t1_error$dev/1000000, t1_error$size)
J = t1_error$size[which.min(t1_error$dev)]
abline(v = J, col = 2, lwd =2, lty=2)

t1_best = prune.tree(t1, best = J)

x11()
plot(t1_best, col = "gray40", # Grigio scuro, più elegante del nero
     lwd = 1.8) # Linee più spesse
text(t1_best, pretty = 4, # Nomi delle variabili per esteso
     digits = 3, # Arrotondamento a 3 cifre
     cex = 0.8, # Font leggermente più piccolo per evitare sovrapposizioni
     col = "darkblue", # Testo blu scuro ben visibile
     font = 2) # Grassetto

yhat_tree <- predict(t1_best, newdata = verifica)
err_tree <-  mean((verifica$y - yhat_tree)^2)

modelli$albero <- err_tree
modelli
dev.off()

## Random forest -----

n_threads <- parallel::detectCores()
library(ranger)
NCOL(stima) |> sqrt(); NCOL(stima) |> log();
mtry_values <- c(2,4,6,8)
num_tree_values <- c(200, 400, 600, 800, 1000)
griddina <- expand.grid(mtry=mtry_values,
                        n_tree=num_tree_values)
modelli_rf <- list()
set.seed(1)
for(i in 1:NROW(griddina)){
  cat(i, "su", NROW(griddina), "\n\n")
  modelli_rf[[i]] <- ranger(y~., num.trees = griddina[i,2],
                            mtry=griddina[i,1],
                            data=stima,
                            num.threads=n_threads)
}
#save(modelli_rf, file="Modelli_stimati/err_rf_favoriti.RData")
#load("Modelli_stimati/err_rf_favoriti.RData")

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

datini2 <- datini |>
  group_by(n_tree) |>
  summarise(err=mean(errore_OOB))
with(datini2,
     plot(x=n_tree, y=err,
          xlab="numero alberi", ylab="errore OOB",
          main="errore OOB nelle RF"))

ntree <- 600
best_id <- which(griddina$mtry==2 & griddina$n_tree==ntree)
rf_best <- modelli_rf[[ best_id ]]

p_rf = predict(rf_best, verifica)
err_rf <-  mean((verifica$y - p_rf$predictions)^2)

modelli$RF <- err_rf
modelli

## Modelli non parametrici con effetti casuali -----

### MERT -----

form <- formula(lm(y~., data=stima |> dplyr::select(-Brand)))

set.seed(123)
mert_model <- MERT(formula = form , random = " + (1|Brand)",
                   data = stima)

pred_tree <- predict(mert_model$Tree, newdata = verifica)
pred_ran <- predict(mert_model$EffectModel, newdata = verifica) 
pred_mert <- pred_tree + pred_ran
err_mert <- mean((verifica$y - pred_mert)^2)
modelli$mert <- err_mert
modelli

### MERF -----

set.seed(123)
merf_model <- MERF_ranger_safe(formula = form, data = stima,
                               random = " + (1|Brand)",
                               num.trees_final = ntree, #stesso di prima
                               num.threads = n_threads-2,
                               mtry_grid = c(2,4,6,8,10)
)

pred_fix <- predict(merf_model$RandomForest, data = verifica)$predictions
pred_ran <- predict(merf_model$EffectModel, newdata = verifica) - predict(merf_model$EffectModel, newdata = verifica, re.form = ~0)
pred_merf<- pred_fix + pred_ran
err_merf <-  mean((verifica$y - pred_merf)^2)
modelli$merf <- err_merf
modelli

### Metboost -----

x_var <- colnames(stima |> dplyr::select(-y, -Brand))
set.seed(1)
par_opt # del GBM classico
met_model <- metboost_fit_path_manual(stima, y_name = "y", vars_x = x_var,
                                      group_var = "Brand",
                                      #M_max = nts_xgb,
                                      M_max = 10,
                                      shrinkage = 0.01, depth = 4)
# group_var effetti casuali
# M_max numero albero, prima ne avevamo 6000, ora per motivi computazionali scendiamo a 300
# inoltre ad ogni step devo stimare un albero con una certa profondità, più depth
# è elevato più ci metterà, quindi metto 2
# lo shrinkage era ottimo a 0.05, ma sappiamo che deve andare assieme al numero di
# alberi e alla profondità. Quindi, dato che fisso 2 come profondità, una scelta per
# bilanciare è di alzare il parametro di shrinkage

#save(met_model, file="met_model.Rdata")
load("met_model.Rdata")

met_model$lmes[[10]]@optinfo$conv$lme4$messages

pdp_land_sf <- metboost_pdp(fit = met_model, data_ref = stima,
                            var_name = "Size", n_grid = 40)
plot(pdp_land_sf$x, pdp_land_sf$y, type="l", xlab = "Size",
     ylab = "y", col = "blue")
pdp_cond <- metboost_pdp(fit = met_model, data_ref = stima,
                         var_name = "Condition")
plot(pdp_cond$x, pdp_cond$y, pch=19)

pred_met <- metboost_predict_manual(met_model, verifica)
err_met  <-  mean((verifica$y - pred_met$pred)^2)
modelli$metboost <- err_met
modelli