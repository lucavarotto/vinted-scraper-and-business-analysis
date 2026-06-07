# Prezzo par ----

rm(list=ls());gc();
load("dati_puliti.Rdata")

library(MASS)
library(tidyverse)
library(car)
library(sandwich)
library(lmtest)
library(lme4)
library(lmerTest)
library(ggplot2)
library(dplyr)

train_p <- dataset_regressione_prezzo[id_stima, ]
test_p  <- dataset_regressione_prezzo[id_verifica, ]


modello_completo_p <- lm(y ~ ., data = train_p)
modello_nullo_p    <- lm(y ~ 1, data = train_p)

modello_step_p <- step(modello_completo_p,
                       scope     = list(lower = modello_nullo_p,
                                        upper = modello_completo_p),
                       direction = "both", trace = 1)
summary(modello_step_p)

pred_p    <- predict(modello_step_p, newdata = test_p)
residui_p <- test_p$y - pred_p

rmse_p <- sqrt(mean(residui_p^2))
mae_p  <- mean(abs(residui_p))
r2_p   <- cor(test_p$y, pred_p)^2

cat("\n Metriche Test Set (MLR Price grezzo) \n")
cat(sprintf("R²:   %.4f\n", r2_p))
cat(sprintf("RMSE: %.4f\n", rmse_p))
cat(sprintf("MAE:  %.4f\n", mae_p))

par(mfrow = c(2, 2)); plot(modello_step_p); par(mfrow = c(1, 1))
# Problema: eteroschedasticità + residui non normali → passiamo a log(y)


dataset_regressione_logprezzo <- dataset_regressione_prezzo %>%
  mutate(y = log(y))

train_lp <- dataset_regressione_logprezzo[id_stima, ]
test_lp  <- dataset_regressione_logprezzo[id_verifica, ]

hist(train_lp$y, breaks = 40,
     main = "Distribuzione LogPrice (training set)",
     xlab = "log(Price)", col = "#2C7BB6", border = "black")

modello_completo_lp <- lm(y ~ ., data = train_lp)
modello_nullo_lp    <- lm(y ~ 1, data = train_lp)

modello_step_lp <- step(modello_completo_lp,
                        scope     = list(lower = modello_nullo_lp,
                                         upper = modello_completo_lp),
                        direction = "both", trace = 1)
summary(modello_step_lp)

par(mfrow = c(2, 2)); plot(modello_step_lp); par(mfrow = c(1, 1))

# Metriche su log-scala
pred_lp    <- predict(modello_step_lp, newdata = test_lp)
residui_lp <- test_lp$y - pred_lp

rmse_lp <- sqrt(mean(residui_lp^2))
mae_lp  <- mean(abs(residui_lp))
r2_lp   <- cor(test_lp$y, pred_lp)^2

# Riconversione in euro
pred_euro  <- exp(pred_lp)
reale_euro <- exp(test_lp$y)
rmse_euro  <- sqrt(mean((reale_euro - pred_euro)^2))
mae_euro   <- mean(abs(reale_euro - pred_euro))

cat("\n Confronto MLR Price vs LogPrice \n")
cat(sprintf("%-20s %10s %10s\n", "Metrica", "Price", "LogPrice"))
cat(sprintf("%-20s %10.4f %10.4f\n", "R²",       r2_p,   r2_lp))
cat(sprintf("%-20s %10.4f %10.4f\n", "RMSE (€)", rmse_p, rmse_euro))
cat(sprintf("%-20s %10.4f %10.4f\n", "MAE  (€)", mae_p,  mae_euro))


vif(modello_step_lp)
# Se tutti i VIF sono vicini a 1 → no multicollinearità



robusto <- coeftest(modello_step_lp,
                    vcov = vcovHC(modello_step_lp, type = "HC3"))
print(robusto)

se_ols     <- summary(modello_step_lp)$coefficients[, 2]
se_robusti <- sqrt(diag(vcovHC(modello_step_lp, type = "HC3")))

confronto_se <- data.frame(
  Coefficiente = names(se_ols),
  SE_OLS       = round(se_ols, 4),
  SE_Robusto   = round(se_robusti, 4),
  Variazione   = round((se_robusti - se_ols) / se_ols * 100, 1)
) %>% arrange(desc(abs(Variazione)))
print(confronto_se)

ic_robusti <- coefci(modello_step_lp,
                     vcov  = vcovHC(modello_step_lp, type = "HC3"),
                     level = 0.95)

# Effetti % con IC robusti
coef_step <- coef(modello_step_lp)
ic_low    <- ic_robusti[, 1]
ic_high   <- ic_robusti[, 2]

variabili_chiave <- c("Is_BoostedTRUE", "Has_Item_VerificationTRUE",
                      "Size", "Condition.L", "Materialnoto",
                      "Favorites_Count", "Shipping_Cost",
                      "BrandMarche di Lusso", "Brandignoto",
                      "Num_Other_Items", "avg_interest_global_last_year",
                      "Seller_Rating_ClassOttimo (4.7 - 4.9)",
                      "Seller_Rating_ClassPerfetto (5.0)")

cat(sprintf("\n%-40s %10s %10s %10s\n", "Variabile", "Effetto%", "IC_low%", "IC_high%"))
for (v in variabili_chiave) {
  if (v %in% names(coef_step)) {
    eff  <- (exp(coef_step[v]) - 1) * 100
    low  <- (exp(ic_low[v])  - 1) * 100
    high <- (exp(ic_high[v]) - 1) * 100
    cat(sprintf("%-40s %9.1f%% %9.1f%% %9.1f%%\n", v, eff, low, high))
  }
}



modello_causale <- lm(y ~ Brand + Size + Condition + Material +
                        Shipping_Cost + Seller_Rating_Class +
                        Num_Other_Items + avg_interest_global_last_year,
                      data = train_lp)

summary(modello_causale)
par(mfrow = c(2, 2)); plot(modello_causale); par(mfrow = c(1, 1))

robusto_causale <- coeftest(modello_causale,
                            vcov = vcovHC(modello_causale, type = "HC3"))
print(robusto_causale)

pred_causale    <- predict(modello_causale, newdata = test_lp)
residui_causale <- test_lp$y - pred_causale

r2_causale   <- cor(test_lp$y, pred_causale)^2
mae_causale  <- mean(abs(residui_causale))
rmse_causale <- sqrt(mean(residui_causale^2))

# Confronto coefficienti completo vs causale
coef_step    <- coef(modello_step_lp)
coef_causale <- coef(modello_causale)
vars_comuni  <- intersect(names(coef_step), names(coef_causale))

confronto_coef <- data.frame(
  Variabile      = vars_comuni,
  Coef_Completo  = round(coef_step[vars_comuni], 4),
  Coef_Causale   = round(coef_causale[vars_comuni], 4),
  Variazione_pct = round((coef_causale[vars_comuni] - coef_step[vars_comuni]) /
                           abs(coef_step[vars_comuni]) * 100, 1)
) %>% arrange(desc(abs(Variazione_pct)))
print(confronto_coef)

# Confronto metriche completo vs causale
pred_euro_step    <- exp(predict(modello_step_lp, newdata = test_lp))
pred_euro_causale <- exp(predict(modello_causale, newdata = test_lp))
reale_euro        <- exp(test_lp$y)

cat("\n  Confronto Completo vs Causale \n")
cat(sprintf("%-20s %10s %10s\n", "Metrica", "Completo", "Causale"))
cat(sprintf("%-20s %10.4f %10.4f\n", "R²",
            r2_lp, r2_causale))
cat(sprintf("%-20s %10.4f %10.4f\n", "RMSE (€)",
            sqrt(mean((reale_euro - pred_euro_step)^2)),
            sqrt(mean((reale_euro - pred_euro_causale)^2))))
cat(sprintf("%-20s %10.4f %10.4f\n", "MAE  (€)",
            mean(abs(reale_euro - pred_euro_step)),
            mean(abs(reale_euro - pred_euro_causale))))


K <- max(fold_id)

risultati_cv_lp <- data.frame(
  fold         = 1:K,
  r2_completo  = NA, mae_completo = NA,
  r2_causale   = NA, mae_causale  = NA
)

for (k in 1:K) {
  cat(sprintf("\nFold %d/%d...", k, K))

  train_k <- dataset_regressione_logprezzo[fold_id != k, ]
  test_k  <- dataset_regressione_logprezzo[fold_id == k, ]

  mod_comp_k <- lm(y ~ Brand + Size + Condition + Material +
                     Favorites_Count + Shipping_Cost + Is_Boosted +
                     Has_Item_Verification + Seller_Rating_Class +
                     Num_Other_Items + avg_interest_global_last_year,
                   data = train_k)
  pred_comp_k <- predict(mod_comp_k, newdata = test_k)
  risultati_cv_lp$r2_completo[k]  <- cor(test_k$y, pred_comp_k)^2
  risultati_cv_lp$mae_completo[k] <- mean(abs(test_k$y - pred_comp_k))

  mod_caus_k <- lm(y ~ Brand + Size + Condition + Material +
                     Shipping_Cost + Seller_Rating_Class +
                     Num_Other_Items + avg_interest_global_last_year,
                   data = train_k)
  pred_caus_k <- predict(mod_caus_k, newdata = test_k)
  risultati_cv_lp$r2_causale[k]  <- cor(test_k$y, pred_caus_k)^2
  risultati_cv_lp$mae_causale[k] <- mean(abs(test_k$y - pred_caus_k))
}

cat("\n\n  Riepilogo Cross-Validation \n")
cat(sprintf("%-20s %10s %10s\n", "Metrica", "Completo", "Causale"))
cat(sprintf("%-20s %10.4f %10.4f\n", "R² medio",
            mean(risultati_cv_lp$r2_completo), mean(risultati_cv_lp$r2_causale)))
cat(sprintf("%-20s %10.4f %10.4f\n", "R² SD",
            sd(risultati_cv_lp$r2_completo), sd(risultati_cv_lp$r2_causale)))
cat(sprintf("%-20s %10.4f %10.4f\n", "MAE medio (log)",
            mean(risultati_cv_lp$mae_completo), mean(risultati_cv_lp$mae_causale)))
cat(sprintf("%-20s %10.4f %10.4f\n", "MAE SD",
            sd(risultati_cv_lp$mae_completo), sd(risultati_cv_lp$mae_causale)))

# Grafico R² per fold
cv_long_lp <- data.frame(
  fold    = rep(1:K, 2),
  R2      = c(risultati_cv_lp$r2_completo, risultati_cv_lp$r2_causale),
  Modello = rep(c("Completo", "Causale"), each = K)
)
medie_lp <- cv_long_lp %>% group_by(Modello) %>% summarise(Media = mean(R2))

ggplot(cv_long_lp, aes(x = fold, y = R2, color = Modello, group = Modello)) +
  geom_line(linewidth = 1) + geom_point(size = 3) +
  geom_hline(data = medie_lp, aes(yintercept = Media, color = Modello),
             linetype = "dashed", linewidth = 0.8) +
  scale_color_manual(values = c("Completo" = "#2980B9", "Causale" = "#E74C3C")) +
  labs(title = "R² per fold — Cross-Validation",
       x = "Fold", y = "R²", color = "Modello") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"), legend.position = "bottom")



ic_robusti <- coefci(modello_step_lp,
                     vcov  = vcovHC(modello_step_lp, type = "HC3"),
                     level = 0.95)
coef_lp <- coef(modello_step_lp)

variabili_causali <- c(
  "Size", "Condition.L", "Condition.Q",
  "Materialnoto", "Shipping_Cost", "avg_interest_global_last_year",
  "BrandMarche di Lusso", "Brandautry", "Brandnew balance",
  "Brandconverse", "Brandvans", "BrandAltro",
  "Brandpuma", "Brandreebok", "Branddiadora",
  "Brandfila", "Brandignoto", "Brandchampion",
  "Seller_Rating_ClassOttimo (4.7 - 4.9)",
  "Seller_Rating_ClassPerfetto (5.0)",
  "Seller_Rating_ClassBuono (4.1 - 4.6)",
  "Num_Other_Items"
)

etichette <- c(
  "Size"                                   = "Taglia",
  "Condition.L"                            = "Condizione (lineare)",
  "Condition.Q"                            = "Condizione (quadratica)",
  "Materialnoto"                           = "Materiale noto",
  "Shipping_Cost"                          = "Costo spedizione",
  "avg_interest_global_last_year"          = "Popolarità brand (Trends)",
  "BrandMarche di Lusso"                   = "Brand: Lusso",
  "Brandautry"                             = "Brand: Autry",
  "Brandnew balance"                       = "Brand: New Balance",
  "Brandconverse"                          = "Brand: Converse",
  "Brandvans"                              = "Brand: Vans",
  "BrandAltro"                             = "Brand: Altro",
  "Brandpuma"                              = "Brand: Puma",
  "Brandreebok"                            = "Brand: Reebok",
  "Branddiadora"                           = "Brand: Diadora",
  "Brandfila"                              = "Brand: Fila",
  "Brandignoto"                            = "Brand: Ignoto",
  "Brandchampion"                          = "Brand: Champion",
  "Seller_Rating_ClassOttimo (4.7 - 4.9)"  = "Rating: Ottimo",
  "Seller_Rating_ClassPerfetto (5.0)"      = "Rating: Perfetto",
  "Seller_Rating_ClassBuono (4.1 - 4.6)"   = "Rating: Buono",
  "Num_Other_Items"                        = "N° altri articoli"
)

forest_df <- data.frame(
  Variabile = variabili_causali,
  Etichetta = etichette[variabili_causali],
  Effetto   = (exp(coef_lp[variabili_causali]) - 1) * 100,
  IC_low    = (exp(ic_robusti[variabili_causali, 1]) - 1) * 100,
  IC_high   = (exp(ic_robusti[variabili_causali, 2]) - 1) * 100
) %>%
  mutate(
    Significativo = !(IC_low <= 0 & IC_high >= 0),
    Direzione     = ifelse(Effetto > 0, "Positivo", "Negativo")
  ) %>%
  arrange(Effetto)

forest_df$Etichetta <- factor(forest_df$Etichetta, levels = forest_df$Etichetta)

ggplot(forest_df, aes(x = Effetto, y = Etichetta,
                      color = Direzione, alpha = Significativo)) +
  geom_vline(xintercept = 0, linetype = "dashed",
             color = "grey50", linewidth = 0.8) +
  geom_errorbarh(aes(xmin = IC_low, xmax = IC_high),
                 height = 0.3, linewidth = 0.8) +
  geom_point(size = 3) +
  scale_color_manual(values = c("Positivo" = "#27AE60", "Negativo" = "#E74C3C")) +
  scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = 0.35),
                     labels = c("TRUE" = "Significativo",
                                "FALSE" = "Non significativo")) +
  labs(title    = "Forest plot — Effetti causali sul log-prezzo",
       subtitle = "Effetto % sul prezzo con IC robusti al 95% (HC3)",
       x = "Effetto sul prezzo (%)", y = NULL,
       color = "Direzione", alpha = "Significatività") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 14),
        legend.position = "bottom",
        panel.grid.major.y = element_line(color = "grey90"))


train_lp_misto <- train_lp
train_lp_misto$Seller_User <- dati$Seller_User[id_stima]
test_lp_misto  <- test_lp
test_lp_misto$Seller_User  <- dati$Seller_User[id_verifica]

modello_misto <- lmer(y ~ Brand + Size + Condition + Material +
                        Shipping_Cost + Seller_Rating_Class +
                        Num_Other_Items + avg_interest_global_last_year +
                        (1 | Seller_User),
                      data = train_lp_misto)
summary(modello_misto)

# ICC
var_comp <- as.data.frame(VarCorr(modello_misto))
icc <- var_comp$vcov[1] / sum(var_comp$vcov)
cat("\nICC:", round(icc, 4), "\n")

# Metriche test set
pred_misto        <- predict(modello_misto, newdata = test_lp_misto,
                             allow.new.levels = TRUE)
pred_euro_misto   <- exp(pred_misto)
reale_euro        <- exp(test_lp_misto$y)
r2_misto          <- cor(test_lp_misto$y, pred_misto)^2
mae_euro_misto    <- mean(abs(reale_euro - pred_euro_misto))
rmse_euro_misto   <- sqrt(mean((reale_euro - pred_euro_misto)^2))

cat("\n Confronto Causale vs Misto \n")
cat(sprintf("%-20s %10s %10s\n", "Metrica", "Causale", "Misto"))
cat(sprintf("%-20s %10.4f %10.4f\n", "R²",
            r2_causale, r2_misto))
cat(sprintf("%-20s %10.4f %10.4f\n", "RMSE (€)",
            sqrt(mean((reale_euro - pred_euro_causale)^2)), rmse_euro_misto))
cat(sprintf("%-20s %10.4f %10.4f\n", "MAE  (€)",
            mean(abs(reale_euro - pred_euro_causale)), mae_euro_misto))

# LRT — random intercept necessario?
modello_misto_ml <- update(modello_misto, REML = FALSE)
ranova(modello_misto_ml)


errori_lp <- test_lp %>%
  as.data.frame() %>%
  mutate(
    Predetto_euro = exp(pred_causale),
    Reale_euro    = exp(y),
    Errore_euro   = abs(Reale_euro - Predetto_euro),
    Errore_pct    = abs(Reale_euro - Predetto_euro) / Reale_euro * 100,
    Sovrastima    = Predetto_euro > Reale_euro
  )

cat("MAE medio (€):", mean(errori_lp$Errore_euro), "\n")
cat("MAPE medio (%):", mean(errori_lp$Errore_pct), "\n")
cat("% sovrastime:", mean(errori_lp$Sovrastima) * 100, "\n")

# Errori per brand
errori_brand <- errori_lp %>%
  group_by(Brand) %>%
  summarise(N = n(), MAE_euro = mean(Errore_euro), MAPE = mean(Errore_pct)) %>%
  filter(N >= 10) %>%
  arrange(desc(MAE_euro))

ggplot(errori_brand, aes(x = reorder(Brand, MAE_euro), y = MAE_euro,
                         fill = MAE_euro)) +
  geom_col(alpha = 0.85) +
  geom_text(aes(label = paste0(round(MAE_euro, 1), "€\n(n=", N, ")")),
            hjust = -0.1, size = 3.5) +
  coord_flip() +
  scale_fill_gradient(low = "#F7DC6F", high = "#E74C3C") +
  labs(title    = "MAE per Brand — Modello Causale",
       subtitle = "Solo brand con almeno 10 osservazioni nel test set",
       x = NULL, y = "MAE (€)") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"), legend.position = "none")

# Errori per fascia di prezzo
errori_lp <- errori_lp %>%
  mutate(Fascia = cut(Reale_euro,
                      breaks = c(0, 20, 50, 100, 200, Inf),
                      labels = c("0-20€", "20-50€", "50-100€",
                                 "100-200€", ">200€")))

errori_fascia <- errori_lp %>%
  group_by(Fascia) %>%
  summarise(N = n(), MAE = mean(Errore_euro), MAPE = mean(Errore_pct))

print(errori_fascia)

# Predetto vs Reale
ggplot(errori_lp, aes(x = Reale_euro, y = Predetto_euro)) +
  geom_point(alpha = 0.3, color = "#2980B9") +
  geom_abline(slope = 1, intercept = 0,
              color = "#E74C3C", linewidth = 1, linetype = "dashed") +
  scale_x_log10() + scale_y_log10() +
  labs(title = "Predetto vs Reale — Modello Causale",
       x = "Prezzo reale (€)", y = "Prezzo predetto (€)") +
  theme_minimal()



cat("\n SINTESI FINALE \n")
cat(sprintf("%-25s %10s %10s %10s\n", "Metrica", "Completo", "Causale", "Misto"))
cat(sprintf("%-25s %10.4f %10.4f %10.4f\n", "R²",
            r2_lp, r2_causale, r2_misto))
cat(sprintf("%-25s %10.4f %10.4f %10.4f\n", "MAE (€)",
            mean(abs(reale_euro - pred_euro_step)),
            mean(abs(reale_euro - pred_euro_causale)),
            mae_euro_misto))
cat(sprintf("%-25s %10.4f %10.4f %10.4f\n", "RMSE (€)",
            sqrt(mean((reale_euro - pred_euro_step)^2)),
            sqrt(mean((reale_euro - pred_euro_causale)^2)),
            rmse_euro_misto))





#4. Endogeneità e modello causale


#Tre variabili del modello presentano un problema di endogeneità per causalità
#inversa: Has_Item_Verification (+198%), Is_Boosted (+139%) e Favorites_Count.
#In tutti e tre i casi la correlazione con il prezzo è reale ma la direzione
#causale è opposta a quella apparente  chi ha una scarpa costosa richiede la
#verifica perché conviene economicamente, non è la verifica a causare il prezzo
#alto.

#La prova empirica più convincente è nel confronto dei coefficienti:
#rimuovendo Has_Item_Verification dal modello, il coefficiente di Condition.L
#sale da 0.99 a 1.38, un aumento del 40%, a dimostrazione che la variabile endogena
#stava assorbendo parte dell'effetto causale della condizione fisica.



#Il modello causale — che esclude le tre endogene — ottiene R²=0.469 e MAE=26€,
#con un costo predittivo reale ma accettabile rispetto al modello completo.
#La variabile Trends mantiene la sua significatività anche nel modello causale
#con un coefficiente leggermente più alto (0.0088 vs 0.0053), a conferma che il
#suo effetto non era distorto dalla presenza delle endogene.

#5. Cross-validation
#La cross-validation a 6 fold conferma la stabilità di entrambi i modelli.
#L'R² medio è 0.661 per il completo e 0.495 per il causale, con deviazioni
#standard rispettivamente di 0.019 e 0.027 — valori molto bassi che indicano
#che la performance non dipende dal particolare split train/test ma è una
#caratteristica strutturale dei dati e del mercato Vinted. Il contributo della
#variabile Trends si conferma anche in cross-validation: il modello completo con
#Trends raggiunge un R² medio di 0.661 contro il 0.63 della versione senza, un
#miglioramento consistente su tutti i fold.

#6. Modello con effetti casuali
#Per completare l'analisi è stato stimato un modello misto con random intercept
#per venditore, per verificare se la struttura gerarchica dei dati — articoli
#annidati dentro venditori — aggiunge potere esplicativo oltre alle variabili
#osservabili. Il test likelihood ratio formale (ranova) rigetta l'ipotesi nulla
#con p<2.2e-16, indicando che il random intercept è statisticamente significativo.
#Tuttavia l'ICC risulta pari a 0.84 — l'84% della variabilità totale sarebbe
#attribuibile al venditore — un valore anomalmente alto che è un artefatto
#strutturale dei dati: su 1.434 osservazioni ci sono 1.325 venditori distinti,
#quasi uno per osservazione. Il modello misto non sta imparando un pattern
#generalizzabile sul venditore ma sta sostanzialmente memorizzando ogni singolo
#venditore del training set. Di conseguenza le metriche predittive sul test set
#sono praticamente identiche al modello causale OLS, con un miglioramento di soli
#1.2€ sul MAE. Il modello causale rimane la scelta corretta per interpretare i
#determinanti strutturali del prezzo.

#7. Analisi degli errori
#L'analisi degli errori del modello causale rivela pattern coerenti con la natura
#del mercato. Il MAE complessivo è 26.6€ ma la distribuzione degli errori non è
#uniforme: nella fascia sotto i 20€ il MAPE raggiunge il 147%, perché piccole
#differenze assolute corrispondono a grandi variazioni percentuali su prezzi già
#bassi. Nella fascia 50-200€ il MAPE si stabilizza intorno al 46%, zona in cui il
#modello è più consistente. Le scarpe oltre i 200€ mostrano un MAE di 210€ su sole
#9 osservazioni — la variabilità intrinseca del mercato del lusso non è catturabile
#con le variabili disponibili. Il grafico predetto vs reale in scala log-log mostra
#un buon allineamento nella fascia centrale 10-100€, con dispersione crescente agli
#estremi.

#8. Conclusioni
#Il percorso analitico risponde alle tre domande iniziali. La condizione fisica
#è il determinante principale del prezzo: una scarpa nuova con cartellino vale
#il +285% rispetto a una in condizioni discrete nel modello causale, l'effetto
#più grande dell'intera analisi. Il brand ha un effetto causale robusto e
#significativo anche con errori HC3: le marche di lusso mostrano un premio del
#+89%, mentre i brand ignoti sono penalizzati del -47%; i coefficienti sono
#stabili tra modello completo e causale. La verifica dell'articolo non causa
#il prezzo alto — è endogena, e la prova empirica del salto di Condition.L
#rimuovendola lo dimostra chiaramente. Il modello completo con R²=0.625 e MAE=22€
#è lo strumento migliore per predire il prezzo di un nuovo articolo. Il modello
#causale con R²=0.469 e MAE=26€ è lo strumento corretto per capire cosa determina
#strutturalmente il prezzo su Vinted. Entrambi sono stabili in cross-validation
#con SD dell'R² inferiore a 0.03.



# Prezzo non par ----

rm(list=ls());gc();
load("dati_puliti.Rdata")

source("utils.R")

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

dataset_regressione_prezzo <-
  dataset_regressione_prezzo |>
  rename(Google_Trends = avg_interest_global_last_year,
         Log_Review_Count = Log_Seller_Reviews_Count,
         Item_Verification = Has_Item_Verification) |>
  dplyr::select(-Item_Verification, -Is_Boosted)

nomi_presentazione <- c(
  "Size"                  = "Taglia",
  "Condition"             = "Condizioni dell'Articolo",
  "Material"              = "Materiale",
  "Favorites_Count"       = "Numero di Preferiti",
  "Shipping_Cost"         = "Costo di Spedizione",
  "Color_new"             = "Colore",
  "Seller_Rating_Class"   = "Fascia Recensione Venditore",
  "Log_Review_Count"      = "Log(N° Recensioni)",
  "Seller_has_distintivi" = "Venditore con Badge",
  "Num_Other_Items"       = "Altri Articoli in Vendita",
  "Google_Trends"         = "Interesse (Google Trends)"
)

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
colnames(matrice_errori_albero) <- 2:(max_size+1)
#for (j in 1:K_cv){
#  id_stima = which(fold != j)
#  id_verifica = which(fold == j)
#
#  m_tree_tmp = tree(y~.,
#                    data=stima[id_stima,],
#                    split='deviance',
#                    control=
#                      tree.control(nobs=NROW(stima),
#                                   minsize=1,
#                                   mindev=0.0001))
#  m_tree_tmp
#
#  tree_list = lapply(2:(max_size+1), # per noi dopo i 50 split é sovra-adattato
#                     # non uso il numero di split m_tree_full perché puó
#                     # essere che alcuni alberi siano piú piccoli
#                     function(l) prune.tree(m_tree_tmp,
#                                            best=l) )
#  pred_list = lapply(tree_list,
#                     function(x) predict(x,
#                                         stima[id_verifica,]))
#  lista_errori_CI = lapply(pred_list,
#                           function(x) MSE(x, stima$y[id_verifica]))
#
#  matrice_errori_albero[j,] = lista_errori_CI |> unlist()
#}
#save(matrice_errori_albero, file="Modelli_stimati/err_tree_prezzo.RData")
load("Modelli_stimati/err_tree_prezzo.RData")
dim(matrice_errori_albero)

errore_albero_CI = apply(matrice_errori_albero, 2, mean)
se_albero <- apply(matrice_errori_albero, 2, function(x) sd(x) / sqrt(max(K_cv)))

B = which.min(errore_albero_CI) |> names() |> as.numeric()

cv_min <- errore_albero_CI[B]
se_min <- se_albero[B]

soglia_1se <- cv_min + se_min

taglie_valide <- which(errore_albero_CI <= soglia_1se)
B_1se <- max(2, min(taglie_valide))

plot(errore_albero_CI, type='l', lwd=2)
abline(v=B-1, col=2, lwd=3)

m_tree = prune.tree(m_tree_full, best=B_1se)

#x11();plot_tree(m_tree, "Plot/albero_prezzo")

yhat_tree <- predict(m_tree, newdata = verifica)
err_tree <-  mean((verifica$y - yhat_tree)^2)

prezzo_medio_verifica <- mean(verifica$y)
devianza_totale_verifica <- sum((verifica$y - prezzo_medio_verifica)^2)
devianza_residua_albero <- sum((verifica$y - yhat_tree)^2)

r2_tree <- 1 - (devianza_residua_albero / devianza_totale_verifica)

r2_prezzo <- data.frame(albero = r2_tree)
r2_prezzo

rmse_albero <- sqrt(mean((verifica$y - yhat_tree)^2))
rmse_prezzo <- data.frame(albero = rmse_albero)
rmse_prezzo

mae_albero <- mean(abs(verifica$y - yhat_tree))
mae_prezzo <- data.frame(albero = mae_albero)
mae_prezzo

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
save(modelli_rf, file="Modelli_stimati/err_rf_prezzo.RData")
load("Modelli_stimati/err_rf_prezzo.RData")

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
set.seed(123)
rf_best <- ranger(y~.,
                  num.trees = ntree_rf, mtry = 8,
                  data=stima, importance="permutation",
                  num.threads=n_threads)

p_rf = predict(rf_best, verifica)
devianza_residua_rf <- sum((verifica$y - p_rf$predictions)^2)

r2_rf <- 1 - (devianza_residua_rf / devianza_totale_verifica)

r2_prezzo$RF <- r2_rf
r2_prezzo

rmse_rf <- sqrt(mean((verifica$y - p_rf$predictions)^2))
rmse_prezzo$RF <- rmse_rf
rmse_prezzo

mae_rf <- mean(abs(verifica$y - p_rf$predictions))
mae_prezzo$RF <- mae_rf
mae_prezzo

### Importanza ----

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

p_imp_rf <- df_imp %>%
  mutate(
    Nome_Bello = coalesce(nomi_presentazione[variabile], variabile),
    Nome_Bello = forcats::fct_reorder(Nome_Bello, importanza)
  ) |>
  ggplot(aes(x = Nome_Bello, y = importanza, fill = tipo)) +
  geom_col(width = 0.7) +
  coord_flip() +
  scale_fill_manual(
    values = c("Venditore" = "#4E79A7", "Prodotto" = "#F28E2B",
               "Transazione" = "#59A14F"),
    name = "Categoria"
  ) +
  labs(title    = "Importanza delle top 10 variabili tramite permutazione",
       x = NULL, y = "Importanza") +
  theme_minimal(base_size = 12) +
  theme(plot.title   = element_text(face = "bold", size = 14),
        legend.position = "bottom")

ggsave("Plot/Importanza_RF_prezzo.png", plot=p_imp_rf,
       width = 8, height = 5)

## GBM -----

nus <- c(0.005, 0.01, 0.05, 0.1) # shrinkage
dps <- c(22, 25, 28, 31, 34) # profondità

tuning_par <- expand.grid(nus, dps)
err_gbm <- rep(1, nrow(tuning_par))

library(gbm)
nts_xgb <- 2000
set.seed(123)
# modello di prova per capire se uso un numero corretto di alberi
#mod_prova <- gbm(stima$y~., data=X_stima,
#                 distribution = "gaussian", n.trees = nts_xgb,
#                 shrinkage = tuning_par[1,1],
#                 interaction.depth = tuning_par[1,2])
#
#yhat_mp <- predict(mod_prova, newdata = X_verifica, n.trees=1:nts_xgb)
#err_pr <- apply(yhat_mp, 2, function(p) mean((verifica$y - p)^2))
#plot(err_pr, type="l")
#abline(h=min(err_pr), col=2)
#abline(v=which.min(err_pr), col=2, lty=2, lwd=2)
#abline(v=100, col=3, lty=2, lwd=2)

#err_pr[450] / err_pr[100]
#err_pr[which.min(err_pr)] / err_pr[100]

nts_xgb <- 800

### Tuning ----

#set.seed(1)
#for(i in 1:nrow(tuning_par)){
#  mod <- gbm(stima$y[idx_a]~., data=X_stima[idx_a,],
#             distribution = "gaussian",
#             shrinkage = tuning_par[i,1], n.trees = nts_xgb,
#             interaction.depth = tuning_par[i,2])
#  passo <- max(30, 1000 * (0.5 - tuning_par[i,1])/(dps/2))
#  alberi_pred <- seq(100, nts_xgb, by = passo)
#  ps <- predict(mod, newdata = X_stima[idx_b,], n.trees = alberi_pred)
#  err_i <- apply(ps, 2, function(p) mean(((stima$y)[idx_b] - p)^2))
#  err_gbm[i] <- min(err_i)
#  cat(round(i/nrow(tuning_par)*100,2), "%\n")
#}
#save(err_gbm, file="Modelli_stimati/err_gbm_prezzo.RData")
load("Modelli_stimati/err_gbm_prezzo.RData")
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
    subtitle = "Il riquadro nero indica la combinazione con l'R2 massimo",
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

set.seed(123)
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

r2_prezzo$GBM <- r2_gbm
r2_prezzo

rmse_gbm <- sqrt(mean((verifica$y - yhat_gbm)^2))
rmse_prezzo$gbm <- rmse_gbm
rmse_prezzo

mae_gbm <- mean(abs(verifica$y - yhat_gbm))
mae_prezzo$gbm <- mae_gbm
mae_prezzo

colnames(X_stima)
dim(X_stima)

## PDP ----

# Trovo le top3 variabili per devianza spiegata
top3_gbm <- summary(mod_gbm, n.trees = n_trees_best,
                    plotit = T, order = TRUE)$var[c(2,3,6)]
# Indici colonna delle top 3 variabili
idx_top3 <- sapply(top3_gbm, function(v) which(colnames(X_stima) == v))

# Definisco la funzione per calcolare i dati del Partial Dependence Plot singolo
pdp_gbm_single <- function(var_idx, modello, X_data, n_trees, dati_orig,
                           n_grid = 50) {
  # Estraggo il nome della variabile, i dati e verifico se è numerica
  var_name <- colnames(X_data)[var_idx]
  x        <- X_data[[var_name]]
  is_num   <- is.numeric(x)

  # Creo la griglia: sequenza per i numerici (escludendo le code 2%-98%), categorie uniche altrimenti
  griglia  <- if (is_num)
    seq(quantile(x, 0.02), quantile(x, 0.98), length.out = n_grid)
  else
    sort(unique(x))

  # Calcolo la predizione media per ogni valore della griglia
  pds <- sapply(griglia, function(v) {
    tmp <- X_data
    tmp[[var_name]] <- v
    mean(predict(modello, newdata = tmp, n.trees = n_trees))
  })

  # Restituisco i risultati impaginati in un dataframe
  data.frame(x = griglia, pd = pds, variabile = var_name, is_num = is_num)
}

# Calcolo i dati PDP per le top 3 variabili applicando la funzione
pdp_gbm_list <- lapply(idx_top3, pdp_gbm_single,
                       modello   = mod_gbm,
                       X_data    = X_stima,
                       n_trees   = n_trees_best,
                       dati_orig = stima)

# Genero la lista dei 3 grafici ggplot
pdp_gbm_plots <- lapply(pdp_gbm_list, function(df) {

  nome_brutto <- df$variabile[1]
  nome_bello  <- coalesce(nomi_presentazione[nome_brutto], nome_brutto)

  # Inizializzo il grafico gestendo l'asse x in base al tipo di variabile
  p <- ggplot(df, aes(x = if (df$is_num[1]) as.numeric(x) else factor(x),
                      y = pd))

  # Aggiungo la geometria adeguata: linea se numerico, barre se categorico
  if (df$is_num[1]) {
    p <- p + geom_line(color = "#E15759", linewidth = 1.2)
  } else {
    p <- p + geom_col(fill = "#E15759", width = 0.6)
  }

  # Formatto i titoli e il tema estetico
  p + labs(title = nome_bello, x = NULL, y = "Prezzo atteso (€)") +
    theme_minimal(base_size = 11) +
    theme(plot.title = element_text(face = "bold"))
})

library(patchwork)
# Unisco i tre grafici affiancati e aggiungo un titolo globale
p_pdp_gbm <- (pdp_gbm_plots[[1]] | pdp_gbm_plots[[2]] | pdp_gbm_plots[[3]]) +
  plot_annotation(
    theme    = theme(plot.title = element_text(face = "bold", size = 14))
  )

ggsave("Plot/gbm_pdp_top3_prezzo.png",
       plot = p_pdp_gbm, width = 12, height = 5, dpi = 200)

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

if (NROW(mert_model$Tree$splits) > 0){
  plot_tree(mert_model$Tree, "Plot/MERT_prezzo")
}

pred_tree <- predict(mert_model$Tree, newdata = verifica)
pred_ran <- predict(mert_model$EffectModel, newdata = verifica)
pred_mert <- pred_tree + pred_ran

devianza_residua_mert <- sum((verifica$y - pred_mert)^2)

r2_mert <- 1 - (devianza_residua_mert / devianza_totale_verifica)

r2_prezzo$mert <- r2_mert
r2_prezzo

rmse_mert <- sqrt(mean((verifica$y - pred_mert)^2))
rmse_prezzo$mert <- rmse_mert
rmse_prezzo

mae_mert <- mean(abs(verifica$y - pred_mert))
mae_prezzo$mert <- mae_mert
mae_prezzo

### MERF -----

str(stima)
form <- formula(lm(y~.,
                   data=stima |> dplyr::select(-Brand)))

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

r2_prezzo$merf <- r2_merf
r2_prezzo

rmse_merf <- sqrt(mean((verifica$y - pred_merf)^2))
rmse_prezzo$merf <- rmse_merf
rmse_prezzo

mae_merf <- mean(abs(verifica$y - pred_merf))
mae_prezzo$merf <- mae_merf
mae_prezzo


### Metboost -----

#x_var <- colnames(stima |> dplyr::select(-y, -Brand))
#set.seed(1)
#par_opt # del GBM classico
#met_model <- metboost_fit_path_manual(stima, y_name = "y", vars_x = x_var,
#                                      group_var = "Brand",
#                                      #M_max = nts_xgb,
#                                      M_max = 300,
#                                      shrinkage = 0.01, depth = 4)
## group_var effetti casuali
## M_max numero albero, prima ne avevamo 6000, ora per motivi computazionali scendiamo a 300
## inoltre ad ogni step devo stimare un albero con una certa profondità, più depth
## è elevato più ci metterà, quindi metto 2
## lo shrinkage era ottimo a 0.05, ma sappiamo che deve andare assieme al numero di
## alberi e alla profondità. Quindi, dato che fisso 2 come profondità, una scelta per
## bilanciare è di alzare il parametro di shrinkage
#
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
#r2_prezzo$metboost <- err_met
#r2_prezzo

cbind(t(r2_prezzo), t(rmse_prezzo), t(mae_prezzo))


# Favoriti par ----

rm(list=ls());gc();
load("dati_puliti.Rdata")

library(MASS)
library(tidyverse)
library(lmtest)
library(sandwich)
library(pscl)
library(ggplot2)
library(dplyr)

cat("Distribuzione Favorites_Count:\n")
summary(dataset_regressione_favoriti$y)
cat("% zeri:", mean(dataset_regressione_favoriti$y == 0) * 100, "\n")
cat("Media:", mean(dataset_regressione_favoriti$y), "\n")
cat("Varianza:", var(dataset_regressione_favoriti$y), "\n")

hist(dataset_regressione_favoriti$y, breaks = 50,
     main = "Distribuzione Favorites_Count",
     xlab = "Numero di preferiti", col = "#2C7BB6", border = "white")

hist(log1p(dataset_regressione_favoriti$y), breaks = 40,
     main = "Distribuzione log1p(Favorites_Count)",
     xlab = "log1p(Favorites_Count)", col = "#27AE60", border = "white")



#1. Obiettivo e variabile risposta
#L'analisi si propone di predire il numero di preferiti ricevuti da un annuncio
#su Vinted, usando le stesse variabili precedenti. Dalla distribuzione della variabile risposta
#emergono   due problemi:
#1)forte asimmetria positiva con mediana 8 e media 16,
#2) fortissima sovradispersione: con varianza pari a 486 contro una media di 16
#che esclude a priori la distribuzione di Poisson.

#Solo il 12% degli annunci ha zero preferiti, quindi la sovradispersione non è
#attribuibile principalmente agli zeri ma alla coda destra molto pesante.



dataset_regressione_logfav <- dataset_regressione_favoriti %>%
  mutate(y = log1p(y))

train_lf <- dataset_regressione_logfav[id_stima, ]
test_lf  <- dataset_regressione_logfav[id_verifica, ]

modello_completo_lf <- lm(y ~ ., data = train_lf)
modello_nullo_lf    <- lm(y ~ 1, data = train_lf)

modello_step_lf <- step(modello_completo_lf,
                        scope     = list(lower = modello_nullo_lf,
                                         upper = modello_completo_lf),
                        direction = "both", trace = 1)
summary(modello_step_lf)

par(mfrow = c(2, 2)); plot(modello_step_lf); par(mfrow = c(1, 1))

pred_lf    <- predict(modello_step_lf, newdata = test_lf)
residui_lf <- test_lf$y - pred_lf

rmse_lf <- sqrt(mean(residui_lf^2))
mae_lf  <- mean(abs(residui_lf))
r2_lf   <- cor(test_lf$y, pred_lf)^2

# Riconversione in n° preferiti
pred_fav  <- expm1(pred_lf)
reale_fav <- expm1(test_lf$y)
mae_fav   <- mean(abs(reale_fav - pred_fav))
rmse_fav  <- sqrt(mean((reale_fav - pred_fav)^2))

cat("\n Metriche Test Set (OLS log1p) \n")
cat(sprintf("R²:            %.4f\n", r2_lf))
cat(sprintf("MAE (preferiti): %.4f\n", mae_fav))
cat(sprintf("RMSE (preferiti): %.4f\n", rmse_fav))



train_nb <- dataset_regressione_favoriti[id_stima, ]
test_nb  <- dataset_regressione_favoriti[id_verifica, ]

# Verifica sovradispersione
cat("\nMedia Favorites_Count (train):", mean(train_nb$y), "\n")
cat("Varianza Favorites_Count (train):", var(train_nb$y), "\n")
cat("Rapporto varianza/media:", var(train_nb$y) / mean(train_nb$y), "\n")

modello_nb_completo <- glm.nb(
  y ~ LogPrice + Brand + Size + Condition + Material +
    Shipping_Cost + Is_Boosted + Has_Item_Verification +
    Color_new + Seller_Rating_Class +
    Log_Seller_Reviews_Count + Seller_has_distintivi +
    Num_Other_Items + avg_interest_global_last_year,
  data = train_nb
)
summary(modello_nb_completo)

modello_nb_nullo <- glm.nb(y ~ 1, data = train_nb)

modello_nb_step <- step(modello_nb_completo,
                        scope     = list(lower = modello_nb_nullo,
                                         upper = modello_nb_completo),
                        direction = "both", trace = 1)
summary(modello_nb_step)

# Metriche test set
pred_nb  <- predict(modello_nb_step, newdata = test_nb, type = "response")
reale_nb <- test_nb$y

mae_nb  <- mean(abs(reale_nb - pred_nb))
rmse_nb <- sqrt(mean((reale_nb - pred_nb)^2))
r2_nb   <- cor(reale_nb, pred_nb)^2

cat("\n  Confronto OLS log1p vs Negative Binomial \n")
cat(sprintf("%-20s %10s %10s\n", "Metrica", "OLS log1p", "Neg. Binom."))
cat(sprintf("%-20s %10.4f %10.4f\n", "MAE  (preferiti)", mae_fav,  mae_nb))
cat(sprintf("%-20s %10.4f %10.4f\n", "RMSE (preferiti)", rmse_fav, rmse_nb))
cat(sprintf("%-20s %10.4f %10.4f\n", "R² (cor²)",        r2_lf,    r2_nb))

# Effetti % sul numero atteso di preferiti
cat("\n Effetti % sul numero atteso di preferiti \n")
coef_nb <- coef(modello_nb_step)
cat(sprintf("%-40s %10s\n", "Variabile", "Effetto%"))
for (v in names(coef_nb)[-1]) {
  eff <- (exp(coef_nb[v]) - 1) * 100
  cat(sprintf("%-40s %9.1f%%\n", v, eff))
}



modello_poisson <- glm(
  formula(modello_nb_step),
  data   = train_nb,
  family = poisson
)

cat("\n Test LR: Negative Binomial vs Poisson \n")
cat("Log-likelihood NB:     ", logLik(modello_nb_step), "\n")
cat("Log-likelihood Poisson:", logLik(modello_poisson), "\n")
cat("Se NB >> Poisson → sovradispersione confermata\n")


modello_zinb <- zeroinfl(y ~ LogPrice + Brand + Is_Boosted +
                           Has_Item_Verification + Seller_Rating_Class +
                           avg_interest_global_last_year | 1,
                         data = train_nb, dist = "negbin")
summary(modello_zinb)

pred_zinb  <- predict(modello_zinb, newdata = test_nb, type = "response")
mae_zinb   <- mean(abs(reale_nb - pred_zinb))
rmse_zinb  <- sqrt(mean((reale_nb - pred_zinb)^2))
r2_zinb    <- cor(reale_nb, pred_zinb)^2

cat("\n── Confronto NB vs ZINB \n")
cat(sprintf("%-20s %10s %10s\n", "Metrica", "NB", "ZINB"))
cat(sprintf("%-20s %10.4f %10.4f\n", "MAE  (preferiti)", mae_nb,   mae_zinb))
cat(sprintf("%-20s %10.4f %10.4f\n", "RMSE (preferiti)", rmse_nb,  rmse_zinb))
cat(sprintf("%-20s %10.4f %10.4f\n", "R² (cor²)",        r2_nb,    r2_zinb))



plot(reale_nb, pred_nb,
     xlab = "Preferiti reali", ylab = "Preferiti predetti",
     main = "Predetto vs Reale — Negative Binomial",
     pch = 16, col = "#2980B950",
     xlim = c(0, 150), ylim = c(0, 150))
abline(0, 1, col = "#E74C3C", lwd = 2, lty = 2)



cat("\n  SINTESI FINALE n")
cat(sprintf("%-20s %10s %10s %10s\n",
            "Metrica", "OLS log1p", "NB", "ZINB"))
cat(sprintf("%-20s %10.4f %10.4f %10.4f\n", "R²",
            r2_lf, r2_nb, r2_zinb))
cat(sprintf("%-20s %10.4f %10.4f %10.4f\n", "MAE (preferiti)",
            mae_fav, mae_nb, mae_zinb))
cat(sprintf("%-20s %10.4f %10.4f %10.4f\n", "RMSE (preferiti)",
            rmse_fav, rmse_nb, rmse_zinb))

cat("\n Conclusione \n")
cat("Le caratteristiche osservabili della scarpa spiegano bene\n")
cat("il prezzo (R²≈0.63) ma quasi per nulla i preferiti (R²≈0.11).\n")
cat("Prezzo e popolarità sono guidati da meccanismi diversi:\n")
cat("il prezzo riflette attributi oggettivi (brand, condizione, taglia),\n")
cat("i preferiti dipendono da fattori non osservabili nel dataset\n")
cat("(qualità foto, descrizione, visibilità algoritmica).\n")
cat("avg_interest_global_last_year aggiunta per testare se la\n")
cat("popolarità del brand spiega i preferiti oltre al prezzo.\n")




#Il primo approccio modellistico replica la strategia usata per il prezzo: trasformazione
#log1p della variabile risposta seguita da regressione lineare con selezione
#stepwise bidirezionale  su AIC.


#Il modello finale seleziona solo 5 variabili: LogPrice, Brand, Material,
#Is_Boosted e Has_Item_Verification. Significativamente, la variabile
#avg_interest_global_last_year viene eliminata già nei primi passi dello stepwise
#la popolarità globale del brand su Google non aggiunge informazione sui
#preferiti oltre a quella già catturata dal brand stesso. Il risultato sul test
#set è deludente: R²=0.11 e MAE di 12 preferiti. Il modello spiega solo l'11%
#della variabilità dei preferiti, contro il 63% ottenuto per il prezzo con le
#stesse variabili.

#3. Negative Binomial
#Usiamo questo per via della sovradispersione.
#Lo stepwise sulla NB seleziona 6 variabili,
#aggiunge Size e Seller_Rating_Class rispetto all'OLS — e anche qui
#avg_interest_global_last_year viene eliminata al primo passo, confermando che
#la popolarità Trends non spiega i preferiti indipendentemente dal brand.
# Nonostante la correttezza teorica del modello,
#le metriche predittive peggiorano rispetto all'OLS: R² scende a 0.07 e MAE
#sale a 13.6 preferiti.


#4. Zero-Inflated Negative Binomial
#Il modello ZINB modella separatamente la probabilità di essere strutturalmente
#a zero preferiti — l'idea è che alcuni annunci non
#ricevono preferiti per ragioni strutturali indipendenti dalle caratteristiche
#della scarpa, come visibilità algoritmica o momento di pubblicazione. Il modello
#viene stimato con zeroinfl tenendo solo l'intercetta nella componente degli zeri,
#poiché non si hanno variabili teoricamente motivate per distinguere i due processi
#. Il risultato è negativo: l'intercetta del modello degli zeri converge a -18.25
#, indicando che la probabilità di zero strutturale è sostanzialmente nulla e il
#modello collassa sulla NB standard. Le metriche sul test set sono praticamente
#identiche alla NB — MAE 13.6 e R² 0.07 — confermando che la componente
#zero-inflated non aggiunge nulla.

#5. Conclusioni
#Il risultato più rilevante dell'intera analisi sui favoriti non è quale modello
#performa meglio, ma il confronto con la regressione sul prezzo. Le stesse variabili
#strutturali spiegano il 63% della varianza del prezzo e solo l'11% della varianza
#dei preferiti.


#La variabile avg_interest_global_last_year rafforza ulteriormente
#questo punto: contribuisce significativamente al prezzo ma viene eliminata da
#tutti i modelli sui preferiti. Prezzo e popolarità sono guidati da meccanismi
#strutturalmente diversi. Il prezzo riflette attributi oggettivi e verificabili
#— brand, condizione fisica, taglia, materiale. I preferiti dipendono da fattori
#non osservabili nel dataset: qualità delle fotografie, attrattività della
#descrizione testuale, momento di pubblicazione e visibilità algoritmica della
#piattaforma Vinted.

# Favoriti non par ----

rm(list=ls());gc();
load("dati_puliti.Rdata")

source("utils.R")

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

dataset_regressione_favoriti <-
  dataset_regressione_favoriti |>
  rename(Google_Trends = avg_interest_global_last_year,
         Log_Review_Count = Log_Seller_Reviews_Count,
         Item_Verification = Has_Item_Verification)

nomi_presentazione <- c(
  "Size"                  = "Taglia",
  "Condition"             = "Condizioni dell'Articolo",
  "Material"              = "Materiale",
  "Favorites_Count"       = "Numero di Preferiti",
  "Shipping_Cost"         = "Costo di Spedizione",
  "Color_new"             = "Colore",
  "Seller_Rating_Class"   = "Fascia Recensione Venditore",
  "Log_Review_Count"      = "Log(N° Recensioni)",
  "Seller_has_distintivi" = "Venditore con Badge",
  "Num_Other_Items"       = "Altri Articoli in Vendita",
  "Google_Trends"         = "Interesse (Google Trends)"
)


stima <- dataset_regressione_favoriti[id_stima,]
verifica <- dataset_regressione_favoriti[id_verifica,]

X_stima <- model.matrix(y~., stima)[,-1] |> as.data.frame()
X_verifica <- model.matrix(y~., verifica)[,-1] |> as.data.frame()

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
#for (j in 1:K_cv){
#  id_stima = which(fold != j)
#  id_verifica = which(fold == j)
#
#  m_tree_tmp = tree(y~.,
#                    data=stima[id_stima,],
#                    split='deviance',
#                    control=
#                      tree.control(nobs=NROW(stima),
#                                   minsize=1,
#                                   mindev=0.0001))
#  m_tree_tmp
#
#  tree_list = lapply(2:(max_size+1), # per noi dopo i 50 split é sovra-adattato
#                     # non uso il numero di split m_tree_full perché puó
#                     # essere che alcuni alberi siano piú piccoli
#                     function(l) prune.tree(m_tree_tmp,
#                                            best=l) )
#  pred_list = lapply(tree_list,
#                     function(x) predict(x,
#                                         stima[id_verifica,]))
#  lista_errori_CI = lapply(pred_list,
#                           function(x) MSE(x, stima$y[id_verifica]))
#
#  matrice_errori_albero[j,] = lista_errori_CI |> unlist()
#}
#save(matrice_errori_albero, file="Modelli_stimati/err_tree_favoriti.RData")
load("Modelli_stimati/err_tree_favoriti.RData")
dim(matrice_errori_albero)

errore_albero_CI = apply(matrice_errori_albero, 2, mean)
se_albero <- apply(matrice_errori_albero, 2, function(x) sd(x) / sqrt(K_cv))

B = which.min(errore_albero_CI)

cv_min <- errore_albero_CI[B]
se_min <- se_albero[B]

soglia_1se <- cv_min + se_min

taglie_valide <- which(errore_albero_CI <= soglia_1se)
B_1se <- max(2, min(taglie_valide))

plot(errore_albero_CI, type='l', lwd=2)
abline(v=B, col=2, lwd=3)

m_tree = prune.tree(m_tree_full, best=B_1se)

plot_tree(m_tree, "Plot/albero_fav")

yhat_tree <- predict(m_tree, newdata = verifica)
err_tree <-  mean((verifica$y - yhat_tree)^2)

fav_medio_verifica <- mean(verifica$y)
devianza_totale_verifica <- sum((verifica$y - fav_medio_verifica)^2)
devianza_residua_albero <- sum((verifica$y - yhat_tree)^2)

r2_tree <- 1 - (devianza_residua_albero / devianza_totale_verifica)

r2_fav <- data.frame(albero = r2_tree)
r2_fav

rmse_albero <- sqrt(mean((verifica$y - yhat_tree)^2))
rmse_fav <- data.frame(albero = rmse_albero)
rmse_fav

mae_albero <- mean(abs(verifica$y - yhat_tree))
mae_fav <- data.frame(albero = mae_albero)
mae_fav

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
#save(modelli_rf, file="Modelli_stimati/err_rf_fav.RData")
load("Modelli_stimati/err_rf_fav.RData")

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
set.seed(123)
rf_best <- ranger(y~.,
                  num.trees = ntree_rf, mtry = 2,
                  data=stima, importance="permutation",
                  num.threads=n_threads)

p_rf = predict(rf_best, verifica)
devianza_residua_rf <- sum((verifica$y - p_rf$predictions)^2)

r2_rf <- 1 - (devianza_residua_rf / devianza_totale_verifica)

r2_fav$RF <- r2_rf
r2_fav

rmse_rf <- sqrt(mean((verifica$y - p_rf$predictions)^2))
rmse_fav$RF <- rmse_rf
rmse_fav

mae_rf <- mean(abs(verifica$y - p_rf$predictions))
mae_fav$RF <- mae_rf
mae_fav

### Importanza ----

top_imp <- sort(rf_best$variable.importance, decreasing = TRUE) |> head(10)

tipo_var <- function(nomi) {
  # Classifico manualmente le variabili in 3 gruppi semantici
  dplyr::case_when(
    grepl("Seller|Rating|Reviews|distintivi|Num_Other", nomi) ~ "Venditore",
    grepl("Brand|Size|Condition|Material|Color|Trends",  nomi) ~ "Prodotto",
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


p_imp_rf <- df_imp %>%
  mutate(
    Nome_Bello = coalesce(nomi_presentazione[variabile], variabile),
    Nome_Bello = forcats::fct_reorder(Nome_Bello, importanza)
  ) |>
  ggplot(aes(x = Nome_Bello, y = importanza, fill = tipo)) +
  geom_col(width = 0.7) +
  coord_flip() +
  scale_fill_manual(
    values = c("Venditore" = "#4E79A7", "Prodotto" = "#F28E2B",
               "Transazione" = "#59A14F"),
    name = "Categoria"
  ) +
  labs(title    = "Importanza delle top 10 variabili tramite permutazione",
       x = NULL, y = "Importanza") +
  theme_minimal(base_size = 12) +
  theme(plot.title   = element_text(face = "bold", size = 14),
        legend.position = "bottom")

ggsave("Plot/Importanza_RF_fav.png", plot=p_imp_rf,
       width = 8, height = 5)


## GBM -----

nus <- c(0.001, 0.005, 0.01) # shrinkage
dps <- c(8, 11, 14, 17, 20) # profondità

tuning_par <- expand.grid(nus, dps)
err_gbm <- rep(1, nrow(tuning_par))

library(gbm)
nts_xgb <- 4000
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
abline(v=3000, col=3, lty=2, lwd=2)

err_pr[450] / err_pr[100]
err_pr[which.min(err_pr)] / err_pr[100]

nts_xgb <- 2750

### Tuning ----

#set.seed(1)
#for(i in 1:nrow(tuning_par)){
#  mod <- gbm(stima$y[idx_a]~., data=X_stima[idx_a,],
#             distribution = "gaussian",
#             shrinkage = tuning_par[i,1], n.trees = nts_xgb,
#             interaction.depth = tuning_par[i,2])
#  passo <- max(30, 1000 * (0.5 - tuning_par[i,1])/(dps/2))
#  alberi_pred <- seq(100, nts_xgb, by = passo)
#  ps <- predict(mod, newdata = X_stima[idx_b,], n.trees = alberi_pred)
#  err_i <- apply(ps, 2, function(p) mean(((stima$y)[idx_b] - p)^2))
#  err_gbm[i] <- min(err_i)
#  cat(round(i/nrow(tuning_par)*100,2), "%\n")
#}
#save(err_gbm, file="Modelli_stimati/err_gbm_fav.RData")
load("Modelli_stimati/err_gbm_fav.RData")
fav_medio_idxb <- mean(stima$y[idx_b])
devianza_totale_idxb <- sum((stima$y[idx_b] - fav_medio_idxb)^2)

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
ggsave("Plot/tuning_gbm_fav.png")

### Fit finale ----

set.seed(123)
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

r2_fav$GBM <- r2_gbm
r2_fav

rmse_gbm <- sqrt(mean((verifica$y - yhat_gbm)^2))
rmse_fav$gbm <- rmse_gbm
rmse_fav

mae_gbm <- mean(abs(verifica$y - yhat_gbm))
mae_fav$gbm <- mae_gbm
mae_fav

colnames(X_stima)
dim(X_stima)

# Trovo le top3 variabili per devianza spiegata
top3_gbm <- summary(mod_gbm, n.trees = n_trees_best,
                    plotit = T, order = TRUE)$var[1:3]
# Indici colonna delle top 3 variabili
idx_top3 <- sapply(top3_gbm, function(v) which(colnames(X_stima) == v))

# Definisco la funzione per calcolare i dati del Partial Dependence Plot singolo
pdp_gbm_single <- function(var_idx, modello, X_data, n_trees, dati_orig,
                           n_grid = 50) {
  # Estraggo il nome della variabile, i dati e verifico se è numerica
  var_name <- colnames(X_data)[var_idx]
  x        <- X_data[[var_name]]
  is_num   <- is.numeric(x)

  # Creo la griglia: sequenza per i numerici (escludendo le code 2%-98%), categorie uniche altrimenti
  griglia  <- if (is_num)
    seq(quantile(x, 0.02), quantile(x, 0.98), length.out = n_grid)
  else
    sort(unique(x))

  # Calcolo la predizione media per ogni valore della griglia
  pds <- sapply(griglia, function(v) {
    tmp <- X_data
    tmp[[var_name]] <- v
    mean(predict(modello, newdata = tmp, n.trees = n_trees))
  })

  # Restituisco i risultati impaginati in un dataframe
  data.frame(x = griglia, pd = pds, variabile = var_name, is_num = is_num)
}

# Calcolo i dati PDP per le top 3 variabili applicando la funzione
pdp_gbm_list <- lapply(idx_top3, pdp_gbm_single,
                       modello   = mod_gbm,
                       X_data    = X_stima,
                       n_trees   = n_trees_best,
                       dati_orig = stima)

# Genero la lista dei 3 grafici ggplot
pdp_gbm_plots <- lapply(pdp_gbm_list, function(df) {

  nome_brutto <- df$variabile[1]
  nome_bello  <- coalesce(nomi_presentazione[nome_brutto], nome_brutto)

  # Inizializzo il grafico gestendo l'asse x in base al tipo di variabile
  p <- ggplot(df, aes(x = if (df$is_num[1]) as.numeric(x) else factor(x),
                      y = pd))

  # Aggiungo la geometria adeguata: linea se numerico, barre se categorico
  if (df$is_num[1]) {
    p <- p + geom_line(color = "#E15759", linewidth = 1.2)
  } else {
    p <- p + geom_col(fill = "#E15759", width = 0.6)
  }

  # Formatto i titoli e il tema estetico
  p + labs(title = nome_bello, x = NULL, y = "Prezzo atteso (€)") +
    theme_minimal(base_size = 11) +
    theme(plot.title = element_text(face = "bold"))
})

library(patchwork)
# Unisco i tre grafici affiancati e aggiungo un titolo globale
p_pdp_gbm <- (pdp_gbm_plots[[1]] | pdp_gbm_plots[[2]] | pdp_gbm_plots[[3]]) +
  plot_annotation(
    theme    = theme(plot.title = element_text(face = "bold", size = 14))
  )

ggsave("Plot/gbm_pdp_top3_fav.png",
       plot = p_pdp_gbm, width = 12, height = 5, dpi = 200)

## Effetti casuali -----

### MERT -----

form <- formula(lm(y~.,
                   data=stima |> dplyr::select(-Brand,
                                               -Condition)))

set.seed(123)
mert_model <- MERT(formula = form , random = " + (1|Brand) + (1|Condition)",
                   data = stima, cv=T)
summary(mert_model$EffectModel)

#plot_tree(mert_model$Tree, "Plot/MERT_fav")

pred_tree <- predict(mert_model$Tree, newdata = verifica)
pred_ran <- predict(mert_model$EffectModel, newdata = verifica)
pred_mert <- pred_tree + pred_ran

devianza_residua_mert <- sum((verifica$y - pred_mert)^2)

r2_mert <- 1 - (devianza_residua_mert / devianza_totale_verifica)

r2_fav$mert <- r2_mert
r2_fav

rmse_mert <- sqrt(mean((verifica$y - pred_mert)^2))
rmse_fav$mert <- rmse_mert
rmse_fav

mae_mert <- mean(abs(verifica$y - pred_mert))
mae_fav$mert <- mae_mert
mae_fav

### MERF -----

form <- formula(lm(y~.,
                   data=stima |> dplyr::select(-Brand)))

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

r2_fav$merf <- r2_merf
r2_fav

rmse_merf <- sqrt(mean((verifica$y - pred_merf)^2))
rmse_fav$merf <- rmse_merf
rmse_fav

mae_merf <- mean(abs(verifica$y - pred_merf))
mae_fav$merf <- mae_merf
mae_fav

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
#r2_fav$metboost <- err_met
#r2_fav

cbind(t(r2_fav), t(rmse_fav), t(mae_fav))


# Classificazione ----

rm(list=ls()); gc();
load("dati_puliti.RData")

#Classificazione sulla qualità: Probit Ordinale

library(MASS)
library(caret)
library(psych)

#Divido i dati
train <- dataset_classificazione_qualita[id_stima, ]
test  <- dataset_classificazione_qualita[id_verifica, ]

#Faccio partire la stepwise dal modello completo
modello_completo <- polr(y ~ ., data = train, method = "probit", Hess = TRUE)
summary(modello_completo)

#Modello nullo come punto di arrivo minimo
modello_nullo <- polr(y ~ 1, data = train, method = "probit", Hess = TRUE)
summary(modello_nullo)

#Stepwise in più direzioni che toglie e aggiunge variabili
modello_step <- step(modello_completo,
                     scope = list(lower = modello_nullo,
                                  upper = modello_completo),
                     direction = "both",
                     trace = 1)
summary(modello_step)


#Il modello stepwise bidirezionale elimina tutte le variabili relative al profilo
#del venditore (rating, recensioni, distintivi e numero di altri articoli )
#tenendo invece (LogPrice, Brand, Size, Favorites_Count, Shipping_Cost, Has_Item_Verification e Color_new.)
#Questo sottolinea che la qualità percepita di una scarpa dipende dalle caratteristiche del prodotto ee non da
#chi lo vende.Questo modello risulta inoltre quello con AIC minore a conferma della scelta.

#LogPrice emerge come la variabile più influente,  al crescere del prezzo aumenta
#la probabilità di appartenere a una classe di qualità superiore
#Mentre per il brand Champion, Diadora e Autry sono associati a qualità più
#elevata, mentre le Marche di Lusso mostrano un coefficiente negativo,
#probabilmente perché vengono rivendute dopo un utilizzo più intenso
#La taglia presenta un effetto negativo significativo, indicando  maggiore
#difficoltà di vendita delle taglie grandi
#la presenza della verifica fisica dell'articolo è associata a qualità
#inferiore risultato intuitivo poiché chi vende scarpe nuove raramente ricorre a questo servizio



#Valutiamo su test set
prob_pred   <- predict(modello_step, newdata = test, type = "probs")
classe_pred <- predict(modello_step, newdata = test, type = "class")

# Conversione a factor ordinato per compatibilità con test$y
classe_pred_ord  <- factor(classe_pred, levels = levels(test$y), ordered = TRUE)
classe_pred_num  <- as.numeric(classe_pred_ord)
classe_reale_num <- as.numeric(test$y)

# Confusion matrix
cm <- confusionMatrix(classe_pred_ord, test$y)
print(cm)

# Metriche modello step
(accuracy_test  <- mean(as.character(classe_pred_ord) == as.character(test$y)))
(mae_test       <- mean(abs(classe_pred_num - classe_reale_num)))
(acc_tol1_test  <- mean(abs(classe_pred_num - classe_reale_num) <= 1))


#Dalla confusion matrix emerge un problema, le classi sono sbilanciate.
#Abbiamo accuracy del 46% che è meno buona del dire sempre che le scarpe sono ottime a caso.

#Notiamo che il modello tende a collassare quasi tutte le predizioni sulle classi ottime
#mentre le discrete e Nuove senza cartellino non vengono mai beccate


#Come possiamo intervenire sullo sbilanciamento?

#1)Peso le classi assegnando ad ogni osservazione un peso inversamente proporzionale
#alla freq della classe.Penalizzo di più errori su classi rare.


#Riassunto: Non risolviamo il problema dello sbilanciamento, proviamo collassando le classi

pesi <- 1 / table(train$y)
pesi_norm <- pesi / sum(pesi) * nlevels(train$y)
w <- as.numeric(pesi_norm[as.character(train$y)])

cat("\nPesi per classe:\n")
print(round(pesi_norm, 3))

modello_pesato <- polr(y ~ LogPrice + Brand + Size + Favorites_Count +
                         Shipping_Cost + Has_Item_Verification + Color_new,
                       data = train, method = "probit",
                       Hess = TRUE, weights = w)
summary(modello_pesato)


classe_pred_pesato <- predict(modello_pesato, newdata = test, type = "class")
classe_pred_pesato_ord <- factor(classe_pred_pesato,
                                 levels = levels(test$y),
                                 ordered = TRUE)

cm_pesato <- confusionMatrix(classe_pred_pesato_ord, test$y)
print(cm_pesato)

accuracy_pesato  <- mean(as.character(classe_pred_pesato_ord) == as.character(test$y))
mae_pesato       <- mean(abs(as.numeric(classe_pred_pesato_ord) - as.numeric(test$y)))
acc_tol1_pesato  <- mean(abs(as.numeric(classe_pred_pesato_ord) - as.numeric(test$y)) <= 1)

cat("\n── Confronto modello step vs pesato ──────────────\n")
cat(sprintf("%-30s %8s %8s\n", "Metrica",           "Step",         "Pesato"))
cat(sprintf("%-30s %8.4f %8.4f\n", "Accuracy",      accuracy_test,  accuracy_pesato))
cat(sprintf("%-30s %8.4f %8.4f\n", "MAE ordinale",  mae_test,       mae_pesato))
cat(sprintf("%-30s %8.4f %8.4f\n", "Acc. tol. 1",   acc_tol1_test,  acc_tol1_pesato))

#RIsultati:

#il modello non collassa più tutte le predizioni su "Ottime"
#e inizia a identificare correttamente le classi rare

#L'accuracy scende però da 46% a 32% e il mae ordinale peggiora da 0.776 a 1.044
#L'unica metrica che migliora nettamente è il Kappa di Cohen, che passa da 0.017 a 0.134,
#segnalando che il modello pesato non è più equivalente a una predizione casuale.


#2)Collasso le classi da 5 a 3 categorie (Usato= Discrete+Buone, Ottime=Ottime, "Nuovo= Nuovo con e senza cartellino)
library(tidyverse)
dataset_classificazione_3classi <- dataset_classificazione_qualita %>%
  mutate(y3 = fct_collapse(y,
                           "Usato"  = c("Discrete", "Buone"),
                           "Ottime" = "Ottime",
                           "Nuovo"  = c("Nuovo senza cartellino", "Nuovo con cartellino")
  ),
  y3 = factor(y3, levels = c("Usato", "Ottime", "Nuovo"), ordered = TRUE)
  ) %>%
  dplyr::select(-y) %>%
  rename(y = y3)


# Distribuzione delle nuove classi
cat("Distribuzione classi collassate:\n")
print(table(dataset_classificazione_3classi$y))
print(round(prop.table(table(dataset_classificazione_3classi$y)) * 100, 1))

train3 <- dataset_classificazione_3classi[id_stima, ]
test3  <- dataset_classificazione_3classi[id_verifica, ]

modello_completo3 <- polr(y ~ ., data = train3, method = "probit", Hess = TRUE)
summary(modello_completo3)

modello_nullo3 <- polr(y ~ 1, data = train3, method = "probit", Hess = TRUE)
summary(modello_nullo3)

modello_step3 <- step(modello_completo3,
                      scope = list(lower = modello_nullo3,
                                   upper = modello_completo3),
                      direction = "both",
                      trace = 1)
summary(modello_step3)



classe_pred3     <- predict(modello_step3, newdata = test3, type = "class")
classe_pred3_ord <- factor(classe_pred3, levels = levels(test3$y), ordered = TRUE)
classe_pred3_num <- as.numeric(classe_pred3_ord)
classe_reale3_num <- as.numeric(test3$y)

# Confusion matrix
cm3 <- confusionMatrix(classe_pred3_ord, test3$y)
print(cm3)

# Metriche
(accuracy3  <- mean(as.character(classe_pred3_ord) == as.character(test3$y)))
(mae3       <- mean(abs(classe_pred3_num - classe_reale3_num)))
(acc_tol1_3 <- mean(abs(classe_pred3_num - classe_reale3_num) <= 1))


cat("\n── Confronto tutti i modelli ─────────────────────────\n")
cat(sprintf("%-30s %8s %8s %8s\n", "Metrica", "Step", "Pesato", "3 Classi"))
cat(sprintf("%-30s %8.4f %8.4f %8.4f\n", "Accuracy",
            accuracy_test, accuracy_pesato, accuracy3))
cat(sprintf("%-30s %8.4f %8.4f %8.4f\n", "MAE ordinale",
            mae_test, mae_pesato, mae3))
cat(sprintf("%-30s %8.4f %8.4f %8.4f\n", "Acc. tol. 1",
            acc_tol1_test, acc_tol1_pesato, acc_tol1_3))



#Risultati:


#Questo risulta essere il modello migliore al momento.
#la distribuzione del target diventa più bilanciata, Usato al 14.5%, Ottime al
#50.3% e Nuovo al 35.3%, rendendo il problema di classificazione più trattabile
#La stepwise ora tiene 8 variabili ( aggiunge ora Seller_has_Distintivi e Material)

#I risultati su test set mostrano un netto miglioramento su tutte le metriche:
#l'accuracy sale a 0.529 (meglio di sparare a caso)

# Il MAE ordinale scende a 0.477, indicando che in media il modello sbaglia
#meno di mezza classe, e l'accuracy con tolleranza di una classe raggiunge il
#99.4%, segno che gli errori gravi — come classificare come Nuovo una scarpa
#Usata — sono praticamente assenti.


#Rimane tuttavia una difficoltà nel riconoscere la classe Usato, la cui
#sensitivity si attesta al 7.7%: su 65 scarpe usate nel test set solo 5 vengono
#identificate correttamente, a conferma che anche con il collasso delle classi
#la sottorappresentazione di questa categoria continua a condizionare le predizioni del modello.



#3) Classi collassate+pesi



# Calcolo pesi inversamente proporzionali alla frequenza
pesi3 <- 1 / table(train3$y)
pesi_norm3 <- pesi3 / sum(pesi3) * nlevels(train3$y)
w3 <- as.numeric(pesi_norm3[as.character(train3$y)])

cat("\nPesi per classe (3 classi):\n")
print(round(pesi_norm3, 3))

# Stima modello pesato con le variabili selezionate dallo stepwise
modello_pesato3 <- polr(y ~ LogPrice + Brand + Size + Material + Favorites_Count +
                          Shipping_Cost + Has_Item_Verification + Seller_has_distintivi,
                        data = train3, method = "probit",
                        Hess = TRUE, weights = w3)
summary(modello_pesato3)

# Predizioni sul test set
classe_pred_p3     <- predict(modello_pesato3, newdata = test3, type = "class")
classe_pred_p3_ord <- factor(classe_pred_p3, levels = levels(test3$y), ordered = TRUE)
classe_pred_p3_num <- as.numeric(classe_pred_p3_ord)

# Confusion matrix
cm_p3 <- confusionMatrix(classe_pred_p3_ord, test3$y)
print(cm_p3)

# Metriche
accuracy_p3  <- mean(as.character(classe_pred_p3_ord) == as.character(test3$y))
mae_p3       <- mean(abs(classe_pred_p3_num - classe_reale3_num))
acc_tol1_p3  <- mean(abs(classe_pred_p3_num - classe_reale3_num) <= 1)

# Confronto completo tutti i modelli
cat("\n── Confronto tutti i modelli ─────────────────────────────\n")
cat(sprintf("%-30s %8s %8s %8s %8s\n",
            "Metrica", "Step", "Pesato", "3 Classi", "3C+Pesi"))
cat(sprintf("%-30s %8.4f %8.4f %8.4f %8.4f\n", "Accuracy",
            accuracy_test, accuracy_pesato, accuracy3, accuracy_p3))
cat(sprintf("%-30s %8.4f %8.4f %8.4f %8.4f\n", "MAE ordinale",
            mae_test, mae_pesato, mae3, mae_p3))
cat(sprintf("%-30s %8.4f %8.4f %8.4f %8.4f\n", "Acc. tol. 1",
            acc_tol1_test, acc_tol1_pesato, acc_tol1_3, acc_tol1_p3))



#Questo modello è il più equilibrato tra tutti.
#La combinazione di collasso delle classi e aggiunta dei pesi funziona meglio delle singole strategie
#perchè : il collasso preliminare riduce lo squilibrio strutturale,
#rendendo i pesi più gestibili e la stima più stabile

#La sensitivity di Usato passa da 7.7% a 66.2% e quella di Nuovo da 37.5% a 63.1%
# Il Kappa di Cohen raggiunge 0.207, il valore più alto tra tutti i modelli testati,
#confermando che l'accordo tra predetto e reale non è attribuibile al caso


#CV

# Eseguiamo la CV sui due modelli estremi:
# - modello_step: il più semplice (5 classi, nessun peso)
# - modello 3C+Pesi: il modello finale scelto
# L'obiettivo è verificare che la performance sul test set fisso
# non dipenda dal particolare split train/test, ma sia stabile.

K=6
risultati_cv <- data.frame(
  fold        = 1:K,
  acc_step    = NA,
  mae_step    = NA,
  acc_3c_pesi = NA,
  mae_3c_pesi = NA
)

for (k in 1:K) {

  cat(sprintf("\nFold %d/%d...", k, K))

  train_k <- dataset_classificazione_qualita[fold_id != k, ]
  test_k  <- dataset_classificazione_qualita[fold_id == k, ]

  mod_step_k <- suppressWarnings(
    polr(y ~ LogPrice + Brand + Size + Favorites_Count +
           Shipping_Cost + Has_Item_Verification + Color_new,
         data = train_k, method = "probit", Hess = TRUE)
  )

  pred_step_k <- factor(
    predict(mod_step_k, newdata = test_k),
    levels = levels(test_k$y), ordered = TRUE
  )

  risultati_cv$acc_step[k] <- mean(
    as.character(pred_step_k) == as.character(test_k$y)
  )
  risultati_cv$mae_step[k] <- mean(
    abs(as.numeric(pred_step_k) - as.numeric(test_k$y))
  )

  train3_k <- dataset_classificazione_3classi[fold_id != k, ]
  test3_k  <- dataset_classificazione_3classi[fold_id == k, ]

  pesi_k      <- 1 / table(train3_k$y)
  pesi_norm_k <- pesi_k / sum(pesi_k) * nlevels(train3_k$y)
  w_k         <- as.numeric(pesi_norm_k[as.character(train3_k$y)])

  mod_3cp_k <- suppressWarnings(
    polr(y ~ LogPrice + Brand + Size + Material + Favorites_Count +
           Shipping_Cost + Has_Item_Verification + Seller_has_distintivi,
         data = train3_k, method = "probit",
         Hess = TRUE, weights = w_k)
  )

  pred_3cp_k <- factor(
    predict(mod_3cp_k, newdata = test3_k),
    levels = levels(test3_k$y), ordered = TRUE
  )

  risultati_cv$acc_3c_pesi[k] <- mean(
    as.character(pred_3cp_k) == as.character(test3_k$y)
  )
  risultati_cv$mae_3c_pesi[k] <- mean(
    abs(as.numeric(pred_3cp_k) - as.numeric(test3_k$y))
  )
}
#Risultati per fold
print(round(risultati_cv, 4))

cat("\n Riepilogo Cross-Validationn")
cat(sprintf("%-30s %8s %8s\n", "Metrica", "Step", "3C+Pesi"))
cat(sprintf("%-30s %8.4f %8.4f\n", "Accuracy media",
            mean(risultati_cv$acc_step),
            mean(risultati_cv$acc_3c_pesi)))
cat(sprintf("%-30s %8.4f %8.4f\n", "Accuracy SD",
            sd(risultati_cv$acc_step),
            sd(risultati_cv$acc_3c_pesi)))
cat(sprintf("%-30s %8.4f %8.4f\n", "MAE medio",
            mean(risultati_cv$mae_step),
            mean(risultati_cv$mae_3c_pesi)))
cat(sprintf("%-30s %8.4f %8.4f\n", "MAE SD",
            sd(risultati_cv$mae_step),
            sd(risultati_cv$mae_3c_pesi)))


#Risultati:

#Viene confermata la scelta del modello con collasso +pesi.
#Le accuracy sono identiche (0.498 vs 0.500) ma il mae medio
#è 0.552 vs 0.724. Il modello finale sbaglia meno.



#Grafici

library(ggplot2)

#1. Confusion Matrix visuale (modello finale 3C+Pesi

cm_df <- as.data.frame(cm_p3$table)

# Aggiunge percentuale per ogni classe reale (utile per leggere la sensitivity)
cm_df <- cm_df %>%
  group_by(Reference) %>%
  mutate(Perc = round(Freq / sum(Freq) * 100, 1)) %>%
  ungroup()

ggplot(cm_df, aes(x = Reference, y = Prediction, fill = Perc)) +
  geom_tile(color = "white", linewidth = 0.8) +
  geom_text(aes(label = paste0(Freq, "\n(", Perc, "%)")), size = 4.5) +
  scale_fill_gradient(low = "white", high = "#2C7BB6",
                      name = "% per\nclasse reale") +
  labs(
    title    = "Confusion Matrix — Modello 3 Classi + Pesi",
    subtitle = "Valori assoluti e percentuale per classe reale (sensitivity per colonna)",
    x        = "Classe reale",
    y        = "Classe predetta"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title    = element_text(face = "bold"),
    panel.grid    = element_blank(),
    axis.text     = element_text(size = 11)
  )

#Mostriamo che Usato viene classificato bene nel 66.2% dei casi
#Nuovo viene classificato bene nel 63%
#Ottimo classifica correttamente solo il 30% (Questo è normale perchè è la più ambigua)
#ha elementi in comune con tutte le altre.




#2. Accuracy per fold — Step vs 3C+Pesi
cv_long <- risultati_cv %>%
  dplyr::select(fold, acc_step, acc_3c_pesi) %>%
  pivot_longer(cols = c(acc_step, acc_3c_pesi),
               names_to  = "Modello",
               values_to = "Accuracy") %>%
  mutate(Modello = case_match(Modello,
                              "acc_step"    ~ "Step (5 classi)",
                              "acc_3c_pesi" ~ "3 Classi + Pesi"))

# Medie per linea orizzontale
medie <- cv_long %>%
  group_by(Modello) %>%
  summarise(Media = mean(Accuracy))

ggplot(cv_long, aes(x = fold, y = Accuracy, color = Modello, group = Modello)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_hline(data = medie,
             aes(yintercept = Media, color = Modello),
             linetype = "dashed", linewidth = 0.8) +
  scale_x_continuous(breaks = 1:6) +
  scale_color_manual(values = c("Step (5 classi)" = "#E74C3C",
                                "3 Classi + Pesi" = "#2C7BB6")) +
  labs(
    title    = "Accuracy per fold — Cross-Validation 6 fold",
    subtitle = "Le linee tratteggiate indicano l'accuracy media di ciascun modello",
    x        = "Fold",
    y        = "Accuracy",
    color    = "Modello"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title   = element_text(face = "bold"),
    legend.position = "bottom"
  )

#Da questo grafico si vede che
#I due modelli sono equivalenti in termini di accuracy media su cv
#Il modello finale ha una variabilità maggiore. La causa probabile è che i pesi vengono ricalcolati ad ogni fold sulla distribuzione locale delle classi — se per caso un fold ha pochi elementi di una classe, il peso cambia molto e la performance oscilla.



#3. MAE per fold — Step vs 3C+Pesi

mae_long <- risultati_cv %>%
  dplyr::select(fold, mae_step, mae_3c_pesi) %>%
  pivot_longer(cols = c(mae_step, mae_3c_pesi),
               names_to  = "Modello",
               values_to = "MAE") %>%
  mutate(Modello = case_match(Modello,
                              "mae_step"    ~ "Step (5 classi)",
                              "mae_3c_pesi" ~ "3 Classi + Pesi"))

medie_mae <- mae_long %>%
  group_by(Modello) %>%
  summarise(Media = mean(MAE))

ggplot(mae_long, aes(x = fold, y = MAE, color = Modello, group = Modello)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_hline(data = medie_mae,
             aes(yintercept = Media, color = Modello),
             linetype = "dashed", linewidth = 0.8) +
  scale_x_continuous(breaks = 1:6) +
  scale_color_manual(values = c("Step (5 classi)" = "#E74C3C",
                                "3 Classi + Pesi" = "#2C7BB6")) +
  labs(
    title    = "MAE ordinale per fold — Cross-Validation 6 fold",
    subtitle = "Le linee tratteggiate indicano il MAE medio di ciascun modello",
    x        = "Fold",
    y        = "MAE ordinale",
    color    = "Modello"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title   = element_text(face = "bold"),
    legend.position = "bottom"
  )



#Da qua si osserva come su qualsiasi suddivisione dei dati il modello finale sbaglia sempre meno in termini di distanza ordinale dalla classe corretta
#Complessivamente questo grafico è la conferma più robusta della scelta del modello 3C+Pesi come soluzione finale: la sua superiorità sul MAE è consistente, stabile e indipendente dal campionamento, che è esattamente ciò che si vuole verificare con una cross-validation.



#Ma si può migliorare ancora di più?
#Riassunto delle prossime 300 righe (693):no


# RANDOM FOREST ORDINALE
library(ordinalForest)
library(caret)
library(ggplot2)
library(dplyr)

# Pulizia dataframe per ordinalForest
# ordinalForest richiede che tutte le variabili siano
# factor o numeric, senza attributi "names" sulle colonne chr

pulisci_per_rf <- function(df) {
  as.data.frame(df) %>%
    mutate(
      Color_new             = factor(unname(Color_new)),
      Material              = factor(Material),
      Is_Boosted            = factor(Is_Boosted),
      Has_Item_Verification = factor(Has_Item_Verification)
    )
}

train3_clean <- pulisci_per_rf(train3)
test3_clean  <- pulisci_per_rf(test3)

# Verifica struttura
str(train3_clean)

#  Stima del modello

set.seed(1)
rf_ordinale <- ordfor(
  depvar       = "y",
  data         = train3_clean,
  nsets        = 1000,
  ntreeperdiv  = 100,
  ntreefinal   = 5000,
  perffunction = "equal"
)

print(rf_ordinale)

#Importanza delle variabili

var_imp_df <- data.frame(
  Variabile  = names(rf_ordinale$varimp),
  Importanza = as.numeric(rf_ordinale$varimp)
) %>% arrange(desc(Importanza))

print(var_imp_df)

ggplot(var_imp_df, aes(x = reorder(Variabile, Importanza), y = Importanza)) +
  geom_col(fill = "#2C7BB6", alpha = 0.85) +
  coord_flip() +
  labs(
    title = "Importanza delle variabili — Random Forest Ordinale",
    x     = NULL,
    y     = "Importanza"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))

#  Valutazione sul test set

pred_rf     <- predict(rf_ordinale, newdata = test3_clean)$ypred
pred_rf_ord <- factor(pred_rf, levels = levels(test3_clean$y), ordered = TRUE)
pred_rf_num <- as.numeric(pred_rf_ord)
reale3_num  <- as.numeric(test3_clean$y)

# Confusion matrix
cm_rf <- confusionMatrix(pred_rf_ord, test3_clean$y)
print(cm_rf)

# Metriche
accuracy_rf  <- mean(as.character(pred_rf_ord) == as.character(test3_clean$y))
mae_rf       <- mean(abs(pred_rf_num - reale3_num))
acc_tol1_rf  <- mean(abs(pred_rf_num - reale3_num) <= 1)

cat("\n── Metriche Test Set (Random Forest Ordinale) ──\n")
cat("Accuracy:               ", accuracy_rf,  "\n")
cat("MAE ordinale:           ", mae_rf,        "\n")
cat("Accuracy (tolleranza 1):", acc_tol1_rf,   "\n")

#Confusion matrix visuale

cm_rf_df <- as.data.frame(cm_rf$table) %>%
  group_by(Reference) %>%
  mutate(Perc = round(Freq / sum(Freq) * 100, 1)) %>%
  ungroup()

ggplot(cm_rf_df, aes(x = Reference, y = Prediction, fill = Perc)) +
  geom_tile(color = "white", linewidth = 0.8) +
  geom_text(aes(label = paste0(Freq, "\n(", Perc, "%)")), size = 4.5) +
  scale_fill_gradient(low = "white", high = "#2C7BB6",
                      name = "% per\nclasse reale") +
  labs(
    title    = "Confusion Matrix — Random Forest Ordinale",
    subtitle = "Valori assoluti e percentuale per classe reale",
    x        = "Classe reale",
    y        = "Classe predetta"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid = element_blank()
  )

# Confronto tutti i modelli

cat("\n Confronto tutti i modelli n")
cat(sprintf("%-30s %8s %8s %8s %8s %8s\n",
            "Metrica", "Step", "Pesato", "3 Classi", "3C+Pesi", "RF Ord."))
cat(sprintf("%-30s %8.4f %8.4f %8.4f %8.4f %8.4f\n", "Accuracy",
            accuracy_test, accuracy_pesato, accuracy3, accuracy_p3, accuracy_rf))
cat(sprintf("%-30s %8.4f %8.4f %8.4f %8.4f %8.4f\n", "MAE ordinale",
            mae_test, mae_pesato, mae3, mae_p3, mae_rf))
cat(sprintf("%-30s %8.4f %8.4f %8.4f %8.4f %8.4f\n", "Acc. tol. 1",
            acc_tol1_test, acc_tol1_pesato, acc_tol1_3, acc_tol1_p3, acc_tol1_rf))
cat(sprintf("%-30s %8.4f %8.4f %8.4f %8.4f %8.4f\n", "Kappa",
            cm$overall["Kappa"], cm_pesato$overall["Kappa"],
            cm3$overall["Kappa"], cm_p3$overall["Kappa"],
            cm_rf$overall["Kappa"]))

# Cross-Validation

risultati_cv_rf <- data.frame(
  fold   = 1:K,
  acc_rf = NA,
  mae_rf = NA
)

for (k in 1:K) {
  cat(sprintf("\nFold %d/%d...", k, K))

  train3_k <- pulisci_per_rf(dataset_classificazione_3classi[fold_id != k, ])
  test3_k  <- pulisci_per_rf(dataset_classificazione_3classi[fold_id == k, ])

  set.seed(k)
  mod_rf_k <- ordfor(
    depvar       = "y",
    data         = train3_k,
    nsets        = 500,
    ntreeperdiv  = 50,
    ntreefinal   = 500,
    perffunction = "equal"
  )

  pred_rf_k     <- predict(mod_rf_k, newdata = test3_k)$ypred
  pred_rf_k_ord <- factor(pred_rf_k, levels = levels(test3_k$y), ordered = TRUE)
  pred_rf_k_num <- as.numeric(pred_rf_k_ord)
  reale_k_num   <- as.numeric(test3_k$y)

  risultati_cv_rf$acc_rf[k] <- mean(
    as.character(pred_rf_k_ord) == as.character(test3_k$y)
  )
  risultati_cv_rf$mae_rf[k] <- mean(abs(pred_rf_k_num - reale_k_num))
}

cat("\n\n── Risultati CV per fold (Random Forest Ordinale) ──\n")
print(round(risultati_cv_rf, 4))

#Riepilogo CV — Confronto 3C+Pesi vs RF

cat("\n  Riepilogo CV — 3C+Pesi vs RF Ordinalen")
cat(sprintf("%-30s %8s %8s\n", "Metrica", "3C+Pesi", "RF Ord."))
cat(sprintf("%-30s %8.4f %8.4f\n", "Accuracy media",
            mean(risultati_cv$acc_3c_pesi), mean(risultati_cv_rf$acc_rf)))
cat(sprintf("%-30s %8.4f %8.4f\n", "Accuracy SD",
            sd(risultati_cv$acc_3c_pesi), sd(risultati_cv_rf$acc_rf)))
cat(sprintf("%-30s %8.4f %8.4f\n", "MAE medio",
            mean(risultati_cv$mae_3c_pesi), mean(risultati_cv_rf$mae_rf)))
cat(sprintf("%-30s %8.4f %8.4f\n", "MAE SD",
            sd(risultati_cv$mae_3c_pesi), sd(risultati_cv_rf$mae_rf)))

#  Grafico CV — Accuracy per fold

cv_long_rf <- data.frame(
  fold    = rep(1:K, 2),
  Accuracy = c(risultati_cv$acc_3c_pesi, risultati_cv_rf$acc_rf),
  Modello  = rep(c("3 Classi + Pesi", "RF Ordinale"), each = K)
)

medie_rf <- cv_long_rf %>%
  group_by(Modello) %>%
  summarise(Media = mean(Accuracy))

ggplot(cv_long_rf, aes(x = fold, y = Accuracy, color = Modello, group = Modello)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_hline(data = medie_rf,
             aes(yintercept = Media, color = Modello),
             linetype = "dashed", linewidth = 0.8) +
  scale_x_continuous(breaks = 1:K) +
  scale_color_manual(values = c("3 Classi + Pesi" = "#E74C3C",
                                "RF Ordinale"     = "#2C7BB6")) +
  labs(
    title    = "Accuracy per fold — 3C+Pesi vs RF Ordinale",
    subtitle = "Le linee tratteggiate indicano l'accuracy media",
    x        = "Fold",
    y        = "Accuracy",
    color    = "Modello"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title      = element_text(face = "bold"),
    legend.position = "bottom"
  )


#Rispetto al modello finale precedente:
#Usato 66 vs 26 (RF) peggioro tanto
#Ottime 30 vs 37 miglioro
#Nuovo 63 vs 83 miglioro



#E se facessi stacking dei diversi modelli? Ho provato per RF ordinale e Finale
#Ma non prediceva praticamente mai usato.

#Grafico Riassuntivo finale
# ──────────────────────────────────────────────
# GRAFICO 1 — 3C+Pesi vs RF Ordinale
# ──────────────────────────────────────────────

library(ggplot2)
library(dplyr)
library(tidyr)

# ── Dataset ──────────────────────────────────

df_g1 <- data.frame(
  Modello  = c("3C+Pesi", "RF Ordinale"),
  Accuracy = c(accuracy_p3, accuracy_rf),
  MAE      = c(mae_p3, mae_rf),
  Kappa    = c(cm_p3$overall["Kappa"], cm_rf$overall["Kappa"]),
  Sensitivity_Usato = c(
    cm_p3$byClass["Class: Usato", "Sensitivity"],
    cm_rf$byClass["Class: Usato", "Sensitivity"]
  ),
  Sensitivity_Ottime = c(
    cm_p3$byClass["Class: Ottime", "Sensitivity"],
    cm_rf$byClass["Class: Ottime", "Sensitivity"]
  ),
  Sensitivity_Nuovo = c(
    cm_p3$byClass["Class: Nuovo", "Sensitivity"],
    cm_rf$byClass["Class: Nuovo", "Sensitivity"]
  )
)

df_g1$Modello <- factor(df_g1$Modello, levels = df_g1$Modello)

df_g1_long <- df_g1 %>%
  pivot_longer(
    cols      = -Modello,
    names_to  = "Metrica",
    values_to = "Valore"
  ) %>%
  mutate(Metrica = case_match(Metrica,
                              "Accuracy"           ~ "Accuracy",
                              "MAE"                ~ "MAE\n(↓ meglio)",
                              "Kappa"              ~ "Kappa",
                              "Sensitivity_Usato"  ~ "Sensitivity\nUsato",
                              "Sensitivity_Ottime" ~ "Sensitivity\nOttime",
                              "Sensitivity_Nuovo"  ~ "Sensitivity\nNuovo"
  ),
  Metrica = factor(Metrica, levels = c(
    "Accuracy", "Kappa", "MAE\n(↓ meglio)",
    "Sensitivity\nUsato", "Sensitivity\nOttime", "Sensitivity\nNuovo"
  )))


colori_g1 <- c("3C+Pesi" = "#27AE60", "RF Ordinale" = "#2980B9")

ggplot(df_g1_long, aes(x = Modello, y = Valore, fill = Modello)) +
  geom_col(alpha = 0.85, width = 0.6) +
  geom_text(aes(label = round(Valore, 2)),
            vjust = -0.4, size = 4, fontface = "bold") +
  facet_wrap(~ Metrica, scales = "free_y", nrow = 1) +
  scale_fill_manual(values = colori_g1) +
  labs(
    title    = "3C+Pesi vs RF Ordinale",
    subtitle = "Confronto diretto dei due modelli migliori — test set (n = 478)",
    x        = NULL,
    y        = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title      = element_text(face = "bold", size = 14),
    plot.subtitle   = element_text(color = "grey40"),
    legend.position = "none",
    axis.text.x     = element_text(size = 10),
    strip.text      = element_text(face = "bold", size = 10),
    panel.spacing   = unit(1.2, "lines")
  )


# ──────────────────────────────────────────────
# GRAFICO 2 — 3 Classi vs 3C+Pesi vs Stacking
# ──────────────────────────────────────────────
df_g2 <- data.frame(
  Modello  = c("Solo Pesi\n(5 classi)", "Solo Collasso\n(3 classi)", "Collasso+Pesi\n(3 classi)"),
  Accuracy = c(accuracy_pesato, accuracy3, accuracy_p3),
  MAE      = c(mae_pesato, mae3, mae_p3),
  Kappa    = c(cm_pesato$overall["Kappa"], cm3$overall["Kappa"],
               cm_p3$overall["Kappa"]),
  Sensitivity_Usato = c(
    cm_pesato$byClass["Class: Discrete", "Sensitivity"],
    cm3$byClass["Class: Usato", "Sensitivity"],
    cm_p3$byClass["Class: Usato", "Sensitivity"]
  ),
  Sensitivity_Ottime = c(
    cm_pesato$byClass["Class: Ottime", "Sensitivity"],
    cm3$byClass["Class: Ottime", "Sensitivity"],
    cm_p3$byClass["Class: Ottime", "Sensitivity"]
  ),
  Sensitivity_Nuovo = c(
    cm_pesato$byClass["Class: Nuovo con cartellino", "Sensitivity"],
    cm3$byClass["Class: Nuovo", "Sensitivity"],
    cm_p3$byClass["Class: Nuovo", "Sensitivity"]
  )
)

df_g2$Modello <- factor(df_g2$Modello, levels = df_g2$Modello)

# ← questa parte mancava
df_g2_long <- df_g2 %>%
  pivot_longer(
    cols      = -Modello,
    names_to  = "Metrica",
    values_to = "Valore"
  ) %>%
  mutate(Metrica = case_match(Metrica,
                              "Accuracy"           ~ "Accuracy",
                              "MAE"                ~ "MAE\n(↓ meglio)",
                              "Kappa"              ~ "Kappa",
                              "Sensitivity_Usato"  ~ "Sensitivity\nUsato",
                              "Sensitivity_Ottime" ~ "Sensitivity\nOttime",
                              "Sensitivity_Nuovo"  ~ "Sensitivity\nNuovo"
  ),
  Metrica = factor(Metrica, levels = c(
    "Accuracy", "Kappa", "MAE\n(↓ meglio)",
    "Sensitivity\nUsato", "Sensitivity\nOttime", "Sensitivity\nNuovo"
  )))

colori_g2 <- c(
  "Solo Pesi\n(5 classi)"     = "#E74C3C",
  "Solo Collasso\n(3 classi)" = "#F39C12",
  "Collasso+Pesi\n(3 classi)" = "#27AE60"
)

ggplot(df_g2_long, aes(x = Modello, y = Valore, fill = Modello)) +
  geom_col(alpha = 0.85, width = 0.6) +
  geom_text(aes(label = round(Valore, 2)),
            vjust = -0.4, size = 4, fontface = "bold") +
  facet_wrap(~ Metrica, scales = "free_y", nrow = 1) +
  scale_fill_manual(values = colori_g2) +
  labs(
    title    = "Effetto delle strategie per lo sbilanciamento",
    subtitle = "Solo Pesi vs Solo Collasso vs Collasso+Pesi — test set (n = 478)",
    x        = NULL,
    y        = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title      = element_text(face = "bold", size = 14),
    plot.subtitle   = element_text(color = "grey40"),
    legend.position = "none",
    axis.text.x     = element_text(size = 10),
    strip.text      = element_text(face = "bold", size = 10),
    panel.spacing   = unit(1.2, "lines")
  )
#Analisi errori

# ──────────────────────────────────────────────
# ANALISI DEGLI ERRORI — MODELLO 3C+Pesi
# ──────────────────────────────────────────────

library(ggplot2)
library(dplyr)

# ── Dataset errori ────────────────────────────
# Costruiamo un dataframe con predetto, reale e flag errore

errori_df <- test3 %>%
  as.data.frame() %>%
  mutate(
    Predetto  = as.character(classe_pred_p3_ord),
    Reale     = as.character(y),
    Errore    = Predetto != Reale,
    Distanza  = abs(as.numeric(classe_pred_p3_ord) - as.numeric(y))
  )

cat("Tasso di errore complessivo:", mean(errori_df$Errore), "\n")
cat("Errori totali:", sum(errori_df$Errore), "su", nrow(errori_df), "\n\n")

# ── 1. Errori per Brand ───────────────────────

errori_brand <- errori_df %>%
  group_by(Brand) %>%
  summarise(
    N          = n(),
    N_errori   = sum(Errore),
    Tasso      = mean(Errore)
  ) %>%
  filter(N >= 10) %>%   # esclude brand con poche osservazioni
  arrange(desc(Tasso))

print(errori_brand)

ggplot(errori_brand, aes(x = reorder(Brand, Tasso), y = Tasso, fill = Tasso)) +
  geom_col(alpha = 0.85) +
  geom_text(aes(label = paste0(N_errori, "/", N)),
            hjust = -0.1, size = 3.5) +
  coord_flip() +
  scale_fill_gradient(low = "#F7DC6F", high = "#E74C3C") +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
  labs(
    title    = "Tasso di errore per Brand — Modello 3C+Pesi",
    subtitle = "Solo brand con almeno 10 osservazioni nel test set",
    x        = NULL,
    y        = "Tasso di errore"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title      = element_text(face = "bold"),
    legend.position = "none"
  )


#Il tasso di errore è molto più alto per i brand più comuni nel dataset (Nike )
#robabilmente perché la loro ampia varietà di modelli copre tutte e tre le classi di qualità in modo indistinguibile

# Le Marche di Lusso, Vans e Autry mostrano invece i tassi più bassi (attorno al 33%), verosimilmente perché tendono a concentrarsi in fasce di qualità più specifiche e riconoscibili dal modello.



# ── 2. Errori per fascia di prezzo ────────────

errori_df <- errori_df %>%
  mutate(Fascia_Prezzo = cut(
    exp(LogPrice),   # riconvertiamo da log a prezzo originale
    breaks = c(0, 10, 20, 35, 50, Inf),
    labels = c("< 10€", "10-20€", "20-35€", "35-50€", "> 50€"),
    right  = FALSE
  ))

errori_prezzo <- errori_df %>%
  group_by(Fascia_Prezzo) %>%
  summarise(
    N        = n(),
    N_errori = sum(Errore),
    Tasso    = mean(Errore)
  )

print(errori_prezzo)

ggplot(errori_prezzo, aes(x = Fascia_Prezzo, y = Tasso, fill = Fascia_Prezzo)) +
  geom_col(alpha = 0.85, width = 0.6) +
  geom_text(aes(label = paste0(N_errori, "/", N)),
            vjust = -0.4, size = 4) +
  scale_fill_brewer(palette = "RdYlGn", direction = -1) +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
  labs(
    title    = "Tasso di errore per fascia di prezzo — Modello 3C+Pesi",
    subtitle = "Prezzo originale (exp(LogPrice))",
    x        = "Fascia di prezzo",
    y        = "Tasso di errore"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title      = element_text(face = "bold"),
    legend.position = "none"
  )


#Le scarpe economiche sono difficili da classificare perché possono essere sia
#usate che nuove a prezzi simili. Le scarpe molto costose invece tendono ad
#essere nuove o quasi nuove ma il modello fatica a distinguerle dalle Ottime.

#La fascia 35-50€ è quella dove il segnale del prezzo è più informativo per la
#qualità.




# ── 5. Mappa degli errori: reale vs predetto ──
# Dove sbaglia il modello? Quali classi confonde di più?

errori_dettaglio <- errori_df %>%
  filter(Errore) %>%
  count(Reale, Predetto) %>%
  mutate(
    Label = paste0("Reale: ", Reale, "\n→ Predetto: ", Predetto)
  )

ggplot(errori_dettaglio, aes(x = Reale, y = Predetto, size = n, color = n)) +
  geom_point(alpha = 0.85) +
  geom_text(aes(label = n), vjust = -1.2, size = 4, color = "black") +
  scale_size_continuous(range = c(5, 20)) +
  scale_color_gradient(low = "#F7DC6F", high = "#E74C3C") +
  labs(
    title    = "Mappa degli errori — Modello 3C+Pesi",
    subtitle = "Dimensione proporzionale al numero di errori",
    x        = "Classe reale",
    y        = "Classe predetta",
    size     = "N errori",
    color    = "N errori"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "right"
  )


#. I due errori più frequenti sono le scarpe Ottime classificate come Usato
#(86 casi) e le scarpe Ottime classificate come Nuovo (79 casi)
#insieme rappresentano 165 errori su 252 totali, quasi il 65% di tutti gli sbagli.


#la classe Ottime è strutturalmente ambigua e il modello fatica a tenerla
#distinta dalle classi adiacenti in entrambe le direzioni. Gli errori gravi
#classificare Nuovo come Usato o viceversa, saltando una classe
#sono invece rari (17 e 5 casi rispettivamente), confermando che il
#modello rispetta l'ordinamento anche quando sbaglia.




#Commento finale

#L'analisi degli errori del modello 3C+Pesi rivela pattern coerenti che aiutano
#a comprendere i limiti strutturali della classificazione. Il tasso di errore
#complessivo si attesta intorno al 53%, distribuito però in modo non uniforme
#tra le variabili esplicative.
#Sul fronte dei brand, Nike e Champion mostrano i tassi più elevati nonostante
#— o proprio perché — sono i brand più rappresentati nel dataset: la loro ampia
#varietà di modelli copre trasversalmente tutte le classi di qualità, rendendo
#il brand da solo poco informativo per distinguere uno stato di usura dall'altro.
#All'opposto, Asics, Autry e Vans mostrano tassi più contenuti, probabilmente
#perché la loro clientela tende a concentrarsi in fasce di qualità più specifiche.
#Il prezzo conferma il suo ruolo ambiguo nella fascia bassa: sotto i 20€ il tasso
#di errore supera il 60%, perché a prezzi così bassi coesistono sia scarpe molto
#usate che scarpe nuove di brand economici, rendendo il segnale del prezzo poco
#discriminante. La fascia 35-50€ è invece quella dove il modello performa meglio,
#suggerendo che è in questa fascia che il prezzo e la qualità sono più correlati
#in modo consistente.
#Colore e taglia non mostrano pattern forti — le differenze tra categorie sono
#contenute e non sistematiche — confermando che queste variabili contribuiscono
#marginalmente alla classificazione della qualità, come già suggerito dalla
#procedura stepwise.
#Il dato più rilevante emerge dalla mappa degli errori: il 65% degli sbagli
#coinvolge la classe Ottime, classificata erroneamente sia come Usato che come
#Nuovo in proporzioni simili. Questo non è un fallimento del modello ma una
#conseguenza della natura ordinale del problema — la classe centrale è per
#definizione quella più ambigua, collocata al confine tra le altre due e
#priva di caratteristiche univoche che la distinguano nettamente.
#Gli errori gravi, ovvero classificare Nuovo come Usato o viceversa
#saltando completamente la classe intermedia, sono invece rari
#(22 casi su 252 totali), a conferma che il modello rispetta la
#struttura ordinale del problema anche quando sbaglia.

# MBA ----

rm(list=ls()); gc();
load("dati_puliti.RData")

## Definizione liste brand scarpe e abbigliamento ----
brand_dal_dataset <- unique(na.omit(tolower(trimws(dataset_MBA$Brand_raw))))

brand_sport_sneakers <- c("nike", "adidas", "puma", "asics", "new balance",
                          "reebok", "converse", "vans", "salomon", "hoka",
                          "mizuno", "diadora", "saucony", "jordan", "under armour",
                          "fila", "kappa", "champion", "sketchers")

brand_street_outdoor <- c("carhartt", "stussy", "supreme", "the north face",
                          "patagonia", "dickies", "levis", "levi s", "timberland",
                          "dr martens", "napapijri", "eastpak", "columbia",
                          "obey", "stone island", "cp company", "woolrich")

brand_casual_premium <- c("ralph lauren", "calvin klein", "tommy hilfiger",
                          "guess", "lacoste", "fred perry", "diesel", "armani",
                          "hugo boss", "michael kors", "furla", "pinko", "liu jo",
                          "twinset", "patrizia pepe", "geox", "nero giardini")

brand_fast_fashion <- c("zara", "hm", "pull and bear", "mango", "bershka",
                        "stradivarius", "shein", "asos", "primark", "kiabi",
                        "ovs", "piazza italia", "tezenis", "intimissimi",
                        "calzedonia", "benetton", "sisley")

brand_luxury <- c("gucci", "prada", "louis vuitton", "dior", "chanel",
                  "versace", "balenciaga", "moncler", "valentino", "moschino",
                  "fendi", "bottega veneta", "saint laurent", "givenchy",
                  "burberry", "dolce gabbana", "dolce and gabbana")

brand_manuali_completi <- c(brand_sport_sneakers, brand_street_outdoor,
                            brand_casual_premium, brand_fast_fashion, brand_luxury)

brand_unici <- unique(c(brand_dal_dataset, brand_manuali_completi))

parole_da_bannare <- c("us", "chaussures", "talla", "scarpe", "shoes",
                       "size", "pointure", "ignoto", "fashion",
                       "vintage dressing", "vintage boutique", "shoe",
                       "worn", "wear", "clo", "idk", "mix feel",
                       "numero", "numeriś", "mai", "made in italy",
                       "vera pelle", "casual", "bambina", "inconnu",
                       "nobrand.pt", "weiss")

# Filtriamo il dizionario!
brand_unici <- brand_unici[!(brand_unici %in% parole_da_bannare)]

brand_unici[grep(pattern ="dolce & gabbana|d&g|dolce e gabbana|dolce gabbana|dolce and gabbana", brand_unici)] <- "dolce & gabbana"
brand_unici[grep(pattern ="levis|levi's|levi_s|levi s", brand_unici)] <- "levi's"
brand_unici <- unique(brand_unici)

brand_url_format <- gsub(" ", "_", brand_unici)


## Rimuovo i venditori PRO
#pro <- which(dataset_MBA$Seller_Is_Pro == TRUE)
#dataset_MBA1 <- dataset_MBA[-pro,]


## Trovare righe con NA in Other_Items_Previewed_URLs
sum(is.na(dataset_MBA$Other_Items_Previewed_URLs))

## Trovare venditori con più righe
#pluto = which(table(dataset_MBA$Seller_User)>1)
#str(pluto)
which(table(dataset_MBA$Seller_User)>1) |> str()
utenti_non_unici <- which(table(dataset_MBA$Seller_User)>1) |> names()

length(unique(dataset_MBA$Seller_User))

## Definizione di una funzione per estrarre brand da URL ----
estrai_brand_da_url <- function(url_string) {
  if (is.na(url_string) || url_string == "") return(character(0))
  brand_trovati <- character(0)
  url_string <- gsub(pattern = "dolce & gabbana|d&g|dolce e gabbana|dolce gabbana|dolce and gabbana",
                     replacement = "dolce & gabbana", url_string)
  url_string <- gsub(pattern = "levis|levi's|levi_s|levi s",
                     replacement = "levi's", url_string)
  singoli_link <- strsplit(url_string, "\\|")[[1]]
  for (link in singoli_link) {
    link_minuscolo <- tolower(link)
    for (brand in brand_url_format) {
      if (grepl(brand, link_minuscolo, fixed = TRUE)) {
        brand_pulito <- tolower(gsub("-", " ", brand))
        brand_pulito <- gsub(" ", "_", brand_pulito)
        brand_trovati <- c(brand_trovati, brand_pulito)
      }
    }
  }
  return(unique(brand_trovati))
}




## Estrazione brand da Other_Items_Previewed_URLs per ogni riga del dataset ----
dataset_MBA$Armadio_Basket <- lapply(dataset_MBA$Other_Items_Previewed_URLs, estrai_brand_da_url)

# Aggiunta del brand principale (utile soprattutto se non c'è tra gli "Other_Items")
dataset_MBA$Armadio_Basket <- mapply(function(basket, brand_corrente) {
  if (!is.na(brand_corrente) && brand_corrente != "") {
    brand_pulito <- tolower(gsub("-", " ", brand_corrente))
    brand_pulito <- gsub(" ", "_", brand_pulito)
    return(unique(c(basket, brand_pulito)))
  } else { return(basket) }
}, dataset_MBA$Armadio_Basket, dataset_MBA$Brand_raw, SIMPLIFY = FALSE)


## Creo matrice di incidenza
m0 <- matrix(0, nrow = length(unique(dataset_MBA$Seller_User)), ncol = length(brand_url_format))
rownames(m0) <- unique(dataset_MBA$Seller_User)
colnames(m0) <- gsub("-", "_", brand_url_format)

sort(colnames(m0))

for(i in 1:nrow(dataset_MBA)){
  nome_utente <- dataset_MBA$Seller_User[i]
  armadio_attuale <- unlist(dataset_MBA[i,"Armadio_Basket"])
  for(j in armadio_attuale){
    nome_colonna <- tolower(gsub("-", "_", j))
    if(nome_colonna %in% colnames(m0)){
      m0[nome_utente, nome_colonna] <- 1
    }
  }
}

library(arules)

m0.trans <- as(m0, "transactions")
itemFrequencyPlot(m0.trans, topN = 23)
image(head(m0.trans, 1000))


# Analisi descrittiva:
# supporto: numero di volte in cui un brand compare nella matrice dei 1725 users
itemFrequency(m0.trans)[1:10]
# li ordino in maniera decrescente per vedere i 10 brand piu' presenti negli armadi
sort(itemFrequency(m0.trans),decreasing=T)[1:10]
# grafico
itemFrequencyPlot(m0.trans) # illeggibile
# visualizziamo le frequenze solo di quelli con supporto almeno 0.08
itemFrequencyPlot(m0.trans,support=.08) # molto più leggibile
itemFrequencyPlot(m0.trans, col="salmon", topN = 10, cex=1.4)


# apriori con "soglie minime" per supporto e confidenza ----
regole <- apriori(m0.trans,
                  parameter = list(supp = 0.02,   # Supporto minimo: 2%
                                   conf = 0.50,   # Confidenza minima: 50%
                                   target = "rules"))

# Ordiniamo le regole in base al Lift (dalla più forte alla più debole)
regole_ord <- sort(regole, by = "lift")

# Vediamo le migliori 30 regole secondo il lift
inspect(head(regole_ord, 30, by="lift")) # by lift anche di default
inspect(regole_ord) # sono 66 regole
# nella prima regola abbiamo che se nell'armadio sono presenti Nike, Adidas e Converse
# allora è presente anche Vans.




# install.packages("xfun")
# Installa e carica il pacchetto se non ce l'hai
# install.packages("gt")
library(xfun)
library(gt)

df_tabella <- data.frame(
  N = c(1, 2, 3, 4, 5, 6, 7, 8, "...", 66),
  Regola = c("{nike, adidas, converse} => {vans}",
             "{nike, adidas, vans} => {converse}",
             "{adidas, vans} => {converse}",
             "{nike, adidas, converse} => {puma}",
             "{adidas, converse} => {puma}",
             "{adidas, vans, jordan} => {nike}",
             "{puma, jordan} => {nike}",
             "{adidas, puma, jordan} => {nike}",
             " ",
             "{adidas} => {nike}"),
  Supporto = c("2.7%", "2.7%", "3.1%", "2.7%", "3.3%", "2.4%", "3.9%", "3.2%", "...", "18.8%"),
  Fiducia = c("53.4%", "54.0%", "51.9%", "53.4%", "50.9%", "100.0%", "98.5%", "98.2%", "...", "51.2%"),
  Lift = c("5.40", "4.56", "4.38", "3.41", "3.25", "2.63", "2.59", "2.58", "...", "1.35")
)


tabella_bella <- gt(df_tabella) |>
  tab_header(
    title = md("**Regole di Associazione**"),
    subtitle = "Ordinate per Lift decrescente"
  ) |>
  opt_align_table_header(align = "left") |>
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(rows = 1)
  ) |>
  cols_align(
    align = "center",
    columns = c(N, Supporto, Fiducia, Lift)
  )

tabella_bella





# Rappresentazione grafica ----
# Cerchiamo di rappresentare queste regole con una serie di strumenti

library(arulesViz)
# 66 regole estratte rappresentate secondo le due dimensioni di interesse:
# fiducia e supporto.
# Viene rappresentato anche il lift con la scala di colori
# (all'aumentare dell'intensità aumenta il lift)
# Le regole con lift piuttosto elevato sono poche.
plot(regole_ord)
plot(regole_ord, method="two-key plot")
# l'ordine è il numero totale di brand coinvolti in quella regola

# Il primo scatter plot ci ha mostrato la FORZA delle regole tramite il Lift,
# permettendoci di individuare la tribù Vans/Converse.
# Questo secondo scatter plot ci mostra la COMPLESSITÀ.
# Ci dimostra che su Vinted gli acquisti non sono casuali e isolati:
# gli utenti costruiscono guardaroba complessi e stratificati (le regole blu e verdi),
# e più il guardaroba diventa complesso, più le regole di abbinamento
# diventano sicure e ad alta confidenza.

# provo a fare i grafici con i pallini più grandi (size o cex non vanno)
library(ggplot2)

dati_regole <- quality(regole_ord)

ggplot(dati_regole, aes(x = support, y = confidence, color = lift)) +
  geom_point(size = 3.5) +
  scale_color_gradient(low = "lightgrey", high = "red") +
  theme_bw() +
  labs(title = "Scatter plot for 66 rules",
       x = "Supporto",
       y = "Fiducia",
       color = "Lift")

# Ordine (quanti brand ci sono in ogni regola)
dati_regole$ordine <- size(lhs(regole_ord)) + size(rhs(regole_ord))

ggplot(dati_regole, aes(x = support, y = confidence, color = as.factor(ordine))) +
  geom_point(size = 3.5) +
  theme_bw() +
  labs(title = "Scatter plot for 66 rules - two-key plot",
       x = "Supporto",
       y = "Fiducia",
       color = "N. di Brand\n(Ordine)")





# Disegna le migliori 20 regole come un grafo a rete!
plot(head(regole_ord, 20), method = "graph", engine = "htmlwidget")

plot(regole_ord, method = "graph", engine = "htmlwidget")


#######
plot(regole_ord)
plot(regole_ord, method = "graph")
plot(regole_ord, method = "graph", engine="interactive")
plot(regole_ord, method = "graph", engine="igraph")
plot(regole_ord, method = "graph", engine="visNetwork") #utile per vedere ogni regola singolarmente
# Ci sono brand che tornano spesso nelle regole
# Quindi si potrebbe fare una rappresentazione logica in forma di rete
# Ci sono brand che sono solo antecedenti e altri che sono solo conseguenti
# Altri che sono sia antecedenti che conseguenti.
# Questo non è necessariamente vero che sia così in senso assoluto,
# ma nei dati estratti sì.




######### Sotto dataset di lusso

brand_lusso <- c("gucci", "prada", "louis-vuitton", "dior", "chanel",
                 "versace", "balenciaga", "moncler", "valentino", "moschino",
                 "fendi", "bottega-veneta", "saint-laurent", "givenchy",
                 "burberry", "dolce-gabbana")

# Troviamo quali brand di lusso sono presenti nei dati
brand_lusso_presenti <- intersect(brand_lusso, itemLabels(m0.trans))

cat("Brand di lusso trovati su Vinted:\n")
brand_lusso_presenti


if(length(brand_lusso_presenti) > 0) {

  m0.trans_lusso <- subset(m0.trans, items %in% brand_lusso_presenti)
  cat("\nFiltro applicato con successo! Armadi Premium:", length(m0.trans_lusso), "\n")

} else {
  cat("\nNessun brand di lusso trovato in questo dataset.\n")
}


# Vediamo quanti armadi "Premium" abbiamo scovato!
cat("Numero di armadi Premium trovati:", length(m0.trans_lusso), "\n")

# 3. Lanciamo l'algoritmo sul nuovo mini-universo
# Nota: Dato che il dataset ora è molto più piccolo, possiamo usare un Supporto più alto (es. 5%)
regole_lusso <- apriori(m0.trans_lusso,
                        parameter = list(supp = 0.05,
                                         conf = 0.50,
                                         target = "rules"))

regole_lusso <- apriori(m0.trans_lusso,
                        parameter = list(supp = 0.03,  # Alzato al 3%
                                         conf = 0.20,  # Confidenza abbassata al 20%
                                         target = "rules"))

# 4. Ordiniamo per Lift e guardiamo i risultati
regole_lusso_ord <- sort(regole_lusso, by = "lift")
inspect(head(regole_lusso_ord, 15))
inspect(regole_lusso_ord)




## RETI ----

# Trasformo la matrice m0 (Utenti x Brand) in una matrice quadrata (Brand x Brand)
# Questo calcola le "co-occorrenze" (quante volte due brand sono nello stesso armadio)
matrice_cooccorrenze <- t(m0) %*% m0

# Azzeriamo la diagonale (non ci interessa quante volte un brand sta con se stesso)
diag(matrice_cooccorrenze) <- 0

# Guardiamo una parte della Matrice di Co-occorrenza (Brand vs Brand):
matrice_cooccorrenze[1:5, 1:5]


# COSTRUZIONE DELLA RETE
library(igraph)

# Creiamo una matrice binaria (1 se c'è almeno un collegamento, 0 altrimenti)
Y_brands <- (matrice_cooccorrenze > 0) * 1

# Creiamo l'oggetto "Rete" indiretta
rete_vinted <- graph_from_adjacency_matrix(Y_brands, mode = "undirected", diag = FALSE)

library(visNetwork)
visIgraph(rete_vinted)

# Apri la tela in formato SVG
svg("rete_brand.svg", width = 12, height = 12)

image(as.matrix(rete_vinted))

# Chiudi e salva
dev.off()



# STATISTICHE DESCRITTIVE DELLA RETE
# Nodi (Brand totali):
vcount(rete_vinted)
V(rete_vinted)

# Archi (Collegamenti totali):
ecount(rete_vinted)
E(rete_vinted)

# Densità della rete:
# (Una densità alta indica che quasi tutti i brand sono stati abbinati ad almeno un altro brand)
# In questo caso la rete è abbastanza sparsa
round(edge_density(rete_vinted), 4) # 12.1%

# Degree Centrality:
# Quali brand hanno il maggior numero di collegamenti unici con altri brand?
gradi <- degree(rete_vinted)
head(sort(gradi, decreasing = TRUE), 5)
hist(gradi,breaks = 10,col='lavender')


# Betweenness Centrality:
# misura di centralità della rete,
# quanto un certo nodo sia di passaggio per arrivare a un altro nodo.
# Quali brand collegano stili diversi che altrimenti non si parlerebbero?
ponti <- betweenness(rete_vinted)
head(sort(ponti, decreasing = TRUE), 5)
hist(ponti, breaks = 20, col = 'lavender')
# la maggior parte = 0, brand poco centrali all'interno dlla rete.


# Diametro (Distanza massima tra due brand):
diameter(rete_vinted)


# Shortest paths
shortest_paths(rete_vinted, from = "shein", to = "jordan") # collegamento diretto
shortest_paths(rete_vinted, from = "prada", to = "autry") # collegamento diretto
shortest_paths(rete_vinted, from = "piazza_italia", to = "prima_classe") # non ci sono collegamenti
shortest_paths(rete_vinted, from = "paw_patrol", to = "prima_classe") # 3 nodi intermedi






# MODELLO AMEN (Social Relation Model) ----
library(amen)

# Uso logaritmo per smussare le cooccorrenze estreme
Y_amen <- log(matrice_cooccorrenze + 1)

# Se gli diamo tutti i brand potrebbe impiegarci troppo
# Filtriamo i TOP 30 brand più frequenti
frequenze_totali <- colSums(m0)
top_30_idx <- order(frequenze_totali, decreasing = TRUE)[1:30]
Y_amen_ridotto <- Y_amen[top_30_idx, top_30_idx]

# Binarizziamo la matrice logaritmica (1 = esiste connessione, 0 = no)
Y_binaria <- (Y_amen_ridotto > 0) * 1
rete30 <- graph_from_adjacency_matrix(Y_binaria, mode = "undirected", diag = FALSE)
edge_density(rete30) # 97%
head(sort(betweenness(rete30), decreasing = TRUE), 5)



dati_google <- read.csv("google_trends_per_laura.csv")
View(dati_google)

dataset_MBA$avg_interest_global_last_year
sotto_dataset <- dataset_MBA[dataset_MBA$Brand_raw %in% colnames(Y_amen_ridotto),c("avg_interest_global_last_year","Brand_raw")]
sotto_dataset <- as.data.frame(sotto_dataset)
duplicated.data.frame(sotto_dataset)
sotto_dataset <- sotto_dataset[duplicated.data.frame(sotto_dataset) == FALSE ,]
colnames(Y_amen_ridotto) %in% sotto_dataset$Brand_raw
colnames(Y_amen_ridotto)[!(colnames(Y_amen_ridotto) %in% sotto_dataset$Brand_raw)]
# 5 brand non sono presenti in sotto_dataset
# -> Aggiungo manualmente i valori di google trends relativi a quei 5 brand
colnames(dati_google)[1] <- "Brand_raw"
sotto_dataset <- rbind(sotto_dataset, dati_google[,c(1,3)])
rownames(sotto_dataset) <- NULL
sotto_dataset$Brand_raw <- gsub(" ", "_", sotto_dataset$Brand_raw)

sotto_dataset1 <- sotto_dataset[sotto_dataset$Brand_raw %in% colnames(Y_amen_ridotto),]
rownames(sotto_dataset1) <- NULL


### ----
# set.seed(23)
# y30 <- sample(colnames(matrice_cooccorrenze),30)
# Y_rid <- Y_amen[y30, y30]
# dati_rid <- dataset_MBA[dataset_MBA$Brand_raw %in% colnames(Y_rid),c("avg_interest_global_last_year","Brand_raw")]
# dati_rid <- as.data.frame(dati_rid)
# duplicated.data.frame(dati_rid)
# dati_rid <- dati_rid[duplicated.data.frame(dati_rid) == FALSE ,]
# colnames(Y_rid) %in% dati_rid$Brand_raw
# colnames(Y_rid)[!(colnames(Y_rid) %in% dati_rid$Brand_raw)]
# 5 brand non sono presenti in dati_rid
# -> Aggiungo manualmente i valori di google trends relativi a quei 5 brand
# colnames(dati_google)[1] <- "Brand_raw"
# dati_rid <- rbind(dati_rid, dati_google[,c(1,3)])
# rownames(dati_rid) <- NULL
# dati_rid$Brand_raw <- gsub(" ", "_", dati_rid$Brand_raw)
#
# dati_rid1 <- dati_rid[dati_rid$Brand_raw %in% colnames(Y_rid),]
# rownames(dati_rid1) <- NULL







# Regressione lineare ----
set.seed(42)
fit_lm<-ame(Y_amen_ridotto,Xrow = sotto_dataset1$avg_interest_global_last_year, Xcol = sotto_dataset1$avg_interest_global_last_year,
            family="nrm", symmetric=TRUE, rvar=FALSE,cvar=FALSE,dcor=FALSE)
summary(fit_lm)
# non voglio che dipendenza diadica ed effetti di riga e di colonna siano presenti
# covariata non significativa (variabile di nodo)
# questo potrebbe essere dovuto al fatto di aver considerato solo i 30 brand più frequenti nel dataset
# che sono tutti anche molto popolari globalmente (punteggio google trends)
# e dunque hanno poca variabilità.
# Può essere che scegliendo casualmente 30 brand la variabilità aumenti
# e che la variabile di nodo risulti significativa.



# SRM ----
set.seed(42)
fit_SRM<-ame(Y_amen_ridotto, family="nrm", symmetric = TRUE, R=0)
summary(fit_SRM)
plot(fit_SRM)



# Supponiamo che tu abbia scaricato e allineato i dati di Google Trends
# Deve essere un vettore numerico lungo 30, es: google_trends <- c(100, 85, 40, ...)
# È consigliabile standardizzarli o usare il logaritmo per evitare scale troppo diverse
# google_trends_log <- log(google_trends + 1)

# SRRM (Social Relations Regression Model) ----
# Aggiungiamo la tua variabile geniale (Google Trends) per spiegare la popolarità.
# R = 0 (ancora niente fattori latenti)
set.seed(42)
fit_SRRM_vinted <- ame(Y_amen_ridotto,
                       Xr = sotto_dataset1$avg_interest_global_last_year, # Covariata per la riga
                       Xc = sotto_dataset1$avg_interest_global_last_year, # Covariata per la colonna (identica)
                       family = "nrm",
                       symmetric = TRUE,
                       R = 0)
summary(fit_SRRM_vinted)
# la variabile di nodo è non significativa



# AME (con dati simmetrici continui) ----
# Vediamo l'avanzamento MCMC nella console
# Vogliamo 2 fattori latenti spaziali (R = 2)
set.seed(42)
fit_ame <- ame(Y_amen_ridotto, Xrow = sotto_dataset1$avg_interest_global_last_year, Xcol = sotto_dataset1$avg_interest_global_last_year,
               family = "nrm", symmetric = TRUE, R = 2)
summary(fit_ame)

set.seed(42)
fit_ame_nocov <- ame(Y_amen_ridotto, family = "nrm", symmetric = TRUE, R = 2)
summary(fit_ame_nocov)
plot(fit_ame_nocov)



# Circle Plot
# I brand posizionati vicini sul cerchio hanno modelli di "esportazione/importazione" (co-occorrenza) molto simili
par(mfrow=c(1,1))
circplot(Y_amen_ridotto, U = fit_ame_nocov$U, V = fit_ame_nocov$V)
# poco leggibile


# Estraiamo le coordinate calcolate dal modello
spazio_latente <- fit_ame_nocov$U


# Sistemo i margini
par(mar = c(5, 5, 4, 2) + 0.1)

x_lim <- range(spazio_latente[, 1])
x_lim_ampi <- c(x_lim[1] - diff(x_lim)*0.2, x_lim[2] + diff(x_lim)*0.2)

y_lim <- range(spazio_latente[, 2])
y_lim_ampi <- c(y_lim[1] - diff(y_lim)*0.2, y_lim[2] + diff(y_lim)*0.2)

plot(spazio_latente, type = "n",
     xlim = x_lim_ampi, ylim = y_lim_ampi,
     xlab = "Dimensione Nascosta 1",
     ylab = "Dimensione Nascosta 2",
     main = "La Mappa Latente dei Brand su Vinted")

abline(h = 0, v = 0, lty = 2, col = "grey80")

# Inseriamo i nomi dei brand (un po' più piccoli con cex = 0.8)
text(spazio_latente, labels = rownames(Y_amen_ridotto), col = "darkred", cex = 0.8)

# Ripristiniamo i margini di default di R per i prossimi grafici
par(mar = c(5, 4, 4, 2) + 0.1)



# DIAGNOSTICA DEL MODELLO AME

# Calcoliamo le statistiche di Goodness of Fit (GoF)
# Confronta le statistiche della tua matrice reale con quelle simulate dal modello
statistiche_gof <- gofstats(Y_amen_ridotto)
statistiche_gof

# Generiamo i 4 grafici diagnostici del laboratorio
# IMPORTANTE: Ti chiederà di premere "Invio" nella console per vedere i grafici successivi
plot(fit_SRM_vinted)
par(mfrow=c(1,1))







## Clustering ----

# Confronto tra Metodi
# (ho due strade possibili: Solo Quantitative vs Dati Misti)

# Gestione NA sui preferiti (sostituzione con 0)
dataset_clustering$Favorites_Count[is.na(dataset_clustering$Favorites_Count)] <- 0

# Creazione variabile numerica per la Condizione (usata nel K-Means per approssimazione)
dataset_clustering$Condition_Num <- as.numeric(dataset_clustering$Condition)

# Isolamento delle variabili quantitative
vars_quant <- c("LogPrice", "Size", "Favorites_Count", "Condition_Num")
MD.quant <- dataset_clustering[, vars_quant]
MD.quant_scaled <- scale(MD.quant)



## CLUSTERING DISTANZE EUCLIDEE: ----

# CLUSTERING GERARCHICO
# per SCELTA DEL K Ottimale tramite Dendrogramma

# Matrice distanze euclidee e legame completo
Mdist_euclidea <- dist(MD.quant_scaled, method = "euclidean")
hc_quant <- hclust(Mdist_euclidea, method = "complete")

# Dendrogramma
par(mar = c(2, 4, 4, 2))
plot(hc_quant, main = "Dendrogramma Vinted (Variabili Quantitative)", labels = FALSE, sub = "", xlab = "")
# Mostriamo visivamente i due tagli possibili (3 e 4)
rect.hclust(hc_quant, k = 3, border = "red")
rect.hclust(hc_quant, k = 4, border = "green")



# TEST DI INTERPRETABILITÀ: 3 vs 4 CLUSTER
cat("\n--- TEST DI INTERPRETABILITÀ: Confronto 3 vs 4 Cluster ---\n")
c3 <- cutree(hc_quant, k = 3)
c4 <- cutree(hc_quant, k = 4)

cat("\nIdentikit a 3 Cluster (Medie):\n")
print(round(aggregate(MD.quant, list(Cluster = c3), mean), 2))

cat("\nIdentikit a 4 Cluster (Medie):\n")
print(round(aggregate(MD.quant, list(Cluster = c4), mean), 2))

cat("\nGIUSTIFICAZIONE SCELTA k=4:\nPassando da 3 a 4 cluster, il blocco degli annunci 'economici' si spacca in due, rivelando un segmento con taglia media molto bassa (circa 26). L'algoritmo ha isolato il mercato 'Bambini', garantendo un'interpretabilità di business di gran lunga superiore. Procediamo con k=4.\n")
cat("\nGIUSTIFICAZIONE SCELTA k=4:\nOsservando il dendrogramma con legame completo, la scelta naturale sembrava ricadere su 3 o 4 cluster. Testando i centroidi per $k=3$, ho notato che il blocco dei prodotti economici risultava poco coeso, presentando una taglia media spuria di circa 36. Eseguendo il taglio a $k=4$,
    l'algoritmo ha diviso questo macro-gruppo in modo chirurgico: ha isolato un cluster con taglia media 39 (il mercato Fast-Fashion per adulti) e ha estratto un cluster purissimo con taglia media 26 e prezzi minimi. Questa scomposizione ha rivelato matematicamente la presenza del mercato 'Kids/Infanzia'.
    Data l'enorme rilevanza di business di questo segmento su Vinted, ho validato $k=4$ come numero ottimale per procedere successivamente con il K-Means.\n")



# K-MEANS DEFINITIVO
library(flexclust)

# Eseguiamo il K-Means per esplorare la varianza (Screeplot)
set.seed(1234)
MD.km_quant <- stepFlexclust(MD.quant_scaled, k = 2:8, nrep = 10, verbose = FALSE)

# Mostriamo che non c'è un 'gomito' chiaro, il che giustifica
# l'aver usato il gerarchico per scegliere k=4
plot(MD.km_quant, xlab = "Numero di Segmenti (k) - K-Means")

# Estraiamo definitivamente il modello a 4 cluster
MD.k4_quant <- MD.km_quant[["4"]]

# Profilazione visiva dei centroidi (Verifichiamo che trovi gli stessi gruppi)
barchart(MD.k4_quant, main = "Profili K-Means Definitivi (k=4, Z-scores)")
# ho provato anche con i barchart di 3, 5, 6, ma la suddivisione in 4 gruppi
# è sicuramente migliore anche dal punto di vista interpretativo
# con 3 univa tutti i costosi, senza discriminare per condizione
# con 5 e 6 sovra-segmentava creando cluster intermedi poco interpretabili



modello_k4 <- getModel(MD.km_quant, "4")
# Estraggo le etichette di assegnazione ai cluster
kmeans4_labels <- clusters(modello_k4)
# Tabella di contingenza per confrontare i 4 gruppi ottenuti con i due algoritmi
table(Gerarchico = c4, KMeans = kmeans4_labels)
cat("\nCONFRONTO I DUE METODI:\nHo confrontato il Gerarchico a 4 gruppi con il K-Means a 4 gruppi
    tramite una tabella di contingenza. Inizialmente le numerosità sembravano discordanti,
    ma l'analisi ha rivelato il potere ottimizzatore del K-Means: mentre il Gerarchico aveva raggruppato
    quasi 1000 annunci in un unico macro-cluster 'costoso', il K-Means ha riassegnato queste osservazioni
    riducendo la varianza interna e separando brillantemente il mercato 'Premium immacolato' dal mercato 'Vintage virale'.
    Al contempo, il segmento 'Kids' è rimasto solidissimo in entrambi gli algoritmi.
    Per questo motivo, ho escluso numerosità superiori come 5 o 6, che avrebbero solo generato over-segmentazione
    priva di valore interpretativo, eleggendo i 4 profili K-Means come risultato definitivo dell'analisi.")


library(ggplot2)

# Calcoliamo le medie delle variabili quantitative per i 4 cluster K-Means
prezzo_km <- tapply(dataset_clustering$LogPrice, kmeans4_labels, mean)
interazioni_km <- tapply(dataset_clustering$Favorites_Count, kmeans4_labels, mean)
taglia_km <- tapply(dataset_clustering$Size, kmeans4_labels, mean)


df_mappa_km <- data.frame(
  Cluster = factor(1:4, labels = c("1: Hype/Virale", "2: Premium", "3: Fast-Fashion", "4: Kids")),
  Prezzo = as.numeric(prezzo_km),
  Interazioni = as.numeric(interazioni_km),
  Taglia = as.numeric(taglia_km)
)

# Grafico
ggplot(df_mappa_km, aes(x = Prezzo, y = Interazioni, size = Taglia, fill = Cluster)) +
  geom_point(alpha = 0.8, shape = 21, color = "black", stroke = 0.8) +
  scale_size(range = c(8, 25), name = "Taglia Media", breaks = c(26, 40), labels = c("26 (Kids)", "40 (Adulti)")) +
  scale_fill_manual(values = c("mediumpurple", "khaki", "lightseagreen", "lightcoral")) +
  geom_text(aes(label = 1:4), size = 6, fontface = "bold", color = "black") +
  scale_x_continuous(expand = expansion(mult = 0.2)) +
  scale_y_continuous(expand = expansion(mult = 0.2)) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14, face = "bold"),
    plot.title = element_text(face = "bold", size = 15),
    panel.grid.minor = element_blank()
  ) +
  labs(
    title = "La Mappa Strategica di Vinted (Modello K-Means)",
    subtitle = "Posizionamento per Prezzo, Interazioni e Taglia Media",
    x = "Prezzo Medio (Log)",
    y = "Interazioni Medie (Preferiti)"
  ) +
  guides(
    fill = guide_legend(order = 1, override.aes = list(size = 6)),
    size = guide_legend(order = 2)
  )




# PROFILAZIONE COMMERCIALE (Regressione Multinomiale)
library(nnet)
library(car)
library(effects)

# Salvo le partizioni K-Means nel dataset originale
dataset_clustering$Segmento_Definitivo <- as.factor(clusters(MD.k4_quant))

# Modello multinomiale: Qual è l'effetto di Brand, Colore e Sponsorizzazione?
# (Condition_Num è ESCLUSA dalla formula, perché l'abbiamo usata per creare i gruppi)
set.seed(12)
dataset_clustering$Color_new <- as.factor(dataset_clustering$Color_new)
modello_profilazione <- multinom(Segmento_Definitivo ~ Is_Boosted + Color_new + Brand,
                                 data = dataset_clustering,
                                 trace = FALSE)

# Verifica della significatività statistica (Test ANOVA)
Anova(modello_profilazione)
# sono tutte significative



# GRAFICI DEGLI EFFETTI (Probabilità)
par(mar = c(5, 4, 4, 2) + 0.1)

# Effetto Sponsorizzazione
plot(effect("Is_Boosted", modello_profilazione),
     main = "Probabilità per Annunci Sponsorizzati",
     ylab = "Probabilità", xlab = "Is Boosted")
plot(effect("Is_Boosted", modello_profilazione),
     multiline = TRUE,
     ci.style = "none", # Spegne le barre d'errore per la massima pulizia visiva
     main = "Mappa di Posizionamento per Annunci Sponsorizzati",
     xlab = "Is Boosted",
     ylab = "Probabilità di Appartenenza al Segmento")

# Effetto Colore
plot(effect("Color_new", modello_profilazione),
     main = "Probabilità per Categoria di Colore",
     ylab = "Probabilità", xlab = "Colore")
plot(effect("Color_new", modello_profilazione),
     multiline = TRUE,
     ci.style = "none",
     main = "Mappa di Posizionamento per Categoria di Colore",
     xlab = "Categoria di Colore",
     ylab = "Probabilità di Appartenenza al Segmento")
# nonostante sia significativa,
# vediamo che in realtà non ha molto effetto rispetto al boost e al brand.

# Effetto Brand (Grafico unificato a linee sovrapposte per leggibilità)
plot(effect("Brand", modello_profilazione),
     multiline = TRUE,
     rotx = 45,
     cex.axis = 0.8,
     main = "Mappa di Posizionamento dei Brand sui 4 Segmenti",
     ylab = "Probabilità di Appartenenza al Segmento",
     xlab = "Brand Vinted")





## CLUSTERING DISTANZA DI GOWER: ----

# CLUSTERING GERARCHICO

library(cluster)

# Trasformiamo il tipo di dato per usare la funzione daisy
# Assicuriamoci che Condition sia ordinato
ordine_stato <- c("Discrete", "Buone", "Ottime", "Nuovo senza cartellino", "Nuovo con cartellino")
dataset_clustering$Condition <- factor(dataset_clustering$Condition, levels = ordine_stato, ordered = TRUE)

# Trasformiamo le variabili in fattori standard
dataset_clustering$Brand <- as.factor(dataset_clustering$Brand)
dataset_clustering$Color_new <- as.factor(dataset_clustering$Color_new)
dataset_clustering$Is_Boosted <- as.factor(dataset_clustering$Is_Boosted)

# Selezione variabili (sia quantitative che qualitative)
vars_mixed <- c("LogPrice", "Size", "Favorites_Count", "Condition", "Brand", "Color_new", "Is_Boosted")
MD.mixed <- dataset_clustering[, vars_mixed]

# Calcolo Matrice Dissimilarità di Gower
Mdist_gower <- daisy(MD.mixed, metric = "gower")

# Clustering Gerarchico su Gower
hc_gower <- hclust(Mdist_gower, method = "complete")

par(mar = c(2, 4, 4, 2))
plot(hc_gower, main = "Dendrogramma Vinted (Variabili miste)", labels = FALSE, sub = "", xlab = "")
# Mostriamo visivamente i due tagli possibili (3 e 4)
rect.hclust(hc_gower, k = 3, border = "red")
rect.hclust(hc_gower, k = 4, border = "green")
# (Taglio a 4 cluster sia per coerenza che per interpretabilità dei gruppi)

# TEST DI INTERPRETABILITÀ: 3 vs 4 CLUSTER
cat("\n--- TEST DI INTERPRETABILITÀ: Confronto 3 vs 4 Cluster ---\n")
cg3 <- cutree(hc_gower, k = 3)
cg4 <- cutree(hc_gower, k = 4)
# Ho provato a guardare anche con 5, 6, 7, 8 gruppi, ma sovra-segmenta


cat("\nIdentikit a 3 Cluster (Medie):\n")
for(i in 1:3) {
  cat("               CLUSTER", i, "               \n")
  # Prendo solo le righe che appartengono al cluster 'i'
  subset_cluster <- MD.mixed[cg3 == i, ]
  # Statistiche descrittive (medie per i numeri, frequenze per le categorie)
  print(summary(subset_cluster))
}

cat("\nIdentikit a 4 Cluster (Medie):\n")
for(i in 1:4) {
  cat("               CLUSTER", i, "               \n")
  subset_cluster <- MD.mixed[cg4 == i, ]
  summary(subset_cluster)
}

cat("\nGIUSTIFICAZIONE SCELTA k=4 (e non 3):\nMentre il clustering con distanza Euclidea aveva isolato le dinamiche socio-demografiche (come il mercato Kids tramite la variabile Size), la matrice di Gower ha cambiato radicalmente la prospettiva, dimostrando un'ipersensibilità alle variabili categoriche forti.Analizzando gli identikit estratti con la funzione summary, è evidente come Gower abbia usato la variabile Is_Boosted come spartiacque primario: i cluster 1 e 2 sono composti esclusivamente da traffico organico (FALSE), mentre i cluster 3 e 4 esclusivamente da traffico pagato (TRUE).Il passaggio da $k=3$ a $k=4$ è stato fondamentale perché l'algoritmo ha utilizzato la variabile 'Colore' per raffinare i macro-gruppi: ha isolato il segmento delle 'Sneakers/Hype sponsorizzate' (dominato dai brand sportivi e dal tratto Multicolor) e il segmento 'Premium a tinta unita', rivelando una struttura di mercato dettata interamente da strategie di marketing visivo.\n")
cat("\nGIUSTIFICAZIONE SCELTA k=4 (e non di più):\nAumentare il numero di cluster con la metrica di Gower porta a una sterile over-segmentazione basata sulla variabile cromatica. L'algoritmo si riduce a fare quello che potremmo fare con un semplice filtro su Excel (es. raggruppare tutti gli annunci 'Falsi e Chiari'). Dal punto di vista del business, una maglietta fast-fashion bianca (Chiara) e una blu (Scura) rispondono alle stesse identiche logiche di mercato (prezzo basso, zero sponsorizzazioni). Non ha senso dedicare loro due cluster separati.\n")
cat("\nIn sintesi:\nL'Euclidea ha trovato l'età (Kids), Gower ha trovato il comportamento di marketing (Sponsorizzati/Colori).\n")
cat("\nMotivo di questa sostanziale differenza (perché Gower considera quasi solo le categoriali):\nGower penalizza massimamente il mismatch categoriale, rendendo le variabili dicotomiche i veri driver della segmentazione gerarchica.\n")


dataset_clustering$Cluster_Gower <- as.factor(cg4)




# Mappatura Finale:
# Bubble Chart Strategico su Prezzo e Interazioni
prezzo_gower <- tapply(dataset_clustering$LogPrice, dataset_clustering$Cluster_Gower, mean)
preferiti_gower <- tapply(dataset_clustering$Favorites_Count, dataset_clustering$Cluster_Gower, mean)
boosted_gower <- tapply((dataset_clustering$Is_Boosted == "TRUE") + 0, dataset_clustering$Cluster_Gower, mean)

plot(prezzo_gower, preferiti_gower,
     cex = (boosted_gower + 0.1) * 20,
     col = c("tomato", "steelblue", "forestgreen", "gold"),
     pch = 19,
     xlim = c(min(prezzo_gower)*0.8, max(prezzo_gower)*1.2),
     ylim = c(min(preferiti_gower)*0.8, max(preferiti_gower)*1.2),
     xlab = "Prezzo Medio (Log)",
     ylab = "Interazioni (Preferiti)",
     main = "La Mappa Strategica di Vinted (Modello Gower)")
text(prezzo_gower, preferiti_gower, labels = 1:4, col = "white", font = 2)


# Grafico più bello:
library(ggplot2)

df_mappa <- data.frame(
  Cluster = factor(1:4, labels = c("1: No boost Chiari", "2: No boost Scuri", "3: Boosted Uniti", "4: Boosted Hype")),
  Prezzo = as.numeric(prezzo_gower),
  Interazioni = as.numeric(preferiti_gower),
  Sponsorizzati = as.numeric(boosted_gower) * 100 # Trasformiamo in percentuale da 0 a 100
)

# Bubble Chart
ggplot(df_mappa, aes(x = Prezzo, y = Interazioni, size = Sponsorizzati, fill = Cluster)) +
  geom_point(alpha = 0.8, shape = 21, color = "black", stroke = 0.8) +
  scale_size(range = c(10, 28), name = "% Sponsorizzati", breaks = c(0, 100)) +
  scale_fill_manual(values = c("tomato", "steelblue", "forestgreen", "gold")) +
  geom_text(aes(label = 1:4), size = 6, fontface = "bold", color = "black") +

  scale_x_continuous(expand = expansion(mult = 0.2)) +
  scale_y_continuous(expand = expansion(mult = 0.2)) +

  theme_minimal() +
  theme(
    legend.position = "right",
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14, face = "bold"),
    plot.title = element_text(face = "bold", size = 15),
    panel.grid.minor = element_blank()
  ) +
  labs(
    title = "La Mappa Strategica di Vinted (Modello Gower)",
    subtitle = "Posizionamento per Prezzo, Interazioni e Investimento in Boost",
    x = "Prezzo Medio (Log)",
    y = "Interazioni Medie (Preferiti)"
  ) +

  guides(
    fill = guide_legend(order = 1, override.aes = list(size = 6)),
    size = guide_legend(order = 2)
  )





# PROFILAZIONE DESCRITTIVA DEI SEGMENTI DI GOWER

# Ripristiniamo i margini per i grafici standard
par(mar = c(4, 4, 3, 2))

# Boxplots per vedere le distribuzioni delle quantitative:

# Distribuzione del Prezzo nei 4 cluster di Gower
boxplot(LogPrice ~ Cluster_Gower, data = dataset_clustering,
        main = "Distribuzione del Prezzo nei Cluster Gower",
        xlab = "Cluster", ylab = "LogPrice",
        col = c("tomato", "steelblue", "forestgreen", "gold"))

# Distribuzione della Taglia (Size)
boxplot(Size ~ Cluster_Gower, data = dataset_clustering,
        main = "Distribuzione della Taglia nei Cluster Gower",
        xlab = "Cluster", ylab = "Size",
        col = c("tomato", "steelblue", "forestgreen", "gold"))
cat("\nCommento boxplot:\nI boxplot confermano che la metrica di Gower ha polarizzato i gruppi non sulla taglia fisica, ma sul valore economico: i segmenti 3 e 4, che investono in marketing, presentano un valore mediano del capo significativamente superiore rispetto ai segmenti 1 e 2 a traffico organico.\n")




# Mosaic Plots per vedere le distribuzioni delle qualitative:

# (Il Mosaic Plot crea rettangoli proporzionali alla numerosità degli incroci.
# Più è largo il rettangolo, più quella combinazione è frequente.)

# Mosaic Plot 1: Cluster Gower vs Condizione dell'oggetto
# (Per vedere se un cluster raggruppa solo roba "Nuova" o "Usurata")
mosaicplot(table(dataset_clustering$Cluster_Gower, dataset_clustering$Condition),
           main = "Mosaic Plot: Cluster Gower vs Condizione",
           xlab = "Cluster Gower", ylab = "Condizione",
           color = TRUE, las = 1, cex.axis = 0.7)

cat("\nCommento Mosaic Plot Condizione:\nI cluster sponsorizzati (3 e 4) hanno rettangoli molto più bassi nella parte superiore (Discrete e Buone) e blocchi molto più grandi in basso (Nuovo). Significa che chi paga per la visibilità vende merce in condizioni perfette o col cartellino.\n")



# Mosaic Plot 2: Cluster Gower vs Colore
# (Per vedere le preferenze cromatiche dei segmenti)
mosaicplot(table(dataset_clustering$Cluster_Gower, dataset_clustering$Color_new),
           main = "Mosaic Plot: Cluster Gower vs Colore",
           xlab = "Cluster Gower", ylab = "Categoria Colore",
           color = TRUE, las = 1, cex.axis = 0.8)

cat("\nCommento Mosaic Plot Colore:\nIl Mosaic Plot evidenzia il peso determinante delle variabili qualitative nel calcolo delle dissimilarità di Gower: il Cluster 2, ad esempio, è stato generato isolando esclusivamente l'inventario organico di colore Scuro, creando un segmento cromaticamente puro.\n")



# (Per variabili con troppe modalità (come il Brand), il mosaic plot esplode.)
# Tabella con distribuzione dei Brand nei 4 Cluster Gower:
tabella_brand <- table(dataset_clustering$Brand, dataset_clustering$Cluster_Gower)
tabella_brand

cat("\nCluster 1 (No Boost Chiari): Domina l'abbigliamento senza pretese. Ci sono i picchi massimi per ignoto (79) e la voce residuale Altro (190).
    \nCluster 2 (No Boost Scuri): Numeri bassissimi, ancora una volta ignoto (46) e Altro (48). È il mondo dei maglioni invernali senza marca.
    \nCluster 3 (Boosted Uniti): Qui c'è il boom del Premium Silenzioso. Guarda il picco delle Marche di Lusso (27, il più alto di tutti i cluster) e di Nike (101).
    \nCluster 4 (Boosted Hype/Multicolor): L'esplosione dello sportwear. Picchi assoluti per Adidas (122), Nike (106) e Converse (36). Le sneakers colorate e virali dominano qui dentro.\n")



# SINTESI FINALE DEI PROFILI DI CLUSTERING VINTED

cat("--- MODELLO 1: K-MEANS (Distanza Euclidea) ---
    \nLente di Analisi: Dinamiche strutturali, fisiche ed economiche.
    \nInsight chiave: Identificazione chirurgica del mercato infanzia.\n\n")

cat(" * Cluster 1 [Hype/Virale]: \n   Capi per adulti ad alto prezzo ma con altissimo tasso di interazione (Preferiti). Il mercato delle sneakers e dell'usato virale.
    \n * Cluster 2 [Premium]: \n   Capi per adulti ad alto prezzo, spesso nuovi o in ottime condizioni, con interazioni standard. Compravendita silenziosa e di qualità.
    \n * Cluster 3 [Fast-Fashion / Svuota-Armadio]: \n   Il macro-mercato di massa. Capi per adulti a bassissimo prezzo, brand commerciali o non dichiarati, basse interazioni.
    \n * Cluster 4 [Kids]: \n   Segmento isolato grazie alla variabile 'Taglia' (media 26). Prezzi bassi e forte rotazione di brand sportivi (scarpine bimbo).\n\n")

cat("--- MODELLO 2: GERARCHICO GOWER (Distanza Mista) ---
    \nLente di Analisi: Comportamento di marketing e attributi visivi.
    \nInsight chiave: Spaccatura perfetta tra traffico Organico e Pagato.\n\n")

cat(" * Cluster 1 [No Boost Chiari]: \n   Traffico 100% organico (Sponsor = FALSE). Capi a basso prezzo, dominati da tinte chiare e multicolor. Fast-fashion da giorno.
    \n * Cluster 2 [No Boost Scuri]: \n   Traffico 100% organico (Sponsor = FALSE). Segmento cromaticamente puro: 100% capi scuri a basso prezzo. Abbigliamento basico/invernale.
    \n * Cluster 3 [Boosted Uniti]: \n   Traffico 100% sponsorizzato (Sponsor = TRUE). Capi costosi a tinta unita (chiaro o scuro). Forte concentrazione di Marche di Lusso e Premium elegante.
    \n * Cluster 4 [Boosted Hype]: \n   Traffico 100% sponsorizzato (Sponsor = TRUE). Capi costosi dominati dal tratto Multicolor. L'habitat naturale e iper-promosso dei brand sportivi (Nike, Adidas).\n")

