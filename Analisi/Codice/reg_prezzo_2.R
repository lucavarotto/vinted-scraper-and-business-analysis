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

















