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
#piattaforma Vinted. .








