
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

