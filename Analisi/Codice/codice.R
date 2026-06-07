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

