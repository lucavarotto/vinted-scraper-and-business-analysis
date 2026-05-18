rm(list=ls());gc();
url <- "https://raw.githubusercontent.com/lucavarotto/vinted-scraper-and-business-analysis/main/Scraping/dati.csv"
dati <- readr::read_csv(url)
cat("\ndimensioni:", dim(dati))
dati[is.na(dati$Condition),]
dati <- dati[!is.na(dati$Condition), ]
cat("\ndimensioni:", dim(dati))
cat("\nrighe uniche:", length(unique(dati$URL)), length(unique(dati$Item_ID)))
dati <- dati[!duplicated(dati$Item_ID),]
cat("\nDopo la rimozione dei duplicati:", dim(dati))

print("Table iniziale")
sort(table(dati$Category))

categorie_da_tenere <- c("Scarpe con i lacci", "Scarpe da tennis", "Scarpe per allenamento indoor", "Scarpe da basket", "Scarpe da corsa",
                         "Scarpe", "Scarpe da ginnastica senza lacci", "Scarpe da ginnastica con i lacci",
                         "Scarpe da ginnastica con chiusura a strappo", "Scarpe da ginnastica")
dati <- dati[dati$Category %in% categorie_da_tenere,]
print("Table finale")
sort(table(dati$Category))
dim(dati)

library(tidyverse)
dati$Item_ID <- NULL
dati$Current_Time <- NULL
dati$Title <- NULL

dati$Brand <- fct_na_value_to_level(dati$Brand, "Ignoto")

dati$Brand <- tolower(dati$Brand)

# Normalizza i brand mancanti, generici o espressi in altre lingue in "ignoto"
dati$Brand[grep(pattern="worn|geen merk|no name|nobrand|nobrand\\.pt|inconnu|sans|ohne|sonstiges|sontignes|geen|sneakers|wear|street|anonyme|idk|mai|mas", dati$Brand)] <- "ignoto"

# pulizia falsi brand
dati$Brand[grep(pattern = "vera pelle|vintage dressing|je m'appelle|numeriś|numero|talla|us|hôtel amour|made in\\b|bambina|novita", dati$Brand)] <- "ignoto"

# pulizia parole troppo generiche
dati$Brand[grep(pattern = "fashion|wear|casual|chaussures|shoe|street|vintage boutique|vintage|gogo", dati$Brand)] <- "ignoto"

dati$Brand[grep(pattern="diadora", dati$Brand)] <- "diadora"
dati$Brand[grep(pattern="nike|jordan|air force|dunk|air max", dati$Brand)] <- "nike"
dati$Brand[grep(pattern="converse|chuck taylor|all star", dati$Brand)] <- "converse"
dati$Brand[grep(pattern="adidas|yeezy|gazelle|stan smith", dati$Brand)] <- "adidas"
dati$Brand[grep(pattern="reebok", dati$Brand)] <- "reebok"
dati$Brand[grep(pattern="kalenji|artengo|quechua|domyos|kiprun|kipsta|tribord", dati$Brand)] <- "decathlon"
dati$Brand[grep(pattern="zara", dati$Brand)] <- "zara"
dati$Brand[grep(pattern="bershka", dati$Brand)] <- "bershka"
dati$Brand[grep(pattern="pull & bear|pull and bear", dati$Brand)] <- "pull & bear"
dati$Brand[grep(pattern="asics|onitsuka", dati$Brand)] <- "asics"
dati$Brand[grep(pattern="vans|off the wall", dati$Brand)] <- "vans"
dati$Brand[grep(pattern="golden goose|ggdb", dati$Brand)] <- "golden goose"
dati$Brand[grep(pattern="mcqueen", dati$Brand)] <- "alexander mcqueen"
dati$Brand[grep(pattern="tommy|hilfiger", dati$Brand)] <- "tommy hilfiger"
dati$Brand[grep(pattern="calvin|ck", dati$Brand)] <- "calvin klein"
dati$Brand[grep(pattern="\\bdc\\b|dc shoes", dati$Brand)] <- "dc"
dati$Brand[grep(pattern="cat", dati$Brand)] <- "caterpillar"
dati$Brand[grep(pattern="ralph|lauren|polo ralph", dati$Brand)] <- "ralph lauren"
dati$Brand[grep(pattern="new balance|nb|n&b", dati$Brand)] <- "new balance"
dati$Brand[grep(pattern="mango|mng", dati$Brand)] <- "mango"
dati$Brand[grep(pattern="h&m|h and m|h & m|divided", dati$Brand)] <- "h&m"
dati$Brand[grep(pattern="shein|romwe|emery rose|cuccoo", dati$Brand)] <- "shein"
dati$Brand[grep(pattern="asos", dati$Brand)] <- "asos"
dati$Brand[grep(pattern="^on$|^on cloud$", dati$Brand)] <- "on running"
dati$Brand[grep(pattern="yasuhiro", dati$Brand)] <- "mihara yasuhiro"
dati$Brand[grep(pattern="valentino", dati$Brand)] <- "valentino"
dati$Brand[grep(pattern="louis vuitton|\\blouis\\b", dati$Brand)] <- "louis vuitton"
dati$Brand[grep(pattern="philipp plein|plein sport", dati$Brand)] <- "philipp plein"
dati$Brand[grep(pattern ="philippe model", dati$Brand)] <- "philippe model" # Attenzione a non confonderlo con Philipp Plein!
dati$Brand[grep(pattern ="dolce & gabbana|d&g|dolce e gabbana", dati$Brand)] <- "dolce & gabbana"
dati$Brand[grep(pattern ="gaëlle paris|gaelle", dati$Brand)] <- "gaelle paris"
dati$Brand[grep(pattern ="boss", dati$Brand)] <- "hugo boss"

dati$Brand_raw <- dati$Brand

sort(table(dati$Brand), decreasing = T)
#head(sort(table(dati$Brand), decreasing = T), 100)

marche_bambini <- c("minecraft", "paw patrol", "disney", "bluey",
                    "frozen", "marvel", "yu-gi-oh!", "chicco", "primigi", "lelli kelly")

marche_lusso <- c("gucci", "prada", "balenciaga", "dior", "louis vuitton",
                  "hermes", "bottega veneta", "givenchy", "saint laurent")

dati$Brand <- as.factor(dati$Brand)
dati$Brand <- fct_collapse(dati$Brand, "Marche Bambini" = marche_bambini, "Marche di Lusso" = marche_lusso)

dati$Brand <- fct_lump_min(dati$Brand, min = 30, other_level = "Altro")

sort(table(dati$Brand), decreasing = T)

sort(table(dati$Color))

# Definiamo i vettori dei colori
chiari <- c("Bianco", "Panna", "Beige", "Argento", "Chiaro", "Albicocca", "Azzurro")
scuri  <- c("Nero", "Grigio", "Blu marino", "Marrone", "Borgogna", "Verde scuro", "Cachi", "Blu", "Rosso")
sgargianti <- c("Rosa", "Arancione", "Giallo", "Lilla", "Verde", "Viola", "Corallo", "Turchese", "Menta", "Senape", "Oro", "Multi")

classifica <- function(stringa) {
  colori <- unlist(strsplit(stringa, ", "))
  n <- length(colori)

  tutti_chiari     <- all(colori %in% chiari)
  tutti_scuri      <- all(colori %in% scuri)
  tutti_sgargianti <- all(colori %in% sgargianti)

  # 1. Multicolor:
  # - Due colori di categorie diverse (es. Bianco + Nero)
  # - Almeno un colore sgargiante + altro colore diverso
  # - Se la stringa è proprio "Multi"
  if (n == 2 && colori[1] != colori[2]) {
    # Se non sono entrambi chiari o entrambi scuri o entrambi sgargianti, sono un mix
    if (!(tutti_chiari || tutti_scuri || tutti_sgargianti)) {
      return("Multicolor")
    }
    # Se c'è almeno uno sgargiante (e sono diversi), è Multicolor per tua richiesta precedente
    if (any(colori %in% sgargianti)) {
      return("Multicolor")
    }
  }
  # Se almeno uno dei due colori è multi allora il prodotto è multicolor
  if (any(colori %in% "Multi")) return("Multicolor")

  # 2. Colorato:
  # - Un solo colore sgargiante (n=1)
  # - Due colori sgargianti UGUALI (es. "Rosa, Rosa")
  if ((n == 1 && stringa %in% sgargianti) || (n == 2 && colori[1] == colori[2] && tutti_sgargianti)) {
    return("Colorato")
  }

  # 3. Mondo Chiaro:
  # - Un solo colore chiaro
  # - Due colori chiari (anche diversi, es. "Bianco, Beige")
  if (tutti_chiari) {
    return("Chiaro")
  }

  # 4. Mondo Scuro:
  # - Un solo colore scuro
  # - Due colori scuri (anche diversi, es. "Nero, Grigio")
  if (tutti_scuri) {
    return("Scuro")
  }

  return("Altro/Non Classificato")
}

# Applicazione al dataframe
dati$Color_new <- sapply(dati$Color, classifica)
table(dati$Color_new)

dati$Color <- NULL
dati$Color1 <- NULL
dati$Color2 <- NULL

dati$Size <- gsub(",", ".", dati$Size)
dati$Size <- as.numeric(dati$Size)
dati <- dati[!is.na(dati$Size),]

hist(dati$Size)
hist(dati$Size |> sqrt())

ordine_stato <- c("Discrete", "Buone", "Ottime", "Nuovo senza cartellino", "Nuovo con cartellino")
sum(is.na(dati$Condition))
dati$Condition <- factor(dati$Condition, levels = ordine_stato, ordered = TRUE)
dati$Condition
table(dati$Condition)

dati$Material <- fct_na_value_to_level(dati$Material, "ignoto")
dati$Material <- ifelse(dati$Material == "ignoto", "ignoto", "noto")
table(dati$Material)

dati$Favorites_Count[is.na(dati$Favorites_Count)] <- 0
hist(dati$Favorites_Count)
boxplot(dati$Favorites_Count)

dim(dati)

dati$Description <- NULL

sum(is.na(dati$Price))

hist(dati$Price)
boxplot(dati$Price)

hist(dati$Price |> log())
boxplot(dati$Price |> log())

dati$LogPrice <- log(dati$Price)

dati$Upload_Date_Raw |> unique()
dati$Upload_Date_Raw <- NULL

colnames(dati)

dati$Shipping_Cost |> boxplot()
sum(dati$Shipping_Cost==0)

dati$Shipping_Options_Count <- NULL
table(dati$Is_Boosted)

dati$Has_Buyer_Protection <- NULL
table(dati$Has_Buyer_Protection)

table(dati$Has_Item_Verification) 

dati$Seller_Location |> unique()

library(stringr)

dati <- dati %>%
  mutate(Seller_Country = str_extract(Seller_Location, "[^,]+$") %>% str_trim())

table(dati$Seller_Country, useNA = "ifany")
View(dati[is.na(dati$Seller_Country),])

library(ggplot2)
ggplot(dati, aes(x = Seller_Country, y = Price, fill = Seller_Country)) +
  geom_boxplot(alpha = 0.7, outlier.color = "red", outlier.shape = 16,
               outliers = F) +
  theme_minimal() +
  labs(
    title = "Distribuzione dei prezzi delle sneakers per nazione",
    subtitle = "Dataset Vinted",
    x = "Nazione del venditore",
    y = "Prezzo (€)"
  ) +
  theme(
    legend.position = "none", # La legenda è ridondante perché le nazioni sono già sull'asse X
    axis.text.x = element_text(angle = 45, hjust = 1) # Ruota i nomi se sono lunghi
  )

dati$Seller_Location <- NULL
dati$Seller_Country <- NULL

dati$Last_Seen <- NULL

dati$Seller_Rating[dati$Seller_Rating |> is.na()] <- "Nessuna recensione"

hist(dati$Seller_Rating |> as.numeric(), na.rm = T)

dati <- dati %>%
  mutate(
    rating_num = suppressWarnings(as.numeric(Seller_Rating)),
    Seller_Rating_Class = case_when(
      Seller_Rating == "Nessuna recensione" ~ "Nessuna recensione",
      rating_num <= 4.0                      ~ "Basso (<= 4.0)",
      rating_num > 4.0 & rating_num < 4.7  ~ "Buono (4.1 - 4.6)",
      rating_num >= 4.7 & rating_num < 5.0  ~ "Ottimo (4.7 - 4.9)",
      rating_num == 5.0                     ~ "Perfetto (5.0)",
      TRUE                                  ~ "Nessuna recensione"
    ),
    
    # Trasformiamo in fattore ordinato (fondamentale per i futuri grafici!)
    Seller_Rating_Class = factor(Seller_Rating_Class, levels = c(
      "Nessuna recensione", "Basso (<= 4.0)", "Buono (4.1 - 4.6)", "Ottimo (4.7 - 4.9)", "Perfetto (5.0)"
    ))
  ) %>% 
  select(-rating_num) # Rimuoviamo la colonna numerica temporanea

table(dati$Seller_Rating_Class)
table(dati$Seller_Rating_Class) |> prop.table()


dati$Seller_Reviews_Count[dati$Seller_Reviews_Count  |> is.na()] <- 0

boxplot(dati$Seller_Reviews_Count)
boxplot(dati$Seller_Reviews_Count |> log1p())

dati$Log_Seller_Reviews_Count <- dati$Seller_Reviews_Count |> log1p()
dati$Seller_Reviews_Count <- NULL

dati$Seller_has_distintivi <- ifelse(dati$Seller_distintivi == "[]",
                                     0, 1)
dati$Seller_distintivi <- NULL

dati$Seller_Is_Pro <- NULL

library(stringr)
dati <- dati %>%
  mutate(
    Num_Other_Items = case_when(
      is.na(Other_Items_Previewed_URLs) | Other_Items_Previewed_URLs == "" ~ 0,
      TRUE ~ str_count(Other_Items_Previewed_URLs, fixed("|")) + 1
    )
  )

dataset_regressione_prezzo <- dati |> 
  select(Price, Brand, Size, Condition, Material, Favorites_Count, Shipping_Cost,
         Is_Boosted, Has_Item_Verification, Seller_Rating, Color_new,
         Seller_Rating_Class, Log_Seller_Reviews_Count, Seller_has_distintivi,
         Num_Other_Items) |> 
  rename(y=Price)

dataset_regressione_favoriti <- dati |> 
  select(LogPrice, Brand, Size, Condition, Material, Favorites_Count, Shipping_Cost,
         Is_Boosted, Has_Item_Verification, Seller_Rating, Color_new,
         Seller_Rating_Class, Log_Seller_Reviews_Count, Seller_has_distintivi,
         Num_Other_Items) |> 
  rename(y=Favorites_Count)

dataset_classificazione_qualita <- dati |> 
  select(LogPrice, Brand, Size, Condition, Material, Favorites_Count, Shipping_Cost,
         Is_Boosted, Has_Item_Verification, Seller_Rating, Color_new,
         Seller_Rating_Class, Log_Seller_Reviews_Count, Seller_has_distintivi,
         Num_Other_Items) |> 
  rename(y=Condition)

dataset_MBA <- dati |> 
  select(URL, Brand_raw, Seller_User, Other_Items_Previewed_URLs)

dataset_clustering <- dati |> 
  select(LogPrice, Brand, Size, Condition, Material, Favorites_Count,
         Is_Boosted, Color_new)

set.seed(1)
id_stima <- sample(1:NROW(dati), size = NROW(dati)*0.75)
id_verifica <- setdiff(1:NROW(dati), id_stima)

K <- 6
fold_id <- sample(1:K, NROW(dati), replace=T)
table(fold_id)

setwd("C:/Users/Utente/OneDrive/Universita/Magistrale/2025-2026/Aziendali/Progetto/Analisi")
save(dataset_regressione_prezzo, dataset_regressione_favoriti,
     dataset_classificazione_qualita, dataset_MBA, dataset_clustering,
     id_stima, id_verifica, fold_id, file="dati_puliti.Rdata")

colMeans(is.na(dati))
