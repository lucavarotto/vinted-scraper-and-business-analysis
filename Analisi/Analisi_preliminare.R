setwd("C:/Users/Utente/OneDrive/Universita/Magistrale/2025-2026/Aziendali/Progetto")
rm(list=ls()); gc()

dati <- readr::read_csv("Scraping/dati.csv")

View(subset(dati, is.na(dati$Seller_User)))

View(dati)

cat("dimensioni:", dim(dati))
dati[is.na(dati$Condition), ]
dati <- dati[!is.na(dati$Condition), ]
cat("\ndimensioni:", dim(dati))
cat("\nrighe uniche:", length(unique(dati$URL)), length(unique(dati$Item_ID)))

dati <- dati[!duplicated(dati$Item_ID),]
dim(dati)

dati[dati$Seller_Is_Pro==T,]$Seller_User

sort(table(dati$Seller_User), decreasing = T) |> 
  head(70)

sum((table(dati$Seller_User) |> sort())>1)

ids <- intersect(which(stringr::str_count(dati$Other_Items_Previewed_URLs, pattern=stringr::fixed("|")) > 30),
          which(!is.na(dati$Seller_User)))

dati$Seller_User[ids]



size_originale <- dati$Size
size_gsub <- gsub(",", ".", dati$Size)
size_numeric <- as.numeric(size_gsub)

dati[size_numeric |> is.na() |> which(),] |> 
  View()


dati <- readr::read_csv("Analisi/dati_puliti_colab.csv")
dati$Description[4]
unique(dati$Brand_raw) |> c()
sum(is.na(dati$Brand_raw))

dati[dati$Brand_raw=="cat",]






brand <- readr::read_csv("Scraping/google_trends_global.csv")
View(brand)

brand[brand$avg_interest_global_last_month < 5,1] |> c()
brand[brand$avg_interest_global_last_month > 60,1] |> c()

boxplot(brand$avg_interest_global_last_month)
