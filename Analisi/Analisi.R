setwd("C:/Users/Utente/OneDrive/Universita/Magistrale/2025-2026/Aziendali/Progetto")
rm(list=ls()); gc()

dati <- readr::read_csv("Scraping/datasets/dati.csv")

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





id <- grep(pattern="min", dati$Upload_Date_Raw)
dati$Upload_Date_Raw[id] <- "0 ore"
table(dati$Upload_Date_Raw)

brand <- readr::read_csv("Scraping/google_trends_brand_country.csv")
View(brand)

View(dati[,c("Brand", "Seller_Location")])
