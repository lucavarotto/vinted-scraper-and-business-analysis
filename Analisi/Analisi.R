rm(list=ls()); gc()

library(readr)

dati <- read_csv("Scraping/datasets/dati.csv")

View(dati)

cat("dimensioni:", dim(dati))
dati[is.na(dati$Condition), ]
dati <- dati[!is.na(dati$Condition), ]
cat("\ndimensioni:", dim(dati))
cat("\nrighe uniche:", length(unique(dati$URL)), length(unique(dati$Item_ID)))

dati <- dati[!duplicated(dati$Item_ID),]
dim(dati)

sum((table(dati$Seller_User) |> sort())>1)

brand <- readr::read_csv("Scraping/google_trends_brand_country.csv")
View(brand)

View(dati[,c("Brand", "Seller_Location")])
