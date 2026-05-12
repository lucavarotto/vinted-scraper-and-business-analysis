rm(list=ls());gc();
setwd("C:/Users/Utente/OneDrive/Universita/Magistrale/2025-2026/Aziendali/Progetto")

dir("Scraping/datasets")
dati1 <- readr::read_csv("Scraping/datasets/vinted_sneakers_20260511_18-15.csv")
dati2 <- readr::read_csv("Scraping/datasets/vinted_sneakers_20260512_15-02.csv")
dati <- rbind(dati1, dati2)

save(dati, file = "Scraping/datasets/dati.csv")

brand <- readr::read_csv("Scraping/google_trends_brand_country.csv")
View(brand)

View(dati[,c("Brand", "Seller_Location")])
