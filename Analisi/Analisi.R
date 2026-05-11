setwd("C:/Users/Utente/OneDrive/Universita/Magistrale/2025-2026/Aziendali/Progetto")

dir("Scraping")
dati <- readr::read_csv("Scraping/vinted_sneakers_20260511_16-31.csv")
View(dati)

brand <- readr::read_csv("Scraping/google_trends_brand_country.csv")
View(brand)

View(dati[,c("Brand", "Seller_Location")])
