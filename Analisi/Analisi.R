rm(list=ls()); gc()

library(readr)

path1 <- "Scraping/datasets/vinted_sneakers_20260511_18-15.csv"
path2 <- "Scraping/datasets/vinted_sneakers_20260512_15-02.csv"

dati1 <- read_csv(path1, locale = locale(encoding = "UTF-8"), show_col_types = FALSE)
dati2 <- read_csv(path2, locale = locale(encoding = "UTF-8"), show_col_types = FALSE)

dati <- rbind(dati1, dati2)

dati <- dati[!is.na(dati$Condition), ]

write_excel_csv(dati, "Scraping/datasets/dati.csv", na = "NA")

dati <- read_csv("Scraping/datasets/dati.csv")


brand <- readr::read_csv("Scraping/google_trends_brand_country.csv")
View(brand)

View(dati[,c("Brand", "Seller_Location")])
