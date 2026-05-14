import pandas as pd
from os import listdir

print(listdir("Progetto/Scraping/datasets"))

# Caricamento dei dataset
dati1 = pd.read_csv("Progetto/Scraping/datasets/vinted_sneakers_20260511_18-15.csv")
dati2 = pd.read_csv("Progetto/Scraping/datasets/vinted_sneakers_20260512_15-02.csv")
dati3 = pd.read_csv("Progetto/Scraping/datasets/vinted_sneakers_20260513_15-08.csv")
dati4 = pd.read_csv("Progetto/Scraping/datasets/vinted_Puma_Blipstream_20260514_12-22.csv")
dati5 = pd.read_csv("Progetto/Scraping/datasets/dati_seller_pro.csv")

# concatenazione
# ignore_index=True serve a rifare la numerazione delle righe da 0 a N
dati = pd.concat([dati1, dati2, dati3, dati4, dati5], ignore_index=True)

# Verifica le dimensioni
print(dati.shape)

# Salvataggio in CSV (con codifica UTF-8 per sicurezza)
dati.to_csv("Progetto/Scraping/datasets/dati.csv", index=False, encoding='utf-8-sig')

print("Finito")