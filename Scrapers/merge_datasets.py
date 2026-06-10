import pandas as pd
import os

# Definiamo il percorso della cartella
path = "Progetto/Scraping/datasets/"
print(os.listdir(path))

# 1. Creiamo una lista con i nomi di tutti i file .csv nella directory
files = os.listdir(path)

# 2. Carichiamo ogni file in una lista di DataFrame usando una "list comprehension"
lista_dataframe = [pd.read_csv(os.path.join(path, f)) for f in files]

# 3. Concateniamo tutto in un unico DataFrame
# ignore_index=True resetta l'indice per avere una numerazione continua
dati_completi = pd.concat(lista_dataframe, ignore_index=True)

# Verifica il risultato
print(f"File uniti: {len(files)}")
print(dati_completi.head())

# Verifica le dimensioni
print(dati_completi.shape)

# Salvataggio in CSV (con codifica UTF-8 per sicurezza)
dati_completi.to_csv("Progetto/Scraping/dati.csv", index=False, encoding='utf-8-sig')

print("Finito")