import time
import logging
import pandas as pd
from pytrends.request import TrendReq
from pytrends.exceptions import ResponseError
import os
from random import random


os.chdir("Progetto")

# Configurazione del sistema di logging per stampare messaggi puliti in console
# Mostrerà il tipo di messaggio (INFO, WARNING, ERROR) seguito dal testo
logging.basicConfig(level=logging.INFO, format="%(levelname)s: %(message)s")
logger = logging.getLogger(__name__)

def fetch_trends_global_batch(
    pytrends: TrendReq,
    keywords: list[str],
    timeframe: str = "today 12-m",
    retries: int = 5,
    base_wait: float = 15.0,
) -> dict[str, float]:
    """
    Invia una richiesta a Google Trends per un gruppo (batch) di massimo 5 parole chiave.
    Opera a livello globale (geo=''), calcola la media dell'interesse nel periodo specificato
    e gestisce i tentativi di recupero in caso di errore 429 (Too Many Requests).
    """
    for attempt in range(retries):
        try:
            # Configura il payload della richiesta. geo='' indica l'estensione mondiale.
            # hl="en-US" (impostato nell'inizializzazione) evita discrepanze di traduzione dei brand.
            # di default si guarda l'ultimo mese
            pytrends.build_payload(keywords, geo='', timeframe=timeframe)

            # Scarica il DataFrame storico dell'interesse nel tempo (valori da 0 a 100)
            df = pytrends.interest_over_time()
            
            # Se Google Trends non restituisce dati per questo blocco, assegna 0.0 a tutti i brand
            if df.empty:
                return {kw: 0.0 for kw in keywords}
            
            result = {}
            for kw in keywords:
                # Se la colonna del brand esiste nel DataFrame, calcola la media dei punteggi del mese
                if kw in df.columns:
                    result[kw] = float(df[kw].mean())
                else:
                    # Se il brand non compare tra le colonne restituite, assegna 0.0
                    result[kw] = 0.0
            return result
            
        except ResponseError as e:
            # Gestione specifica del Rate Limit di Google (Errore HTTP 429)
            if "429" in str(e) and attempt < retries - 1:
                # Applica il Backoff Esponenziale: raddoppia l'attesa a ogni fallimento successivo
                wait = base_wait * (2 ** attempt)
                logger.warning(f"Rate limit hit, attendo {wait:.0f}s...")
                time.sleep(wait)
            else:
                # Se l'errore non è un 429 o sono finiti i tentativi a disposizione, registra l'errore definitivo
                logger.error(f"Errore API per {keywords}: {e}")
                # Restituisce None per identificare i brand falliti nel dataset finale
                return {kw: None for kw in keywords}

def main(
    input_csv: str = "Analisi/dati_puliti_colab.csv",
    output_csv: str = "Scraping/google_trends_global.csv",
    delay_between_batches: float = 3.0,
):
    # Legge il file CSV dei brand precedentemente pulito in R
    df = pd.read_csv(input_csv)

    # Estrae i valori unici dalla colonna dei brand grezzi, rimuovendo i valori nulli (NaN)
    unique_brands = df["Brand_raw"].dropna().unique().tolist()

    # TRASFORMAZIONE STRATEGICA: Converte ogni brand nella stringa "shoes {nome_brand}"
    # Questo serve a ridurre l'omonimia su Google Trends (es. evita l'animale "Puma" o il film "Frozen").
    # Viene contestualmente escluso il valore "ignoto" generato dalla pulizia in R.
    unique_brands = [f"shoes {brand}" for brand in unique_brands if brand != "ignoto"]
    
    logger.info(f"Brand unici da analizzare a livello globale: {len(unique_brands) + 1}")

    # hl="en-US" forza i trend globali in lingua inglese. tz=60 imposta il fuso orario.
    # timeout=(10, 25) blocca la richiesta se Google non risponde entro 10s (connessione) o 25s (lettura)
    pytrends = TrendReq(hl="en-US", tz=60, timeout=(10, 25))
    results = []

    # Google Trends accetta al massimo 5 keyword per richiesta per fare il confronto.
    # Il ciclo 'for' avanza a passi di 5 (0, 5, 10, 15...) per creare finestre mobili (slicing).
    for i in range(0, len(unique_brands), 1):
        # Seleziona il blocco corrente di 5 brand (es. da i a i+5)
        batch = unique_brands[i : i + 1]
        logger.info(f"Batch {i + 1}/{len(unique_brands) + 1}: {batch}")
        
        # Interroga Google Trends passando l'istanza pytrends e il blocco di brand corretto
        scores = fetch_trends_global_batch(pytrends, batch)
        
        # Elabora le risposte restituite dalla funzione di scraping
        for search_term, score in scores.items():
            
            # PULIZIA INVERSA: Rimuove la parola "shoes " per salvare nel CSV finale 
            # solo il nome puro e pulito del brand (es. "shoes nike" -> "nike")
            brand_clean = search_term.replace("shoes ", "")

            # Salva i dati strutturati mantenendo traccia sia del brand che della query reale utilizzata
            results.append({
                "brand": brand_clean,
                "search_term": search_term, 
                "avg_interest_global_last_month": score
            })
        
        # Pausa di sicurezza obbligatoria tra un batch e l'altro per simulare un comportamento umano
        # ed evitare il ban immediato dell'indirizzo IP
        time.sleep(delay_between_batches)

    # Converte la lista di dizionari in un DataFrame strutturato di Pandas
    trends_df = pd.DataFrame(results)

    # Esporta i risultati in un nuovo file CSV pronto per essere analizzato o re-importato in R
    trends_df.to_csv(output_csv, index=False)
    logger.info(f"Risultati globali salvati in: {output_csv}")
    
    # Stampa di controllo in console dei 10 brand che hanno ottenuto la media di interesse più alta
    print("\nTop 10 Brand per interesse globale:")
    print(trends_df.sort_values("avg_interest_global_last_month", ascending=False).head(10))

if __name__ == "__main__":
    main()