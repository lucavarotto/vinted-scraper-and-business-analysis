import time
import logging
import pandas as pd
from pytrends.request import TrendReq
from pytrends.exceptions import ResponseError
import os


os.chdir("Progetto")

# Configurazione logging
logging.basicConfig(level=logging.INFO, format="%(levelname)s: %(message)s")
logger = logging.getLogger(__name__)

def fetch_trends_global_batch(
    pytrends: TrendReq,
    keywords: list[str],
    timeframe: str = "today 1-m",
    retries: int = 5,
    base_wait: float = 20.0,
) -> dict[str, float]:
    """
    Chiama Google Trends a livello MONDIALE (geo='') per un batch di keyword.
    """
    for attempt in range(retries):
        try:
            # geo='' indica la ricerca globale
            pytrends.build_payload(keywords, geo='', timeframe=timeframe)
            df = pytrends.interest_over_time()
            
            if df.empty:
                return {kw: 0.0 for kw in keywords}
            
            result = {}
            for kw in keywords:
                if kw in df.columns:
                    result[kw] = float(df[kw].mean())
                else:
                    result[kw] = 0.0
            return result
            
        except ResponseError as e:
            if "429" in str(e) and attempt < retries - 1:
                wait = base_wait * (2 ** attempt)
                logger.warning(f"Rate limit hit, attendo {wait:.0f}s...")
                time.sleep(wait)
            else:
                logger.error(f"Errore API per {keywords}: {e}")
                return {kw: None for kw in keywords}

def main(
    input_csv: str = "Analisi/dati_puliti_colab.csv",
    output_csv: str = "Scraping/google_trends_global.csv",
    delay_between_batches: float = 5.0,
):
    # 1. Carica il dataset e prendi i brand UNICI
    df = pd.read_csv(input_csv)
    unique_brands = df["Brand_raw"].dropna().unique().tolist()
    
    logger.info(f"Brand unici da analizzare a livello globale: {len(unique_brands)}")

    # 2. Inizializza Pytrends
    pytrends = TrendReq(hl="en-US", tz=60, timeout=(10, 25))
    results = []

    # 3. Processa in batch da 5 (limite API)
    for i in range(0, len(unique_brands), 5):
        batch = unique_brands[i : i + 5]
        logger.info(f"Batch {i//5 + 1}/{len(unique_brands)//5 + 1}: {batch}")
        
        scores = fetch_trends_global_batch(pytrends, batch)
        
        for brand, score in scores.items():
            results.append({
                "brand": brand, 
                "avg_interest_global_last_month": score
            })
        
        # Pausa per evitare ban immediati
        time.sleep(delay_between_batches)

    # 4. Salva i risultati
    trends_df = pd.DataFrame(results)
    trends_df.to_csv(output_csv, index=False)
    logger.info(f"Risultati globali salvati in: {output_csv}")
    
    print("\nTop 10 Brand per interesse globale:")
    print(trends_df.sort_values("avg_interest_global_last_month", ascending=False).head(10))

if __name__ == "__main__":
    main()