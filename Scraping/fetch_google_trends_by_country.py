"""
fetch_google_trends.py
----------------------
Scarica l'interesse di ricerca su Google (Google Trends) per ogni brand
presente nel dataset Vinted, filtrato per paese del venditore, nell'ultimo mese.

Output: google_trends_brand_country.csv
Colonne: brand, country_code, avg_interest_last_month

Dipendenze:
    pip install pytrends pandas

Note:
- Google Trends restituisce un indice relativo da 0 a 100 ("interest over time"),
  non il numero assoluto di ricerche. È comunque un ottimo proxy della popolarità.
- L'API non ufficiale ha rate limiting: lo script include retry con backoff
  esponenziale per evitare errori 429.
- pytrends accetta al massimo 5 keyword per chiamata; i brand vengono
  quindi processati in batch da 5.
"""

import time
import logging
import pandas as pd
from pytrends.request import TrendReq
from pytrends.exceptions import ResponseError
import os

try:
    os.chdir("Progetto/Scraping")
except FileNotFoundError:
    print("Cartella Progetto/Scraping non trovata, uso la cartella corrente.")

logging.basicConfig(level=logging.INFO, format="%(levelname)s: %(message)s")
logger = logging.getLogger(__name__)

# ---------------------------------------------------------------------------
# Mappa paese: dal testo di Seller_Location al geo-code di Google Trends
# Aggiungere altri paesi se necessario.
# ---------------------------------------------------------------------------
COUNTRY_NAME_TO_GEO = {
    "italia":       "IT",
    "italy":        "IT",
    "francia":      "FR",
    "france":       "FR",
    "spagna":       "ES",
    "spain":        "ES",
    "germania":     "DE",
    "germany":      "DE",
    "portogallo":   "PT",
    "portugal":     "PT",
    "belgio":       "BE",
    "belgium":      "BE",
    "olanda":       "NL",
    "netherlands":  "NL",
    "svizzera":     "CH",
    "switzerland":  "CH",
    "austria":      "AT",
    "polonia":      "PL",
    "poland":       "PL",
    "regno unito":  "GB",
    "united kingdom": "GB",
    "uk":           "GB",
}


def extract_country_code(location: str) -> str | None:
    """Estrae il codice paese da una stringa tipo 'Inzago, Italia'."""
    if not isinstance(location, str):
        return None
    location_lower = location.lower()
    for name, code in COUNTRY_NAME_TO_GEO.items():
        if name in location_lower:
            return code
    return None


def fetch_trends_batch(
    pytrends: TrendReq,
    keywords: list[str],
    geo: str,
    timeframe: str = "today 1-m",
    retries: int = 5,
    base_wait: float = 60.0,
) -> dict[str, float]:
    """
    Chiama Google Trends per un batch di keyword (max 5) e restituisce
    {keyword: media_interesse_periodo}.
    In caso di rate limit (429) esegue retry con backoff esponenziale.
    """
    for attempt in range(retries):
        try:
            pytrends.build_payload(keywords, geo=geo, timeframe=timeframe)
            df = pytrends.interest_over_time()
            if df.empty:
                return {kw: 0.0 for kw in keywords}
            # Media temporale per ogni keyword (escludi colonna 'isPartial')
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
                logger.warning(f"Rate limit hit, attendo {wait:.0f}s prima del retry...")
                time.sleep(wait)
            else:
                logger.error(f"Errore API per keywords {keywords} / geo={geo}: {e}")
                return {kw: None for kw in keywords}
        except Exception as e:
            logger.error(f"Errore imprevisto: {e}")
            return {kw: None for kw in keywords}


def main(
    input_csv: str = "vinted_sneakers.csv",
    output_csv: str = "google_trends_brand_country.csv",
    delay_between_batches: float = 5.0,   # secondi tra un batch e l'altro
):
    # ------------------------------------------------------------------
    # 1. Carica il dataset e ricava le coppie (brand, country_code) uniche
    # ------------------------------------------------------------------
    df = pd.read_csv(input_csv)
    df["country_code"] = df["Seller_Location"].apply(extract_country_code)

    pairs = (
        df[["Brand", "country_code"]]
        .dropna(subset=["Brand", "country_code"])
        .drop_duplicates()
        .rename(columns={"Brand": "brand"})
        .reset_index(drop=True)
    )

    logger.info(f"Coppie (brand, paese) da interrogare: {len(pairs)}")
    logger.info(f"Brand unici: {pairs['brand'].nunique()}")
    logger.info(f"Paesi unici: {pairs['country_code'].unique().tolist()}")

    # 2. Raggruppa per paese e interroga Google Trends in batch da 5
    pytrends = TrendReq(hl="en-US", tz=60, timeout=(10, 25))
    results = []

    for country_code, group in pairs.groupby("country_code"):
        brands = group["brand"].tolist()
        logger.info(f"[{country_code}] Fetching {len(brands)} brand...")

        # Batch da massimo 5 keyword
        for i in range(0, len(brands), 5):
            batch = brands[i : i + 5]
            logger.info(f"  Batch {i//5 + 1}: {batch}")
            scores = fetch_trends_batch(pytrends, batch, geo=country_code)
            for brand, score in scores.items():
                results.append(
                    {"brand": brand, "country_code": country_code, "avg_interest_last_month": score}
                )
            time.sleep(delay_between_batches)

    # 3. Salva il dataset di output
    trends_df = pd.DataFrame(results)
    trends_df.to_csv(output_csv, index=False)
    logger.info(f"Salvato: {output_csv}  ({len(trends_df)} righe)")

    # Preview
    print("\nAnteprima risultati:")
    print(trends_df.sort_values("avg_interest_last_month", ascending=False).head(15).to_string(index=False))
    return trends_df


if __name__ == "__main__":
    main()
