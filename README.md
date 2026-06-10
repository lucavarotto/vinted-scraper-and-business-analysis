```text
.
├── Analisi/                        # Nucleo operativo della parte di machine learning
│   ├── 0_pulizia.R                 # Script R per il pre-processing e il cleaning dei dati grezzi
│   ├── dati_puliti.Rdata           # Dataset strutturato generato da "0_pulizia.R" e pronto per la modellazione
│   ├── 1_analisi.R                 # Script R per la fase di machine learning (da eseguire dopo la pulizia)
│   ├── utils.R                     # Funzioni ausiliarie e algoritmi di stima personalizzati utilizzati negli script principali
│   └── Plot/                       # Directory di output per il salvataggio dei grafici generati
│
├── Dati/                           # Cartella contenente i dataset grezzi estratti e utilizzati nelle varie fasi
│   ├── dati.csv                    # Dataset principale di partenza, ottenuto tramite lo scraping di Vinted
│   ├── google_trends_global.csv    # Trend di ricerca globali iniziali estratti tramite l'API di Google Trends
│   ├── brand_pt2.csv               # Dataset integrativo sui brand, arrivato dall'analisi dei dati di rete
│   └── google_trends_global_p2.csv # Dati di trend globali supplementari, necessari per completare l'analisi dei dati di rete
│
├── Scrapers/                       # Raccolta del codice utilizzato per la raccolta e l'estrazione dei dati
│   ├── Vinted_with_backup.py       # Script Python per il web scraping da Vinted (con sistema di backup)
│   └── Google_trends_globally.py   # Script Python per l'interfacciamento con le API di Google Trends
│
├── .gitignore                      # Esclusione dei file temporanei, locali o eccessivamente pesanti dal controllo di versione
├── Martini_Tomietto_Varotto.pptx   # Presentazione e report finale con la sintesi dei risultati aziendali e statistici
└── README.md                       # Questo file di documentazione
```

# Data-Driven Vinted Analytics

Questo progetto applica metodologie statistiche avanzate ed econometriche a un contesto business con mercato C2C: Vinted. Questo è un ecosistema ad alta variabilità. Le analisi si sono basate su un dataset di ~2000 scarpe in vendita, estratte tramite web scraping.

# Struttura del Progetto & Framework di Consulenza

L'analisi sul dataset Vinted è stata strutturata simulando un intervento di consulenza direzionale per la piattaforma, articolato in 4 pilastri analitici:

1. Algoritmo di Raccomandazione dei Prezzi: sviluppo di un modello predittivo per stimare il prezzo di vendita di prodotti simili.

2. Ottimizzazione del Motore di Ricerca: analisi delle determinanti dell'attrattività di un annuncio, misurata tramite il numero di preferiti, per supportare il team di sviluppo del search engine.

3. Valutazione e Percezione della Qualità: modellizzazione delle caratteristiche che definiscono un prodotto di "alta qualità" agli occhi della community. Utilizzo di una regressione Probit Ordinale per mappare la variabile risposta.

4. Analisi delle Reti e Sistemi di Raccomandazione: studio delle interdipendenze tra i brand e delle abitudini di stoccaggio degli utenti.

Metodologia: Applicazione della Market Basket Analysis (MBA) e della Network Analysis sulla variabile Other_Items_Previewed_URLs per mappare i grafi di co-occorrenza dei brand all'interno degli stessi armadi. L'obiettivo è generare un algoritmo di raccomandazione cross-selling per cluster di brand affini.
