import pandas as pd
import time
import random
import re
import os
from selenium import webdriver
from selenium.webdriver.chrome.service import Service
from webdriver_manager.chrome import ChromeDriverManager
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from selenium.common.exceptions import (
    NoSuchElementException, TimeoutException, StaleElementReferenceException
)
from datetime import datetime

try:
    os.chdir("Progetto/Scraping")
except FileNotFoundError:
    print("Cartella Progetto/Scraping non trovata, uso la cartella corrente.")

# ──────────────────────────────────────────────
# CONFIGURAZIONE
# ──────────────────────────────────────────────

SCROLL_PAUSE  = 0.6
PAGE_LOAD_MIN = 3.0
PAGE_LOAD_MAX = 7.0
ITEM_LOAD_MIN = 3.0
ITEM_LOAD_MAX = 6.0

# ──────────────────────────────────────────────
# DRIVER
# ──────────────────────────────────────────────

def init_driver():
    """Inizializza Chrome con impostazioni anti-rilevamento bot."""
    options = webdriver.ChromeOptions()
    options.add_argument("--disable-blink-features=AutomationControlled")
    options.add_argument("window-size=1280,900")
    options.add_argument(
        "user-agent=Mozilla/5.0 (Windows NT 10.0; Win64; x64)"
        "AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0.0.0 Safari/537.36"
    )
    options.add_experimental_option("excludeSwitches", ["enable-automation"])
    options.add_experimental_option("useAutomationExtension", False)
    driver = webdriver.Chrome(service=Service(ChromeDriverManager().install()), options=options)
    driver.execute_cdp_cmd(
        "Page.addScriptToEvaluateOnNewDocument",
        {"source": "Object.defineProperty(navigator, 'webdriver', {get: () => undefined})"}
    )
    return driver

# ──────────────────────────────────────────────
# UTILITÀ DOM
# ──────────────────────────────────────────────

def accept_cookies(driver):
    """Chiude il banner cookie OneTrust se presente."""
    try:
        btn = WebDriverWait(driver, 5).until(
            EC.element_to_be_clickable((By.ID, "onetrust-accept-btn-handler"))
        )
        btn.click()
        time.sleep(1)
    except (TimeoutException, NoSuchElementException):
        pass

def scroll_to_bottom(driver, max_scrolls=8):
    """
    Scrolla gradualmente per triggerare il lazy-load.
    Senza questo passaggio elementi come Views, Badge e Seller_Info
    non vengono renderizzati e risultano assenti dal DOM.
    """
    last_height = driver.execute_script("return document.body.scrollHeight")
    for _ in range(max_scrolls):
        driver.execute_script("window.scrollBy(0, 600);")
        time.sleep(SCROLL_PAUSE)
        new_height = driver.execute_script("return document.body.scrollHeight")
        if new_height == last_height:
            break
        last_height = new_height
    driver.execute_script("window.scrollTo(0, 0);")
    time.sleep(0.4)


def get_attr_value(driver, testid, itemprop=None):
    """
    Estrae il valore pulito di un attributo prodotto Vinted.

    La struttura HTML di Vinted è:
      <div data-testid="{testid}">
        <div class="details-list__item-value"><span>Label</span></div>
        <div class="details-list__item-value" itemprop="{itemprop}">
          <span>Valore<button>testo-spazzatura</button></span>
        </div>
      </div>

    Selenium .text() include il testo di tutti i nodi figli, compresi i <button>
    con testi come "Informazioni sulla taglia" o "Condition information".
    Questo helper usa JS per percorrere l'albero DOM ignorando button/svg/path
    e restituisce solo il primo nodo testuale significativo.

    Parametri
    ---------
    testid   : valore del data-testid del container (es. "item-attributes-size")
    itemprop : valore dell'attributo itemprop del nodo valore (es. "size").
               Se fornito, il selettore è più preciso e robusto.
               Se None, si usa il secondo .details-list__item-value come fallback.
    """
    try:
        if itemprop:
            css = f"[data-testid='{testid}'] [itemprop='{itemprop}']"
        else:
            css = f"[data-testid='{testid}'] .details-list__item-value:last-child"

        elem = driver.find_element(By.CSS_SELECTOR, css)

        text = driver.execute_script("""
            function firstText(el) {
                for (var node of el.childNodes) {
                    if (node.nodeType === 3 && node.textContent.trim()) {
                        return node.textContent.trim();
                    }
                    var tag = node.nodeName ? node.nodeName.toUpperCase() : '';
                    if (node.nodeType === 1
                            && tag !== 'BUTTON' && tag !== 'SVG' && tag !== 'PATH') {
                        var t = firstText(node);
                        if (t) return t;
                    }
                }
                return '';
            }
            return firstText(arguments[0]);
        """, elem)
        return text.strip() if text else None

    except (NoSuchElementException, StaleElementReferenceException):
        return None


def safe_find_text(driver, css_selector=None, xpath=None, split_last=True):
    """
    Cerca un elemento tramite CSS o XPath e restituisce il testo.
    Usato per elementi che non seguono la struttura standard degli attributi
    (titolo, descrizione, sezione venditore, spedizione, ecc.).

    split_last=True estrae l'ultimo token dopo newline:
    utile per blocchi "Etichetta\\nValore".
    """
    try:
        if css_selector:
            elem = driver.find_element(By.CSS_SELECTOR, css_selector)
        elif xpath:
            elem = driver.find_element(By.XPATH, xpath)
        else:
            return None

        text = elem.text.strip()
        if not text:
            text = driver.execute_script(
                "return arguments[0].innerText;", elem
            ).strip()

        if split_last and '\n' in text:
            return text.split('\n')[-1].strip()
        return text or None

    except (NoSuchElementException, StaleElementReferenceException):
        return None


# ──────────────────────────────────────────────
# UTILITÀ NUMERICHE
# ──────────────────────────────────────────────

def extract_number(text):
    """
    Estrae il primo numero da una stringa in formato italiano.

    Priorità (dalla più specifica alla meno):
      1. Formato europeo con decimali: "1.234,56 €"  → "1234.56"
                                       "60,00 €"      → "60.00"
      2. Migliaia senza decimali:      "1.234"        → "1234"
      3. Intero semplice:              "123 articoli" → "123"
    """
    if not text:
        return None
    m = re.search(r'\b(\d{1,3}(?:\.\d{3})*),(\d{2})\b', text)
    if m:
        return m.group(1).replace('.', '') + '.' + m.group(2)
    m = re.search(r'\b(\d{1,3}(?:\.\d{3})+)\b', text)
    if m:
        return m.group(1).replace('.', '')
    m = re.search(r'\b\d+\b', text)
    return m.group() if m else None


def extract_decimal(text):
    """
    Estrae un numero intero o decimale da una stringa.
    Usato per il rating venditore (es. "4.8", "29", "2,8").
    """
    if not text:
        return None
    match = re.search(r"(\d+[\.,]\d+|\d+)", text)
    if match:
        return float(match.group(1).replace(',', '.'))
    return None

# ──────────────────────────────────────────────
# FASE 1 – RACCOLTA LINK + BOOST
# ──────────────────────────────────────────────

def get_item_links(driver, search_query, limit):
    """
    Raccoglie i link agli articoli dalle pagine di risultati.

    FIX #1 — BOOST: restituisce una lista di tuple (url, is_boosted) invece
    di semplici URL. Il flag boost è estratto qui perché la label "Con boost"
    appare solo nella griglia di ricerca (data-testid="*--bump-text") e non
    nella pagina del singolo articolo.

    Scarta automaticamente URL di profili (/member/, /profile/).
    """
    # Dizionario url → is_boosted per evitare duplicati e preservare il flag
    links_dict = {}
    page = 1
    cookies_accepted = False

    while True:
        url = f"https://www.vinted.it/catalog?search_text={search_query}&page={page}"
        print(f"  Raccolta link — pagina {page}...")
        driver.get(url)
        time.sleep(random.uniform(PAGE_LOAD_MIN, PAGE_LOAD_MAX))

        if not cookies_accepted:
            accept_cookies(driver)
            cookies_accepted = True

        try:
            WebDriverWait(driver, 12).until(
                EC.presence_of_element_located((By.CSS_SELECTOR, ".feed-grid__item"))
            )
        except TimeoutException:
            print("  Timeout: nessun risultato o Vinted ha bloccato la richiesta.")
            break

        scroll_to_bottom(driver, max_scrolls=5)

        # Iteriamo sui box-articolo per estrarre sia l'href che il flag boost
        # in un'unica passata, evitando ricerche ripetute nel DOM.
        grid_items = driver.find_elements(By.CSS_SELECTOR, ".feed-grid__item")
        initial_count = len(links_dict)

        for grid_item in grid_items:
            try:
                # Estrai il link dal tag <a> all'interno del box
                a_tag = grid_item.find_element(By.CSS_SELECTOR, "a[href*='/items/']")
                href = a_tag.get_attribute("href")
                if not href:
                    continue
                clean = href.split("?")[0]

                # FIX #1 — BOOST: cerca il tag con data-testid="*--bump-text"
                # Presente solo negli articoli promossi; assente negli altri.
                # Struttura HTML: <p data-testid="product-item-id-XXXXX--bump-text">Con boost</p>
                try:
                    bump_elem = grid_item.find_element(
                        By.CSS_SELECTOR, "[data-testid$='--bump-text']"
                    )
                    is_boosted = bump_elem.text.strip().lower() == "con boost"
                except NoSuchElementException:
                    is_boosted = False

                if clean not in links_dict:
                    links_dict[clean] = is_boosted
                    if limit != 0 and len(links_dict) >= limit:
                        print(f"  Limite raggiunto: {len(links_dict)} link.")
                        return list(links_dict.items())

            except (StaleElementReferenceException, NoSuchElementException):
                continue

        if len(links_dict) == initial_count:
            print("  Nessun nuovo link: fine dei risultati.")
            break

        page += 1
        if page > 15:
            break

    print(f"  Totale link raccolti: {len(links_dict)}")
    return list(links_dict.items())


# ──────────────────────────────────────────────
# FASE 2 – SCRAPING DETTAGLIATO
# ──────────────────────────────────────────────

def scrape_item_details(driver, url, is_boosted=False):
    """
    Estrae tutti i dati di un articolo dalla sua pagina prodotto.

    Struttura dei campi per progetto ML:
    - Regressione prezzo      : Price, Brand, Size, Condition, Color, Material,
                                Shipping_Cost, Favorites_Count, Current_Time,
                                Category, Seller_Rating,
                                Seller_Reviews_Count
    - Classificazione ordinale: Condition è ordinale nativa su Vinted
                                ("Nuovo con etichetta" > "Ottime" > "Buone" > "Soddisfacenti")
    - Badge di fiducia        : Has_Buyer_Protection, Has_Item_Verification,
                                Is_Boosted (dalla griglia)
    - Analisi di rete         : Seller_User, Seller_Location,
                                Other_Items_Previewed_URLs
                                (grafo bipartito venditore <-> categoria)
    - Nuove variabili aggiunte: Is_Boosted, Has_Buyer_Protection,
                                Has_Item_Verification, Seller_Is_Pro
    """
    driver.get(url)
    time.sleep(random.uniform(ITEM_LOAD_MIN, ITEM_LOAD_MAX))

    item_data = {
        # ── Identificatori ─────────────────────────────────────────────────
        "URL":     url,
        "Item_ID": re.search(r'/items/(\d+)', url).group(1)
                   if re.search(r'/items/(\d+)', url) else None,
        "Current_Time": None,

        # ── Prodotto ───────────────────────────────────────────────────────
        "Title":       None,
        "Price":       None,
        "Brand":       None,
        "Category":    None,
        "Size":        None,
        "Condition":   None,
        "Color":       None,
        "Material":    None,
        "Description": None,

        # ── Engagement ─────────────────────────────────────────────────────
        "Favorites_Count": None,
        "Upload_Date_Raw": None,

        # ── Spedizione ─────────────────────────────────────────────────────
        "Shipping_Cost":          None,
        "Shipping_Options_Count": None,

        # ── Badge di fiducia ───────────────────────────────────────────────
        # Is_Boosted: flag proveniente dalla griglia di ricerca (Fase 1).
        # Non è estraibile dalla pagina del singolo articolo.
        "Is_Boosted": is_boosted,

        # Has_Buyer_Protection: Indica se l'articolo mostra il blocco
        # "Commissione per la Protezione acquisti". Selettore: data-testid="item-service-fee-title"
        "Has_Buyer_Protection": None,

        # Has_Item_Verification: Indica se l'articolo mostra il blocco "Verifica dell'articolo"
        # (servizio a pagamento). Selettore: data-testid="item-offline-verification-block-title"
        "Has_Item_Verification": None,

        # ── Venditore ──────────────────────────────────────────────────────
        "Seller_User":          None,
        "Seller_Location":      None,
        "Last_Seen":            None,
        "Seller_Rating":        None,
        "Seller_Reviews_Count": None,
        "Seller_distintivi":    None,

        # Seller_Is_Pro: Vinted distingue venditori privati da professionisti con un badge apposito.
        # Utile per segmentare l'analisi di mercato (pro vs. privati).
        "Seller_Is_Pro": None,

        # ── Analisi di rete ────────────────────────────────────────────────
        "Other_Items_Previewed_URLs": None,
    }

    accept_cookies(driver)
    scroll_to_bottom(driver, max_scrolls=10)

    item_data["Current_Time"] = datetime.now().strftime("%Y%m%d_%H-%M")

    # ── TITOLO ───────────────────────────────────────────
    item_data["Title"] = safe_find_text(driver, xpath="//h1", split_last=False)

    # ── PREZZO BASE ──────────────────────────────────────
    # Il testo del nodo è "2,00\xa0€": extract_number gestisce il formato europeo.
    try:
        price_elem = driver.find_element(By.CSS_SELECTOR, "[data-testid='item-price']")
        item_data["Price"] = extract_number(price_elem.text)
    except NoSuchElementException:
        pass

    # ── ATTRIBUTI PRODOTTO ────────────────────────────────
    # Usiamo get_attr_value (con itemprop) invece di safe_find_text perché ogni
    # container contiene un <button> figlio il cui testo sporcherebbe il risultato
    # di .text() di Selenium (es. "43Informazioni sulla taglia").
    item_data["Size"]      = get_attr_value(driver, "item-attributes-size",     "size")
    item_data["Condition"] = get_attr_value(driver, "item-attributes-status",   "status")
    item_data["Color"]     = get_attr_value(driver, "item-attributes-color",    "color")
    item_data["Material"]  = get_attr_value(driver, "item-attributes-material", "material")

    # Brand: il container NON ha data-testid proprio, ma il link del brand contiene
    # sempre itemprop="url" e uno <span itemprop="name"> con il nome pulito.
    item_data["Brand"] = safe_find_text(
        driver,
        css_selector="a[href*='/brand/'] span[itemprop='name']",
        split_last=False
    )

    # ── DATA CARICAMENTO ──────────────────────────────────
    # Testid corretto: "item-attributes-upload_date" (non "created_at").
    # Il valore è testo relativo: "17 ore fa".
    item_data["Upload_Date_Raw"] = get_attr_value(
        driver, "item-attributes-upload_date", "upload_date"
    )

    # ── FAVORITES ─────────────────────────────────────────
    fav_raw = safe_find_text(
        driver,
        css_selector="[data-testid='item-wishlist-button'] span",
        split_last=False
    )
    if not fav_raw:
        fav_raw = safe_find_text(
            driver,
            xpath="//*[contains(@aria-label,'preferit')]",
            split_last=False
        )
    item_data["Favorites_Count"] = extract_number(fav_raw)

    # ── CATEGORIA ─────────────────────────────────────────
    # Il breadcrumb Vinted IT è una <ul class="breadcrumbs">.
    # Struttura: Casa > Uomo > Scarpe > Scarpe da ginnastica > Nike Scarpe da ginnastica
    # L'ultimo elemento combina brand + categoria: va scartato.
    # Il penultimo contiene la categoria pura ("Scarpe da ginnastica").
    try:
        crumb_spans = driver.find_elements(
            By.CSS_SELECTOR, "ul.breadcrumbs li span[itemprop='title']"
        )
        if len(crumb_spans) >= 2:
            item_data["Category"] = crumb_spans[-2].text.strip() or None
        elif len(crumb_spans) == 1:
            item_data["Category"] = crumb_spans[0].text.strip() or None
    except Exception:
        pass

    # ── DESCRIZIONE ───────────────────────────────────────
    desc = safe_find_text(
        driver,
        css_selector="[data-testid='item-description']",
        split_last=False
    )
    if not desc:
        desc = safe_find_text(
            driver, xpath="//*[@itemprop='description']", split_last=False
        )
    item_data["Description"] = desc.strip() if desc else None

    # ── SPEDIZIONE ────────────────────────────────────────
    # Raccoglie tutto il testo dai nodi con data-testid contenente "shipping"/"shipment",
    # estrae i prezzi in formato europeo e salva il minimo come Shipping_Cost.
    try:
        ship_elems = driver.find_elements(
            By.XPATH,
            "//*[contains(@data-testid,'shipping') or contains(@data-testid,'shipment')]"
        )
        all_ship_text = " ".join(e.text for e in ship_elems if e.text.strip())

        if not all_ship_text:
            try:
                ship_section = driver.find_element(
                    By.XPATH,
                    "//section[.//*[contains("
                    "translate(.,'SPEDIZIONE','spedizione'),'spedizione')]]"
                )
                all_ship_text = ship_section.text
            except NoSuchElementException:
                pass

        if all_ship_text:
            price_matches = re.findall(r'\d{1,3}(?:\.\d{3})*,\d{2}', all_ship_text)
            if price_matches:
                prices_float = [
                    float(p.replace('.', '').replace(',', '.')) for p in price_matches
                ]
                min_price = price_matches[prices_float.index(min(prices_float))]
                item_data["Shipping_Cost"]          = min_price.replace(',', '.')
                item_data["Shipping_Options_Count"] = len(set(price_matches))
            elif re.search(r'grat(is|uita)', all_ship_text, re.IGNORECASE):
                item_data["Shipping_Cost"]          = "0.00"
                item_data["Shipping_Options_Count"] = 1
    except Exception:
        pass

    # ── BADGE "COMMISSIONE PER LA PROTEZIONE ACQUISTI" ───
    # FIX #2a — Has_Buyer_Protection
    # Struttura HTML: <h2 data-testid="item-service-fee-title">Commissione per la Protezione acquisti</h2>
    # Il blocco è quasi sempre presente, ma lo verifichiamo esplicitamente per coerenza.
    try:
        driver.find_element(By.CSS_SELECTOR, "[data-testid='item-service-fee-title']")
        item_data["Has_Buyer_Protection"] = True
    except NoSuchElementException:
        item_data["Has_Buyer_Protection"] = False

    # ── BADGE "VERIFICA DELL'ARTICOLO" ───────────────────
    # FIX #2b — Has_Item_Verification
    # Struttura HTML: <h2 data-testid="item-offline-verification-block-title">Verifica dell'articolo</h2>
    # Presente solo per categorie/articoli idonei alla verifica fisica da parte di Vinted.
    try:
        driver.find_element(
            By.CSS_SELECTOR, "[data-testid='item-offline-verification-block-title']"
        )
        item_data["Has_Item_Verification"] = True
    except NoSuchElementException:
        item_data["Has_Item_Verification"] = False

    # ── DATI VENDITORE ────────────────────────────────────
    item_data["Seller_User"] = safe_find_text(
        driver, css_selector="[data-testid='profile-username']", split_last=False
    )
    item_data["Seller_Location"] = safe_find_text(
        driver, css_selector="[data-testid='seller-location']", split_last=True
    )
    item_data["Last_Seen"] = safe_find_text(
        driver, css_selector="[data-testid='seller-last-logged-in']", split_last=True
    )

    # ── RATING VENDITORE ──────────────────────────────────
    # Cerchiamo l'elemento con il gruppo di stelle e leggiamo l'aria-label.
    try:
        rating_elem = driver.find_element(
            By.CSS_SELECTOR, "[role='group'][aria-label*='valutazione']"
        )
        rating_raw = rating_elem.get_attribute("aria-label")
    except (NoSuchElementException, StaleElementReferenceException):
        rating_raw = None
    item_data["Seller_Rating"] = extract_decimal(rating_raw)

    # ── RECENSIONI VENDITORE ──────────────────────────────
    reviews_raw = safe_find_text(
        driver,
        css_selector=".web_ui__Rating__label span",
        split_last=False
    )
    if not reviews_raw:
        reviews_raw = safe_find_text(
            driver,
            xpath="//div[contains(@class,'Rating__rating')]"
                  "//div[contains(@class,'label')]//span",
            split_last=False
        )
    item_data["Seller_Reviews_Count"] = extract_number(reviews_raw)

    # ── VENDITORE PRO ─────────────────────────────────────
    # FIX NUOVA VARIABILE — Seller_Is_Pro
    # I venditori professionisti mostrano un'etichetta dedicata nella sezione profilo.
    # Utile per separare listing privati da listing commerciali nell'analisi.
    try:
        driver.find_element(
            By.XPATH,
            "//*[contains(@data-testid,'pro-badge') "
            "or contains(text(),'Venditore professionista') "
            "or contains(text(),'Professional seller')]"
        )
        item_data["Seller_Is_Pro"] = True
    except NoSuchElementException:
        item_data["Seller_Is_Pro"] = False

    # ── SELLER BADGE (DISTINZIONI) ────────────────────────
    badge_candidates = [
        "//*[contains(@data-testid,'badge')]",
        "//*[contains(text(),'Annunci frequenti')]",
        "//*[contains(text(),'Venditore affidabile')]",
        "//*[contains(text(),'Professionista')]",
        "//*[contains(text(),'Super venditore')]",
        "//*[contains(text(),'Spedizione rapida')]",
    ]
    item_data["Seller_distintivi"] = []
    for xp in badge_candidates:
        badge = safe_find_text(driver, xpath=xp, split_last=False)
        if badge and len(badge) < 60:
            item_data["Seller_distintivi"].append(badge)
            break  # raccoglie solo il primo match; rimuovere break per raccoglierli tutti

    # ── ALTRI ARTICOLI DEL VENDITORE (per analisi di rete) ─
    try:
        preview_links = driver.find_elements(
            By.XPATH,
            "//section[.//h2[contains(text(),'utente') or contains(text(),'membro') "
            "or contains(text(),'venditore')]]//a[contains(@href,'/items/')]"
        )
        unique = {
            lnk.get_attribute("href").split("?")[0]
            for lnk in preview_links
            if lnk.get_attribute("href")
            and "/items/" in lnk.get_attribute("href")
            and lnk.get_attribute("href").split("?")[0] != url
        }
        if unique:
            item_data["Other_Items_Previewed_URLs"] = "|".join(sorted(unique))
    except Exception:
        pass

    return item_data

# ──────────────────────────────────────────────
# MAIN
# ──────────────────────────────────────────────

def main():
    print("=" * 50)
    print("  VINTED SCRAPER — ML Edition")
    print("=" * 50)

    query = input("\nCosa vuoi cercare (es: sneakers nike): ").strip()

    print("\nQuanti elementi vuoi raccogliere?")
    print("  1 → 10  (test rapido)")
    print("  2 → 200")
    print("  3 → 500")
    print("  4 → 3000")
    print("  5 → Tutti quelli disponibili (lento)")
    scelta = input("Scelta (inserisci un numero tra 1 e 5): ").strip()

    limit = {"1": 10, "2": 200, "3": 500, "4": 3000, "5": 0}.get(scelta, 10)

    start = time.time()
    driver = init_driver()

    try:
        # ── Fase 1: raccolta link + flag boost ───────────────
        print(f"\n[1/2] Raccolta link per '{query}'...")
        # get_item_links restituisce ora una lista di tuple (url, is_boosted)
        item_links = get_item_links(driver, query, limit)
        if not item_links:
            print("Nessun link trovato. Controlla la query o la connessione.")
            return

        # ── Fase 2: scraping dettagliato ─────────────────────
        print(f"\n[2/2] Scraping di {len(item_links)} articoli...")
        dataset, errors = [], []

        for i, (link, is_boosted) in enumerate(item_links, 1):
            print(f"  [{i:3d}/{len(item_links)}] {link}  (boost={is_boosted})")
            try:
                # Passiamo is_boosted come parametro così viene salvato nel dizionario
                dataset.append(scrape_item_details(driver, link, is_boosted=is_boosted))
            except Exception as e:
                print(f"    Errore: {e}")
                errors.append({"URL": link, "Error": str(e)})

            # ── SALVATAGGIO INTERMEDIO ogni 100 prodotti ──────────
            if len(dataset) % 50 == 0 and len(dataset) > 0:
                timestamp = datetime.now().strftime("%Y%m%d_%H-%M")
                checkpoint_file = f"vinted_{query.replace(' ', '_')}_checkpoint_{timestamp}.csv"
                df_checkpoint = pd.DataFrame(dataset)
                df_checkpoint.to_csv(checkpoint_file, index=False, encoding="utf-8-sig")
                print(f"\n  [CHECKPOINT] {len(dataset)} prodotti salvati → {checkpoint_file}\n")

            if i % 10 == 0:
                pause = random.uniform(8, 15)
                print(f"  Pausa anti-ban: {pause:.0f}s...")
                time.sleep(pause)

        # ── Fase 3: pulizia e salvataggio ────────────────────
        df = pd.DataFrame(dataset)

        # Colonne da convertire in numerici (gli altri restano stringhe/bool)
        numeric_cols = [
            "Price", "Favorites_Count",
            "Seller_Rating", "Seller_Reviews_Count",
            "Shipping_Cost", "Shipping_Options_Count",
        ]
        for col in numeric_cols:
            if col in df.columns:
                df[col] = pd.to_numeric(df[col], errors="coerce")

        timestamp = datetime.now().strftime("%Y%m%d_%H-%M")
        nome_file = f"vinted_{query.replace(' ', '_')}_{timestamp}.csv"

        df.to_csv(nome_file, index=False, encoding="utf-8-sig")
        print(f"\nSalvato: {nome_file}  ({len(df)} righe, {len(df.columns)} colonne)")

        if errors:
            err_file = f"vinted_{query.replace(' ', '_')}_errors.csv"
            pd.DataFrame(errors).to_csv(err_file, index=False)
            print(f"  {len(errors)} errori → {err_file}")

        # ── Riepilogo completezza ─────────────────────────────
        print("\n── Completezza colonne ──")
        for col in df.columns:
            filled = df[col].notna().sum()
            pct    = filled / len(df) * 100 if len(df) > 0 else 0
            print(f"  {col:<32} {filled:>4}/{len(df)} ({pct:5.1f}%)")

    finally:
        driver.quit()

    end = time.time()
    print(f"\nTempo totale: {(end - start) / 60:.1f} minuti")


if __name__ == "__main__":
    main()
