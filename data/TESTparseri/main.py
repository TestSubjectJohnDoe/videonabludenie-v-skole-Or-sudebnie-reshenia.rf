import sys, os, re, json, time
from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.edge.service import Service
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from selenium.common.exceptions import TimeoutException, WebDriverException
from bs4 import BeautifulSoup

# --------------------- НАСТРОЙКИ ---------------------
BASE_URL = "https://xn--90afdbaav0bd1afy6eub5d.xn--p1ai"
SEARCH_QUERY = "видеонаблюдение в школе"

# Словарь: ключ → англ. название папки, значение → русский текст в дереве (label)
DISTRICTS = {
    "Tsentralny_FO"            : "Центральный ФО",
    "Severo_Zapadny_FO"       : "Северо-Западный ФО",
    "Yuzhny_FO"               : "Южный ФО",
    "Privolzhsky_FO"          : "Приволжский ФО",
    "Uralsky_FO"              : "Уральский ФО",
    "Sibirsky_FO"             : "Сибирский ФО",
    "Dalnevostochny_FO"       : "Дальневосточный ФО",
    "Severo_Kavkazsky_FO"     : "Северо-Кавказский ФО",
    "Krymsky_FO"              : "Крымский ФО",
}

DELAY_BETWEEN_PAGES = 2.0
DELAY_BETWEEN_DOWNLOADS = 2.0
MAX_RETRIES = 3
RETRY_DELAY = 3
PAUSE_AFTER_CONSECUTIVE_FAILS = 15
# ------------------------------------------------------

def safe_get(url, retries=MAX_RETRIES):
    for attempt in range(1, retries + 1):
        try:
            driver.get(url)
            return True
        except (TimeoutException, WebDriverException) as e:
            print(f"    Попытка {attempt} не удалась: {e}")
            if attempt < retries:
                time.sleep(RETRY_DELAY)
    return False

def select_district(driver, label_text):
    """Раскрываем узел ФО и ставим все галочки внутри него."""
    # Находим все элементы дерева
    tree_lines = driver.find_elements(By.CSS_SELECTOR, "div.aciTreeLine")
    target_line = None
    for line in tree_lines:
        try:
            text_el = line.find_element(By.CSS_SELECTOR, ".aciTreeText")
            if text_el.text.strip() == label_text:
                target_line = line
                break
        except:
            continue
    if not target_line:
        raise Exception(f"Не найден узел дерева с текстом '{label_text}'")

    # Кликаем по кнопке раскрытия (+, если есть) – span.aciTreePush
    try:
        push_btn = target_line.find_element(By.CSS_SELECTOR, ".aciTreePush span")
        driver.execute_script("arguments[0].click();", push_btn)
        time.sleep(0.8)   # ждём подгрузки дочерних узлов
    except:
        pass   # узел уже раскрыт или не имеет кнопки

    # Теперь ищем все чекбоксы внутри этого узла (включая дочерние)
    # Для надёжности прокликиваем все чекбоксы в этом поддереве
    checks = target_line.find_elements(By.CSS_SELECTOR, ".aciTreeCheck")
    for cb in checks:
        try:
            # Проверяем, отмечен ли уже (по классу aciTreeChecked)
            if "aciTreeChecked" not in cb.get_attribute("class"):
                driver.execute_script("arguments[0].click();", cb)
                time.sleep(0.2)
        except:
            pass
    print(f"Выбраны все суды/регионы для: {label_text}")

# ================== ГЛАВНЫЙ БЛОК ==================
if __name__ == "__main__":
    if len(sys.argv) != 2:
        print("Использование: python district_parser.py <КЛЮЧ_ОКРУГА>")
        print("Доступные ключи:")
        for k in DISTRICTS.keys():
            print(f"  {k}")
        sys.exit(1)

    selected_key = sys.argv[1]
    if selected_key not in DISTRICTS:
        print(f"Неизвестный округ: {selected_key}")
        sys.exit(1)

    selected_label = DISTRICTS[selected_key]
    output_dir = f"{selected_key}_Videonablyudenie_v_skole"
    case_ids_file = f"case_ids_{selected_key}.json"
    failed_file = f"failed_ids_{selected_key}.txt"

    os.makedirs(output_dir, exist_ok=True)

    # ----- Инициализация драйвера -----
    options = webdriver.EdgeOptions()
    options.add_argument("--disable-extensions")
    options.add_argument("--disable-gpu")
    options.add_argument("--no-sandbox")
    options.add_argument("--window-size=1920,1080")
    prefs = {
        "profile.managed_default_content_settings.images": 2,
        "profile.managed_default_content_settings.stylesheets": 2,
        "profile.managed_default_content_settings.javascript": 1,
    }
    options.add_experimental_option("prefs", prefs)
    options.add_argument("--disable-features=msSmartScreenProtection")

    service = Service(executable_path="./msedgedriver.exe")
    driver = webdriver.Edge(service=service, options=options)
    driver.set_page_load_timeout(30)
    driver.implicitly_wait(10)

    try:
        # ----- Загрузка/сбор ID -----
        if os.path.exists(case_ids_file):
            with open(case_ids_file, "r") as f:
                all_ids = json.load(f)
            print(f"Загружено {len(all_ids)} ID из {case_ids_file}")
        else:
            print("Файл с ID не найден, начинаю поиск с выбором округа...")
            safe_get(BASE_URL)
            time.sleep(2)

            # --- Выбор округа в дереве ---
            select_district(driver, selected_label)

            # --- Заполнение поискового запроса ---
            search_input = driver.find_element(By.NAME, "simpleSearch[content]")
            search_input.clear()
            search_input.send_keys(SEARCH_QUERY)
            print("Введён запрос")

            # --- Отправка формы ---
            submit_btn = driver.find_element(By.CSS_SELECTOR, "button[type='submit'], input[type='submit']")
            submit_btn.click()
            print("Поиск отправлен, ждём результаты...")
            time.sleep(3)

            # --- Сбор ID со всех страниц ---
            all_ids = []
            page_num = 1
            while True:
                print(f"\nОбработка страницы {page_num}")
                try:
                    WebDriverWait(driver, 10).until(
                        EC.presence_of_element_located((By.CLASS_NAME, "case_quantity"))
                    )
                except:
                    pass
                time.sleep(1)
                soup = BeautifulSoup(driver.page_source, "lxml")
                links = soup.select("table.table-bordered a[href^='/']")
                new = []
                for a in links:
                    href = a.get("href")
                    if href and re.match(r"/\d+$", href):
                        cid = href.strip("/")
                        if cid not in all_ids:
                            all_ids.append(cid)
                            new.append(cid)
                print(f"  Найдено новых ID: {len(new)} (всего: {len(all_ids)})")

                # Следующая страница
                try:
                    next_link = driver.find_element(By.XPATH, "//ul[contains(@class, 'pagination')]//a[contains(text(), '»')]")
                    next_url = next_link.get_attribute("href")
                    if next_url:
                        safe_get(next_url)
                        page_num += 1
                        time.sleep(DELAY_BETWEEN_PAGES)
                    else:
                        break
                except:
                    print("  Пагинация завершена.")
                    break

            # Сохраняем ID в файл
            with open(case_ids_file, "w") as f:
                json.dump(all_ids, f)
            print(f"Собрано {len(all_ids)} ID, сохранено в {case_ids_file}")

        if not all_ids:
            print("Нет ID для обработки. Выход.")
            driver.quit()
            exit()

        # ----- Определяем, что уже скачано -----
        existing = set(os.listdir(output_dir))
        remaining = [cid for cid in all_ids if f"{cid}.html" not in existing]
        print(f"Уже скачано: {len(all_ids) - len(remaining)}; осталось: {len(remaining)}")

        if not remaining:
            print("Всё уже загружено!")
            driver.quit()
            exit()

        # ----- Загрузка недостающих -----
        print("Начинаю загрузку решений...")
        downloaded = 0
        fails_in_row = 0
        failed_ids = []

        for i, cid in enumerate(remaining, 1):
            url = f"{BASE_URL}/{cid}"
            print(f"[{i}/{len(remaining)}] {url}")
            if safe_get(url):
                try:
                    WebDriverWait(driver, 5).until(
                        EC.presence_of_element_located((By.CSS_SELECTOR, "table.table-bordered"))
                    )
                except:
                    pass
                path = os.path.join(output_dir, f"{cid}.html")
                with open(path, "w", encoding="utf-8") as f:
                    f.write(driver.page_source)
                downloaded += 1
                fails_in_row = 0
                print("    Сохранён")
            else:
                print(f"    *** Ошибка загрузки после {MAX_RETRIES} попыток ***")
                failed_ids.append(cid)
                fails_in_row += 1
                if fails_in_row >= 3:
                    print(f"    Слишком много ошибок подряд, пауза {PAUSE_AFTER_CONSECUTIVE_FAILS} сек...")
                    time.sleep(PAUSE_AFTER_CONSECUTIVE_FAILS)
                    fails_in_row = 0
            time.sleep(DELAY_BETWEEN_DOWNLOADS)

        # Сохранение списка неудачных ID
        if failed_ids:
            with open(failed_file, "w") as f:
                for fid in failed_ids:
                    f.write(fid + "\n")
            print(f"Неудачные ID сохранены в {failed_file} ({len(failed_ids)} шт.)")

        print(f"Готово. За сессию скачано: {downloaded} файлов.")

    except Exception as e:
        print(f"Критическая ошибка: {e}")
    finally:
        driver.quit()