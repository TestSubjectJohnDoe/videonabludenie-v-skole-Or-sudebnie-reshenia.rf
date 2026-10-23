import os
import time
from pathlib import Path
from selenium import webdriver
from selenium.webdriver.edge.service import Service
from selenium.webdriver.edge.options import Options
from selenium.common.exceptions import WebDriverException

# -------------------- НАСТРОЙКИ --------------------
BASE_DIR = r"C:\Users\rewhe\Desktop\pravosudie\vcode"   # папка с папками округов
DRIVER_PATH = "./msedgedriver.exe"                      # путь к драйверу Edge
RESTART_EVERY = 200                                      # перезапуск браузера каждые N файлов
# ---------------------------------------------------

def create_driver():
    """Создать headless Edge WebDriver."""
    options = Options()
    options.add_argument("--disable-extensions")
    options.add_argument("--disable-gpu")
    options.add_argument("--no-sandbox")
    options.add_argument("--headless")
    options.add_argument("--window-size=1920,1080")
    # Отключаем изображения для скорости
    prefs = {"profile.managed_default_content_settings.images": 2}
    options.add_experimental_option("prefs", prefs)
    service = Service(DRIVER_PATH)
    return webdriver.Edge(service=service, options=options)

def html_to_pdf(driver, html_path, pdf_path):
    """Конвертировать один HTML в PDF через Print-to-PDF."""
    driver.get("file:///" + html_path.replace("\\", "/"))
    # Небольшая пауза на рендеринг (можно увеличить, если страницы сложные)
    time.sleep(0.5)
    try:
        # Получаем PDF как base64
        pdf_data = driver.execute_cdp_cmd("Page.printToPDF", {
            "printBackground": True,
            "paperWidth": 8.27,   # A4
            "paperHeight": 11.69,
            "marginTop": 0.4,
            "marginBottom": 0.4,
            "marginLeft": 0.4,
            "marginRight": 0.4,
        })["data"]
        import base64
        with open(pdf_path, "wb") as f:
            f.write(base64.b64decode(pdf_data))
        return True
    except Exception as e:
        print(f"    Ошибка при создании PDF: {e}")
        return False

# -------------------------------------------------------
def main():
    # Сбор всех HTML файлов
    html_files = []
    for root, dirs, files in os.walk(BASE_DIR):
        if "_Videonablyudenie_v_skole" in root:
            for f in files:
                if f.endswith(".html"):
                    html_files.append(os.path.join(root, f))

    total = len(html_files)
    print(f"Найдено HTML-файлов: {total}")

    if total == 0:
        return

    # Определяем, какие уже имеют PDF
    to_convert = []
    for path in html_files:
        pdf_path = os.path.splitext(path)[0] + ".pdf"
        if not os.path.exists(pdf_path):
            to_convert.append((path, pdf_path))
        else:
            pass  # уже есть

    remaining = len(to_convert)
    print(f"Уже конвертировано: {total - remaining}, осталось: {remaining}")

    if remaining == 0:
        print("Все файлы уже конвертированы.")
        return

    driver = create_driver()
    processed = 0
    success = 0
    errors = 0

    try:
        for idx, (html_path, pdf_path) in enumerate(to_convert, 1):
            print(f"[{idx}/{remaining}] {html_path} -> {pdf_path}")
            try:
                if html_to_pdf(driver, html_path, pdf_path):
                    success += 1
                else:
                    errors += 1
            except WebDriverException as e:
                print(f"    Критическая ошибка драйвера: {e}. Перезапуск браузера...")
                driver.quit()
                time.sleep(2)
                driver = create_driver()
                # Повторная попытка для текущего файла
                try:
                    if html_to_pdf(driver, html_path, pdf_path):
                        success += 1
                    else:
                        errors += 1
                except Exception:
                    errors += 1

            processed += 1
            if processed % RESTART_EVERY == 0:
                print(f"    ... перезапуск браузера после {processed} файлов")
                driver.quit()
                time.sleep(1)
                driver = create_driver()

    finally:
        driver.quit()

    print(f"\nГотово! Успешно: {success}, ошибок: {errors}")

if __name__ == "__main__":
    main()