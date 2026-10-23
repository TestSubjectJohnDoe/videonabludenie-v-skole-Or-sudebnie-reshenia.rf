# ============================================================
# repair_downloads.R
# Автоматическая проверка и дозагрузка "битых" или отсутствующих файлов
# МАКСИМАЛЬНО ОПТИМИЗИРОВАННАЯ ВЕРСИЯ (для работы с очень большими файлами)
# ============================================================

suppressPackageStartupMessages({
  library(chromote)
  library(stringr)
  library(jsonlite)
})

# --------------------- НАСТРОЙКИ ---------------------
BASE_URL <- "https://xn--90afdbaav0bd1afy6eub5d.xn--p1ai"
BASE_DIR <- "C:/Users/rewhe/Desktop/videonabludenie"

MIN_FILE_SIZE <- 25000 

ERROR_PATTERNS <- c(
  "can't reach this page", "не удается получить доступ к сайту", 
  "hmmm", "err_connection", "err_name_not_resolved", 
  "502 bad gateway", "504 gateway timeout", "503 service unavailable",
  "404 not found", "403 forbidden"
)

# ОПТИМИЗАЦИЯ 1: Компилируем регулярное выражение ОДИН РАЗ при запуске.
# Это избавляет от необходимости склеивать строки и запускать цикл внутри функции проверки.
ERROR_REGEX <- paste(ERROR_PATTERNS, collapse = "|")

# Проверка пути к браузеру
edge_path <- "C:/Program Files/Microsoft/Edge/Application/msedge.exe"
if (!file.exists(edge_path)) {
  edge_path <- "C:/Program Files (x86)/Microsoft/Edge/Application/msedge.exe"
}
Sys.setenv(CHROMOTE_CHROME = edge_path)

# --------------------- ВСПОМОГАТЕЛЬНЫЕ ФУНКЦИИ ---------------------

# Ожидание реальной загрузки контента
wait_for_real_content <- function(session, max_seconds = 50) { # было 10 или 8
  for (i in 1:max_seconds) {
    res <- tryCatch(
      session$Runtime$evaluate("document.documentElement.outerHTML.length", returnByValue = TRUE),
      error = function(e) NULL
    )
    if (!is.null(res) && !is.null(res$result$value) && res$result$value > 5000) {
      return(TRUE)
    }
    Sys.sleep(1)
  }
  return(FALSE)
}

# Безопасная навигация
safe_get <- function(session, url, retries = 3) {
  for (attempt in seq_len(retries)) {
    ok <- tryCatch({
      session$Page$navigate(url)
      Sys.sleep(4) # Больше времени на отклик
      TRUE
    }, error = function(e) FALSE)
    if (ok) return(TRUE)
    if (attempt < retries) Sys.sleep(3)
  }
  FALSE
}
is_bad_file <- function(file_path) {
  f_size <- file.info(file_path)$size
  if (is.na(f_size) || f_size < MIN_FILE_SIZE) return(TRUE)
  
  # Читаем ТОЛЬКО первые 20 строк для быстрой проверки (оптимизация для больших файлов)
  lines <- tryCatch(
    readLines(file_path, warn = FALSE, encoding = "UTF-8", n = 20),
    error = function(e) character(0)
  )
  if (length(lines) == 0) return(TRUE)
  
  header_text <- tolower(paste(lines, collapse = " "))
  
  # 1. ПРОВЕРКА НА СТРАНИЦУ ОШИБКИ БРАУЗЕРА (Chromium/Chrome/Edge)
  # Эти страницы могут весить сотни КБ, но они являются мусором
  browser_error_patterns <- c(
    "err_", "chromium", "no internet", "name not resolved", 
    "connection refused", "connection reset", "net::err"
  )
  for (patt in browser_error_patterns) {
    if (str_detect(header_text, patt)) return(TRUE)
  }
  
  # 2. ПРОВЕРКА НА КАПЧУ / БЛОКИРОВКУ / АНТИБОТ
  block_patterns <- c(
    "cloudflare", "captcha", "проверка браузера", "доступ ограничен",
    "access denied", "just a moment", "ddos protection"
  )
  for (patt in block_patterns) {
    if (str_detect(header_text, patt)) return(TRUE)
  }
  
  # 3. ПРОВЕРКА НА ПУСТУЮ ОБОЛОЧКУ САЙТА
  # Если в первых 20 строках нет тега <body>, значит HTML битый или незагруженный
  if (!str_detect(header_text, "<body")) return(TRUE)
  
  # 4. ПРОВЕРКА НА СЛИШКОМ МАЛЕНЬКИЙ ФАЙЛ
  # Реальное решение суда с таблицами и текстом всегда > 25000 символов
  if (f_size < 25000) return(TRUE)
  
  return(FALSE)
}

# --------------------- ГЛАВНАЯ ЛОГИКА ---------------------
main_repair <- function() {
  cat("=== Запуск проверки и восстановления загрузок ===\n")
  
  json_files <- list.files(BASE_DIR, pattern = "^case_ids_.*\\.json$", full.names = TRUE)
  
  if (length(json_files) == 0) {
    cat("Файлы case_ids_*.json не найдены в", BASE_DIR, "\n")
    return(invisible())
  }
  
  cat(sprintf("Найдено %d файлов со списками ID.\n", length(json_files)))
  
  # ОПТИМИЗАЦИЯ 3: Используем списки для накопления результатов, 
  # чтобы избежать квадратичной деградации скорости при использовании c()
  ids_to_redownload <- list()
  districts_for_ids <- list()
  
  for (json_file in json_files) {
    district <- str_extract(basename(json_file), "(?<=case_ids_).*?(?=\\.json)")
    target_folder <- file.path(BASE_DIR, paste0(district, "_Videonablyudenie_v_skole"))
    
    cat(sprintf("\n[Проверка] Округ: %s\n", district))
    
    all_ids <- fromJSON(json_file)
    if (is.list(all_ids)) all_ids <- unlist(all_ids, use.names = FALSE)
    
    n_ids <- length(all_ids)
    cat(sprintf("  Всего ID в файле: %d. Проверка...\n", n_ids))
    
    # Векторизованная проверка путей
    file_paths <- file.path(target_folder, paste0(all_ids, ".html"))
    exists_mask <- file.exists(file_paths)
    is_bad <- logical(n_ids)
    
    for (j in seq_len(n_ids)) {
      if (!exists_mask[j]) {
        is_bad[j] <- TRUE
      } else {
        is_bad[j] <- is_bad_file(file_paths[j])
        if (is_bad[j]) {
          file.remove(file_paths[j]) # Удаляем битый файл сразу
        }
      }
      
      # Индикатор прогресса каждые 500 файлов
      if (j %% 500 == 0) {
        cat(sprintf("    Проверено: %d / %d\n", j, n_ids))
      }
    }
    
    missing_count <- sum(!exists_mask)
    too_small_count <- sum(is_bad & exists_mask)
    
    cat(sprintf("  ИТОГО: Проверено: %d | Отсутствует: %d | Битых (удалено): %d\n", 
                n_ids, missing_count, too_small_count))
    
    # Безопасное добавление в список без деградации производительности
    if (any(is_bad)) {
      ids_to_redownload <- c(ids_to_redownload, list(all_ids[is_bad]))
      districts_for_ids <- c(districts_for_ids, list(rep(district, sum(is_bad))))
    }
  }
  
  # Распаковываем списки в плоские векторы только один раз в конце
  ids_to_redownload <- unlist(ids_to_redownload, use.names = FALSE)
  districts_for_ids <- unlist(districts_for_ids, use.names = FALSE)
  
  total_to_fix <- length(ids_to_redownload)
  cat(sprintf("\n=== ИТОГ ПРОВЕРКИ: Найдено %d файлов для перезагрузки ===\n", total_to_fix))
  
  if (total_to_fix == 0) {
    cat("Отлично! Все файлы загружены корректно и имеют достаточный размер.\n")
    return(invisible())
  }
  
  # 3. Процесс перезагрузки
  cat("\nЗапуск браузера для дозагрузки...\n")
  session <- ChromoteSession$new()
  on.exit(session$close(), add = TRUE)
  
  success_count <- 0
  failed_count <- 0
  
  # Предвыделяем память для лога ошибок
  failed_log <- character(total_to_fix)
  
  for (i in seq_len(total_to_fix)) {
    cid <- ids_to_redownload[i]
    district <- districts_for_ids[i]
    url <- paste0(BASE_URL, "/", cid)
    target_folder <- file.path(BASE_DIR, paste0(district, "_Videonablyudenie_v_skole"))
    file_path <- file.path(target_folder, paste0(cid, ".html"))
    
    dir.create(target_folder, showWarnings = FALSE, recursive = TRUE)
    
    cat(sprintf("[%d/%d] ID: %s (%s) ... ", i, total_to_fix, cid, district))
    
    if (safe_get(session, url)) {
      # явно указываем 20 секунд ожидания
      content_loaded <- wait_for_real_content(session, max_seconds = 50) 
      
      if (content_loaded) {
        res <- tryCatch(
          session$Runtime$evaluate("document.documentElement.outerHTML", returnByValue = TRUE),
          error = function(e) list(result = list(value = NULL))
        )
        html_content <- res$result$value
        
        if (!is.null(html_content)) {
          # Используем тот же оптимизированный подход с одним regex
          has_error <- grepl(ERROR_REGEX, html_content, ignore.case = TRUE)
          
          if (has_error) {
            cat("FAIL (Ошибка в контенте)\n")
            failed_count <- failed_count + 1
            failed_log[failed_count] <- cid
          } else if (nchar(html_content) >= MIN_FILE_SIZE) {
            writeLines(html_content, file_path, useBytes = TRUE)
            cat("OK (", nchar(html_content), "симв.)\n")
            success_count <- success_count + 1
          } else {
            cat("FAIL (Слишком короткий)\n")
            failed_count <- failed_count + 1
            failed_log[failed_count] <- cid
          }
        } else {
          cat("FAIL (Пустой ответ)\n")
          failed_count <- failed_count + 1
          failed_log[failed_count] <- cid
        }
      } else {
        cat("FAIL (Таймаут)\n")
        failed_count <- failed_count + 1
        failed_log[failed_count] <- cid
      }
    } else {
      cat("FAIL (Ошибка навигации)\n")
      failed_count <- failed_count + 1
      failed_log[failed_count] <- cid
    }
    
    Sys.sleep(1.5) # Пауза между запросами
  }
  
  cat("\n=== ОТЧЕТ О ВОССТАНОВЛЕНИИ ===\n")
  cat(sprintf("Успешно перезагружено: %d файлов\n", success_count))
  cat(sprintf("Не удалось загрузить: %d файлов\n", failed_count))
  
  if (failed_count > 0) {
    log_file <- file.path(BASE_DIR, "permanently_failed_ids.txt")
    writeLines(failed_log[1:failed_count], log_file)
    cat(sprintf("Список окончательно неудачных ID сохранен в: %s\n", log_file))
  }
  
  cat("Процесс завершен.\n")
}

main_repair()