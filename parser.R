# ============================================================
# universal_parser.R
# Аналог universal_parser.py на языке R
# Требуется: chromote, rvest, stringr, jsonlite, xml2, base64enc
# ============================================================

suppressPackageStartupMessages({
  library(chromote)
  library(rvest)
  library(stringr)
  library(jsonlite)
  library(xml2)
  library(base64enc)
})

# --------------------- НАСТРОЙКИ ---------------------
BASE_URL      <- "https://xn--90afdbaav0bd1afy6eub5d.xn--p1ai"
DEFAULT_QUERY <- "видеонаблюдение в школе"

# Автоматическая проверка пути к Edge (64-bit предпочтительнее)
edge_path <- "C:/Program Files/Microsoft/Edge/Application/msedge.exe"
if (!file.exists(edge_path)) {
  edge_path <- "C:/Program Files (x86)/Microsoft/Edge/Application/msedge.exe"
}
Sys.setenv(CHROMOTE_CHROME = edge_path)
cat(sprintf("Используется браузер: %s\n", Sys.getenv("CHROMOTE_CHROME")))

DISTRICTS <- list(
  "Tsentralny_FO"       = "Центральный ФО",
  "Severo_Zapadny_FO"   = "Северо-Западный ФО",
  "Yuzhny_FO"           = "Южный ФО",
  "Privolzhsky_FO"      = "Приволжский ФО",
  "Uralsky_FO"          = "Уральский ФО",
  "Sibirsky_FO"         = "Сибирский ФО",
  "Dalnevostochny_FO"   = "Дальневосточный ФО",
  "Severo_Kavkazsky_FO" = "Северо-Кавказский ФО",
  "Krymsky_FO"          = "Крымский ФО"
)

DELAY_BETWEEN_PAGES        <- 10.0
DELAY_BETWEEN_DOWNLOADS    <- 10.0
MAX_RETRIES                <- 3
RETRY_DELAY                <- 10
PAUSE_AFTER_CONSECUTIVE_FAILS <- 15

# --- Папка для сохранения результатов ---
BASE_DIR <- "C:/Users/rewhe/Desktop/videonabludenie"
dir.create(BASE_DIR, showWarnings = FALSE, recursive = TRUE)
cat(sprintf("DEBUG: BASE_DIR = %s\n", BASE_DIR))

# --------------------- ВСПОМОГАТЕЛЬНЫЕ ФУНКЦИИ ---------------------

# Безопасное выполнение JS в браузере (с защитой от ошибок)
eval_js <- function(session, js) {
  res <- tryCatch(
    session$Runtime$evaluate(js, returnByValue = TRUE),
    error = function(e) list(result = NULL)
  )
  if (!is.null(res$result) && !is.null(res$result$value)) {
    return(res$result$value)
  }
  return(NULL)
}

# Навигация с повторами и увеличенным таймаутом
safe_get <- function(session, url, retries = MAX_RETRIES) {
  for (attempt in seq_len(retries)) {
    ok <- tryCatch({
      # Увеличиваем таймаут до 60 секунд (60000 мс)
      session$Page$navigate(url, timeout = 60000)
      Sys.sleep(4) # Даем больше времени на первоначальную отрисовку DOM
      TRUE
    }, error = function(e) {
      cat(sprintf("    Попытка %d не удалась: %s\n", attempt, conditionMessage(e)))
      FALSE
    })
    if (ok) return(TRUE)
    if (attempt < retries) Sys.sleep(RETRY_DELAY)
  }
  FALSE
}

# 
# # Получить HTML текущей страницы
# get_html <- function(session) {
#   res <- session$Runtime$evaluate(
#     "document.documentElement.outerHTML",
#     returnByValue = TRUE
#   )
#   res$result$value
# }
# Ошибка при обработке округа Центральный ФО:
#   no applicable method for 'read_xml'
# applied to an object of class "NULL"

get_html <- function(session, retries = 5) {
  for (i in seq_len(retries)) {
    res <- tryCatch(
      session$Runtime$evaluate(
        "document.documentElement.outerHTML",
        returnByValue = TRUE
      ),
      error = function(e) NULL
    )
    
    # ЖЕСТКАЯ ПРОВЕРКА: гарантируем, что вернется строка, а не NULL
    if (!is.null(res) && 
        !is.null(res$result) && 
        !is.null(res$result$value) && 
        is.character(res$result$value) && 
        nzchar(res$result$value)) {
      return(res$result$value)
    }
    Sys.sleep(1)
  }
  stop("Не удалось получить HTML страницы после нескольких попыток (вернулся NULL или пустая строка)")
}

select_district <- function(session, label_text) {
  # Ждем полной загрузки главной страницы и прохождения антибот-проверки
  cat("Ожидание загрузки главной страницы и прохождения проверок...\n")
  Sys.sleep(5) # Даем 5 секунд на выполнение скрытых JS-скритов сайта
  
  # УВЕЛИЧЕННЫЙ ТАЙМАУТ: ждем до 60 секунд появления дерева судов
  cat("Ожидание загрузки дерева судов (до 60 секунд)...\n")
  tree_loaded <- FALSE
  for (i in 1:60) {
    # Проверяем наличие элементов дерева
    n_lines <- eval_js(session, "document.querySelectorAll('div.aciTreeLine').length")
    
    # Дополнительная проверка: ищем конкретный текст округа в дереве
    label_json <- jsonlite::toJSON(label_text, auto_unbox = TRUE)
    tree_has_label <- eval_js(session, sprintf("
      (function() {
        let lines = document.querySelectorAll('div.aciTreeLine');
        for (let line of lines) {
          let txt = line.querySelector('.aciTreeText');
          if (txt && txt.textContent.trim() === %s) {
            return true;
          }
        }
        return false;
      })()
    ", label_json))
    
    if (!is.null(n_lines) && n_lines > 0 && !is.null(tree_has_label) && tree_has_label == TRUE) {
      cat(sprintf("Дерево загружено (найдено %d элементов, округ найден). Попытка %d.\n", n_lines, i))
      tree_loaded <- TRUE
      break
    }
    
    if (i %% 10 == 0) {
      cat(sprintf("  ... ожидание %d секунд (найдено %d элементов дерева)\n", i, ifelse(is.null(n_lines), 0, n_lines)))
    }
    Sys.sleep(1)
  }
  
  if (!tree_loaded) {
    # Сохраняем HTML для диагностики
    debug_html <- get_html(session)
    writeLines(debug_html, file.path(BASE_DIR, "debug_tree_not_loaded.html"))
    stop(sprintf("Дерево судов не загрузилось за 60 секунд. Сохранен debug_tree_not_loaded.html для диагностики.\n",
                 "Возможные причины:\n",
                 "1. Сайт заблокировал автоматический доступ\n",
                 "2. Рекламные скрипты блокируют загрузку дерева\n",
                 "3. Структура дерева изменилась"))
  }
  
  # БЕЗОПАСНАЯ передача строки в JS через jsonlite
  label_json <- jsonlite::toJSON(label_text, auto_unbox = TRUE)
  
  js <- sprintf("
    (function() {
      let lines = document.querySelectorAll('div.aciTreeLine');
      for (let line of lines) {
        let txt = line.querySelector('.aciTreeText');
        if (txt && txt.textContent.trim() === %s) {
          let push = line.querySelector('.aciTreePush');
          if (push) { push.click(); }
          let check = line.querySelector('.aciTreeCheck');
          if (check && !check.classList.contains('aciTreeChecked')) {
            check.click();
          }
          return 'ok';
        }
      }
      return 'not_found';
    })()
  ", label_json)
  
  res <- eval_js(session, js)
  if (is.null(res) || res != "ok") {
    stop(sprintf("Не найден узел дерева с текстом '%s'.", label_text))
  }
  Sys.sleep(2)
  
  # Дополнительно кликаем все дочерние чекбоксы, которые могли появиться
  js2 <- "
    (function() {
      let checks = document.querySelectorAll('.aciTreeCheck');
      let n = 0;
      for (let c of checks) {
        if (!c.classList.contains('aciTreeChecked')) {
          c.click();
          n++;
        }
      }
      return n;
    })()
  "
  eval_js(session, js2)
  Sys.sleep(1)
  cat(sprintf("Выбраны все суды/регионы для: %s\n", label_text))
}

# 12/10/26 Обновили сайт! Больше нельзя просто нажать на нужную кнопку!
# select_district <- function(session, label_text) {
#   # Ждем полной загрузки главной страницы и прохождения антибот-проверки
#   cat("Ожидание загрузки главной страницы и прохождения проверок...\n")
#   Sys.sleep(5) # Даем 5 секунд на выполнение скрытых JS-скриптов сайта
#   
#   for (i in 1:30) {
#     # Проверяем наличие именно дерева судов, а не просто длины страницы
#     tree_ready <- eval_js(session, "document.querySelectorAll('div.aciTreeLine').length > 0")
#     if (!is.null(tree_ready) && tree_ready == TRUE) {
#       cat(sprintf("Дерево судов загружено (попытка %d).\n", i))
#       break
#     }
#     Sys.sleep(1)
#   }
#   
#   # Дополнительная диагностика: выводим заголовок страницы
#   page_title <- eval_js(session, "document.title")
#   cat(sprintf("Заголовок страницы: %s\n", ifelse(is.null(page_title), "NULL", page_title)))
#   
#   # Ждем появления элементов дерева в DOM (до 20 секунд)
#   cat("Ожидание загрузки дерева судов...\n")
#   for (i in 1:20) {
#     n_lines <- eval_js(session, "document.querySelectorAll('div.aciTreeLine').length")
#     if (!is.null(n_lines) && n_lines > 0) {
#       cat(sprintf("Дерево загружено (найдено %d элементов).\n", n_lines))
#       break
#     }
#     Sys.sleep(1)
#   }
#   
#   # Если дерево так и не появилось, выводим диагностику
#   if (is.null(n_lines) || n_lines == 0) {
#     # Сохраняем HTML для анализа
#     debug_html <- get_html(session)
#     writeLines(debug_html, file.path(BASE_DIR, "debug_page.html"))
#     cat("ДИАГНОСТИКА: Дерево судов не появилось. Сохранен debug_page.html\n")
#     cat("Проверьте файл debug_page.html в браузере.\n")
#     stop(sprintf("Не найден узел дерева с текстом '%s'. Дерево судов не загрузилось.", label_text))
#   }
#   
#   # БЕЗОПАСНАЯ передача строки в JS через jsonlite
#   label_json <- jsonlite::toJSON(label_text, auto_unbox = TRUE)
#   
#   js <- sprintf("
#     (function() {
#       let lines = document.querySelectorAll('div.aciTreeLine');
#       for (let line of lines) {
#         let txt = line.querySelector('.aciTreeText');
#         if (txt && txt.textContent.trim() === %s) {
#           let push = line.querySelector('.aciTreePush');
#           if (push) { push.click(); }
#           let check = line.querySelector('.aciTreeCheck');
#           if (check && !check.classList.contains('aciTreeChecked')) {
#             check.click();
#           }
#           return 'ok';
#         }
#       }
#       return 'not_found';
#     })()
#   ", label_json)
#   
#   res <- eval_js(session, js)
#   if (is.null(res) || res != "ok") {
#     stop(sprintf("Не найден узел дерева с текстом '%s'. Возможно, дерево еще не загрузилось.", label_text))
#   }
#   Sys.sleep(2)
#   
#   # Дополнительно кликаем все дочерние чекбоксы, которые могли появиться
#   js2 <- "
#     (function() {
#       let checks = document.querySelectorAll('.aciTreeCheck');
#       let n = 0;
#       for (let c of checks) {
#         if (!c.classList.contains('aciTreeChecked')) {
#           c.click();
#           n++;
#         }
#       }
#       return n;
#     })()
#   "
#   eval_js(session, js2)
#   Sys.sleep(1)
#   cat(sprintf("Выбраны все суды/регионы для: %s\n", label_text))
# }

# Конвертация одного HTML в PDF
convert_single_case <- function(session, html_path, delete_html = FALSE) {
  pdf_path <- sub("\\.html$", ".pdf", html_path)
  abs_path <- normalizePath(html_path, mustWork = FALSE)
  file_url <- paste0("file:///", gsub("\\\\", "/", abs_path))
  tryCatch({
    session$Page$navigate(file_url)
    Sys.sleep(1)
    res <- session$Page$printToPDF(
      printBackground = TRUE,
      paperWidth      = 8.27,
      paperHeight     = 11.69,
      marginTop       = 0.4,
      marginBottom    = 0.4,
      marginLeft      = 0.4,
      marginRight     = 0.4
    )
    writeBin(base64decode(res$data), pdf_path)
    if (delete_html) file.remove(html_path)
    TRUE
  }, error = function(e) {
    cat(sprintf("    Ошибка конвертации %s: %s\n", html_path, conditionMessage(e)))
    FALSE
  })
}

# Скачивание дел одного округа
download_district <- function(session, selected_key, selected_label, query,
                              max_pages = NULL,
                              convert_to_pdf = FALSE,
                              delete_html_after_pdf = FALSE) {
  output_dir     <- file.path(BASE_DIR, paste0(selected_key, "_Videonablyudenie_v_skole"))
  case_ids_file  <- file.path(BASE_DIR, paste0("case_ids_", selected_key, ".json"))
  failed_file    <- file.path(BASE_DIR, paste0("failed_ids_", selected_key, ".txt"))
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  
  if (file.exists(case_ids_file)) {
    all_ids <- fromJSON(case_ids_file)
    if (is.list(all_ids)) all_ids <- unlist(all_ids, use.names = FALSE)
    if (is.null(all_ids))  all_ids <- character(0)
    cat(sprintf("Загружено %d ID из %s\n", length(all_ids), case_ids_file))
  } else {
    # cat("Файл с ID не найден, начинаю поиск с выбором округа...\n")
    # safe_get(session, BASE_URL)
    # Sys.sleep(2)
    # 
    # select_district(session, selected_label)
    
    cat("Файл с ID не найден, начинаю поиск с выбором округа...\n")
    
    # Открываем главную страницу с проверкой успеха
    nav_ok <- safe_get(session, BASE_URL)
    if (!nav_ok) {
      stop("Не удалось открыть главную страницу после нескольких попыток. Пропускаю округ.")
    }
    Sys.sleep(2)
    
    select_district(session, selected_label)
    
    # --- 1. Заполняем поле поиска и отправляем форму ---
    query_json <- jsonlite::toJSON(query, auto_unbox = TRUE)
    js_search <- sprintf("
      (function() {
        let input = document.querySelector('input[name=\"simpleSearch[content]\"]');
        if (!input) return 'no_input';
        input.value = %s;
        let btn = document.querySelector('#simpleSearch_search');
        if (btn) { btn.click(); return 'clicked'; }
        let form = input.closest('form');
        if (form) { form.submit(); return 'submitted'; }
        return 'no_form';
      })()
    ", query_json)
    
    res <- eval_js(session, js_search)
    cat(sprintf("Отправка формы: %s\n", res))
    
    # --- 2. Ждём появления таблицы с результатами поиска (до 30 сек) ---
    cat("Ожидание загрузки результатов поиска...\n")
    table_found <- FALSE
    for (i in 1:30) {
      n_tables <- eval_js(session, "document.querySelectorAll('table.table-bordered').length")
      if (!is.null(n_tables) && n_tables > 0) {
        cat(sprintf("Результаты появились (таблиц: %d, попытка %d)\n", n_tables, i))
        table_found <- TRUE
        break
      }
      Sys.sleep(1)
    }
    
    # --- 3. Если таблица не появилась — переходим на /search напрямую ---
    if (!table_found) {
      cat("Таблица результатов не появилась. Перехожу на /search напрямую...\n")
      safe_get(session, paste0(BASE_URL, "/search"))
      Sys.sleep(3)
      
      # Повторная проверка
      for (i in 1:15) {
        n_tables <- eval_js(session, "document.querySelectorAll('table.table-bordered').length")
        if (!is.null(n_tables) && n_tables > 0) {
          cat(sprintf("Результаты появились после перехода на /search (таблиц: %d)\n", n_tables))
          table_found <- TRUE
          break
        }
        Sys.sleep(1)
      }
    }
    
    # --- 4. Диагностика: если таблиц всё равно нет, сохраняем HTML ---
    if (!table_found) {
      debug_html <- get_html(session)
      writeLines(debug_html, file.path(BASE_DIR, "debug_search_empty.html"))
      cat("ДИАГНОСТИКА: Таблица результатов так и не появилась.\n")
      cat("Сохранён debug_search_empty.html для анализа.\n")
      stop("Не удалось получить результаты поиска. Проверьте debug_search_empty.html")
    }
    
    all_ids  <- character(0)
    page_num <- 1
    
    repeat {
      if (!is.null(max_pages) && page_num > max_pages) {
        cat(sprintf("Достигнуто ограничение в %d стр., останавливаю сбор.\n", max_pages))
        break
      }
      cat(sprintf("\nОбработка страницы %d\n", page_num))
      Sys.sleep(1)
      
      html <- get_html(session)
      
      # ЖЕСТКАЯ ЗАЩИТА ПЕРЕД read_html: предотвращает ошибку read_xml(NULL)
      if (is.null(html) || !is.character(html) || !nzchar(html)) {
        stop("Получен пустой или NULL HTML. Невозможно распарсить страницу.")
      }
      
      soup <- read_html(html, encoding = "UTF-8")
      links <- soup %>% html_nodes("table.table-bordered a") %>% html_attr("href")
      links <- links[!is.na(links)]
      
      new_ids <- character(0)
      for (href in links) {
        if (str_detect(href, "^/\\d+$")) {
          cid <- str_remove(href, "^/")
          if (!(cid %in% all_ids)) {
            all_ids  <- c(all_ids, cid)
            new_ids  <- c(new_ids, cid)
          }
        }
      }
      cat(sprintf("  Найдено новых ID: %d (всего: %d)\n", length(new_ids), length(all_ids)))
      
      # # Клик по кнопке "Следующая страница"
      # js_next <- "
      #   (function() {
      #     let links = document.querySelectorAll('ul.pagination a');
      #     for (let l of links) {
      #       let t = l.textContent.trim();
      #       if (t === '»' || t === '›' || t === '>') {
      #         l.click();
      #         return 'next';
      #       }
      #     }
      #     return 'end';
      #   })()
      # "
      # chromote не всегда дожидается load
      
      # --- Находим URL следующей страницы ---
      js_next_href <- "
        (function() {
          let links = document.querySelectorAll('ul.pagination a');
          for (let l of links) {
            let t = l.textContent.trim();
            if (t === '»' || t === '›' || t === '>') {
              return l.href;
            }
          }
          return null;
        })()
      "
      next_url <- eval_js(session, js_next_href)
      
      if (is.null(next_url) || !nzchar(next_url) || next_url == "null") {
        cat("  Пагинация завершена.\n")
        break
      }
      
      cat(sprintf("  Переход на %s\n", next_url))
      
      # Используем safe_get вместо прямого navigate, чтобы были повторные попытки при таймауте
      nav_success <- safe_get(session, next_url)
      
      if (!nav_success) {
        cat("  Не удалось загрузить страницу после нескольких попыток (таймаут сервера). Останавливаю сбор.\n")
        break
      }
      
      # Ждём, пока на новой странице появятся таблицы результатов
      for (i in 1:20) {
        n_tables <- eval_js(session, "document.querySelectorAll('table.table-bordered').length")
        if (!is.null(n_tables) && n_tables > 0) break
        Sys.sleep(1)
      }
      Sys.sleep(DELAY_BETWEEN_PAGES)
      
      page_num <- page_num + 1
      
      #   res <- eval_js(session, js_next)
      #   if (is.null(res) || res == "end") {
      #     cat("  Пагинация завершена.\n")
      #     break
      #   }
      #   page_num <- page_num + 1
      #   Sys.sleep(DELAY_BETWEEN_PAGES)
    }
    # не актуален 
    
    write_json(all_ids, case_ids_file, auto_unbox = TRUE, pretty = TRUE)
    cat(sprintf("Собрано %d ID, сохранено в %s\n", length(all_ids), case_ids_file))
  }
  
  if (length(all_ids) == 0) {
    cat("Нет ID для обработки.\n")
    return(invisible())
  }
  
  existing  <- list.files(output_dir)
  remaining <- all_ids[!(paste0(all_ids, ".html") %in% existing)]
  cat(sprintf("Уже скачано: %d; осталось: %d\n",
              length(all_ids) - length(remaining), length(remaining)))
  if (length(remaining) == 0) {
    cat("Всё уже загружено!\n")
    return(invisible())
  }
  # Минимальная длина для валидного решения (без учета огромных страниц ошибок браузера)
  MIN_VALID_LENGTH <- 25000 
  
  cat("Начинаю загрузку решений...\n")
  downloaded_count <- 0
  fails_in_row <- 0
  failed_ids <- character(0)
  
  for (i in seq_along(remaining)) {
    cid <- remaining[i]
    url <- paste0(BASE_URL, "/", cid)
    cat(sprintf("[%d/%d] %s\n", i, length(remaining), url))
    
    success <- FALSE
    
    # Цикл повторных попыток для одного ID
    for (attempt in 1:MAX_RETRIES) {
      if (attempt > 1) {
        cat(sprintf("    Попытка %d: ждем и пробуем снова...\n", attempt))
        Sys.sleep(5)
      }
      
      if (safe_get(session, url)) {
        html_content <- get_html(session)
        html_lower <- tolower(html_content)
        
        # 1. ПРОВЕРКА НА СТРАНИЦУ ОШИБКИ БРАУЗЕРА (Chromium/Chrome)
        # Эти страницы могут быть огромными, но они не являются решением суда
        is_browser_error <- str_detect(html_lower, "err_") ||
          str_detect(html_lower, "chromium") ||
          str_detect(html_lower, "no internet") ||
          str_detect(html_lower, "name not resolved") ||
          str_detect(html_lower, "connection refused")
        
        # 2. ПРОВЕРКА НА КАПЧУ / БЛОКИРОВКУ
        is_blocked <- str_detect(html_lower, "cloudflare|captcha|проверка браузера|доступ ограничен")
        
        # 3. ПРОВЕРКА НА ДЛИНУ (только если это не ошибка браузера и не капча)
        is_too_short <- !is_browser_error && !is_blocked && (nchar(html_content) < MIN_VALID_LENGTH)
        
        if (is_browser_error || is_blocked) {
          cat("    Обнаружена ошибка браузера или блокировка. Пропускаю этот ID.\n")
          break # Выходим из цикла попыток, этот ID точно не загрузится
          
        } else if (is_too_short) {
          cat(sprintf("    Страница слишком короткая (%d симв.). Повторная попытка...\n", nchar(html_content)))
          # Цикл for продолжится, увеличив attempt
          
        } else {
          # ВСЁ ОТЛИЧНО! Сохраняем файл
          html_path <- file.path(output_dir, paste0(cid, ".html"))
          writeLines(html_content, html_path, useBytes = TRUE)
          cat(sprintf("    Сохранён (%d симв.)\n", nchar(html_content)))
          success <- TRUE
          break # Успех, выходим из цикла попыток
        }
      } else {
        cat("    Ошибка навигации. Повторная попытка...\n")
      }
    } # Конец цикла попыток для одного ID
    
    # Обновляем счетчики ПОСЛЕ завершения попыток для текущего ID
    if (success) {
      downloaded_count <- downloaded_count + 1
      fails_in_row <- 0
    } else {
      failed_ids <- c(failed_ids, cid)
      fails_in_row <- fails_in_row + 1
      cat("    Не удалось скачать после всех попыток.\n")
      
      if (fails_in_row >= 3) {
        cat(sprintf("    Слишком много ошибок подряд, пауза %d сек...\n", PAUSE_AFTER_CONSECUTIVE_FAILS))
        Sys.sleep(PAUSE_AFTER_CONSECUTIVE_FAILS)
        fails_in_row <- 0
      }
    }
    
    Sys.sleep(DELAY_BETWEEN_DOWNLOADS)
  }
  
  # Финальный отчет
  if (length(failed_ids) > 0) {
    failed_file <- file.path(BASE_DIR, paste0("failed_ids_", selected_key, ".txt"))
    writeLines(failed_ids, failed_file)
    cat(sprintf("Неудачные ID сохранены в %s (%d шт.)\n", failed_file, length(failed_ids)))
  }
  
  cat(sprintf("Готово. За сессию успешно скачано: %d файлов.\n", downloaded_count))

  }

# =============== ГЛАВНОЕ МЕНЮ ===============
main <- function() {
  cat("=== Универсальный парсер судебных решений (R) ===\n")
  
  query <- readline(sprintf("Введите поисковый запрос (Enter для '%s'): ", DEFAULT_QUERY))
  if (query == "") query <- DEFAULT_QUERY
  cat(sprintf("Будет использован запрос: '%s'\n", query))
  
  cat("\nВыберите режим федерального округа:\n")
  cat("1. Один округ\n")
  cat("2. Все округа (9 штук)\n")
  mode_choice <- readline("Ваш выбор (1/2): ")
  
  if (mode_choice == "1") {
    cat("\nДоступные округа:\n")
    keys   <- names(DISTRICTS)
    labels <- unlist(DISTRICTS, use.names = FALSE)
    for (i in seq_along(keys)) cat(sprintf("  %d. %s\n", i, labels[i]))
    idx <- suppressWarnings(as.integer(readline("Введите номер округа: ")))
    if (is.na(idx) || idx < 1 || idx > length(keys)) {
      cat("Некорректный номер. Выход.\n"); return(invisible())
    }
    selected_keys   <- keys[idx]
    selected_labels <- labels[idx]
  } else if (mode_choice == "2") {
    selected_keys   <- names(DISTRICTS)
    selected_labels <- unlist(DISTRICTS, use.names = FALSE)
  } else {
    cat("Неверный выбор. Выход.\n"); return(invisible())
  }
  
  pages_input <- readline("\nСколько страниц результатов обработать? (Enter - все страницы): ")
  if (pages_input == "") {
    max_pages <- NULL
  } else {
    mp <- suppressWarnings(as.integer(pages_input))
    if (is.na(mp) || mp <= 0) {
      cat("Некорректное число, будет использовано значение по умолчанию (все страницы).\n")
      max_pages <- NULL
    } else max_pages <- mp
  }
  
  cat("\nВыберите режим обработки HTML-файлов:\n")
  cat("1. Оставить только HTML (без конвертации в PDF)\n")
  cat("2. Сразу конвертировать в PDF и УДАЛИТЬ исходный HTML\n")
  cat("3. Сразу конвертировать в PDF, но СОХРАНИТЬ HTML\n")
  convert_choice <- readline("Ваш выбор (1/2/3): ")
  
  if (convert_choice == "2") {
    convert_to_pdf        <- TRUE
    delete_html_after_pdf <- TRUE
  } else if (convert_choice == "3") {
    convert_to_pdf        <- TRUE
    delete_html_after_pdf <- FALSE
  } else {
    convert_to_pdf        <- FALSE
    delete_html_after_pdf <- FALSE
  }
  
  cat("\nЗапуск браузера...\n")
  # session <- ChromoteSession$new()
  # Создаем сессию с размером окна обычного пользователя
  session <- ChromoteSession$new(width = 1920, height = 1080)
  
  # 1. Устанавливаем User-Agent обычного Chrome
  session$Network$setUserAgentOverride(
    userAgent = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36"
  )
  
  # 2. Маскируем признак автоматизации (headless)
  session$Runtime$evaluate("Object.defineProperty(navigator, 'webdriver', {get: () => false});")
  
  on.exit(session$close(), add = TRUE)
  
  for (i in seq_along(selected_keys)) {
    key   <- selected_keys[i]
    label <- selected_labels[i]
    cat(sprintf("\n=== Обработка округа: %s ===\n", label))
    tryCatch({
      download_district(session, key, label, query, max_pages,
                        convert_to_pdf, delete_html_after_pdf)
    }, error = function(e) {
      cat(sprintf("ОШИБКА при обработке округа %s: %s\n",
                  label, conditionMessage(e)))
    })
  }
  
  cat("\nВсе операции завершены.\n")
}

# Точка входа
main()