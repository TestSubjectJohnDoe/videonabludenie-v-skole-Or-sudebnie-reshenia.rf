# ============================================================
# extract_features.R
# Извлекает признаки из скачанных HTML-решений в единый CSV
# Надежное извлечение дат и суда, сохранение ID
# ============================================================

suppressPackageStartupMessages({
  library(rvest)
  library(stringr)
  library(dplyr)
  library(purrr)
  library(tidyr)
  library(readr)
})

BASE_DIR <- "C:/Users/rewhe/Desktop/videonabludenie"

# --- Читаем CSV с релевантными ID ---
csv_path <- file.path(BASE_DIR, "data", "relevant_cases_with_type_and_case_ending.csv")
relevant_df <- read_csv(csv_path, show_col_types = FALSE)
relevant_ids <- as.character(relevant_df$ID)
relevant_ids <- trimws(relevant_ids)
relevant_ids <- str_remove(relevant_ids, "\\.0$")
cat(sprintf("Загружено %d релевантных ID из CSV\n", length(relevant_ids)))

# --- Ищем папки округов (они лежат в подпапке data) ---
data_dir <- file.path(BASE_DIR, "data")
folders <- list.dirs(data_dir, recursive = FALSE)
folders <- folders[str_detect(basename(folders), "_Videonablyudenie_v_skole$")]

# --- Ищем файлы по именам (ID.html) в папках округов ---
html_files <- unlist(lapply(folders, function(f) {
  expected_files <- file.path(f, paste0(relevant_ids, ".html"))
  existing_files <- expected_files[file.exists(expected_files)]
  return(existing_files)
}))

cat(sprintf("Найдено HTML-файлов для обработки: %d из %d\n", length(html_files), length(relevant_ids)))

# --- Проверка: есть ли файлы вообще ---
if (length(html_files) == 0) {
  stop("Не найдено ни одного HTML-файла. Проверьте:\n",
       "1. Правильность BASE_DIR (сейчас: ", BASE_DIR, ")\n",
       "2. Что внутри папок округов лежат файлы вида 'ID.html'")
}

# --- Проверка первых 3 ID ---
if (length(relevant_ids) >= 3) {
  first_3_ids <- relevant_ids[1:3]
  found_ids <- tools::file_path_sans_ext(basename(html_files))
  found_count <- sum(first_3_ids %in% found_ids)
  if (found_count == 0) {
    stop("КРИТИЧЕСКАЯ ОШИБКА: Первые 3 ID из CSV не найдены в папках округов.\n",
         "Проверенные ID: ", paste(first_3_ids, collapse = ", "))
  }
}

# 12/10/26 Полное изменение html файлов! Дела нужно парсить иначе!
# extract_one <- function(path) {
#   district <- basename(dirname(path)) %>%
#     str_remove("_Videonablyudenie_v_skole$")
#   cid <- tools::file_path_sans_ext(basename(path))
#   
#   html <- tryCatch(read_html(path, encoding = "UTF-8"), error = function(e) NULL)
#   if (is.null(html)) return(NULL)
#   
#   text <- html %>% html_text2()
#   text_lower <- tolower(text)
#   text_length <- nchar(text)
#   
#   # Оболочка / битый файл
#   if (text_length < 3000) {
#     meta_desc <- html %>% html_node("meta[name='description']") %>% html_attr("content")
#     return(tibble(
#       id = cid, district = district, is_valid = FALSE,
#       court = ifelse(!is.na(meta_desc), str_extract(meta_desc, "^[^,]+"), "Не найден"),
#       case_type = "Не определено (файл обрезан)",
#       date_in = NA_character_,
#       date_out = str_extract(meta_desc, "\\d{2}\\.\\d{2}\\.\\d{4}"),
#       text_length = text_length,
#       kw_camera = FALSE, kw_school = FALSE, kw_children = FALSE,
#       kw_incident = FALSE, kw_absence = FALSE, kw_obligate = FALSE,
#       n_camera = 0, n_kamera = 0, n_school = 0, n_children = 0
#     ))
#   }
#   
#   # Суд
#   court_match <- str_match(text, "Суд:\\s*(.+)")
#   court <- if (!is.na(court_match[1, 1])) str_trim(court_match[1, 2]) else "Не найден"
#   
#   # Даты
#   date_in_match <- str_match(text, "Дата начала:\\s*(\\d{2}\\.\\d{2}\\.\\d{4})")
#   date_in <- if (!is.na(date_in_match[1, 1])) date_in_match[1, 2] else NA_character_
#   
#   date_out_match <- str_match(text, "Дата рассмотрения:\\s*(\\d{2}\\.\\d{2}\\.\\d{4})")
#   date_out <- if (!is.na(date_out_match[1, 1])) date_out_match[1, 2] else NA_character_
#   
#   if (is.na(date_out)) {
#     m_out <- str_match(text, "(?:изготовлен|вынесен|принят|опубликован).*?(\\d{1,2})\\s+(января|февраля|марта|апреля|мая|июня|июля|августа|сентября|октября|ноября|декабря)\\s+(\\d{4})")
#     if (!is.na(m_out[1, 1])) {
#       day <- m_out[1, 2]
#       month_str <- m_out[1, 3]
#       year <- m_out[1, 4]
#       month_num <- switch(month_str,
#                           "января" = "01", "февраля" = "02", "марта" = "03", "апреля" = "04",
#                           "мая" = "05", "июня" = "06", "июля" = "07", "августа" = "08",
#                           "сентября" = "09", "октября" = "10", "ноября" = "11", "декабря" = "12")
#       date_out <- sprintf("%02d.%s.%s", as.integer(day), month_num, year)
#     }
#   }
#   
#   # Тип дела
#   case_type <- case_when(
#     str_detect(text_lower, "административн.*(?:иск|дело|производство)") ~ "Административное",
#     str_detect(text_lower, "уголовн.*(?:дело|производство)") ~ "Уголовное",
#     str_detect(text_lower, "гражданск.*(?:дело|производство|иск)") ~ "Гражданское",
#     str_detect(text_lower, "коап|об административных правонарушениях") ~ "КоАП",
#     TRUE ~ "Не определено"
#   )
#   
#   # Ключевые слова
#   kw_camera   <- str_detect(text_lower, "видеонаблюдени|камер")
#   kw_school   <- str_detect(text_lower, "школ|образовательн")
#   kw_children <- str_detect(text_lower, "дет|школьник|подрост|несовершеннолетн|учащ")
#   kw_incident <- str_detect(text_lower, "инцидент|происшеств|драк|нападен|травм")
#   kw_absence  <- str_detect(text_lower, "отсутств|не\\s+установлен|не\\s+оснащен|неисправн")
#   kw_obligate <- str_detect(text_lower, "обязать|возложить обязанность")
#   
#   n_camera   <- str_count(text_lower, "видеонаблюдени")
#   n_kamera   <- str_count(text_lower, "камер")
#   n_school   <- str_count(text_lower, "школ")
#   n_children <- str_count(text_lower, "несовершеннолетн|школьник|учащ")
#   
#   tibble(
#     id = cid, district = district, is_valid = TRUE,
#     court = court, case_type = case_type,
#     date_in = date_in, date_out = date_out,
#     text_length = text_length,
#     kw_camera = kw_camera, kw_school = kw_school, kw_children = kw_children,
#     kw_incident = kw_incident, kw_absence = kw_absence, kw_obligate = kw_obligate,
#     n_camera = n_camera, n_kamera = n_kamera, n_school = n_school, n_children = n_children
#   )
# }

extract_one <- function(path) {
  district <- basename(dirname(path)) %>% str_remove("_Videonablyudenie_v_skole$")
  cid <- tools::file_path_sans_ext(basename(path))
  
  # 1. Читаем HTML
  html <- tryCatch(read_html(path, encoding = "UTF-8"), error = function(e) NULL)
  if (is.null(html)) return(NULL)
  
  # 2. БЕЗОПАСНОЕ удаление скриптов, стилей и рекламы перед подсчетом длины текста
  tryCatch({
    html %>% 
      html_elements("script, style, noscript, .ad-banner, .translate, #translator") %>% 
      xml2::xml_remove()
  }, error = function(e) NULL) # Игнорируем ошибку, если таких элементов нет
  
  # 3. Извлекаем чистый текст
  text <- html %>% html_text2()
  text_lower <- tolower(text)
  text_length <- nchar(text)
  
  # 4. ПРОВЕРКА НА "ОБОЛОЧКУ" (если текст слишком короткий, значит это пустая страница с рекламой)
  if (text_length < 3000) {
    meta_desc <- html %>% html_element("meta[name='description']") %>% html_attr("content")
    
    return(tibble(
      id = cid, 
      district = district, 
      is_valid = FALSE,
      court = ifelse(!is.na(meta_desc), str_extract(meta_desc, "^[^,]+"), "Не найден"),
      case_type = "Не определено (файл обрезан или пуст)",
      date_in = NA_character_, 
      date_out = str_extract(meta_desc, "\\d{2}\\.\\d{2}\\.\\d{4}"),
      text_length = text_length,
      kw_camera = FALSE, kw_school = FALSE, kw_children = FALSE,
      kw_incident = FALSE, kw_absence = FALSE, kw_obligate = FALSE,
      n_camera = 0, n_kamera = 0, n_school = 0, n_children = 0
    ))
  }
  
  # 5. Извлечение СУДА
  court_match <- str_match(text, "Суд:\\s*(.+)")
  court <- if (!is.na(court_match[1, 1])) str_trim(court_match[1, 2]) else "Не найден"
  
  # 6. Извлечение ДАТ
  date_in_match <- str_match(text, "Дата начала:\\s*(\\d{2}\\.\\d{2}\\.\\d{4})")
  date_in <- if (!is.na(date_in_match[1, 1])) date_in_match[1, 2] else NA_character_
  
  date_out_match <- str_match(text, "Дата рассмотрения:\\s*(\\d{2}\\.\\d{2}\\.\\d{4})")
  date_out <- if (!is.na(date_out_match[1, 1])) date_out_match[1, 2] else NA_character_
  
  # Если дата рассмотрения не найдена в блоке, ищем в тексте (например, "изготовлено 25 июня 2026 года")
  if (is.na(date_out)) {
    m_out <- str_match(text, "(?:изготовлен|вынесен|принят|опубликован).*?(\\d{1,2})\\s+(января|февраля|марта|апреля|мая|июня|июля|августа|сентября|октября|ноября|декабря)\\s+(\\d{4})")
    if (!is.na(m_out[1, 1])) {
      day <- m_out[1, 2]
      month_str <- m_out[1, 3]
      year <- m_out[1, 4]
      
      month_num <- switch(month_str,
                          "января" = "01", "февраля" = "02", "марта" = "03", "апреля" = "04",
                          "мая" = "05", "июня" = "06", "июля" = "07", "августа" = "08",
                          "сентября" = "09", "октября" = "10", "ноября" = "11", "декабря" = "12")
      date_out <- sprintf("%02d.%s.%s", as.integer(day), month_num, year)
    }
  }
  
  # 7. Тип дела
  case_type <- case_when(
    str_detect(text_lower, "административн.*(?:иск|дело|производство)") ~ "Административное",
    str_detect(text_lower, "уголовн.*(?:дело|производство)") ~ "Уголовное",
    str_detect(text_lower, "гражданск.*(?:дело|производство|иск)") ~ "Гражданское",
    str_detect(text_lower, "коап|об административных правонарушениях") ~ "КоАП",
    TRUE ~ "Не определено"
  )
  
  # 8. Ключевые слова
  kw_camera   <- str_detect(text_lower, "видеонаблюдени|камер")
  kw_school   <- str_detect(text_lower, "школ|образовательн")
  kw_children <- str_detect(text_lower, "дет|школьник|подрост|несовершеннолетн|учащ")
  kw_incident <- str_detect(text_lower, "инцидент|происшеств|драк|нападен|травм")
  kw_absence  <- str_detect(text_lower, "отсутств|не\\s+установлен|не\\s+оснащен|неисправн")
  kw_obligate <- str_detect(text_lower, "обязать|возложить обязанность")
  pat_malfunction <- str_detect(text_lower, "неисправн|сломан|не работ|поврежден|вышел из строя|не функционирует|не подключен|отключен|не эксплуатируется")
  pat_privacy <-     str_detect(text_lower, "раздевалк|туалет|санузл|приватн|личн(ая|ой|ую) жизн|жилое помещение|место отдыха")
  pat_storage <- str_detect(text_lower, "утер|хранени.*запис|несанкционированн|доступ к запис|архив|срок хранения|уничтожен")
  pat_evidence <- str_detect(text_lower, "доказательств|видеофиксаци|запис[ьи] с камер|подтвержд|видеозапис|материал.*видео")
  pat_no_act <- str_detect(text_lower, "отсутстви(е|я|и) положени|нет локальн.*акт|не принят приказ|регламент.*видеонаблюдени")
  pat_absence_camera <- str_detect(text_lower, "отсутств.*видеонаблюдени|не установлен.*камер|нет камер|не оснащен.*видеонаблюдени")
  pat_improper_placement <- str_detect(text_lower, "направлен.*на (окна|жилые|раздевалк|туалет)|расположен.*неправильно|нарушени.*расположени|видеонаблюдени.*в (раздевалк|туалет)")
  pat_no_consent <- str_detect(text_lower, "согласи.*на обработку|персональн.*данн.*дет|согласи.*родител|без согласия")
  pat_violation_sanpin <- str_detect(text_lower, "санпин|санитарн.*норм|роспотребнадзор|нарушени.*требован.*безопасности")
  pat_failure_to_install <- str_detect(text_lower, "не исполнен.*предписани|не выполн.*требовани.*установк|обязан.*установить.*камер")
  pat_data_leak <- str_detect(text_lower, "утечк|распространен.*запис|публикаци.*видео|доступ.*посторонн|незаконн.*доступ.*запис")
  pat_operational_violation <- str_detect(text_lower, "несвоевременн.*обслуживан|не проводится.*обслуживан|нарушени.*эксплуатаци|отсутствие.*техобслуживан")
  n_violations <- sum(pat_malfunction, pat_privacy, pat_storage, pat_evidence, 
                      pat_no_act, pat_absence_camera, pat_improper_placement, 
                      pat_no_consent, pat_violation_sanpin, pat_failure_to_install, 
                      pat_data_leak, pat_operational_violation, na.rm = TRUE)
  
  # 9. Количество упоминаний
  n_camera   <- str_count(text_lower, "видеонаблюдени")
  n_kamera   <- str_count(text_lower, "камер")
  n_school   <- str_count(text_lower, "школ")
  n_children <- str_count(text_lower, "несовершеннолетн|школьник|учащ")
  
  tibble(
    id = cid, 
    district = district, 
    is_valid = TRUE,
    court = court, 
    case_type = case_type,
    date_in = date_in, 
    date_out = date_out,
    text_length = text_length,
    kw_camera = kw_camera, kw_school = kw_school, kw_children = kw_children,
    kw_incident = kw_incident, kw_absence = kw_absence, kw_obligate = kw_obligate,
    n_camera = n_camera, n_kamera = n_kamera, n_school = n_school, n_children = n_children,
    pat_malfunction = pat_malfunction,
    pat_privacy = pat_privacy,
    pat_storage = pat_storage,
    pat_evidence = pat_evidence,
    pat_no_act = pat_no_act,
    pat_absence_camera = pat_absence_camera,
    pat_improper_placement = pat_improper_placement,
    pat_no_consent = pat_no_consent,
    pat_violation_sanpin = pat_violation_sanpin,
    pat_failure_to_install = pat_failure_to_install,
    pat_data_leak = pat_data_leak,
    pat_operational_violation = pat_operational_violation,
    n_violations = n_violations
  )
}

# --- Применяем ко всем файлам ---
cat("Извлекаю признаки...\n")
features <- map_dfr(html_files, extract_one, .progress = TRUE)

# --- Проверка результата ---
if (is.null(features) || nrow(features) == 0) {
  stop("Не удалось извлечь ни одного признака.")
}

# --- Постобработка ---
features <- features %>%
  mutate(
    date_in  = as.Date(date_in,  format = "%d.%m.%Y"),
    date_out = as.Date(date_out, format = "%d.%m.%Y"),
    duration = as.numeric(date_out - date_in),
    month_out = format(date_out, "%Y-%m")
  )

# --- Сохранение ---
write_csv(features, file.path(BASE_DIR, "data", "cases_features.csv"))

library(writexl)  # install.packages("writexl")
write_xlsx(features, file.path(BASE_DIR, "data", "cases_features.xlsx"))
cat("Excel-файл сохранён\n")

cat(sprintf("Готово! Сохранено %d наблюдений, %d признаков\n",
            nrow(features), ncol(features)))