# ============================================================
# eda.R — Разведочный анализ данных (с паттернами правонарушений)
# ============================================================
suppressPackageStartupMessages({
  library(readr); library(dplyr); library(ggplot2)
  library(tidyr); library(stringr)
})

BASE_DIR <- "C:/Users/rewhe/Desktop/videonabludenie"
df <- read_csv(file.path(BASE_DIR, "data", "cases_features.csv"), show_col_types = FALSE)

cat(sprintf("Загружено %d строк из cases_features.csv\n", nrow(df)))

# Приводим is_valid к логическому типу
if ("is_valid" %in% colnames(df)) {
  df$is_valid <- as.logical(df$is_valid)
} else {
  df$is_valid <- TRUE
  cat("Колонка 'is_valid' не найдена, считаем все файлы валидными.\n")
}

# Фильтруем: только валидные файлы и дела с определённым типом
df <- df %>%
  filter(
    is_valid == TRUE,
    !str_detect(case_type, "Не определено")
  )

cat(sprintf("После фильтрации осталось %d наблюдений из 625\n", nrow(df)))

# --- Определяем колонки паттернов (все, что начинаются на pat_) ---
pattern_cols <- grep("^pat_", colnames(df), value = TRUE)
if (length(pattern_cols) == 0) {
  warning("Колонки паттернов (pat_*) не найдены. Проверьте extract_features.R")
} else {
  cat(sprintf("Найдено %d паттернов: %s\n", length(pattern_cols), paste(pattern_cols, collapse=", ")))
}

# ============================================================
# 1. Описательная статистика
# ============================================================
summary(df)

cat("\nРаспределение по округам:\n")
df %>% count(district, sort = TRUE) %>% print()

cat("\nРаспределение по типам дел:\n")
df %>% count(case_type, sort = TRUE) %>% print()

cat("\nДлительность рассмотрения (дни):\n")
df %>% summarise(
  mean   = mean(duration, na.rm = TRUE),
  median = median(duration, na.rm = TRUE),
  sd     = sd(duration, na.rm = TRUE),
  min    = min(duration, na.rm = TRUE),
  max    = max(duration, na.rm = TRUE)
) %>% print()

# --- Статистика по паттернам правонарушений ---
if (length(pattern_cols) > 0) {
  cat("\nРаспределение паттернов правонарушений:\n")
  pattern_summary <- df %>%
    summarise(across(all_of(pattern_cols), ~ sum(.x, na.rm = TRUE))) %>%
    pivot_longer(everything(), names_to = "Паттерн", values_to = "Количество") %>%
    mutate(Доля = round(Количество / nrow(df) * 100, 1))
  print(pattern_summary)
}

# ============================================================
# Функция-генератор графиков (сохраняет в подпапку внутри eda_plots)
# ============================================================
plot_and_save <- function(data, base_dir, folder_name) {
  save_dir <- file.path(base_dir, folder_name)
  dir.create(save_dir, showWarnings = FALSE, recursive = TRUE)
  
  if (nrow(data) == 0) {
    cat(sprintf("  [Пропуск] %s: нет данных для графиков\n", folder_name))
    return(invisible())
  }
  
  cat(sprintf("  -> Генерация графиков для: %s (папка: %s)\n", folder_name, save_dir))
  
  custom_theme <- theme_minimal(base_size = 14) +
    theme(
      panel.grid.major = element_line(color = "grey70", linewidth = 0.5),
      panel.grid.minor = element_line(color = "grey85", linewidth = 0.25, linetype = "dashed"),
      panel.border = element_rect(color = "grey50", fill = NA, linewidth = 0.8)
    )
  
  # --- 1. Гистограмма длительности ---
  dur_data <- data %>% filter(!is.na(duration), duration < 365)
  if (nrow(dur_data) > 0) {
    max_dur <- max(dur_data$duration, na.rm = TRUE)
    step <- ifelse(max_dur <= 50, 5, ifelse(max_dur <= 150, 10, 20))
    
    p1 <- ggplot(dur_data, aes(x = duration)) +
      geom_histogram(binwidth = step, fill = "steelblue", color = "white", alpha = 0.85) +
      scale_x_continuous(breaks = seq(0, max_dur + step, by = step)) +
      labs(title = paste0(folder_name, ": Длительность рассмотрения дел"),
           x = "Дни", y = "Количество дел") + 
      custom_theme
    
    print(p1)
    ggsave(file.path(save_dir, "hist_duration.png"), p1, width = 8, height = 5, dpi = 150)
  }
  
  # --- 2. Столбчатая диаграмма типов дел ---
  type_data <- data %>% 
    count(case_type) %>%
    filter(!is.na(case_type), 
           !str_detect(case_type, "Не определено|ошибка загрузки|файл обрезан"))
  
  if (nrow(type_data) > 0) {
    max_n <- max(type_data$n, na.rm = TRUE)
    step_n <- ifelse(max_n <= 20, 2, ifelse(max_n <= 100, 10, 25))
    
    p2 <- ggplot(type_data, aes(x = reorder(case_type, -n), y = n)) +
      geom_col(fill = "coral", alpha = 0.85) +
      scale_y_continuous(breaks = seq(0, max_n + step_n, by = step_n)) +
      labs(title = paste0(folder_name, ": Типы судебных дел"), x = NULL, y = "Количество") +
      custom_theme + 
      theme(axis.text.x = element_text(angle = 30, hjust = 1))
    
    print(p2)
    ggsave(file.path(save_dir, "bar_case_types.png"), p2, width = 8, height = 5, dpi = 150)
  }
  
  # --- 3. Boxplot по округам (только для ALL) ---
  if (folder_name == "ALL" && length(unique(data$district)) > 1) {
    box_data <- data %>% filter(!is.na(duration), duration < 365)
    if (nrow(box_data) > 0) {
      max_dur <- max(box_data$duration, na.rm = TRUE)
      step <- ifelse(max_dur <= 50, 5, ifelse(max_dur <= 150, 10, 20))
      
      p3 <- ggplot(box_data, aes(x = reorder(district, duration, FUN = median), 
                                 y = duration, fill = district)) +
        geom_boxplot(alpha = 0.8) + 
        scale_y_continuous(breaks = seq(0, max_dur + step, by = step)) +
        coord_flip() + 
        guides(fill = "none") +
        labs(title = "Сравнение длительности по федеральным округам", 
             x = NULL, y = "Дни") + 
        custom_theme
      
      print(p3)
      ggsave(file.path(save_dir, "box_districts.png"), p3, width = 9, height = 6, dpi = 150)
    }
  }
  
  # --- 4. Распределение по ключевым словам ---
  kw_summary <- tibble(
    "Видеонаблюдение" = sum(data$kw_camera, na.rm = TRUE),
    "Школа/Образование" = sum(data$kw_school, na.rm = TRUE),
    "Дети/Несовершеннолетние" = sum(data$kw_children, na.rm = TRUE),
    "Инциденты" = sum(data$kw_incident, na.rm = TRUE),
    "Отсутствие оснащения" = sum(data$kw_absence, na.rm = TRUE),
    "Обязательные требования" = sum(data$kw_obligate, na.rm = TRUE)
  ) %>% pivot_longer(everything(), names_to = "Категория", values_to = "Количество")
  
  if (sum(kw_summary$Количество) > 0) {
    max_kw <- max(kw_summary$Количество, na.rm = TRUE)
    step_kw <- ifelse(max_kw <= 20, 2, ifelse(max_kw <= 100, 10, 25))
    
    p5 <- ggplot(kw_summary, aes(x = reorder(Категория, -Количество), y = Количество)) +
      geom_col(fill = "seagreen3", alpha = 0.85) +
      scale_y_continuous(breaks = seq(0, max_kw + step_kw, by = step_kw)) +
      labs(title = paste0(folder_name, ": Упоминания ключевых тем"), x = NULL, y = "Количество дел") +
      custom_theme +
      theme(axis.text.x = element_text(angle = 30, hjust = 1))
    
    print(p5)
    ggsave(file.path(save_dir, "keywords.png"), p5, width = 9, height = 5, dpi = 150)
  }
  
  # --- 5. Распределение паттернов правонарушений ---
  if (length(pattern_cols) > 0) {
    pat_data <- data %>%
      summarise(across(all_of(pattern_cols), ~ sum(.x, na.rm = TRUE))) %>%
      pivot_longer(everything(), names_to = "Паттерн", values_to = "Количество") %>%
      filter(Количество > 0)
    
    if (nrow(pat_data) > 0) {
      max_pat <- max(pat_data$Количество, na.rm = TRUE)
      step_pat <- ifelse(max_pat <= 20, 2, ifelse(max_pat <= 100, 10, 25))
      
      p6 <- ggplot(pat_data, aes(x = reorder(Паттерн, -Количество), y = Количество)) +
        geom_col(fill = "darkorange", alpha = 0.85) +
        scale_y_continuous(breaks = seq(0, max_pat + step_pat, by = step_pat)) +
        labs(title = paste0(folder_name, ": Паттерны правонарушений"), x = NULL, y = "Количество дел") +
        custom_theme +
        theme(axis.text.x = element_text(angle = 30, hjust = 1))
      
      print(p6)
      ggsave(file.path(save_dir, "patterns.png"), p6, width = 10, height = 5, dpi = 150)
    }
  }
  
  # --- 6. Тепловая карта: регион × паттерн (только для ALL) ---
  if (folder_name == "ALL" && length(pattern_cols) > 0 && length(unique(data$district)) > 1) {
    heat_data <- data %>%
      group_by(district) %>%
      summarise(across(all_of(pattern_cols), ~ mean(.x, na.rm = TRUE) * 100)) %>%
      pivot_longer(-district, names_to = "Паттерн", values_to = "Процент")
    
    p7 <- ggplot(heat_data, aes(x = Паттерн, y = district, fill = Процент)) +
      geom_tile(color = "white") +
      geom_text(aes(label = sprintf("%.0f%%", Процент)), size = 3) +
      scale_fill_gradient(low = "white", high = "red") +
      labs(title = "Доля дел с паттерном по федеральным округам, %", x = NULL, y = NULL) +
      theme_minimal(base_size = 12) +
      theme(axis.text.x = element_text(angle = 30, hjust = 1))
    
    print(p7)
    ggsave(file.path(save_dir, "heatmap_patterns.png"), p7, width = 10, height = 6, dpi = 150)
  }
}

# ============================================================
# 1. Генерация ОБЩИХ графиков по всей базе папка eda_plots/ALL/
# ============================================================
cat("\n=== Построение общих графиков ===\n")
plot_and_save(df, 
              base_dir = file.path(BASE_DIR, "eda_plots"), 
              folder_name = "ALL")

# --- Матрица корреляций (сохраняем в папку ALL) ---
# Включаем паттерны, если они есть
cor_vars <- c("duration", "text_length", "n_camera", "n_kamera", "n_school", "n_children")
if (length(pattern_cols) > 0) {
  cor_vars <- c(cor_vars, pattern_cols)
}

num_vars <- df %>%
  select(all_of(cor_vars)) %>%
  mutate(across(all_of(pattern_cols), as.numeric)) %>%
  cor(use = "pairwise.complete.obs")

print(round(num_vars, 2))

if (requireNamespace("reshape2", quietly = TRUE)) {
  melted <- reshape2::melt(num_vars)
  p4 <- ggplot(melted, aes(Var1, Var2, fill = value)) +
    geom_tile(color = "white") +
    geom_text(aes(label = round(value, 2)), size = 3.5) +
    scale_fill_gradient2(low = "red", mid = "white", high = "steelblue", midpoint = 0) +
    labs(title = "Матрица корреляций признаков (включая паттерны)", x = NULL, y = NULL) +
    theme_minimal(base_size = 12) +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))
  
  print(p4)
  ggsave(file.path(BASE_DIR, "eda_plots", "ALL", "corr_matrix.png"), 
         p4, width = 10, height = 8, dpi = 150)
}

# ============================================================
# 2. Генерация графиков ДЛЯ КАЖДОГО ОКРУГА, папка eda_plots/Имя_Округа/
# ============================================================
cat("\n=== Построение графиков по округам ===\n")
for (dist in unique(df$district)) {
  df_dist <- df %>% filter(district == dist)
  plot_and_save(df_dist, 
                base_dir = file.path(BASE_DIR, "eda_plots"), 
                folder_name = dist)
}

# ============================================================
# Функция: доля дел с паттерном по федеральным округам (%)
# Возвращает таблицу, печатает копируемый TSV и сохраняет CSV
# ============================================================
pattern_share_by_district <- function(data,
                                      base_dir = BASE_DIR,
                                      pattern_cols = NULL,
                                      save_csv = TRUE,
                                      round_digits = 1) {
  # Если паттерны не переданы — берём все колонки, начинающиеся на pat_
  if (is.null(pattern_cols)) {
    pattern_cols <- grep("^pat_", colnames(data), value = TRUE)
  }
  if (length(pattern_cols) == 0) {
    warning("Колонки паттернов (pat_*) не найдены.")
    return(invisible(NULL))
  }
  
  # Считаем долю (в %) по каждому округу, плюс строку ALL (вся база)
  shares <- data %>%
    group_by(district) %>%
    summarise(
      n_cases = n(),
      across(all_of(pattern_cols), ~ round(mean(.x, na.rm = TRUE) * 100, round_digits)),
      .groups = "drop"
    )
  
  shares_all <- data %>%
    summarise(
      district = "ALL",
      n_cases = n(),
      across(all_of(pattern_cols), ~ round(mean(.x, na.rm = TRUE) * 100, round_digits))
    )
  
  result <- bind_rows(shares, shares_all)
  
  # --- Печать копируемой таблицы (TSV: разделитель — табуляция) ---
  cat("\n=== Доля дел с паттерном по федеральным округам, % ===\n")
  cat("(скопируйте и вставьте в Excel — разделитель табуляции)\n\n")
  
  # Заголовок
  cat(paste(colnames(result), collapse = "\t"), "\n", sep = "")
  # Строки
  for (i in seq_len(nrow(result))) {
    cat(paste(unlist(result[i, ]), collapse = "\t"), "\n", sep = "")
  }
  cat("\n")
  
  # --- Сохранение в CSV ---
  if (save_csv) {
    out_path <- file.path(base_dir, "data", "pattern_share_by_district.csv")
    dir.create(dirname(out_path), showWarnings = FALSE, recursive = TRUE)
    readr::write_csv(result, out_path)
    cat(sprintf("CSV сохранён: %s\n", out_path))
  }
  
  invisible(result)
}

pattern_share_by_district(df)
cat("\nВсе графики сохранены в папку 'eda_plots' с разбивкой по подпапкам!\n")