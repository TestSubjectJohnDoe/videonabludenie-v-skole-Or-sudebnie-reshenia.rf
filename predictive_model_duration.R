# ============================================================
# predictive_model_duration.R
# ============================================================
suppressPackageStartupMessages({
  library(readr); library(dplyr); library(stringr)
  library(ggplot2); library(randomForest)
})

BASE_DIR <- "C:/Users/rewhe/Desktop/videonabludenie"

# --- 1. Загрузка ---
df <- read_csv(file.path(BASE_DIR, "data", "cases_features.csv"),
               show_col_types = FALSE)
cat(sprintf("Загружено %d наблюдений\n", nrow(df)))

# --- 2. Паттерны ---
pattern_features <- c(
  "pat_evidence", "pat_storage", "pat_absence_camera",
  "pat_violation_sanpin", "pat_data_leak", "pat_malfunction"
)

# --- 3. Фильтрация + признаки ---
df_model <- df %>%
  filter(
    text_length > 5000,
    !is.na(case_type),
    !str_detect(case_type, "Не определено"),
    !is.na(duration), duration > 0, duration < 365,
    !is.na(district)
  ) %>%
  mutate(
    n_violations = rowSums(across(all_of(pattern_features)), na.rm = TRUE)
  ) %>%
  mutate(
    district    = factor(district),
    case_type   = factor(case_type),
    kw_children = factor(kw_children),
    kw_incident = factor(kw_incident),
    kw_absence  = factor(kw_absence),
    across(all_of(pattern_features), ~ factor(.x))
  )

cat(sprintf("Для моделирования отобрано %d наблюдений\n", nrow(df_model)))

# --- 4. Сводка отбракованных строк  ← ЭТО БЫЛО ПРОПУЩЕНО ---
n_removed <- nrow(df) - nrow(df_model)
cat(sprintf("Отброшено строк: %d (%.1f%% от исходных %d)\n",
            n_removed, 100 * n_removed / nrow(df), nrow(df)))

# --- 5. Очищенный датасет для Power BI  ← ЭТО ТОЖЕ БЫЛО ПРОПУЩЕНО ---
df_clean <- df_model %>%
  mutate(across(where(is.factor), as.character))
write_csv(df_clean, file.path(BASE_DIR, "data", "cases_features_clean.csv"))
cat(sprintf("Очищенный датасет сохранён (%d строк, %d колонок)\n",
            nrow(df_clean), ncol(df_clean)))

# --- 6. Сравнение трёх моделей ---
fml1 <- duration ~ district + case_type + text_length +
  kw_children + kw_incident + kw_absence

fml2 <- duration ~ district + case_type + text_length +
  kw_children + kw_incident + kw_absence + n_violations

fml3 <- as.formula(paste(
  "duration ~ district + case_type + text_length +",
  "kw_children + kw_incident + kw_absence +",
  paste(pattern_features, collapse = " + ")
))

set.seed(42); m1 <- randomForest(fml1, df_model, ntree = 500, importance = TRUE)
set.seed(42); m2 <- randomForest(fml2, df_model, ntree = 500, importance = TRUE)
set.seed(42); m3 <- randomForest(fml3, df_model, ntree = 500, importance = TRUE)

compare <- tibble(
  Модель = c("Базовая", "+ n_violations", "+ отдельные паттерны"),
  R2     = c(m1$rsq[500], m2$rsq[500], m3$rsq[500]) * 100,
  MAE    = c(
    mean(abs(predict(m1) - df_model$duration)),
    mean(abs(predict(m2) - df_model$duration)),
    mean(abs(predict(m3) - df_model$duration))
  )
)
print(compare)
write_csv(compare, file.path(BASE_DIR, "data", "model_comparison.csv"))

# --- 7. Финальная модель (выберите лучшую по R²/MAE) ---
model_rf <- m3   # например, с отдельными паттернами

cat("\n--- Резюме финальной модели ---\n")
print(model_rf)
n_removed <- nrow(df) - nrow(df_model)
df_clean  <- df_model %>% mutate(across(where(is.factor), as.character))
write_csv(df_clean, file.path(BASE_DIR, "data", "cases_features_clean.csv"))

# --- 8. Важность признаков ---
imp_data <- as.data.frame(importance(model_rf))
imp_data$Признак <- rownames(imp_data)
imp_data <- imp_data %>% arrange(desc(IncNodePurity))
print(imp_data)

p_imp <- ggplot(imp_data, aes(reorder(Признак, IncNodePurity), IncNodePurity)) +
  geom_col(fill = "steelblue", alpha = 0.85) + coord_flip() +
  labs(title = "Важность признаков для предсказания длительности",
       subtitle = "IncNodePurity (чем выше — тем сильнее признак влияет на срок)",
       x = NULL, y = "Важность") +
  theme_minimal(base_size = 14)

ggsave(file.path(BASE_DIR, "eda_plots", "ALL", "model_importance_duration.png"),
       p_imp, width = 9, height = 5, dpi = 150)

# --- 9. Качество финальной модели ---
predictions <- predict(model_rf)
mae  <- mean(abs(predictions - df_model$duration))
rmse <- sqrt(mean((predictions - df_model$duration)^2))

cat(sprintf("MAE: %.1f дней\n", mae))
cat(sprintf("RMSE: %.1f дней\n", rmse))
cat(sprintf("OOB R²: %.1f%%\n", model_rf$rsq[500] * 100))

# --- 10. Распределение n_violations ---
cat("\nРаспределение n_violations:\n")
print(table(df_model$n_violations))

# --- 11. Сохранение важности ---
write_csv(imp_data, file.path(BASE_DIR, "data", "model_duration_importance.csv"))

# --- 12. Markdown-отчёт ---
top3 <- head(imp_data, 3)

md_lines <- c(
  "# Результаты моделирования: Предсказание длительности дела",
  "",
  sprintf("**Дата:** %s", Sys.time()),
  sprintf("**Исходная выборка:** %d наблюдений", nrow(df)),
  sprintf("**После фильтрации:** %d наблюдений (отброшено %d)", nrow(df_model), n_removed),
  "**Целевая переменная:** Длительность рассмотрения дела (в днях)",
  "",
  "## Сравнение моделей",
  "",
  "| Модель | R², % | MAE, дней |",
  "|--------|------:|----------:|",
  sprintf("| Базовая | %.1f | %.1f |", compare$R2[1], compare$MAE[1]),
  sprintf("| + n_violations | %.1f | %.1f |", compare$R2[2], compare$MAE[2]),
  sprintf("| + отдельные паттерны | %.1f | %.1f |", compare$R2[3], compare$MAE[3]),
  "",
  "## Качество финальной модели",
  "",
  "- **Метод:** Случайный лес (регрессия, 500 деревьев)",
  sprintf("- **MAE:** %.1f дней", mae),
  sprintf("- **RMSE:** %.1f дней", rmse),
  sprintf("- **OOB R²:** %.1f%%", model_rf$rsq[500] * 100),
  "",
  "## Важность признаков",
  "",
  "| Признак | IncNodePurity |",
  "|---------|--------------:|",
  sapply(1:nrow(imp_data), function(i)
    sprintf("| %s | %.4f |", imp_data$Признак[i], imp_data$IncNodePurity[i])),
  "",
  "## Топ-3 признака",
  sapply(1:nrow(top3), function(i) sprintf("- **%s**", top3$Признак[i])),
  "",
  "## Распределение n_violations",
  "",
  paste(capture.output(print(table(df_model$n_violations))), collapse = "\n"),
  "",
  "## Файлы",
  "",
  "- `data/cases_features_clean.csv` — очищенный датасет",
  "- `data/model_comparison.csv` — сравнение трёх моделей",
  "- `data/model_duration_importance.csv` — важность признаков"
)

writeLines(md_lines, file.path(BASE_DIR, "data", "model_duration_report.md"))
cat("\nОтчёт сохранён.\nМоделирование завершено.\n")