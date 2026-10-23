# csv_coverter.R
# используется для вывода данных с global environment

# 1. Create an empty table to hold our documentation
doc <- data.frame(
  Name = character(),
  Category = character(),
  Value_or_Signature = character(),
  Explanation_Notes = character(), # Empty column for your future notes
  stringsAsFactors = FALSE
)

# 2. Extract the Values (the "name: num" part)
vals <- c("BASE_DIR", "BASE_URL", "DEFAULT_QUERY", 
          "DELAY_BETWEEN_DOWNLOADS", "DELAY_BETWEEN_PAGES", 
          "MAX_RETRIES", "PAUSE_AFTER_CONSECUTIVE_FAILS", "RETRY_DELAY")

for (v in vals) {
  doc <- rbind(doc, data.frame(
    Name = v,
    Category = "Value",
    Value_or_Signature = paste(get(v), collapse = ", "),
    Explanation_Notes = ""
  ))
}

# 3. Extract the DISTRICTS list summary
doc <- rbind(doc, data.frame(
  Name = "DISTRICTS",
  Category = "Data List",
  Value_or_Signature = paste("List of", length(DISTRICTS), "items. Names:", paste(names(DISTRICTS), collapse=", ")),
  Explanation_Notes = ""
))

# 4. Extract the Functions (name + what arguments they take)
funcs <- c("convert_single_case", "download_district", "eval_js", 
           "get_html", "main", "safe_get", "select_district")

for (f in funcs) {
  # Get the function signature (arguments)
  sig <- paste(deparse(args(get(f))), collapse = " ")
  doc <- rbind(doc, data.frame(
    Name = f,
    Category = "Function",
    Value_or_Signature = sig,
    Explanation_Notes = ""
  ))
}

# 5. Save it as a CSV
write.csv(doc, "envs_documentation.csv", row.names = FALSE)

# 6. View it in RStudio to check
View(doc)