# ============================================================
# app.R
# Интерактивное веб-приложение для анализа судебных решений
# по теме "Видеонаблюдение в школе" (с паттернами)
# ============================================================

library(shiny)
library(dplyr)
library(ggplot2)
library(DT)
library(readr)
library(stringr)
library(tidyr)

# --- Загрузка данных ---
BASE_DIR <- "C:/Users/rewhe/Desktop/videonabludenie"
df <- read_csv(file.path(BASE_DIR, "data", "cases_features.csv"), show_col_types = FALSE)

# Фильтрация валидных данных
df <- df %>%
  filter(
    text_length > 5000,
    !is.na(case_type),
    !str_detect(case_type, "Не определено"),
    !is.na(duration),
    duration > 0,
    duration < 365
  )

# Максимальное число нарушений для ползунка (защита от NA)
max_viol <- max(df$n_violations, na.rm = TRUE)
if (is.na(max_viol)) max_viol <- 1

# ============================================================
# UI (Интерфейс)
# ============================================================
ui <- fluidPage(
  titlePanel("Анализ судебных решений: Видеонаблюдение в школах"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Фильтры"),
      
      selectInput("district", "Федеральный округ:",
                  choices = c("Все округа" = "ALL", sort(unique(df$district))),
                  selected = "ALL"),
      
      selectInput("case_type", "Тип дела:",
                  choices = c("Все типы" = "ALL", sort(unique(df$case_type))),
                  selected = "ALL"),
      
      selectInput("pattern", "Ключевой паттерн:",
                  choices = c("Все паттерны" = "ALL",
                              "Использование как доказательство" = "pat_evidence",
                              "Нарушение хранения" = "pat_storage",
                              "Отсутствие камер" = "pat_absence_camera",
                              "Нарушение СанПиН" = "pat_violation_sanpin",
                              "Утечка данных" = "pat_data_leak",
                              "Неисправность" = "pat_malfunction"),
                  selected = "ALL"),
      
      sliderInput("duration", "Длительность рассмотрения (дни):",
                  min = 0, max = 365, value = c(0, 365)),
      
      sliderInput("n_violations", "Количество нарушений в деле:",
                  min = 0, max = max_viol, value = c(0, max_viol), step = 1),
      
      checkboxInput("only_children", "Только дела с упоминанием детей", FALSE),
      checkboxInput("only_incident", "Только дела с упоминанием инцидентов", FALSE),
      
      hr(),
      p(strong("Всего наблюдений:"), nrow(df))
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel("Обзор",
                 h3("Ключевые метрики"),
                 fluidRow(
                   column(2, wellPanel(
                     h4(textOutput("total_cases")),
                     p("Всего дел", style="font-size:12px;")
                   )),
                   column(2, wellPanel(
                     h4(textOutput("avg_duration")),
                     p("Ср. длительность", style="font-size:12px;")
                   )),
                   column(2, wellPanel(
                     h4(textOutput("avg_violations")),
                     p("Ср. нарушений", style="font-size:12px;")
                   )),
                   column(3, wellPanel(
                     h4(textOutput("children_pct")),
                     p("Доля с детьми", style="font-size:12px;")
                   )),
                   column(3, wellPanel(
                     h4(textOutput("violations_pct")),
                     p("Доля с нарушениями", style="font-size:12px;")
                   ))
                 ),
                 
                 h3("Распределение длительности"),
                 plotOutput("hist_duration", height = "350px"),
                 
                 fluidRow(
                   column(6, 
                          h3("Типы судебных дел"),
                          plotOutput("bar_case_types", height = "350px")
                   ),
                   column(6,
                          h3("Частота паттернов"),
                          plotOutput("bar_patterns", height = "350px")
                   )
                 )
        ),
        
        tabPanel("География",
                 h3("Длительность по федеральным округам"),
                 plotOutput("box_districts", height = "500px")
        ),
        
        tabPanel("Тематики",
                 h3("Упоминания ключевых тем"),
                 plotOutput("keywords", height = "400px")
        ),
        
        tabPanel("Таблица данных",
                 h3("Детальная информация по делам"),
                 DTOutput("data_table")
        )
      )
    )
  )
)

# ============================================================
# Server (Логика)
# ============================================================
server <- function(input, output, session) {
  
  # Реактивный фильтр данных
  filtered_data <- reactive({
    d <- df
    
    if (input$district != "ALL") {
      d <- d %>% filter(district == input$district)
    }
    
    if (input$case_type != "ALL") {
      d <- d %>% filter(case_type == input$case_type)
    }
    
    # Фильтрация по выбранному паттерну
    if (input$pattern != "ALL") {
      # .data[[input$pattern]] позволяет динамически обращаться к колонке по её имени
      d <- d %>% filter(.data[[input$pattern]] == TRUE)
    }
    
    d <- d %>% filter(duration >= input$duration[1] & duration <= input$duration[2])
    
    d <- d %>% filter(n_violations >= input$n_violations[1] & n_violations <= input$n_violations[2])
    
    if (input$only_children) {
      d <- d %>% filter(kw_children == TRUE)
    }
    
    if (input$only_incident) {
      d <- d %>% filter(kw_incident == TRUE)
    }
    
    d
  })
  
  # Ключевые метрики
  output$total_cases <- renderText({
    nrow(filtered_data())
  })
  
  output$avg_duration <- renderText({
    sprintf("%.1f", mean(filtered_data()$duration, na.rm = TRUE))
  })
  
  output$avg_violations <- renderText({
    sprintf("%.1f", mean(filtered_data()$n_violations, na.rm = TRUE))
  })
  
  output$children_pct <- renderText({
    pct <- mean(filtered_data()$kw_children, na.rm = TRUE) * 100
    sprintf("%.1f%%", pct)
  })
  
  output$violations_pct <- renderText({
    pct <- mean(filtered_data()$n_violations > 0, na.rm = TRUE) * 100
    sprintf("%.1f%%", pct)
  })
  
  # Гистограмма длительности
  output$hist_duration <- renderPlot({
    ggplot(filtered_data(), aes(x = duration)) +
      geom_histogram(binwidth = 10, fill = "steelblue", color = "white", alpha = 0.85) +
      labs(title = "Распределение длительности рассмотрения дел",
           x = "Длительность (дни)", y = "Количество дел") +
      theme_minimal(base_size = 14)
  })
  
  # Столбчатая диаграмма типов дел
  output$bar_case_types <- renderPlot({
    type_data <- filtered_data() %>% count(case_type)
    ggplot(type_data, aes(x = reorder(case_type, -n), y = n)) +
      geom_col(fill = "coral", alpha = 0.85) +
      labs(title = "Распределение по типам", x = NULL, y = "Количество дел") +
      theme_minimal(base_size = 14) +
      theme(axis.text.x = element_text(angle = 30, hjust = 1))
  })
  
  # График паттернов
  output$bar_patterns <- renderPlot({
    pat_summary <- tibble(
      "Доказательства" = sum(filtered_data()$pat_evidence, na.rm = TRUE),
      "Хранение" = sum(filtered_data()$pat_storage, na.rm = TRUE),
      "Нет камер" = sum(filtered_data()$pat_absence_camera, na.rm = TRUE),
      "СанПиН" = sum(filtered_data()$pat_violation_sanpin, na.rm = TRUE),
      "Утечка" = sum(filtered_data()$pat_data_leak, na.rm = TRUE),
      "Неисправность" = sum(filtered_data()$pat_malfunction, na.rm = TRUE)
    ) %>% pivot_longer(everything(), names_to = "Паттерн", values_to = "Количество")
    
    ggplot(pat_summary, aes(x = reorder(Паттерн, -Количество), y = Количество)) +
      geom_col(fill = "darkorange", alpha = 0.85) +
      labs(title = "Паттерны нарушений", x = NULL, y = "Количество дел") +
      theme_minimal(base_size = 14) +
      theme(axis.text.x = element_text(angle = 30, hjust = 1))
  })
  
  # Boxplot по округам
  output$box_districts <- renderPlot({
    ggplot(filtered_data(), aes(x = reorder(district, duration, FUN = median),
                                y = duration, fill = district)) +
      geom_boxplot(alpha = 0.8) +
      coord_flip() +
      labs(title = "Длительность по округам", x = NULL, y = "Дни") +
      theme_minimal(base_size = 14) +
      guides(fill = "none")
  })
  
  # Ключевые темы
  output$keywords <- renderPlot({
    kw_summary <- tibble(
      "Видеонаблюдение" = sum(filtered_data()$kw_camera, na.rm = TRUE),
      "Школа" = sum(filtered_data()$kw_school, na.rm = TRUE),
      "Дети" = sum(filtered_data()$kw_children, na.rm = TRUE),
      "Инциденты" = sum(filtered_data()$kw_incident, na.rm = TRUE),
      "Отсутствие" = sum(filtered_data()$kw_absence, na.rm = TRUE),
      "Обязательства" = sum(filtered_data()$kw_obligate, na.rm = TRUE)
    ) %>% pivot_longer(everything(), names_to = "Тема", values_to = "Количество")
    
    ggplot(kw_summary, aes(x = reorder(Тема, -Количество), y = Количество)) +
      geom_col(fill = "seagreen3", alpha = 0.85) +
      labs(title = "Ключевые темы (слова)", x = NULL, y = "Количество дел") +
      theme_minimal(base_size = 14) +
      theme(axis.text.x = element_text(angle = 30, hjust = 1))
  })
  
  # Таблица данных
  output$data_table <- renderDT({
    datatable(
      filtered_data() %>%
        select(id, district, court, case_type, date_in, date_out, duration, n_violations, 
               pat_evidence, pat_storage, pat_absence_camera) %>%
        mutate(
          date_in = format(date_in, "%d.%m.%Y"),
          date_out = format(date_out, "%d.%m.%Y")
        ),
      options = list(pageLength = 10, scrollX = TRUE),
      rownames = FALSE
    ) %>%
      formatStyle('duration', backgroundColor = styleInterval(
        c(30, 60), c('white', 'lightyellow', 'lightblue')
      )) %>%
      formatStyle('n_violations', backgroundColor = styleInterval(
        c(0, 2), c('white', 'lightyellow', 'lightsalmon')
      ))
  })
}

# Запуск приложения
shinyApp(ui, server)