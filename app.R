# app.R

library(shiny)
library(shinydashboard)

# -----------------------
# Charger les modules
# -----------------------
source("modules/tokyo_analyst.R")
source("modules/bangkok_analyst.R")

# -----------------------
# Chargement des données
# -----------------------
path_bangkok <- "data/listings_bangkok.csv"
path_tokyo   <- "data/listings_tokyo.csv"

bangkok <- NULL
tokyo   <- NULL

if (file.exists(path_bangkok)) bangkok <- read.csv(path_bangkok, stringsAsFactors = FALSE)
if (file.exists(path_tokyo))   tokyo   <- read.csv(path_tokyo, stringsAsFactors = FALSE)

# -----------------------
# Préparation "globale" (overview + price stats)
# -----------------------
bangkok_clean <- NULL
tokyo_clean   <- NULL

# BANGKOK
if (!is.null(bangkok)) {
  bangkok_clean <- bangkok
  
  if (is.character(bangkok_clean$price)) {
    bangkok_clean$price[bangkok_clean$price == ""] <- NA
    bangkok_clean$price <- as.numeric(gsub("[\\$,]", "", bangkok_clean$price))
  }
  
  bangkok_clean <- bangkok_clean[!is.na(bangkok_clean$price), ]
  
  # THB -> EUR (approx : 1 EUR = 39 THB)
  bangkok_clean$price_eur <- round(bangkok_clean$price / 39, 2)
  
  # Outliers (visu global)
  bangkok_clean <- subset(bangkok_clean, price_eur > 0 & price_eur < 2000)
}

# TOKYO
if (!is.null(tokyo)) {
  tokyo_clean <- tokyo
  
  if (is.character(tokyo_clean$price)) {
    tokyo_clean$price[tokyo_clean$price == ""] <- NA
    tokyo_clean$price <- as.numeric(gsub("[\\$,]", "", tokyo_clean$price))
  }
  
  tokyo_clean <- tokyo_clean[!is.na(tokyo_clean$price), ]
  
  # JPY -> EUR (approx : 1 EUR = 160 JPY)
  tokyo_clean$price_eur <- round(tokyo_clean$price / 160, 2)
  
  # Outliers (visu global)
  tokyo_clean <- subset(tokyo_clean, price_eur > 0 & price_eur < 2000)
}

# -----------------------
# UI
# -----------------------
ui <- dashboardPage(
  skin = "purple",
  
  dashboardHeader(title = "R'bnb Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Global Overview", tabName = "overview", icon = icon("globe")),
      menuItem("Price Stats", tabName = "price", icon = icon("chart-line")),
      menuItem("Bangkok (Deep Dive)", tabName = "bangkok_advanced",
               icon = icon("search-plus"), badgeLabel = "New", badgeColor = "green"),
      menuItem("Tokyo (Deep Dive)", tabName = "tokyo_advanced",
               icon = icon("search-plus"))
    )
  ),
  
  dashboardBody(
    tabItems(
      
      # -----------------------
      # OVERVIEW AMÉLIORÉ
      # -----------------------
      tabItem(tabName = "overview",
              
              fluidRow(
                valueBoxOutput("bkk_n", width = 3),
                valueBoxOutput("bkk_med", width = 3),
                valueBoxOutput("tok_n", width = 3),
                valueBoxOutput("tok_med", width = 3)
              ),
              
              fluidRow(
                box(title = "Bangkok - Indicateurs", width = 6, status = "primary", solidHeader = TRUE,
                    tableOutput("bkk_kpis")),
                box(title = "Tokyo - Indicateurs", width = 6, status = "primary", solidHeader = TRUE,
                    tableOutput("tok_kpis"))
              ),
              
              fluidRow(
                box(title = "Comparaison Bangkok vs Tokyo (Prix médian en €)", width = 12,
                    status = "warning", solidHeader = TRUE,
                    plotOutput("compare_median_plot", height = 300))
              ),
              
              fluidRow(
                box(title = "Bangkok - Types de logement (Top 5)", width = 6,
                    status = "info", solidHeader = TRUE,
                    plotOutput("roomtype_bkk", height = 300)),
                box(title = "Tokyo - Types de logement (Top 5)", width = 6,
                    status = "info", solidHeader = TRUE,
                    plotOutput("roomtype_tok", height = 300))
              )
      ),
      
      # -----------------------
      # PRICE STATS AMÉLIORÉ (outliers + log + boxplot)
      # -----------------------
      tabItem(tabName = "price",
              
              fluidRow(
                box(title = "Réglages", width = 12, status = "primary", solidHeader = TRUE,
                    sliderInput("trim_q",
                                "Coupe des outliers (quantile max conservé)",
                                min = 0.95, max = 0.995, value = 0.99, step = 0.005),
                    radioButtons("scale_mode",
                                 "Échelle :",
                                 choices = c("Linéaire (€)" = "linear", "Log(€)" = "log"),
                                 inline = TRUE)
                )
              ),
              
              fluidRow(
                box(title = "Bangkok - Distribution", width = 6, status = "info", solidHeader = TRUE,
                    plotOutput("hist_bkk")),
                box(title = "Tokyo - Distribution", width = 6, status = "info", solidHeader = TRUE,
                    plotOutput("hist_tokyo"))
              ),
              
              fluidRow(
                box(title = "Comparaison Bangkok vs Tokyo (Boxplot, même coupe)", width = 12,
                    status = "warning", solidHeader = TRUE,
                    plotOutput("box_compare", height = 350))
              )
      ),
      
      # -----------------------
      # MODULE BANGKOK
      # -----------------------
      tabItem(tabName = "bangkok_advanced",
              bangkokAnalystUI("my_bangkok_analysis")
      ),
      
      # -----------------------
      # MODULE TOKYO
      # -----------------------
      tabItem(tabName = "tokyo_advanced",
              tokyoAnalystUI("my_tokyo_analysis")
      )
    )
  )
)

# -----------------------
# SERVER
# -----------------------
server <- function(input, output) {
  
  # ---- VALUE BOXES ----
  output$bkk_n <- renderValueBox({
    req(bangkok_clean)
    valueBox(
      value = format(nrow(bangkok_clean), big.mark = " "),
      subtitle = "Bangkok - annonces (prix OK)",
      icon = icon("house"),
      color = "aqua"
    )
  })
  
  output$bkk_med <- renderValueBox({
    req(bangkok_clean)
    valueBox(
      value = paste0(round(median(bangkok_clean$price_eur, na.rm = TRUE), 0), " €"),
      subtitle = "Bangkok - prix médian",
      icon = icon("euro-sign"),
      color = "green"
    )
  })
  
  output$tok_n <- renderValueBox({
    req(tokyo_clean)
    valueBox(
      value = format(nrow(tokyo_clean), big.mark = " "),
      subtitle = "Tokyo - annonces (prix OK)",
      icon = icon("city"),
      color = "aqua"
    )
  })
  
  output$tok_med <- renderValueBox({
    req(tokyo_clean)
    valueBox(
      value = paste0(round(median(tokyo_clean$price_eur, na.rm = TRUE), 0), " €"),
      subtitle = "Tokyo - prix médian",
      icon = icon("euro-sign"),
      color = "green"
    )
  })
  
  # ---- KPI TABLES ----
  kpi_table <- function(df) {
    
    pct_entire <- if ("room_type" %in% names(df)) {
      round(mean(df$room_type == "Entire home/apt", na.rm = TRUE) * 100, 1)
    } else NA
    
    med_rating <- if ("review_scores_rating" %in% names(df)) {
      round(median(df$review_scores_rating, na.rm = TRUE), 2)
    } else NA
    
    data.frame(
      KPI = c("Prix moyen (€)", "Prix médian (€)", "Prix Q1 (€)", "Prix Q3 (€)",
              "% logements entiers", "Note médiane"),
      Valeur = c(
        round(mean(df$price_eur, na.rm = TRUE), 1),
        round(median(df$price_eur, na.rm = TRUE), 1),
        round(quantile(df$price_eur, 0.25, na.rm = TRUE), 1),
        round(quantile(df$price_eur, 0.75, na.rm = TRUE), 1),
        pct_entire,
        med_rating
      ),
      check.names = FALSE
    )
  }
  
  output$bkk_kpis <- renderTable({
    req(bangkok_clean)
    kpi_table(bangkok_clean)
  }, striped = TRUE, hover = TRUE, spacing = "s")
  
  output$tok_kpis <- renderTable({
    req(tokyo_clean)
    kpi_table(tokyo_clean)
  }, striped = TRUE, hover = TRUE, spacing = "s")
  
  # ---- COMPARISON PLOT (Median €) ----
  output$compare_median_plot <- renderPlot({
    req(bangkok_clean, tokyo_clean)
    meds <- c(
      Bangkok = median(bangkok_clean$price_eur, na.rm = TRUE),
      Tokyo   = median(tokyo_clean$price_eur, na.rm = TRUE)
    )
    barplot(meds,
            main = "Prix médian par nuit (€)",
            ylab = "€",
            col = c("steelblue", "darkred"))
  })
  
  # ---- ROOM TYPE TOP 5 ----
  output$roomtype_bkk <- renderPlot({
    req(bangkok_clean)
    req("room_type" %in% names(bangkok_clean))
    t <- sort(table(bangkok_clean$room_type), decreasing = TRUE)[1:5]
    barplot(t, las = 2, col = "steelblue", main = "Bangkok")
  })
  
  output$roomtype_tok <- renderPlot({
    req(tokyo_clean)
    req("room_type" %in% names(tokyo_clean))
    t <- sort(table(tokyo_clean$room_type), decreasing = TRUE)[1:5]
    barplot(t, las = 2, col = "darkred", main = "Tokyo")
  })
  
  # ---- PRICE STATS (outliers + log + boxplot) ----
  trim_prices <- function(x, q) {
    x <- x[!is.na(x)]
    if (length(x) == 0) return(x)
    cut <- as.numeric(stats::quantile(x, q, na.rm = TRUE))
    x[x <= cut]
  }
  
  output$hist_bkk <- renderPlot({
    req(bangkok_clean)
    q <- input$trim_q
    x <- trim_prices(bangkok_clean$price_eur, q)
    req(length(x) > 0)
    
    if (input$scale_mode == "log") {
      x <- log(x)
      hist(x,
           breaks = 40,
           col = "steelblue", border = "white",
           main = paste0("Bangkok - Distribution log(€) (≤ Q", q, ")"),
           xlab = "log(Prix €)")
    } else {
      hist(x,
           breaks = 40,
           col = "steelblue", border = "white",
           main = paste0("Bangkok - Distribution (€) (≤ Q", q, ")"),
           xlab = "Prix par nuit (€)")
    }
  })
  
  output$hist_tokyo <- renderPlot({
    req(tokyo_clean)
    q <- input$trim_q
    x <- trim_prices(tokyo_clean$price_eur, q)
    req(length(x) > 0)
    
    if (input$scale_mode == "log") {
      x <- log(x)
      hist(x,
           breaks = 40,
           col = "darkred", border = "white",
           main = paste0("Tokyo - Distribution log(€) (≤ Q", q, ")"),
           xlab = "log(Prix €)")
    } else {
      hist(x,
           breaks = 40,
           col = "darkred", border = "white",
           main = paste0("Tokyo - Distribution (€) (≤ Q", q, ")"),
           xlab = "Prix par nuit (€)")
    }
  })
  
  output$box_compare <- renderPlot({
    req(bangkok_clean, tokyo_clean)
    q <- input$trim_q
    
    bkk <- trim_prices(bangkok_clean$price_eur, q)
    tok <- trim_prices(tokyo_clean$price_eur, q)
    
    req(length(bkk) > 0, length(tok) > 0)
    
    if (input$scale_mode == "log") {
      boxplot(list(Bangkok = log(bkk), Tokyo = log(tok)),
              col = c("steelblue", "darkred"),
              main = paste0("Boxplot log(€) (≤ Q", q, ")"),
              ylab = "log(Prix €)",
              outline = FALSE)
    } else {
      boxplot(list(Bangkok = bkk, Tokyo = tok),
              col = c("steelblue", "darkred"),
              main = paste0("Boxplot (€) (≤ Q", q, ")"),
              ylab = "Prix par nuit (€)",
              outline = FALSE)
    }
  })
  
  # ---- MODULES (données brutes) ----
  bangkokAnalystServer("my_bangkok_analysis", data_raw = bangkok)
  tokyoAnalystServer("my_tokyo_analysis", data_raw = tokyo)
}

shinyApp(ui, server)
