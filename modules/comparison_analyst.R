# 1. UI du Module
comparisonUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      box(
        title = "Paramètres", width = 12, status = "warning", solidHeader = TRUE,
        sliderInput(ns("price_limit"), 
                    "Filtrer les prix max (€) pour y voir plus clair :", 
                    min = 50, max = 1000, value = 300, step = 10)
      )
    ),
    
    fluidRow(
      # Graphique 1 : Bangkok
      box(
        title = "Bangkok : Note vs Prix", width = 6, status = "primary", solidHeader = TRUE,
        plotOutput(ns("plot_bkk"), height = "400px"),
        textOutput(ns("corr_bkk"))
      ),
      
      # Graphique 2 : Tokyo
      box(
        title = "Tokyo : Note vs Prix", width = 6, status = "danger", solidHeader = TRUE,
        plotOutput(ns("plot_tok"), height = "400px"),
        textOutput(ns("corr_tok"))
      )
    )
  )
}

# 2. Server du Module
comparisonServer <- function(id, bkk_data, tokyo_data) {
  moduleServer(id, function(input, output, session) {
    
    # --- Fonction interne pour nettoyer et filtrer ---
    get_city_data <- function(df, max_price) {
      req(df)
      # On vérifie les colonnes
      if(!"review_scores_rating" %in% names(df) || !"price_eur" %in% names(df)) return(NULL)
      
      # On ne garde que les colonnes utiles
      d <- data.frame(x = df$review_scores_rating, y = df$price_eur)
      
      # On vire les NA et on applique le filtre prix
      d <- d[!is.na(d$x) & !is.na(d$y), ]
      d <- d[d$y <= max_price, ]
      return(d)
    }
    
    # --- Graphique Bangkok ---
    output$plot_bkk <- renderPlot({
      df <- get_city_data(bkk_data, input$price_limit)
      req(nrow(df) > 0)
      
      plot(df$x, df$y, 
           pch = 19, col = rgb(0.27, 0.51, 0.71, 0.4), # Bleu transparent
           xlab = "Note (Rating)", ylab = "Prix (€)",
           main = "Bangkok", las = 1)
      grid()
      
      # Ajout de la courbe de tendance (Rouge)
      if(nrow(df) > 10) {
        lines(lowess(df$x, df$y), col = "red", lwd = 3) # Courbe lissée
      }
    })
    
    output$corr_bkk <- renderText({
      df <- get_city_data(bkk_data, input$price_limit)
      req(nrow(df) > 0)
      correl <- round(cor(df$x, df$y), 2)
      paste("📊 Corrélation :", correl, "(Proche de 0 = Aucun lien)")
    })
    
    # --- Graphique Tokyo ---
    output$plot_tok <- renderPlot({
      df <- get_city_data(tokyo_data, input$price_limit)
      req(nrow(df) > 0)
      
      plot(df$x, df$y, 
           pch = 19, col = rgb(0.8, 0.2, 0.2, 0.4), # Rouge transparent
           xlab = "Note (Rating)", ylab = "Prix (€)",
           main = "Tokyo", las = 1)
      grid()
      
      # Ajout de la courbe de tendance (Bleue foncée pour contraste)
      if(nrow(df) > 10) {
        lines(lowess(df$x, df$y), col = "darkblue", lwd = 3) 
      }
    })
    
    output$corr_tok <- renderText({
      df <- get_city_data(tokyo_data, input$price_limit)
      req(nrow(df) > 0)
      correl <- round(cor(df$x, df$y), 2)
      paste("📊 Corrélation :", correl)
    })
    
  })
}