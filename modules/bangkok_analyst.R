# modules/bangkok_analyst.R

# 1. UI du Module
bangkokAnalystUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    sidebarLayout(
      sidebarPanel(
        h4("Filtres Avancés (Bangkok)"),
        
        sliderInput(ns("max_price"),
                    "Prix Maximum (€) :",
                    min = 10, max = 500, value = 120),
        
        sliderInput(ns("max_rooms"),
                    "Nombre de chambres Max :",
                    min = 0, max = 8, value = 3),
        
        hr(),
        textOutput(ns("nb_lines"))
      ),
      
      mainPanel(
        tabsetPanel(
          tabPanel("🏆 Top Quartiers",
                   h3("Les quartiers les moins chers et les plus chers"),
                   plotOutput(ns("plotQuartiers"), height = "800px")
          ),
          
          tabPanel("🛏️ Prix vs Chambres",
                   h3("Impact du nombre de pièces sur le prix"),
                   textOutput(ns("txtCorrelation")),
                   plotOutput(ns("plotBedrooms"), height = "600px")
          )
        )
      )
    )
  )
}

# 2. Server du Module
bangkokAnalystServer <- function(id, data_raw) {
  moduleServer(id, function(input, output, session) {
    
    # --- Nettoyage / préparation des données ---
    clean_data <- reactive({
      req(data_raw)
      df <- data_raw
      
      # price peut arriver en character avec "$" et ","
      if (is.character(df$price)) {
        df$price[df$price == ""] <- NA
        df$price <- as.numeric(gsub("[\\$,]", "", df$price))
      }
      
      # supprimer les NA price
      df <- df[!is.na(df$price), ]
      
      # bedrooms en numérique (peut contenir NA)
      df$bedrooms <- suppressWarnings(as.numeric(df$bedrooms))
      
      # Conversion THB -> EUR (1 EUR ≈ 39 THB)
      df$price_eur <- round(df$price / 39, 2)
      
      # Optionnel mais utile : retirer valeurs aberrantes évidentes (ex: > 2000€)
      # (tu peux commenter ces 2 lignes si tu ne veux pas)
      df <- subset(df, price_eur <= 2000)
      
      return(df)
    })
    
    # --- Filtrage réactif selon les sliders ---
    filtered_data <- reactive({
      df <- clean_data()
      
      # Filtre prix en EUR
      df <- subset(df, price_eur <= input$max_price)
      
      # Filtre chambres
      df <- subset(df, !is.na(bedrooms) & bedrooms <= input$max_rooms)
      
      return(df)
    })
    
    # --- Outputs ---
    output$nb_lines <- renderText({
      paste("Logements analysés :", nrow(filtered_data()))
    })
    
    output$plotQuartiers <- renderPlot({
      df <- filtered_data()
      req(nrow(df) > 0)
      req("neighbourhood_cleansed" %in% names(df))
      
      df_summary <- aggregate(price_eur ~ neighbourhood_cleansed, data = df, FUN = mean)
      names(df_summary) <- c("Quartier", "Prix_Moyen_EUR")
      df_summary$Prix_Moyen_EUR <- round(df_summary$Prix_Moyen_EUR, 0)
      
      df_final <- df_summary[order(df_summary$Prix_Moyen_EUR), ]
      
      top_cheap <- head(df_final, 10)
      top_expensive <- tail(df_final, 10)
      
      par(mfrow = c(2, 1), mar = c(8, 4, 4, 2))
      
      barplot(top_cheap$Prix_Moyen_EUR,
              names.arg = top_cheap$Quartier,
              las = 2, cex.names = 0.8, col = "#69b3a2",
              main = "Top 10 MOINS chers (€)",
              ylab = "Prix Moyen (€)")
      
      barplot(top_expensive$Prix_Moyen_EUR,
              names.arg = top_expensive$Quartier,
              las = 2, cex.names = 0.8, col = "#ff6f69",
              main = "Top 10 PLUS chers (€)",
              ylab = "Prix Moyen (€)")
    })
    
    output$plotBedrooms <- renderPlot({
      df <- filtered_data()
      req(nrow(df) > 0)
      
      # garder uniquement bedrooms raisonnables pour lisibilité
      df <- subset(df, bedrooms <= 10)
      
      boxplot(price_eur ~ bedrooms, data = df,
              main = "Distribution des prix par nombre de chambres",
              xlab = "Nombre de Chambres", ylab = "Prix (€)",
              col = "lightblue", outline = FALSE)
      
      # ligne de tendance
      abline(lm(price_eur ~ bedrooms, data = df), col = "red", lwd = 2)
    })
    
    output$txtCorrelation <- renderText({
      df <- filtered_data()
      req(nrow(df) > 2)
      
      # corr nécessite 2 variables numériques sans NA
      df2 <- df[!is.na(df$bedrooms) & !is.na(df$price_eur), ]
      req(nrow(df2) > 2)
      
      corr <- cor(df2$bedrooms, df2$price_eur)
      paste("📊 Coefficient de corrélation actuel :", round(corr, 2))
    })
    
  })
}
