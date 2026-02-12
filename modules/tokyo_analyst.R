# modules/tokyo_analyst.R

# 1. UI du Module
tokyoAnalystUI <- function(id) {
  # Création de l'espace de nom (Namespace)
  ns <- NS(id)
  
  tagList(
    # On garde ton layout sidebar spécifique à l'intérieur de l'onglet
    sidebarLayout(
      sidebarPanel(
        h4("Filtres Avancés (Tokyo)"),
        
        # Note l'utilisation de ns() pour les IDs
        sliderInput(ns("max_price"),
                    "Prix Maximum (€) :",
                    min = 100, max = 500, value = 150),
        
        sliderInput(ns("max_rooms"),
                    "Nombre de chambres Max :",
                    min = 1, max = 8, value = 4),
        
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
tokyoAnalystServer <- function(id, data_raw) {
  moduleServer(id, function(input, output, session) {
    
    # --- Traitement des données (Spécifique à ta logique) ---
    # On rend les données réactives et on applique TON nettoyage
    clean_data <- reactive({
      req(data_raw) # Vérifie que les données sont chargées
      df <- data_raw
      
      # Ton nettoyage spécifique
      # Note: On vérifie si price est déjà numérique pour éviter les erreurs
      if(is.character(df$price)) {
        df$price <- as.numeric(gsub("[\\$,]", "", df$price))
      }
      
      df <- df[!is.na(df$price), ]
      df$bedrooms <- as.numeric(df$bedrooms)
      
      # Ta conversion Euro spécifique
      df$price_eur <- round(df$price * 0.0062, 2)
      
      return(df)
    })
    
    # --- Filtrage Réactif (Tes Sliders) ---
    filtered_data <- reactive({
      df <- clean_data()
      
      # Filtres
      df <- subset(df, price_eur <= input$max_price)
      df <- subset(df, !is.na(bedrooms) & bedrooms <= input$max_rooms)
      
      return(df)
    })
    
    # --- Tes Outputs ---
    
    output$nb_lines <- renderText({
      paste("Logements analysés :", nrow(filtered_data()))
    })
    
    output$plotQuartiers <- renderPlot({
      df <- filtered_data()
      req(nrow(df) > 0)
      
      df_summary <- aggregate(price_eur ~ neighbourhood_cleansed, data = df, FUN = mean)
      names(df_summary) <- c("Quartier", "Prix_Moyen_EUR")
      df_summary$Prix_Moyen_EUR <- round(df_summary$Prix_Moyen_EUR, 0)
      
      df_final <- df_summary[order(df_summary$Prix_Moyen_EUR), ]
      
      top_cheap <- head(df_final, 10)
      top_expensive <- tail(df_final, 10)
      
      par(mfrow=c(2, 1), mar=c(8, 4, 4, 2))
      
      barplot(top_cheap$Prix_Moyen_EUR, names.arg = top_cheap$Quartier, 
              las = 2, cex.names = 0.8, col = "#69b3a2", 
              main = "Top 10 MOINS chers (€)", ylab = "Prix Moyen (€)")
      
      barplot(top_expensive$Prix_Moyen_EUR, names.arg = top_expensive$Quartier, 
              las = 2, cex.names = 0.8, col = "#ff6f69", 
              main = "Top 10 PLUS chers (€)", ylab = "Prix Moyen (€)")
    })
    
    output$plotBedrooms <- renderPlot({
      df <- filtered_data()
      req(nrow(df) > 0)
      
      boxplot(price_eur ~ bedrooms, data = df,
              main = "Distribution des prix par nombre de chambres",
              xlab = "Nombre de Chambres", ylab = "Prix (€)",
              col = "lightblue", outline = FALSE)
      
      abline(lm(price_eur ~ bedrooms, data = df), col = "red", lwd = 2)
    })
    
    output$txtCorrelation <- renderText({
      df <- filtered_data()
      req(nrow(df) > 2) # Besoin d'au moins 2 points pour une corrélation
      corr <- cor(df$bedrooms, df$price_eur)
      paste("📊 Coefficient de corrélation actuel :", round(corr, 2))
    })
    
  })
}