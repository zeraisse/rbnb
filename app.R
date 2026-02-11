library(shiny)

# ==============================================================================
# 1. GLOBAL : CHARGEMENT ET NETTOYAGE (S'exécute une seule fois au lancement)
# ==============================================================================
file_path <- "Documents/IPSSI/Rlang/Airbnb/data/tokyo/listings.csv"

if (!file.exists(file_path)) {
  stop("Le fichier CSV est introuvable. Vérifie le chemin 'file_path' ligne 8.")
}

# --- Chargement ---
tokyoData <- read.csv(file_path, stringsAsFactors = FALSE)

# --- Nettoyage préliminaire ---
# Prix
tokyoData$price <- as.numeric(gsub("[\\$,]", "", tokyoData$price))
tokyoData <- tokyoData[!is.na(tokyoData$price), ]

# Chambres
tokyoData$bedrooms <- as.numeric(tokyoData$bedrooms)

# Conversion Euros (Statique pour gagner du temps)
tokyoData$price_eur <- round(tokyoData$price * 0.0062, 2)


# ==============================================================================
# 2. UI : L'INTERFACE UTILISATEUR
# ==============================================================================
ui <- fluidPage(
  
  # Titre de l'application
  titlePanel("Analyse R'bnb Tokyo 🇯🇵"),
  
  # Barre latérale (Sidebar) pour les filtres
  sidebarLayout(
    sidebarPanel(
      h4("Filtres globaux"),
      
      # Slider pour filtrer les prix aberrants (Outliers)
      sliderInput("max_price",
                  "Prix Maximum (€) :",
                  min = 100,
                  max = 500,
                  value = 150),
      
      # Slider pour le nombre de chambres
      sliderInput("max_rooms",
                  "Nombre de chambres Max :",
                  min = 1,
                  max = 8,
                  value = 4),
      
      hr(),
      textOutput("nb_lines")
    ),
    
    # Panneau principal avec des Onglets (Tabs)
    mainPanel(
      tabsetPanel(
        # Onglet 1 : Les Tops Quartiers
        tabPanel("🏆 Top Quartiers", 
                 h3("Les quartiers les moins chers et les plus chers"),
                 # On met une grande hauteur car il y a 2 graphiques
                 plotOutput("plotQuartiers", height = "800px") 
        ),
        
        # Onglet 2 : Analyse des Chambres
        tabPanel("🛏️ Prix vs Chambres", 
                 h3("Impact du nombre de pièces sur le prix"),
                 textOutput("txtCorrelation"), # Affichage du texte de corrélation
                 plotOutput("plotBedrooms", height = "600px")
        )
      )
    )
  )
)

# ==============================================================================
# 3. SERVER : LA LOGIQUE (S'exécute à chaque interaction)
# ==============================================================================
server <- function(input, output) {
  
  # --- Données Réactives ---
  # Cette fonction filtre les données à chaque fois que l'utilisateur bouge un slider
  filtered_data <- reactive({
    data <- tokyoData
    
    # Filtre sur le prix (basé sur le slider input$max_price)
    data <- subset(data, price_eur <= input$max_price)
    
    # Filtre sur les chambres (basé sur le slider input$max_rooms)
    data <- subset(data, !is.na(bedrooms) & bedrooms <= input$max_rooms)
    
    return(data)
  })
  
  # --- Output : Compteur de lignes ---
  output$nb_lines <- renderText({
    paste("Logements analysés :", nrow(filtered_data()))
  })
  
  # --- Output : Graphique Quartiers (Onglet 1) ---
  output$plotQuartiers <- renderPlot({
    df <- filtered_data()
    
    # Agrégation (Moyenne par quartier)
    df_summary <- aggregate(price_eur ~ neighbourhood_cleansed, data = df, FUN = mean)
    names(df_summary) <- c("Quartier", "Prix_Moyen_EUR")
    df_summary$Prix_Moyen_EUR <- round(df_summary$Prix_Moyen_EUR, 0)
    
    # Tri
    df_final <- df_summary[order(df_summary$Prix_Moyen_EUR), ]
    
    # Sélection
    top_cheap <- head(df_final, 10)
    top_expensive <- tail(df_final, 10)
    
    # Affichage (2 graphes l'un sur l'autre)
    par(mfrow=c(2, 1), mar=c(8, 4, 4, 2))
    
    # Graphe 1
    barplot(top_cheap$Prix_Moyen_EUR, names.arg = top_cheap$Quartier, 
            las = 2, cex.names = 0.8, col = "#69b3a2", 
            main = "Top 10 MOINS chers (€)", ylab = "Prix Moyen (€)")
    
    # Graphe 2
    barplot(top_expensive$Prix_Moyen_EUR, names.arg = top_expensive$Quartier, 
            las = 2, cex.names = 0.8, col = "#ff6f69", 
            main = "Top 10 PLUS chers (€)", ylab = "Prix Moyen (€)")
  })
  
  # --- Output : Graphique Chambres (Onglet 2) ---
  output$plotBedrooms <- renderPlot({
    df <- filtered_data()
    
    # Boxplot
    boxplot(price_eur ~ bedrooms, data = df,
            main = "Distribution des prix par nombre de chambres",
            xlab = "Nombre de Chambres", ylab = "Prix (€)",
            col = "lightblue", outline = FALSE)
    
    # Ligne de tendance rouge
    abline(lm(price_eur ~ bedrooms, data = df), col = "red", lwd = 2)
  })
  
  # --- Output : Texte Corrélation (Onglet 2) ---
  output$txtCorrelation <- renderText({
    df <- filtered_data()
    # On recalcule la corrélation dynamiquement
    corr <- cor(df$bedrooms, df$price_eur)
    paste("📊 Coefficient de corrélation actuel :", round(corr, 2))
  })
}

# Lancement de l'application
shinyApp(ui = ui, server = server)