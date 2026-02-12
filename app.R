library(shiny)
library(shinydashboard)

# 1. Charger ton module
source("modules/tokyo_analyst.R")

# -----------------------
# Chargement des données
# -----------------------

# NOTE : Vérifie bien ces chemins selon ton dossier
# J'utilise des chemins relatifs standards. 
# Si ton CSV est dans data/tokyo/listings.csv, adapte ici :
path_bangkok <- "data/listings_bangkok.csv" # Adapte si besoin
path_tokyo   <- "data/listings_tokyo.csv"    # Adapte selon ton arborescence

# Chargement sécurisé
if(file.exists(path_bangkok)) bangkok <- read.csv(path_bangkok, stringsAsFactors = FALSE)
if(file.exists(path_tokyo))   tokyo   <- read.csv(path_tokyo, stringsAsFactors = FALSE)


# 1. BANGKOK
if(exists("bangkok")) {
  # Nettoyage du texte "$1,200.00" -> nombre
  bangkok$price <- as.numeric(gsub("[$,]", "", bangkok$price))
  
  # Conversion THB -> EUR (Taux approx : 1 EUR = 37 THB)
  bangkok$price_eur <- round(bangkok$price / 37, 2)
  
  # SUPPRESSION DES OUTLIERS (Correction des millions)
  # On ne garde que les prix entre 0 et 2000€
  bangkok <- subset(bangkok, price_eur > 0 & price_eur < 2000)
}

# 2. TOKYO (Pour l'onglet global de la collègue)
if(exists("tokyo")) {
  # On garde une version "brute" pour ton module, et une propre pour le dashboard global
  tokyo_clean <- tokyo 
  tokyo_clean$price <- as.numeric(gsub("[$,]", "", tokyo_clean$price))
  
  # Conversion JPY -> EUR (Taux approx : 1 EUR = 160 JPY)
  tokyo_clean$price_eur <- round(tokyo_clean$price / 160, 2)
  
  # SUPPRESSION DES OUTLIERS (Correction des millions)
  # On ne garde que les prix entre 0 et 2000€
  tokyo_clean <- subset(tokyo_clean, price_eur > 0 & price_eur < 2000)
}

# -----------------------
# Interface utilisateur
# -----------------------

ui <- dashboardPage(
  skin = "purple", # Un peu de style
  
  dashboardHeader(title = "R'bnb Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Global Overview", tabName = "overview", icon = icon("globe")),
      menuItem("Price Stats", tabName = "price", icon = icon("chart-line")),
      # ICI : Ton nouvel onglet
      menuItem("Tokyo (Deep Dive)", tabName = "tokyo_advanced", icon = icon("search-plus"), badgeLabel = "New", badgeColor = "green")
    )
  ),
  
  dashboardBody(
    tabItems(
      
      # --- Onglets de la collègue ---
      tabItem(tabName = "overview",
              fluidRow(
                box(title = "Bangkok - Résumé", width = 6, 
                    if(exists("bangkok")) verbatimTextOutput("summary_bkk") else "Data not found"),
                box(title = "Tokyo - Résumé", width = 6, 
                    if(exists("tokyo_clean")) verbatimTextOutput("summary_tokyo") else "Data not found")
              )
      ),
      
      tabItem(tabName = "price",
              fluidRow(
                box(title = "Bangkok - Histogramme", width = 6, plotOutput("hist_bkk")),
                box(title = "Tokyo - Histogramme", width = 6, plotOutput("hist_tokyo"))
              )
      ),
      
      # --- TON Onglet (Appel du Module) ---
      tabItem(tabName = "tokyo_advanced",
              # On appelle l'UI de ton module avec un ID unique "my_tokyo_analysis"
              tokyoAnalystUI("my_tokyo_analysis")
      )
    )
  )
)

# -----------------------
# Logique serveur
# -----------------------

server <- function(input, output) {
  
  output$summary_bkk <- renderPrint({
    req(bangkok)
    # On résume la colonne EUR maintenant
    cat("Statistiques en Euros (€) :\n")
    summary(bangkok$price_eur) 
  })
  
  output$summary_tokyo <- renderPrint({
    req(tokyo_clean)
    cat("Statistiques en Euros (€) :\n")
    summary(tokyo_clean$price_eur)
  })
  
  output$hist_bkk <- renderPlot({
    req(bangkok)
    hist(bangkok$price_eur, 
         breaks = 50, col = "steelblue", border = "white",
         main = "Bangkok - Distribution des prix (€)", 
         xlab = "Prix par nuit (€)")
  })
  
  output$hist_tokyo <- renderPlot({
    req(tokyo_clean)
    hist(tokyo_clean$price_eur, 
         breaks = 50, col = "darkred", border = "white",
         main = "Tokyo - Distribution des prix (€)", 
         xlab = "Prix par nuit (€)")
  })
  
  # 2. Appel de TON Module
  # On lui passe l'ID unique et les données brutes de Tokyo
  # Le module gérera son propre nettoyage sans casser le reste
  tokyoAnalystServer("my_tokyo_analysis", data_raw = tokyo)
}

shinyApp(ui, server)