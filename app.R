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

# --- Nettoyage basique pour la vue globale (Collègue) ---
# (Ton module refera son propre nettoyage plus précis en interne)
if(exists("bangkok")) {
  bangkok$price <- as.numeric(gsub("[$,]", "", bangkok$price))
  bangkok <- bangkok[!is.na(bangkok$price), ]
  bangkok$price_eur <- bangkok$price / 39
}

if(exists("tokyo")) {
  # On garde une version "brute" pour ton module, et une propre pour le dashboard global
  tokyo_clean <- tokyo 
  tokyo_clean$price <- as.numeric(gsub("[$,]", "", tokyo_clean$price))
  tokyo_clean <- tokyo_clean[!is.na(tokyo_clean$price), ]
  tokyo_clean$price_eur <- tokyo_clean$price / 160 # Taux de la collègue
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
  
  # 1. Logique de la collègue (Globale)
  output$summary_bkk <- renderPrint({ req(bangkok); summary(bangkok$price) })
  output$summary_tokyo <- renderPrint({ req(tokyo_clean); summary(tokyo_clean$price) })
  
  output$hist_bkk <- renderPlot({
    req(bangkok)
    hist(bangkok$price, breaks = 50, col = "steelblue", main = "Bangkok", xlab = "Prix")
  })
  
  output$hist_tokyo <- renderPlot({
    req(tokyo_clean)
    hist(tokyo_clean$price, breaks = 50, col = "darkred", main = "Tokyo", xlab = "Prix")
  })
  
  # 2. Appel de TON Module
  # On lui passe l'ID unique et les données brutes de Tokyo
  # Le module gérera son propre nettoyage sans casser le reste
  tokyoAnalystServer("my_tokyo_analysis", data_raw = tokyo)
}

shinyApp(ui, server)