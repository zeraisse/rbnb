library(shiny)
library(shinydashboard)

# -----------------------
# Chargement des données
# -----------------------

bangkok <- read.csv("../data/listings_bangkok.csv", stringsAsFactors = FALSE)
tokyo   <- read.csv("../data/listings_tokyo.csv", stringsAsFactors = FALSE)


# Nettoyage du prix
bangkok$price <- as.numeric(gsub("[$,]", "", bangkok$price))
tokyo$price  <- as.numeric(gsub("[$,]", "", tokyo$price))

# Suppression des NA
bangkok <- bangkok[!is.na(bangkok$price), ]
tokyo  <- tokyo[!is.na(tokyo$price), ]

# -----------------------
# Conversion en euros
# -----------------------

# Bangkok : THB → EUR
bangkok$price_eur <- bangkok$price / 39

# Tokyo : JPY → EUR
tokyo$price_eur <- tokyo$price / 160


# -----------------------
# Interface utilisateur
# -----------------------

ui <- dashboardPage(
  
  dashboardHeader(title = "Airbnb Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("chart-bar")),
      menuItem("Price Distribution", tabName = "price", icon = icon("dollar-sign"))
    )
  ),
  
  dashboardBody(
    tabItems(
      
      tabItem(tabName = "overview",
              fluidRow(
                box(title = "Bangkok - Résumé des prix",
                    verbatimTextOutput("summary_bkk"),
                    width = 6),
                
                box(title = "Tokyo - Résumé des prix",
                    verbatimTextOutput("summary_tokyo"),
                    width = 6)
              )
      ),
      
      tabItem(tabName = "price",
              fluidRow(
                box(title = "Bangkok - Histogramme",
                    plotOutput("hist_bkk"),
                    width = 6),
                
                box(title = "Tokyo - Histogramme",
                    plotOutput("hist_tokyo"),
                    width = 6)
              )
      )
    )
  )
)

# -----------------------
# Logique serveur
# -----------------------

server <- function(input, output) {
  
  output$summary_bkk <- renderPrint({
    summary(bangkok$price)
  })
  
  output$summary_tokyo <- renderPrint({
    summary(tokyo$price)
  })
  
  output$hist_bkk <- renderPlot({
    hist(bangkok$price,
         breaks = 50,
         col = "steelblue",
         main = "Bangkok - Distribution des prix",
         xlab = "Prix")
  })
  
  output$hist_tokyo <- renderPlot({
    hist(tokyo$price,
         breaks = 50,
         col = "darkred",
         main = "Tokyo - Distribution des prix",
         xlab = "Prix")
  })
}

# -----------------------
# Lancement de l'app
# -----------------------

shinyApp(ui, server)
