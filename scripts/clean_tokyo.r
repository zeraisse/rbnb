# ==============================================================================
# 1. CHARGEMENT ET NETTOYAGE
# ==============================================================================
file_path <- "Documents/IPSSI/Rlang/Airbnb/data/tokyo/listings.csv"
tokyoData <- read.csv(file_path, stringsAsFactors = FALSE)

# Nettoyage prix (texte -> nombre)
tokyoData$price <- as.numeric(gsub("[\\$,]", "", tokyoData$price))

# Suppression NA et Outliers (> 500 000 Yens)
tokyoData <- tokyoData[!is.na(tokyoData$price), ]
tokyoData <- subset(tokyoData, price < 500000)

# Conversion Yens -> Euros (1 JPY = 0.0062 EUR)
tokyoData$price_eur <- round(tokyoData$price * 0.0062, 2)

# ==============================================================================
# 2. AGRÉGATION PAR QUARTIER
# ==============================================================================
# Calcul de la moyenne par quartier
df_summary <- aggregate(
  price_eur ~ neighbourhood_cleansed, 
  data = tokyoData, 
  FUN = mean
)

# Renommer les colonnes et arrondir
names(df_summary) <- c("Quartier", "Prix_Moyen_EUR")
df_summary$Prix_Moyen_EUR <- round(df_summary$Prix_Moyen_EUR, 0)

# Tri croissant (du moins cher au plus cher)
df_final <- df_summary[order(df_summary$Prix_Moyen_EUR), ]

# ==============================================================================
# 3. PRÉPARATION DES DONNÉES À AFFICHER
# ==============================================================================
# Les 10 moins chers (tête de liste)
top_10_cheap <- head(df_final, 10)

# Les 10 plus chers (queue de liste)
top_10_expensive <- tail(df_final, 10)

# Affichage console pour vérification
cat("--- TOP 5 MOINS CHERS ---\n")
print(head(df_final, 5))
cat("\n--- TOP 5 PLUS CHERS ---\n")
print(tail(df_final, 5))

# ==============================================================================
# 4. GÉNÉRATION DES GRAPHIQUES
# ==============================================================================

# Diviser la fenêtre graphique en 2 lignes, 1 colonne (pour avoir 2 graphes l'un sous l'autre)
par(mfrow=c(2, 1))

# Réglage des marges (Bas, Gauche, Haut, Droite) - La marge du bas (10) est grande pour les noms
par(mar=c(10, 4, 4, 2)) 

# GRAPHIQUE 1 : Les Moins Chers (Vert)
barplot(
  top_10_cheap$Prix_Moyen_EUR, 
  names.arg = top_10_cheap$Quartier, 
  las = 2,              # Texte vertical
  cex.names = 0.8,      # Taille du texte un peu plus petite
  col = "lightgreen", 
  main = "Top 10 des quartiers les MOINS chers à Tokyo (€)",
  ylab = "Prix Moyen (€)"
)

# GRAPHIQUE 2 : Les Plus Chers (Saumon)
barplot(
  top_10_expensive$Prix_Moyen_EUR, 
  names.arg = top_10_expensive$Quartier, 
  las = 2, 
  cex.names = 0.8,
  col = "salmon", 
  main = "Top 10 des quartiers les PLUS chers à Tokyo (€)",
  ylab = "Prix Moyen (€)"
)

# Remettre la grille graphique par défaut (optionnel)
par(mfrow=c(1, 1))