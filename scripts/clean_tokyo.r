
tokyoData <- read.csv("Documents/IPSSI/Rlang/Airbnb/data/tokyo/listings.csv", stringsAsFactors = FALSE)
summary(tokyoData)

# Nettoyage du texte : on enlève le '$' et la ',' pour convertir en nombres
# gsub remplace les caractères indésirables par "" (rien)
tokyoData$price <- as.numeric(gsub("[\\$,]", "", tokyoData$price))
print(paste("Nombre de NA avant suppression :", sum(is.na(tokyoData$price))))
tokyoData <- tokyoData[!is.na(tokyoData$price), ]
summary(tokyoData$price)


# On enleve la notation scientifique (1e+06)
options(scipen=999)
# Affiche un graphique boîte à moustaches
boxplot(tokyoData$price, 
        main="Distribution des prix (avec outliers)", 
        ylab="Prix (Yen)")

# On ne garde que les prix inférieurs à 500 000
tokyoData <- subset(tokyoData, price < 500000)

# Vérifie le nouveau summary
summary(tokyoData$price)


# breaks = 50 permet d'avoir plus de barres pour plus de précision
hist(tokyoData$price, 
     breaks = 50, 
     col = "lightblue", 
     main = "Distribution des prix Airbnb à Tokyo (filtré)",
     xlab = "Prix (Yen)")

# la moyenne
abline(v = mean(tokyoData$price), col = "red", lwd = 2)
