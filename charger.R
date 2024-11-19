#install.packages(c("ggplot2", "dplyr", "car", "lmtest", "nortest"))

library(ggplot2)
library(dplyr)
library(car)
library(lmtest)
library(nortest)


charger <- function(matricule) {
  set.seed(matricule)
  mondata <- read.csv2("./DevoirD_A24.csv")[sample(288,200),]
  return(mondata)
}

mes_donnees <- charger(2045362)

head(mes_donnees)

### -----------------------------  PHASE 1 --------------------------------------------

## ------- a) -------
# Histogramme
ggplot(mes_donnees, aes(x = Sales)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(title = "Histogramme des ventes", x = "Ventes (en milliers)")

# Boxplot
boxplot(mes_donnees$Sales, main = "Boxplot des ventes", horizontal = TRUE)


# Droite de Henry (Normal Q-Q Plot)
qqnorm(mes_donnees$Sales)
qqline(mes_donnees$Sales)

# Test de normalité Shapiro-Wilk
shapiro.test(mes_donnees$Sales)

# Calcul des statistiques descriptives
moyenne <- mean(mes_donnees$Sales)
ecart_type <- sd(mes_donnees$Sales)
quartiles <- quantile(mes_donnees$Sales)
IDC_moyenne <- t.test(mes_donnees$Sales)$conf.int

# Création du tableau de statistiques descriptives
tableau_descriptif <- data.frame(
  Statistiques = c("Moyenne", "1er Quartile", "Médiane", "3e Quartile", "Écart-type", "Intervalle de confiance"),
  Valeurs = c(
    moyenne,
    quartiles[2],
    quartiles[3],
    quartiles[4],
    ecart_type,
    paste0("[", round(IDC_moyenne[1], 2), ", ", round(IDC_moyenne[2], 2), "]")
  )
)

# Afficher le tableau descriptif
print(tableau_descriptif)


## ------- b) -------
# Histogrammes par région
ggplot(mes_donnees, aes(x = Sales, fill = as.factor(Region))) +
  geom_histogram(position = "dodge", binwidth = 1, alpha = 0.7) +
  labs(title = "Histogrammes des ventes par région", x = "Ventes (en milliers)", fill = "Région")

# Boxplots par région
ggplot(mes_donnees, aes(x = as.factor(Region), y = Sales)) +
  geom_boxplot() +
  labs(title = "Boxplots des ventes par région", x = "Région", y = "Ventes")

mes_donnees %>%
  group_by(Region) %>%
  summarise(
    moyenne = mean(Sales),
    ecart_type = sd(Sales),
    Q1 = quantile(Sales, 0.25),
    mediane = median(Sales),
    Q3 = quantile(Sales, 0.75),
    IC_lower = t.test(Sales)$conf.int[1],
    IC_upper = t.test(Sales)$conf.int[2]
  )

# Test égalité des variances
var.test(Sales ~ Region, data = mes_donnees)

# Test égalité des moyennes
t.test(Sales ~ Region, data = mes_donnees, var.equal = TRUE)
