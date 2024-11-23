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


### -----------------------------  PHASE 2 --------------------------------------------

## ------- c) -------

donnees_modele <- mes_donnees[c("Sales", "Price", "Age")]
colnames(donnees_modele) <- c("Y", "X1", "X2")

square <- function(x) {
  return(x^2)
}

modeles <- list(
  ## modele 1
  list(variableGauche="Y", variableDroite="X1", formuleGauche=identity, formuleDroite=identity, transformation_B0=identity, formule = function (b0, b1, x) { return(b0 + b1 * x) }),
  ## modele 2
  list(variableGauche="Y", variableDroite="X2", formuleGauche=identity, formuleDroite=identity, transformation_B0=identity, formule = function (b0, b1, x) { return(b0 + b1 * x) }),
  ## modele 3
  list(variableGauche="Y", variableDroite="X1", formuleGauche=identity, formuleDroite=square, transformation_B0=identity, formule = function (b0, b1, x) { return(b0 + b1 * (x ^2)) }),
  ## modele 4
  list(variableGauche="Y", variableDroite="X2", formuleGauche=identity, formuleDroite=square, transformation_B0=identity, formule = function (b0, b1, x) { return(b0 + b1 * (x ^2)) }),
  ## modele 5
  list(variableGauche="Y", variableDroite="X1", formuleGauche=log, formuleDroite=identity, transformation_B0=exp, formule = function (b0, b1, x) { return(b0 * exp(b1 * x))}),
  ## modele 6
  list(variableGauche="Y", variableDroite="X2", formuleGauche=log, formuleDroite=identity, transformation_B0=exp, formule = function (b0, b1, x) { return(b0 * exp(b1 * x))}),
  ## modele 7
  list(variableGauche="Y", variableDroite="X1", formuleGauche=log, formuleDroite=log, transformation_B0=exp, formule = function (b0, b1, x) { return(b0 * (x^ b1))}),
  ## modele 8
  list(variableGauche="Y", variableDroite="X2", formuleGauche=log, formuleDroite=log, transformation_B0=exp, formule = function (b0, b1, x) { return(b0 * (x ^ b1))})
)

analyseModeles <- data.frame()

for (i in seq_along(modeles)) {
  cat("\n\n------- Modèle", i, "-------\n")
  formuleDroite <- modeles[[i]]$formuleDroite
  formuleGauche <- modeles[[i]]$formuleGauche
  variableDroite <- formuleDroite(donnees_modele[[modeles[[i]]$variableDroite]])
  variableGauche <- formuleGauche(donnees_modele[[modeles[[i]]$variableGauche]])
  
  model <- lm(variableGauche ~ variableDroite, data = donnees_modele)
  coefficientCorrelation <- summary(model)$r.squared
  cat("Le coefficient de détermination R^2 est :", coefficientCorrelation, "\n")
  
  ## ------- c) 1: -------
  b <- coef(model)[2]
  a <- coef(model)[1]
  coefficientsRegression <- data.frame(a = as.numeric(a), b = as.numeric(b))
  
  print(coefficientsRegression)
  
  b0 <- modeles[[i]]$transformation_B0(a)
  b1 <- b
  coefficients <- data.frame(b0 = as.numeric(b0), b1 = as.numeric(b1))
  
  print(coefficients)
  analyseVariance = anova(model)
  print(analyseVariance)
  
  ## ------- c) 2: -------
  seuilB0PValue <- 0.05
  accepteVariance <- analyseVariance[["Pr(>F)"]][1] < seuilPValue
  accepteVariance
  analyseVariance[["Pr(>F)"]][1]
  
  if (accepteVariance) {
    cat("\nH0 rejeté, car le seuil (0.05) est supérieur à ", analyseVariance[["Pr(>F)"]][1])
    cat("\nLe modèle est donc globalement significatif")
  }else{
    cat("\nH0 accepté, car le seuil (0.05) est inférieur à ", analyseVariance[["Pr(>F)"]][1])
    cat("\nLe modèle n'est donc PAS globalement significatif")
  }
  
  ## Test/Analyse de normalité
  seuilNormalitePValue <- 0.05
  residus <- residuals(model)
  normalitePValue <- shapiro.test(residus)$p.value
  residuNormalementDistribue <- normalitePValue >= seuilNormalitePValue
  if (residuNormalementDistribue) {
    cat("Les résidus suivent une distribution normale au seuil", normalitePValue)
  } else {
    cat("Les résidus NE suivent PAS une distribution normale au seuil", normalitePValue)
  }
  
  qqnorm(residus, main = c("Normalité des résidus - Modele", i), xlab = modeles[[i]]$variableDroite, ylab = modeles[[i]]$variableGauche)
  qqline(residus, col = "red", lwd = 2)
  
  ## Test/Analyse de l'homoscédasticité
  seuilHomoscédasticitéPValue <- 0.05
  bp_test <- bptest(model)
  varianceConstante <- bp_test$p.value >= seuilHomoscédasticitéPValue;
  if (varianceConstante) {
    cat("\nLa variance est constante au seuil", bp_test$p.value)
  } else {
    cat("\nLa variance N'est PAS constante au seuil", bp_test$p.value)
  }
  valeurs_predites <- fitted(model)
  plot(valeurs_predites, residus, 
       main = c("Résidus vs Valeurs prédites - Modele", i), 
       xlab = "Valeurs prédites", 
       ylab = "Résidus", 
       pch = 19, col = "blue")
  abline(h = 0, col = "red", lwd = 2)
  
  ## Trouver les points atypiques
  residus_standardises <- rstandard(model)
  seuil_residus <- 2
  points_atypiques <- which(abs(residus_standardises) > seuil_residus)
  points(valeurs_predites[points_atypiques], residus[points_atypiques], 
         col = alpha("red", 1), pch = 21, cex = 1.5)
  
  
  ## ------- c) 3: -------
  seuilConfiance <- 0.95
  conf <- confint(model, level=seuilConfiance)
  conf_b0 <- modeles[[i]]$transformation_B0(conf[1, ])
  cat("\n\nB0 est situé au seuil de", seuilConfiance , "dans l'interval: ", conf_b0)
  
  conf_b1 <- conf[2, ]
  cat("\nB1 est situé au seuil de", seuilConfiance , "dans l'interval: ", conf_b1)
  
  analyseModeles <- rbind(analyseModeles, data.frame(b0 = b0,
                                                     b1 = b1,
                                                     r2 = coefficientCorrelation,
                                                     accepteVariance = accepteVariance,
                                                     pValueVariance = analyseVariance[["Pr(>F)"]][1],
                                                     residueNormaux = residuNormalementDistribue,
                                                     pValueNormalite = normalitePValue,
                                                     varianceConstante = varianceConstante,
                                                     pValueConstance = bp_test$p.value))
  rownames(analyseModeles)[nrow(analyseModeles)] <- i
  
  plot(formuleDroite(donnees_modele[[modeles[[i]]$variableDroite]]),
       formuleGauche(donnees_modele[[modeles[[i]]$variableGauche]]), 
       main = c("Analyse Linéaire - Modele", i),
       pch = 19, col = "blue")
  
  abline(a, b, col = "red", lwd = 2)
}

## ------- d) -------


donnesPrediction <- data.frame(X1 = 115, X2 = 35, X3 = 1)


numeroModeleChoisi = 1

b0 <- analyseModeles$b0[[numeroModeleChoisi]]
b1 <- analyseModeles$b1[[numeroModeleChoisi]]
x <- donnesPrediction[[modeles[[numeroModeleChoisi]]$variableDroite]]

prediction <- modeles[[numeroModeleChoisi]]$formule(b0, b1, x)

n <- nrow(donnees_modele)
formuleDroite <- modeles[[i]]$formuleDroite
formuleGauche <- modeles[[i]]$formuleGauche
variableDroite <- formuleDroite(donnees_modele[[modeles[[i]]$variableDroite]])
variableGauche <- formuleGauche(donnees_modele[[modeles[[i]]$variableGauche]])
model <- lm(variableGauche ~ variableDroite, data = donnees_modele)

mse <- mean(model$residuals^2)
xbar <- mean(donnees_modele[[modeles[[numeroModeleChoisi]]$variableDroite]])
Sxx <- sum((donnees_modele[[modeles[[numeroModeleChoisi]]$variableDroite]] - xbar)^2)

intervalePrediction <- qt(1 - 0.05 / 2, df = n - 2) * sqrt(mse * (1 + 1/n + (x - xbar)^2 / Sxx))
cat("Lower bound", prediction - intervalePrediction, "Upper bound", prediction + intervalePrediction,"Prediction",  prediction)


