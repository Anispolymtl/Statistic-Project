charger <- function(matricule) {
  set.seed(matricule)
  mondata <- read.csv2("DevoirD_A24.csv")[sample(288,200),]
}

matricule <- 2247873;
charged_data <- charger(matricule)

sales <- charged_data[,c(1)]
price <- charged_data[,c(2)]
age <- charged_data[,c(3)]
region <- charged_data[,c(4)]

#Phase 1 : Analyse statistique et inférence
#a) 
#Histogramme et boxplot
hist(sales, col = "blue",  main= "Histogramme des ventes", xlab = "Nb de sièges vendus en milliers", 
ylab = "Fréquence", cex.main = 2, cex.lab = 2, cex.axis = 2)
boxplot(sales, horizontal = TRUE,col = "green", main = "Boîte de Turkey des ventes", 
xlab = "Nb de sièges vendus en milliers", cex.main = 2, cex.lab = 2, cex.axis = 2)
#Droite de Henry
qqnorm(sales, main = "Droite de Henry des ventes", xlab = "Quantiles théoriques", ylab = "Quantiles observés",
cex.main = 2, cex.lab = 2, cex.axis = 2)
qqline(sales, col = "red")
shapiro.test(sales)
#Tableau de statistiques
summary(sales)
sd(sales) #Ecart type
t.test(sales, conf.level = 0.95) #Intervalles de confiance à 95%

#b)
#Deux histogrammes juxtaposés 
layout(matrix(1:2, 1,2))
hist(sales[region == "0"], col = "lightblue", main = "Lieu rural", xlab = "Nb de sièges vendus en milliers", 
ylab = "Fréquence", cex.main = 2, cex.lab = 2, cex.axis = 2)
hist(sales[region == "1"], col = "lightgreen", main = "Lieu urbain", xlab = "Nb de sièges vendus en milliers", 
ylab = "Fréquence",cex.main = 2, cex.lab = 2, cex.axis = 2)
#Deux boxplots juxtaposés
boxplot(sales ~ region, col=c("lightblue", "lightgreen"),
        main="Comparaison des ventes par région", 
        xlab="Région", 
        ylab="Ventes (en milliers)",
        cex.main = 1.8, cex.lab = 1.8, cex.axis = 1.8)

#Tableau de statistiques descriptives par région
summary((sales[region == "0"]))
sd(sales[region == "0"]) #Ecart type
var(sales[region == "0"]) #Variance
t.test(sales[region == "0"], conf.level = 0.95) #Intervalles de confiance à 95%

summary((sales[region == "1"]))
sd(sales[region == "1"]) #Ecart type
var(sales[region == "1"]) #Variance
t.test(sales[region == "1"], conf.level = 0.95) #Intervalles de confiance à 95%

#Test d'hypothèse sur l'égalité des variances 
var.test(sales[region == "0"], sales[region == "1"])

#Test d'hypothèse sur l'égalité des moyennes
t.test(sales[region == "0"], sales[region == "1"], conf.level = 0.95)



#Phase 2: Recherche du meilleur modèle 
#c)
#Modèle 1 : 
#Tableau des coefficients de régression
model1 <- lm(sales ~ price)
summary(model1)
#Régression linéaire de la variable 
plot(price, sales, col = "red", main = "Nuage de points des ventes du Modèle 1", 
xlab = "Prix", ylab = "Nb de sièges vendus en millier", cex.main = 2, cex.lab = 2, cex.axis = 1.7)
abline(model1, col = "blue")
#Tableau d'analyse de la variance
anova(model1)
#Analyse des résidus
par(mfrow=c(2,2))
plot(model1, cex.main = 1.8, cex.lab = 1.8, cex.axis = 1.4)
#Intervalle de confiance
confint(model1, level = 0.95)

#Modèle 2 : 
#Tableau des coefficients de régression
model2 <- lm(sales ~ age)
summary(model2)
#Régression linéaire de la variable 
plot(age, sales, col = "red", main = "Nuage de points du Modèle 2", 
xlab = "Âge", ylab = "Nb de sièges vendus en millier", cex.main = 2, cex.lab = 2, cex.axis = 1.7)
abline(model2, col = "blue")
#Tableau d'analyse de la variance
anova(model2)
#Analyse des résidus
par(mfrow=c(2,2))
plot(model2, cex.main = 1.8, cex.lab = 1.8, cex.axis = 1.4)
#Intervalle de confiance
confint(model2, level = 0.95)

#Modèle 3 :
#Tableau des coefficients de régression
model3 <- lm(sales ~ I(price^2))
summary(model3)
#Régression linéaire de la variable 
plot(I(price^2), sales, col = "red", main = "Nuage de points des ventes du Modèle 3", 
xlab = "Prix^2", ylab = "Nb de sièges vendus en millier", cex.main = 1.9, cex.lab = 2, cex.axis = 1.7)
abline(model3, col = "blue")
#Tableau d'analyse de la variance
anova(model3)
#Analyse des résidus
par(mfrow=c(2,2))
plot(model3, cex.main = 1.8, cex.lab = 1.8, cex.axis = 1.4)
#Intervalle de confiance
confint(model3, level = 0.95)

#Modèle 4 :
#Tableau des coefficients de régression
model4 <- lm(sales ~ I(age^2))
summary(model4)
#Régression linéaire de la variable 
plot(I(age^2), sales, col = "red", main = "Nuage de points des ventes du Modèle 4", 
xlab = "Âge^2", ylab = "Nb de sièges vendus en millier", cex.main = 1.9, cex.lab = 2, cex.axis = 1.7)
abline(model4, col = "blue")
#Tableau d'analyse de la variance
anova(model4)
#Analyse des résidus
par(mfrow=c(2,2))
plot(model4, cex.main = 1.8, cex.lab = 1.8, cex.axis = 1.4)
#Intervalle de confiance
confint(model4, level = 0.95)

#Modèle 5 : 
#Tableau des coefficients de régression
model5 <- lm(log(sales) ~ price)
summary(model5)
#Régression linéaire de la variable 
plot(price, log(sales), col = "red", main = "Nuage de points des ventes du Modèle 5", 
xlab = "Prix", ylab = "Nb de sièges vendus en millier", cex.main = 2, cex.lab = 2, cex.axis = 1.7)
abline(model5, col = "blue")
#Tableau d'analyse de la variance
anova(model5)
#Analyse des résidus
par(mfrow=c(2,2))
plot(model5, cex.main = 1.8, cex.lab = 1.8, cex.axis = 1.4)
#Intervalle de confiance
confint(model5, level = 0.95)

#Modèle 6 :
#Tableau des coefficients de régression
model6 <- lm(log(sales) ~ age)
summary(model6)
#Régression linéaire de la variable 
plot(age, log(sales), col = "red", main = "Nuage de points des ventes du Modèle 6", 
xlab = "Âge", ylab = "Nb de sièges vendus en millier", cex.main = 2, cex.lab = 2, cex.axis = 1.7)
abline(model6, col = "blue")
#Tableau d'analyse de la variance
anova(model6)
#Analyse des résidus
par(mfrow=c(2,2))
plot(model6, cex.main = 1.8, cex.lab = 1.8, cex.axis = 1.4)
#Intervalle de confiance
confint(model6, level = 0.95)


#Modèle 7: 
#Tableau des coefficients de régression
model7 <- lm(log(sales) ~ log(price))
summary(model7)
#Régression linéaire de la variable 
plot(log(price), log(sales), col = "red", main = "Nuage de points des ventes du Modèle 7", 
xlab = "Prix", ylab = "Nb de sièges vendus en millier", cex.main = 2, cex.lab = 2, cex.axis = 1.7)
abline(model7, col = "blue")
#Tableau d'analyse de la variance
anova(model7)
#Analyse des résidus
par(mfrow=c(2,2))
plot(model7, cex.main = 1.8, cex.lab = 1.8, cex.axis = 1.4)
#Intervalle de confiance
confint(model7, level = 0.95)

#Modèle 8 :
#Tableau des coefficients de régression
model8 <- lm(log(sales) ~ log(age))
summary(model8)
#Régression linéaire de la variable 
plot(log(age), log(sales), col = "red", main = "Nuage de points des ventes du Modèle 8", 
xlab = "Âge", ylab = "Nb de sièges vendus en millier", cex.main = 2, cex.lab = 2, cex.axis = 1.7)
abline(model8, col = "blue")
#Tableau d'analyse de la variance
anova(model8)
#Analyse des résidus
par(mfrow=c(2,2))
plot(model8, cex.main = 1.8, cex.lab = 1.8, cex.axis = 1.4)
#Intervalle de confiance
confint(model8, level = 0.95)

#d)
interprev <- data.frame(price=115, age=35, region=1)
predict(model3, interprev, interval="prediction")