library(forecast)
library(tseries)
library(Kendall)

serie_temp=read.table('C:/Users/crist/Downloads/serie/serie', col.names = c('t','Xt'))
View(serie_temp)
plot(serie_temp$t,serie_temp$Xt)
sub_serie=serie_temp[1:100, ]
plot(sub_serie$t,sub_serie$Xt,'o', col='blue',main = 'Graphique de cent premieres observation de Xt',ylab = 'Xt', xlab ='t')
#En regardant le graphique de cent premieres Xt, on peut clairement voire la saisonalité et la tendance lineaire.
Xt=ts(serie_temp$Xt,frequency = 12)
Xt_100=sub_serie$Xt
t=serie_temp$t
acf(Xt,type = 'correlation', main='Autocorrélation de Xt', xlab='t',ylab='Xt')
#on observe que ca converge pas vers, que ca diminnue pas vers 0, donc la serie temporelle Xt n'est stationnaire.


q=6
d=12
#Lissage par moyenne mobile
moymob <- function(Xt,q){
  n <- length(Xt)
  mt <- rep(0,n-2*q)
  for (t in (q+1):(n-q)){
    mt[t-q] <- 1/(2*q+1)*sum(Xt[(t-q):(t+q)])
  }
  mt
}

LissXt=moymob(Xt,q)#on a utlise la moyenne mobile pour calculer la tendance estimé pour chaque t
#le vecteur avec les tendance estimé pour chaque t.
t_values <- 1:988

# Créer un data frame avec les valeurs de t et les tendances estimées
LissXt_df <- data.frame(t = t_values, LissXt = LissXt)

write.table(LissXt_df, file = "C:/Users/crist/OneDrive/Documente/R langage TD/trend_estim.txt", row.names = FALSE, col.names = TRUE, sep = "\t")

# Tracer la tendance estimée
plot(LissXt_df$t, LissXt_df$LissXt, type = "l", main = "Tendance estimée par moyenne mobile", xlab = "Temps", ylab = "Valeur estimée de la tendance")
plot(LissXt,type="l")

#Estimation de la saison
Estimsaison <- function(Xt,q,d){
  n=length(Xt)
  LissXt <- moymob(Xt,q)
  RecalXt <- Xt[(q+1):(n-q)]
  resid <- RecalXt-LissXt
  n <- length(resid)
  wk <- rep(0,d)
  for (k in 1:d){
    wk[(k+q)%%d]=mean(resid[seq(from=k,to=n,by=d)])
  }
  wk-mean(wk)
}

hatst=Estimsaison(Xt,q,d)
t_values <- 1:12
hatst_df=data.frame(t= t_values, hatst = hatst)
write.table(hatst_df, file = "C:/Users/crist/OneDrive/Documente/R langage TD/saison_estim.txt", row.names = FALSE, col.names = TRUE, sep = "\t")


saison_estim_100=rep(hatst, times = 9)
saison_estim=rep(hatst,times=90)
Yt_100=Xt_100-LissXt[1:100]-saison_estim[1:100]
Yt=Xt[1:988]-LissXt-saison_estim[1:988]
adf_result=adf.test(Yt)#le test qui me permet de savoir si la serie temporelle Yt est bien statinnaire, d'apres ce test ce n'est pas sur.
pp.test(Yt)#d'apres ce test est staionnaire
plot(hatst,type="l")
plot(t[1:100],Yt[1:100], main='Graphique de 100 premiers Yt',xlab = 't',ylab = 'Xt',type = 'o', col='red')
acf(Yt,type = "correlation", main='Autocorrélation de Yt', xlab='t',ylab='Yt',col='blue',lag.max = 30 )#graphiquement on a envie de dire que c'est stationnaire
pacf(Yt,main='Autocorrlation partielle de Yt', lag.max = 30)


#####Ex2####
# Identifier l'ordre ARMA optimal en utilisant l'algorithme de Box-Jenkins
arma_order <- auto.arima(Yt,max.p = 5, max.q = 5)
#D'apres l'algo  de Box Jenkins, l'ordre p optiaml est 5 de AR
#et l'order q optimal de MA est 3.


#Le meilleure ARMA est celui d'ordre (3,5)
models <- list()
for (p in 0:10) {
  model <- arima(Yt, order = c(p, 0, 0))  # AR(p) model sans les termes MA
  models[[paste0("AR(", p, ")")]] <- model
}
# Calculer les valeurs du BIC pour chaque modèle
best_ar <- sapply(models, BIC)#meilleure AR(3)
#cette boucle nous permet de trouver le meilleure AR qui est ici est d'ordre 3.


models1 <- list()
for (q in 0:5) {
  model1 <- arima(Yt, order = c(0, 0, q))  # MA(q) model with no MA terms
  models1[[paste0("MA(", q, ")")]] <- model1
}
best_ma <- sapply(models1, BIC)
#le meilleure d'order de MA est 5 

#On estime ici ARMA a l'aide d'ARIMA avec la differntiation egala a 0.
# Estimation du modèle ARMA(p, q) avec la méthode de la vraisemblance maximal
Arma_Yt_0=arima(Yt, order =c(5,0,3))
Arma_Yt_1=arima(Yt,order = c(3,0,1))
#Grace a la fonction auto.arima on a obtenu que le p=5 et q=3
#En regardant les graphiques PACF et ACF on constate que p=3 et q=5 et en appliquant a la fonction arima avec d=0,
#on constante que ARMA(3,5) est meilleure que ARMA(5,3) si on se base sur le critere BIC,[2933<3038]. 
BIC(Arma_Yt_0)
BIC(Arma_Yt_1)
#Pour trouver le bon ordre de AR on a regarde le graphqhique de PACF et on a constanté que  le plus grand pic estait en 3 et en 12 donc le p=3, car 12 est multiple de 3 et et on prend pass 12 car cela signifie calculer 12 parametres au vu de notre nombre de données.
#Pour trouver q on a regardé ACf ou le plus grand pic etaint en 5.


#Box.test(resid(final.arma), lag=5, type="Ljung-Box")

# Estimer le modèle ARMA(p, q)
# Estimer le modèle ARMA(p, q)
modele_arma <- arima(Xt, order = c(3, 0, 5))

# Faire des prédictions sur les prochaines observations
predictions <- predict(modele_arma, n.ahead = 100)

# Extraire les prédictions
predictions <- predictions$pred

# Générer une séquence de valeurs pour la colonne t
t_values <- seq(1001, 1100, by = 1)

# Créer un data frame avec les valeurs de t et les prédictions
predictions_df <- data.frame(t = t_values, predictions = predictions)

# Écrire les prédictions dans un fichier texte avec deux colonnes
write.table(predictions_df, file = "C:/Users/crist/OneDrive/Documente/R langage TD/predictionXtArma.txt", row.names = FALSE, col.names = TRUE, sep = "\t")

####Modelisation de SARIMA###
d=12

Zt=Xt[(1+d):1000]-Xt[1:(1000-d)]
Zt=ts(Zt,frequency = 12)
plot(Zt[1:100],type="o",xlab='t',ylab='Zt',main = "Les premiers 100 variables de Zt")
acf(Zt,main='Autocorrélation de Zt', xlab='t', ylab='Zt',lag.max = 30)#graphiquement on a envie de dire que la serie Zt est stationnaire.
pacf(Zt)
adf.test(Zt)#le test nous dis aussi cela qu'elle sationnaire.    
pp.test(Zt)#ce test ausi, test Phillips-Perron Unit Root Test
#En regardant acf on peut constatnter qu'elle staionnaire.

#decom=decompose(Zt)
#autoplot(decom,main = "Décomposition additive de Xt")

sarima_model=auto.arima(Zt)
summary(sarima_model)#ici c'est p=2, d=1, q=4
#En regardant le graphique acf de Zt je peux remarquer que le plus grands pics sont dans les lags 3 ou 4. 
#En regardant Pacf on peut remarquer que le plus grand pick s'sont en 2,4 pour p
#maintenant je vais calculer, le meilleure Aic a le SARima est de arima(2,0,4) pour q.
model_sarima_1=arima(Xt,order=c(0,0,0), seasonal = list(order=c(0,1,1)))
Box.test(model_sarima_1$residuals, lag = 12, type = "Ljung-Box")

# Estimer le modèle ARMA(p, q)
modele_sarima <- arima(Xt, order = c(3, 0, 5))

# Faire des prédictions sur les prochaines observations
predictions_xt <- predict(modele_sarima, n.ahead = 100)


# Extraire les prédictions
predictions_xt <- predictions_xt$pred
acf(predictions_xt)

#on peut calculer l'erreur quadratique
RMSE <- sqrt(mean((Xt[901:1000] - predictions_xt)^2))
RMSE1=sqrt(mean((Xt[901:1000] - predictions)^2))
#en terme d'erreur quadratique c'est mieux le sarima.



# Sélection des ordres pour SARIMA avec auto.arima
auto.arima(Xt, seasonal = TRUE, stepwise = FALSE, approximation = FALSE)

# Ajustement du modèle SARIMA
sarima_model <- arima(Xt, order = c(p, d, q), seasonal = list(order = c(P, D, Q), period = d))

# Résumé du modèle
summary(sarima_model)
decom=decompose(Xt)
plot(decom)
#La serie temoporelle doit etre un processus staionnaire si on souhaite modeliser Sarima.
#Pour traiter cette saisonalité anule , on diferencie une premire fois .
decom1=decompose(Zt)
plot(decom1)
MannKendall(Zt)
#En utilisant cette fonction on remarque qu'on aplus de tendace significative
#Dans le graphique on remarque que ona un spike importat en 11. On poura implementer un MA(1) dans notre sarima
#Un MA(11) n'est pas envisageable du fait que cela demandent l'estimation des 11 parametres, car on a peu de variable.
#Et pour Pacf on a des spikes important en 2 et 10 donc il semble raisonable d'avoir un AR(2)
model1=arima(Xt, order = c(0,0,0), seasonal = list(order = c(0,1,1), period = 12))#ici on a MA(1), aic =3678
model2=arima(Xt, order = c(0,0,0), seasonal = list(order = c(2,1,0), period = 12))#ici on a AR(1), aic=3870
#On choisit model1, car plus petit aic.
#On utilise les residus pour verifier qu'on aun bruit blan grace au test de Ljung bOX
Box.test(model1$residuals, lag = 12, type = "Ljung-Box")
#les residus sont correles 
pacf(model1$residuals, lag.max = 12, main = "Fonction d'autocorrélation partielle (I-B^12)(1+0.2B^12)Xt")
#Avec la seule valeur significative est en p=1 donc on accepte lentre de ce terme
model2 <- arima(Xt, order = c(0,0,1), seasonal = list(order = c(1,1,0), period = 12))
#Ainsi on obtient le model2.
plot(model2$residuals, main = "Résidus du modèle SARIMA12 (0,0,1)(1,1,0)", xlab = "Année", ylab = "Valeur")
acf(model2$residuals, lag.max = 24, main = "ACF des résidus du modèle SARIMA12 (0,0,1)(1,1,0)", ci.type = "ma")
#On constate visuellement que aucune corrélation ou autocorrélation partielle des résidus n’est présente. De ce fait on peut conclure que le modèle SARIMA12 (0,0,1)(0,1,1) est correct pour notre série temporelle.
d <- 12
Z <- diff(Xt, lag = d)

# Graphique des 100 premières valeurs de Z
plot(Z[1:100], type="l", main="100 premières valeurs de Z", xlab="Temps", ylab="Valeur différenciée")
acf(Z, main="Fonction d'autocorrélation de Z")
sarima_model=auto.arima(Xt, seasonal = TRUE, stepwise = FALSE, approximation = FALSE)
summary(sarima_model)
predictions2 <- forecast(sarima_model, h = 100)
predictions2
acf(predictions2$residuals, main = "ACF des résidus de SARIMA", xlab = "Lag", ylab = "ACF")
plot(predictions2)<
  
  d <- 12
Z <- diff(Xt, lag = d)

# Graphique des 100 premières valeurs de Z
plot(Z[1:100], type="l", main="100 premières valeurs de Z", xlab="Temps", ylab="Valeur différenciée")
acf(Z, main="Fonction d'autocorrélation de Z")

# Estimation du modèle SARIMA
sarima_model <- auto.arima(Xt, seasonal = TRUE, stepwise = FALSE, approximation = FALSE)
summary(sarima_model)

# Prédictions avec le modèle SARIMA
predictions2 <- forecast(sarima_model, h = 100)

# Affichage des prédictions
print(predictions2)

# Graphique des prédictions
plot(predictions2)

# Écrire les prédictions dans un fichier texte avec deux colonnes
t_values <- seq(length(Xt) + 1, length(Xt) + 100, by = 1)
predictions_df <- data.frame(t = t_values, predictions2=predictions2$mean)
write.table(predictions_df, file = "C:/Users/crist/OneDrive/Documente/R langage TD/predictionSarimaXt.txt", row.names = FALSE, col.names = TRUE, sep = "\t")

# Affichage de l'ACF des résidus de SARIMA
acf(predictions2$residuals, main = "ACF des résidus de SARIMA", xlab = "Lag", ylab = "ACF")

