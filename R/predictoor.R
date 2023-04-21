library(tidyverse)

## get parent script folder
folder <- dirname(rstudioapi::getSourceEditorContext()$path)
parentFolder <- dirname(folder)

insurance <- read.csv(("C:/Users/USUARIO/Downloads/macine_learning/datasets/insurance.csv")
                      , stringsAsFactors = TRUE)

#obtenemos la variable de edad al cuadrado
insurance$age2 <- insurance$age^2
#otraa forma de hacer para tener la edad al cuadrado
#insurancem<-mutate(insurance,age2
#              (age^2) )#nuevo dataset que contiene la variable edad al cuadrado

# volvemos la variable BMI en binario para saber si la persona es o no obesa
#despues de 30 cuenta como una persona obesa
insurance$bmi30 <- ifelse(insurance$bmi >= 30, 1, 0)
#clasificamos si una persona es fumadora
insurance$smokeryes<-ifelse(insurance=='yes', 1, 0)
#insurance$bmi30*smoker<-insurance$bmi30 + smokeryes + bmi30:smokeryes
#agregamos interaccion de Bmi30 y esmokeryes


colnames(insurance)
summary(insurance)
summary(insurance$charges)
hist(insurance$charges, breaks=100)
pairs(insurance[c("age2", "bmi", "children", "charges")]

      ,pch=21, bg=c("red","green3","blue", "orange")[unclass(insurance$region)])

if(!require(psych))
  install.packages("psych")

library(psych)
pairs.panels(insurance[c("age2",

                         "bmi"

                         , "children", "charges")]

             ,pch=21, bg=c("red","green3","blue", "orange")[unclass(insurance$region)])
#predictors
#set.seed(1)
predictors <- colnames(insurance)[-7]#todas la variables predictores menos la variable objetivo charges
sample.index <- sample(1:nrow(insurance)#recorre todos los datos(1338) y le asigna un numero
                       ,nrow(insurance)*0.7#sacamos el  70% de todos los numeros
                       ,replace = F)#no se reeemplazan los datos

train.data <- insurance[sample.index,c(predictors,"charges"),drop=F]#filas de sample.index,todas las colomnas,sige estrutura de dataframe drop=F
test.data <- insurance[-sample.index,c(predictors,"charges"),drop=F]#todas las muestras que sean el sample.index



# as we are using all variables, we can write ~ .
#añadimos a la formula el efecto de interaccion
ins_model <- lm(charges ~ bmi30 + smokeryes + bmi30:smokeryes, data = train.data)#creamos modelo predice los charges en funcion de los predictores con el dataset de entrenamiento

#tenemos al final un modelo de prediccion un poco mas acertado con un 78% con el modelo de #smokeryes y obecidad
#bmi tiene un costo por cambio de unidad de 1151 y esmokeryes de 13619 cuesta mas un #ciudadano que fuma a una que es obeso y mucho mas cuando es obeso y es fumador

#ins_model <- lm(charges ~ bmi30 + smokeryes + bmi30:smokeryes, data = train.data)
ins_model
summary(ins_model)
# al aumentar la edad al cuadrado no predice bien los cargos por cambio de unidad de edad en mis coeficientes beta

prediction <- predict(ins_model, test.data)
#calculate RMSE
RMSE.df = data.frame(predicted = prediction, actual = test.data$charges,

                     SE = sqrt((prediction - test.data$charges)^2))

head(RMSE.df)
sum(RMSE.df$SE)/nrow(RMSE.df)

