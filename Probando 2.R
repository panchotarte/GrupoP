#Instalamos algunos paquetes
install.packages("readr")
install.packages("lmtest")
install.packages("dplyr")
install.packages("datos")
install.packages("tidyverse")
install.packages("haven")
install.packages("assertthat")
install.packages("car")
#los activamos
library("readr")
library("haven")
library("lmtest")
library("dplyr")
library("datos")
library("tidyverse")
library("haven")
library("assertthat")
library("car")
####
###
# SUBIR BASE 
base <- read_sav("BASE_ENS_Formulario Individual.sav")
#####
# SELECCIONAR VARIABLES:
# Problemas Cardiacos - Refrescos con azucar - Carne - Huevos - Snakcs - 
# - Cafe - Sal - Agrega más sal - Hipertención - Diabetes - Obesidad - Sexo - Edad - Nivel de actividad fisica
base_select <- select(base, 
                      aed76h,#problemas cardiacos
                      
                      #Alimentación
                      ahv86h, # refresco
                      ahv86e, #carne
                      ahv86g, #huevo
                      ahv86i, #snacks
                      ahv87g, #cafe
                      ahv91, #Sal
                      ahv92, # mas sal
                     
                       #enfermedades
                      aed75c1, #hipertención
                      aed75c2, # diabetes
                      aed75c4,#obesidad
                      aed76g, #insuficiencia renal
                      
                      #Personales
                       pssexo_ok, #Sexo
                      psedad_ok, #Edad
                      ahv115,#nivel de actividad fisica
                      ) 
#####
####
# NOMBRAR VARIABLES
c = c("pcard","refresco" ,"carne", "huevo", "snacks", "cafe", "sal", "massal", "hiper", "diabet", "obesi", "insufrenal", "sexo", "edad", "actfisica" )
names(base_select) = c
#####
####
# Redefinimos las variables 
# Pasando a reflejar la cantidad de elementos consumidos por semana. 
# El valor 1 que representa "A diario" le dimos valor 7, que representa el consumo diario de huevos por semana y asi las otras 5 opciones
# el valor 2 que representa "3 o más veces por semana" pasa a ser 3 
# el valor 3 que representa "1 o 2 veces por semana" pasa a ser 2
# el valor 4 que representa "Menos de 1 vez por semana" pasa a ser 0.7,
# el valor 5 que representa "Nunca o casi nunca" pasa a ser 0,
base_select$huevo <-  recode(base_select$huevo, "1=7; 2=3; 3=2; 4=0.7; 5=0")
base_select$carne <-  recode(base_select$carne, "1=7; 2=3; 3=2; 4=0.7; 5=0")
base_select$snacks <-  recode(base_select$snacks, "1=7; 2=3; 3=2; 4=0.7; 5=0")
base_select$refresco <-  recode(base_select$refresco, "1=7; 2=3; 3=2; 4=0.7; 5=0")
base_select$actfisica <-  recode(base_select$actfisica, "1=7; 2=3; 3=2; 4=0.7; 5=0")
#####
####
#BORRAR
table(base_select$huevo)
base_select$huevo
#####
####
##
# Eliminamos variables no estan definidas, aparecen signos negativos
base_select <-  base_select %>% filter(pcard >= 0)
base_select <-  base_select %>% filter(hiper >= 0)
base_select <-  base_select %>% filter(diabet >= 0)
base_select <-  base_select %>% filter(obesi >= 0)
base_select <-  base_select %>% filter(actfisica >= 0)
base_select <- base_select[!is.na(base_select$actfisica),]
#####
####
##
# CREAR AGREGAR VARIABLE
edad2 <- (base_select$edad)^2
carne <-  (base_select$carne)
sal <-  (base_select$sal)
#
base_select <- base_select %>%  mutate(edad2, carne*sal)
#####
####
####
##
# Tiramos el primer MPL de encontrar un problema cardiaco 
Modelo1 <- lm(pcard ~ carne + refresco + huevo + snacks + sal + cafe  + hiper + massal + diabet + obesi +insufrenal + sexo + edad+ edad2 + actfisica, data = base_select)
summary(Modelo1)
summary(base_select)
#####
######
#####
####
# Tiramos el segundo MPL de encontrar un problema cardiaco 

Modelo2 <- lm(pcard ~ , data = base_select)
summary(Modelo2)
#####
######
#####
####















######################################################################################################################################################################################
############################################################################################################################################################
########################################################################################################
########  VIDEO MPL MANUEL
## Grafico valores ajustados contra valores observados
base_select$prediccion <- predict.lm(Modelo1)
data_mod <- data.frame(Predicted = predict(Modelo1), Observed = base_select$pcard)
ggplot(data_mod, aes(x = Predicted,  y = Observed))+
  geom_point(alpha=0.1)+
  geom_smooth(method="lm", se=FALSE)+
  scale_y_continuous(breaks = c(1,2), labels=c("1","2"))+
  labs(x="Predicción del modelo", y="Valor observado o probabilidad de 1 predicha")+
  theme_bw()
abline(h = 1, lty = 2, col = "darkred")
abline(h = 0, lty = 2, col = "darkred")
text(2.5, 0.9, cex = 0.8, "pcard")
text(2.5, -0.1, cex= 0.8, "no pcard")

## Histograma de residuos
hist(Modelo1$residuals, main = "Histograma de residuos del Modelo 1", xlab = "Residuos del Modelo 1")

base_select <- mutate(pcard_hat=prediccion)

## Bondad de ajuste: Tabla de predicciones correctas e incorrectas
base_select$pcard_hat <- ifelse(base_select$prediccion > 0.5, 1, 0)
summary(base_select$pcard_hat)
addmargins(table(base_select$pcard,base_select$pcard_hat))
prop.table(table(base_select$pcard,base_select$pcard_hat),1)
prop.table(table(base_select$pcard,base_select$pcard_hat),2)
addmargins(prop.table(table(base_select$pcard,base_select$pcard_hat)))

## El MPL se dice que es "intrínsecamente heteroscedástico"
data_het <- data.frame(pred=Modelo1$fitted.values, res2=Modelo1$residuals^2)
ggplot(data=data_het, mapping=aes(x=pred, y=res2)) + 
  geom_point() +
  labs(x="Predicción de la probabilidad de cesárea", y="Residuo al cuadrado") +
  labs(title="Predicción de la varianza condicional del término de error")

