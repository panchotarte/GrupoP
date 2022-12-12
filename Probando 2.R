####
rm(list=ls())


#Instalamos algunos paquetes
install.packages("readr")
install.packages("lmtest")
install.packages("dplyr")
install.packages("datos")
install.packages("tidyverse")
install.packages("haven")
install.packages("assertthat")
install.packages("car")
install.packages('tseries')
install.packages('sandwich')
install.packages('ivreg')
install.packages("AER")
install.packages('data.table')
install.packages('psych')
install.packages('jtools')
install.packages('skedastic')



#los activamos
library("readr")
library("haven")
library("zoo")
library("lmtest")
library("dplyr")
library("datos")
library("tidyverse")
library("assertthat")
library("car")
library("tseries")
library("ggplot2")
library("sandwich")
library("ivreg")
library("AER")
library("data.table")
library("psych")
library("jtools")
library("skedastic")




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
                      nhv86j, #comida rapida
                     
                       #enfermedades
                      aed75c1, #hipertención
                      aed75c2, # diabetes
                      aed75c4,#obesidad
                      aed76g, #insuficiencia renal
                      
                      
                      #control
                       pssexo_ok, #hombre
                      psedad_ok, #Edad
                      ahv115,#Días a la semana actividad física habitual
                      filtroedad_a1,
                      
                      #Otras
                      Hvbañosi, # vivienda con baño
                      Hvcocina, #lugar para cocinar en la casa
                      acg1, #estado civil
                    
                
                      ) 
#####
####
# NOMBRAR VARIABLES
c = c("pcard","refresco" ,"carne", "huevo", "snacks", "cafe", "sal", "massal","comirapida", "hiper", "diabet", "obesi", "insufrenal", "hombre", "edad", "actfisica", "filtroedad", "baño", "cocina", "estadocivil" )
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
base_select$pcard <-  recode(base_select$pcard, "1=1; 2=0")
base_select$hiper <-  recode(base_select$hiper, "1=1; 2=0")
base_select$obesi <-  recode(base_select$obesi, "1=1; 2=0")
base_select$diabt <-  recode(base_select$diabet, "1=1; 2=0")
base_select$hombre <-  recode(base_select$hombre, "1=1; 2=0")
base_select$sal <-  recode(base_select$sal, "1=1; 2=0")
base_select$cafe <-  recode(base_select$cafe, "1=1; 2=0")

base_select$insufrenal <-  recode(base_select$insufrenal, "1=1; 2=0")
base_select$huevo <-  recode(base_select$huevo, "1=7; 2=3; 3=2; 4=0.7; 5=0")
base_select$carne <-  recode(base_select$carne, "1=7; 2=3; 3=2; 4=0.7; 5=0")
base_select$snacks <-  recode(base_select$snacks, "1=7; 2=3; 3=2; 4=0.7; 5=0")
base_select$refresco <-  recode(base_select$refresco, "1=7; 2=3; 3=2; 4=0.7; 5=0")
base_select$actfisica <-  recode(base_select$actfisica, "1=3; 2=2; 3=1")
base_select$comirapida <-  recode(base_select$comirapida, "1=7; 2=3; 3=2; 4=0.7; 5=0")
base_select$baño <-  recode(base_select$baño, "1=2; 2=1; 3=0")
base_select$cocina <-  recode(base_select$cocina, "1=2; 2=1; 3=0")
base_select$estadocivil <-  recode(base_select$estadocivil, "1=2; 2=1; 3=0")



base_select$massal <-  recode(base_select$massal, "1=0; 2=1; 3=2")

#####
####
#BORRAR
table(base_select$edad)
base_select$huevo
hist(base_select$edad)

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
base_select <-  base_select %>% filter(edad >= 18)
#####
####
##
# CREAR Y AGREGAR VARIABLE
edad2 <- (base_select$edad)^2
sal <-  (base_select$sal)
snacks <- (base_select$snacks)
refresco <- (base_select$refresco)
hiper <-  (base_select$hiper)
obesi <-  (base_select$obesi)
edad <-  (base_select$edad)
hombre <-  (base_select$hombre)
huevo <-  (base_select$huevo)
actfisica <- (base_select$actfisica)
cocina*sal <- (base_select$cocina)

table(base_select$sal)
table(base_select$hiper)

######
#####
####
base_select <- base_select %>%  mutate(edad2, carne*sal, snacks*refresco, huevo*obesi, obesi*hiper, sal*snacks, actfisica*obesi, cocina*snacks)
#####
####
####
##
# Tiramos el primer MPL de encontrar un problema cardiaco 
ModeloBasico <- lm(pcard ~  huevo + cafe + refresco +  snacks + hiper + obesi + actfisica +  hombre  + edad + edad2, data = base_select)
summary(ModeloBasico)
#####
######
#####
PrimerModelo <-  lm(pcard ~ huevo + cafe + refresco +  snacks + hiper + obesi + actfisica +  hombre  + edad + edad2 + sal+ snacks*sal, data = base_select)
summary(PrimerModelo)
####
# Tiramos el modelo con su forma reducida tomando como variable dependiente snaks y instrumento cocina.
forma.reducida <-  lm(snacks ~ huevo + cafe + refresco + hiper + obesi + actfisica +  hombre  + edad + edad2 + sal + cocina , data = base_select)
summary(forma.reducida)
#####
######
#####
####
### Variables instrumentales, iverg. 
Modelo1instru<- ivreg(pcard ~ huevo + cafe + refresco+ snacks +  hiper + obesi + actfisica+ hombre +edad + edad2+ sal +snacks*sal |
                        huevo + cafe + refresco + hiper + obesi + actfisica + hombre + edad + edad2 + sal +  cocina + cocina*sal, data = base_select)
summary(Modelo1instru)
########
####
####
#Creamos compenetens necesarios para tirar el modelo de minimos cuadrados ponderados
base_select$cuad.res.m1 <- PrimerModelo$residuals^2      #Agregamos la variable cuadrados de los residuos
base_select$var <- predict.lm(PrimerModelo)*(1-predict.lm(PrimerModelo)) #La varianza para cada una de las observaciones ( de los errores condicionales)
pond <- 1/sqrt(base_select$var)
###Modelo de minimos cuadrados ponderados
PrimerModeloMCP <-  lm(pcard ~ huevo + cafe + refresco +  snacks + hiper + obesi + actfisica +  hombre  + edad + edad2 + sal+ snacks*sal , data = base_select, weights =pond )
summary(PrimerModeloMCP)

########################
####################
#Prueba reset
resettest(PrimerModeloMCP, 
          power = 2:3, 
          type = "fitted") 
#######################
######################################################################################################################################################################################
############################################################################################################################################################
########################################################################################################
########  VIDEO MPL MANUEL
#Agregamos los valores de prediccion a la tabla
base_select$prediccion <- predict.lm(PrimerModelo)
base_select$pred <- predict.lm(PrimerModeloMCP)



####
##
#
data_mod <- data.frame(Predicted = predict(Modelo1), Observed = base_select$pcard)
ggplot(data_mod, aes(x = Predicted,  y = Observed))+
  geom_point(alpha=0.1)+
  geom_smooth(method="lm", se=FALSE)+
  scale_y_continuous(breaks = c(0,1), labels=c("0","1"))+
  labs(x="Predicción del modelo", y="Valor observado o probabilidad de 1 predicha")+
  theme_bw()
abline(h = 1, lty = 2, col = "darkred")
abline(h = 0, lty = 2, col = "darkred")
text(2.5, 0.9, cex = 0.8, "Cesárea")
text(2.5, -0.1, cex= 0.8, "Vaginal")

## Histograma de residuos
hist(Modelo1$residuals, main = "Histograma de residuos del Modelo 1", xlab = "Residuos del Modelo 1")

#Calculamos la proporcion de problemas cardiacos para usar en la bondad de ajuste.
table(base_select$pcard)
131/1359
## Bondad de ajuste: Tabla de predicciones correctas e incorrectas
base_select$pcard_hat <- ifelse(base_select$prediccion > 0.096, 1, 0)
summary(base_select$pcard_hat)
addmargins(table(base_select$pcard,base_select$pcard_hat))
prop.table(table(base_select$pcard,base_select$pcard_hat),1)
prop.table(table(base_select$pcard,base_select$pcard_hat),2)

## El MPL se dice que es "intrínsecamente heteroscedástico"
data_het <- data.frame(pred=Modelo1$fitted.values, res2=Modelo1$residuals^2)
ggplot(data=data_het, mapping=aes(x=pred, y=res2)) + 
  geom_point() +
  labs(x="Predicción de la probabilidad de cesárea", y="Residuo al cuadrado") +
  labs(title="Predicción de la varianza condicional del término de error")

#tiramos el modelo robusto
coeftest(Modelo1, vcov. = vcovHC)
