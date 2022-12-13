####
rm(list=ls())
if (!require("naniar")) install.packages("naniar")
if (!require("visdat")) install.packages("visdat")
if (!require("simputation")) install.packages("simputation")

library(naniar)
library(visdat)
library(simputation)


#Instalamos algunos paquetes
install.packages("readr")
install.packages("lmtest")
install.packages("dplyr")
install.packages("datos")
install.packages("tidyverse")
install.packages("ggplot2")
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

                       #enfermedades
                      aed75c1, #hipertención
                      aed75c4,#obesidad

                      
                      #control
                       pssexo_ok, #hombre
                      psedad_ok, #Edad
                      ahv115,#Días a la semana actividad física habitual

                      #Otras
                      Hvcocina, #lugar para cocinar en la casa
                      
                    
                
                      ) 
#####
####
# NOMBRAR VARIABLES
c = c("pcard","refresco" ,"carne", "huevo", "snacks", "cafe", "sal", "hiper", "obesi", "hombre", "edad", "actfisica", "cocina" )
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
base_select$hombre <-  recode(base_select$hombre, "1=1; 2=0")
base_select$sal <-  recode(base_select$sal, "1=1; 2=0")
base_select$cafe <-  recode(base_select$cafe, "1=1; 2=0")
base_select$huevo <-  recode(base_select$huevo, "1=7; 2=3; 3=2; 4=0.7; 5=0")
base_select$carne <-  recode(base_select$carne, "1=7; 2=3; 3=2; 4=0.7; 5=0")
base_select$snacks <-  recode(base_select$snacks, "1=7; 2=3; 3=2; 4=0.7; 5=0")
base_select$refresco <-  recode(base_select$refresco, "1=7; 2=3; 3=2; 4=0.7; 5=0")
base_select$actfisica <-  recode(base_select$actfisica, "1=3; 2=2; 3=1")
base_select$cocina <-  recode(base_select$cocina, "1=2; 2=1; 3=0")
#####
####
#####
####
##
# Eliminamos variables no estan definidas, aparecen signos negativos
base_select <-  base_select %>% filter(pcard >= 0)
base_select <-  base_select %>% filter(hiper >= 0)
base_select <-  base_select %>% filter(obesi >= 0)
base_select <-  base_select %>% filter(actfisica >= 0)
base_select <-  base_select %>% filter(edad >= 18) #definimos la edad en mayores de 18 años
#####
####
##
# CREAR Y AGREGAR VARIABLE
edad2 <- (base_select$edad)^2
sal <-  (base_select$sal)
snacks <- (base_select$snacks)
refresco <- (base_select$refresco)
edad <-  (base_select$edad)

######
#####
####
base_select <- base_select %>%  mutate(edad2,sal*snacks)
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
base_select_red <-  base_select %>% filter(var >= 0)
pond1 <- 1/sqrt(base_select_red$var)
###Modelo de minimos cuadrados ponderados
PrimerModeloMCP <-  lm(pcard ~ huevo + cafe + refresco +  snacks + hiper + obesi + actfisica +  hombre  + edad + edad2 + sal+ snacks*sal, data = base_select_red, weights =pond1)
summary(PrimerModeloMCP)

########################
####################
#Prueba reset para el modelo MCO
resettest(PrimerModelo, 
          power = 2:3, 
          type = "fitted") 
#Prueba reset para el modelo MCP
resettest(PrimerModeloMCP, 
          power = 2:3, 
          type = "fitted") 
#######################
######################################################################################################################################################################################
##########################################################################################
#Modelo auxiliar
Modelo.auxPcard <- lm(cuad.res.m1 ~  huevo + cafe + refresco +  snacks + hiper + obesi + actfisica +  hombre  + edad + edad2 + sal+ snacks*sal , data=base_select, x=T, y=T)
summary(Modelo.auxPcard)

BP.realizacionPcard <- dim(base_select)[1]*summary(Modelo.auxPcard)$r.squared
v.critic <- qchisq(0.95,17)
bptest(PrimerModelo) # sin reescalar

## Reescalando
sigmapc <- sum(base_select$cuad.res.m1)/dim(base_select)[1]
Modelo.aux.1pc <- lm(cuad.res.m1/sigmapc~ huevo + cafe + refresco +  snacks + hiper + obesi + actfisica +  hombre  + edad + edad2 + sal+ snacks*sal , data=base_select, x=T, y=T)
summary(Modelo.aux.1pc)
SCE  <-  sum((Modelo.aux.1pc$fitted.values - mean(base_select$cuad.res.m1/sigmapc))^2)
Realizacion.test <- SCE/2
white(PrimerModelo, interactions=T)

#White modificado
base_select$f.values <- PrimerModelo$fitted.values
base_select$f.values.2 <- base_select$f.values^2
Modelo.Wmodpc <- lm(cuad.res.m1~ f.values + f.values.2, data=base_select, x=T, y=T)
summary(Modelo.Wmodpc)
nR2auxpc <- nrow(Modelo.Wmodpc$x)*summary(Modelo.Wmodpc)$r.squared
v.critic <- qchisq(0.95,2)
pchisq(nR2auxpc, 2, ncp = 0, lower.tail = F, log.p = FALSE)
####
##
#Nuevo modelo para la varianza condicional del termino de error, guiado por el modelo autxiliar y estimado con los residuos del modelo 1 
Modelo.Vpc <- lm(log(cuad.res.m1)~ huevo + refresco + hiper + obesi + hombre + sal + sal*snacks, data=base_select, x=T, y=T)
summary(Modelo.Vpc)

g <- Modelo.Vpc$fitted.values
w <-exp(g)
pond <- 1/sqrt(w)

PrimerModeloMPCPC <- lm(pcard~  huevo + cafe + refresco +  snacks + hiper + obesi + actfisica +  hombre  + edad + edad2 + sal+ snacks*sal, data = base_select, weights=pond)

summary(PrimerModeloMPCPC)

summ(PrimerModelo, robust = "HC3") ## HC1 replica stata, recomendado en R HC3

resettest(PrimerModeloMPCPC, 
          power = 2:3, 
          type = "fitted")

##################################################################
########################################################################################################
########  VIDEO MPL MANUEL
#Agregamos los valores de prediccion a la tabla
base_select$prediccion <- predict.lm(PrimerModelo)
base_select_red$prediccion <- predict.lm(PrimerModeloMCP)
####
##
#
#####
###GRAFICO
data_mod <- data.frame(Predicted = predict(PrimerModelo), Observed = base_select$pcard)
ggplot(data_mod, aes(x = Predicted,  y = Observed))+
  geom_point(alpha=0.1)+
  geom_smooth(method="lm", se=FALSE)+
  scale_y_continuous(breaks = c(0,1), labels=c("0","1"))+
  labs(x="Predicción del modelo", y="Valor observado o probabilidad de 1 predicha")+
  theme_bw() 

## Histograma de residuos del PrimerModelo MCO
hist(PrimerModelo$residuals, main = "Histograma de residuos del PrimerModelo", xlab = "Residuos del PrimerModelo")

#Calculamos la proporcion de problemas cardiacos para usar en la bondad de ajuste.
table(base_select$pcard)
b = 131/1359
######
## Bondad de ajuste: Tabla de predicciones correctas e incorrectas
base_select$pcard_hat <- ifelse(base_select$prediccion > b, 1, 0)
summary(base_select$pcard_hat)
addmargins(table(base_select$pcard,base_select$pcard_hat))
prop.table(table(base_select$pcard,base_select$pcard_hat),1)
prop.table(table(base_select$pcard,base_select$pcard_hat),2)
######
####Modelo MCP
table(base_select_red$pcard)
b_red= 131/1061

base_select_red$pcard_hat <- ifelse(base_select_red$prediccion > b_red, 1, 0)
summary(base_select_red$pcard_hat)
addmargins(table(base_select_red$pcard,base_select_red$pcard_hat))
prop.table(table(base_select_red$pcard,base_select_red$pcard_hat),1)
prop.table(table(base_select_red$pcard,base_select_red$pcard_hat),2)


## El MPL se dice que es "intrínsecamente heteroscedástico"
data_het <- data.frame(pred=PrimerModelo$fitted.values, res2=PrimerModelo$residuals^2)
ggplot(data=data_het, mapping=aes(x=pred, y=res2)) + 
  geom_point() +
  labs(x="Predicción de la probabilidad de problemas cardiacos", y="Residuo al cuadrado") +
  labs(title="Predicción de la varianza condicional del término de error")

#tiramos el modelo robusto
coeftest(PrimerModelo, vcov. = vcovHC)
