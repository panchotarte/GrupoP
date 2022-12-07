if (!require("lmtest")) install.packages("lmtest")
library("readr")
library("lmtest")
library("dplyr")
library("datos")
library("tidyverse")
library("haven")
library("assertthat")
library("haven")
library("car")

base <- read_sav("~/FCEA/Econometria 1/Proyecto/R - Proyecto /Base datos/BASE_ENS_Formulario Individual.sav")
#seleccionamos las variables que son de interes 
base_select <- select(base, aed76h, ahv86h, ahv86e, ahv86g, ahv86i, ahv87g,ahv91,ahv92, aed75c1, aed75c2, aed75c4, pssexo_ok,psedad_ok, ahv115 ) 
#con este vector le damos nombre a las variables
c = c("pcard","refresco" ,"carne", "huevo", "snacks", "cafe", "sal", "massal", "hiper", "diabet", "obesi", "sexo", "edad", "actfisica" )
names(base_select) = c


#combertimos el valor de la variable
base_select$huevo <-  recode(base_select$huevo, "1=7; 2=3; 3=2; 4=0.7; 5=0")
base_select$carne <-  recode(base_select$carne, "1=7; 2=3; 3=2; 4=0.7; 5=0")
base_select$snacks <-  recode(base_select$snacks, "1=7; 2=3; 3=2; 4=0.7; 5=0")
base_select$refresco <-  recode(base_select$refresco, "1=7; 2=3; 3=2; 4=0.7; 5=0")
base_select$actfisica <-  recode(base_select$actfisica, "1=7; 2=3; 3=2; 4=0.7; 5=0")

table(base_select$huevo)

base_select$huevo
##eliminamos variables que son negativas o cero 
base_select <-  base_select %>% filter(pcard >= 0)
base_select <-  base_select %>% filter(hiper >= 0)
base_select <-  base_select %>% filter(diabet >= 0)
base_select <-  base_select %>% filter(obesi >= 0)
base_select <-  base_select %>% filter(actfisica >= 0)

base_select <- base_select[!is.na(base_select$actfisica),]

#agregamos edad2
edad2 <- (base_select$edad)^2
base_select <- mutate(base_select, edad2)

#Modelo MPL probabilidad de encontrar un problema cardiaco 
Modelo1 <- lm(pcard ~ carne + huevo + snacks + sal + cafe  + hiper + massal +refresco + diabet + obesi  + sexo + edad+ edad2 + actfisica, data = base_select)

summary(Modelo1)
summary(base_select
        )

Modelo2 <- lm(pcard ~ carne + huevo + snacks + sal + cafe + sal + hiper + diabet + obesi, data = base_select)
summary(Modelo2)

s#agregamos una nueva variable que sea carne*sal 
base_select <- base_select %>%  mutate( carnexsal = carne*sal)
#Modelo con carne*sal
ModeloProbando <- lm(pcard ~ carne + sal + carnexsal, data= base_select)
summary(ModeloProbando)

#Video de MPL
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

