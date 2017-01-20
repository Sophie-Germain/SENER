###########################################################################################
## Este codigo genera el modelo de prediccion de consumo gas natural por subramo industrial
##########################################################################################

################################
## Librerias
################################

#Manipulacion datos
library(plyr)
library(dplyr)
library(data.table)
library(readr)
## Gráficas
library(ggplot2)
#fechas
library(lubridate)
#manipular strings
library(stringr)
#series de tiempo
library(tseries)
library(gclus)
library(graphics)
library(corrplot)
library(ggplot2)
library(lmtest)
library(tseries)
library(Hmisc)

################################
## BASE COMPLETA
################################
## ----------------Grafica de todas las series de tiempo----------------------
## alimentos
alimentos        <- fread("datos_alimentos.csv")
alimentos$PIB    <- as.numeric(alimentos$PIB) 
names(alimentos) <- c("cat", "rama", "año", "demanda",
                     "precio_gas", "precio_combu","pib", "efi")
alimentos        <- ts(alimentos[, c("demanda"), with = FALSE], start=2005, frequency=1)
## papel
papel        <- fread("datos_papel.csv")
papel$PIB    <- as.numeric(papel$PIB) 
names(papel) <- c("cat", "rama", "año", "demanda",
                 "precio_gas", "precio_combu","pib", "efi")
papel        <- ts(papel[, c("demanda"), with = FALSE], start=2005, frequency=1)
#cemento&vidrio
cem_vid        <- fread("datos_cem_vidrio.csv")
cem_vid$PIB    <- as.numeric(cem_vid$PIB) 
names(cem_vid) <- c("cat", "rama", "año", "demanda",
                   "precio_gas", "precio_combu","pib", "efi")
cem_vid <- ts(cem_vid[, c("demanda"), with = FALSE], start=2005, frequency=1)
#cerveza
cerv <- fread("datos_cerv.csv")
cerv$PIB <- as.numeric(cerv$PIB) 
names(cerv) <- c("cat", "rama", "año", "demanda", "precio_gas", "precio_combu","pib", "efi")
cerv <- ts(cerv[, c("demanda"), with = FALSE], start=2005, frequency=1)
#metales
met <- fread("datos_metales.csv")
met$PIB <- as.numeric(met$PIB) 
names(met) <- c("cat", "rama", "año", "demanda", "precio_gas", "precio_combu","pib", "efi")
met <- ts(met[, c("demanda"), with = FALSE], start=2005, frequency=1)
#mineria
min <- fread("datos_mineria.csv")
min$PIB <- as.numeric(min$PIB) 
names(min) <- c("cat", "rama", "año", "demanda", "precio_gas", "precio_combu","pib", "efi")
min <- ts(min[, c("demanda"), with = FALSE], start=2005, frequency=1)
#no_metales
nomet <- fread("datos_no_metales.csv")
nomet$PIB <- as.numeric(nomet$PIB) 
names(nomet) <- c("cat", "rama", "año", "demanda", "precio_gas", "precio_combu","pib", "efi")
nomet <- ts(nomet[, c("demanda"), with = FALSE], start=2005, frequency=1)
#quimica
quim <- fread("datos_quimica.csv")
quim$PIB <- as.numeric(quim$PIB) 
names(quim) <- c("cat", "rama", "año", "demanda", "precio_gas", "precio_combu","pib", "efi")
quim <- ts(quim[, c("demanda"), with = FALSE], start=2005, frequency=1)
#textil
textil <- fread("datos_textil.csv")
textil$PIB <- as.numeric(textil$PIB) 
names(textil) <- c("cat", "rama", "año", "demanda", "precio_gas", "precio_combu","pib", "efi")
textil <- ts(textil[, c("demanda"), with = FALSE], start=2005, frequency=1)

png("./output/graphs/time_series.png")
ts.plot(alimentos, papel, cem_vid, cerv, met, min, nomet, quim, textil, gpars= list(col=rainbow(9)), ylab="Demanda (millones ft cúbicos)", xlab="Tiempo", main="Demanda de gas natural por subsector industrial (IMP) ")
legend("topright", legend = c("alimentos", "papel", "cem&vidrio", "cerveza", "metales", "mineria", "no_metales", "química", "textil"), col = rainbow(9), lty = 1)
dev.off()
---------------------------------------------------------------------------
##Leemos todos los datos
datos        <- fread("prueba_gas_natural.csv")
datos$PIB    <- readr::parse_number(datos$PIB) 
datos$Anio   <- as.numeric(datos$Anio)
names(datos) <- c("cat", "rama", "anio", "demanda", "precio_gas", "precio_combu","pib", "efi")
datos        <- datos[,-c(1,8), with = FALSE]

## Boxplot de todos los datos
png("./output/graphs/box_plot_all.png")
boxplot(scale(datos[,-1:-2, with = FALSE]), col="blue", main="Boxplot de variables estandarizadas")
dev.off()
####MODELO CONJUNTO-----------------------------------------------------------------------------------------------------------

#Estandarización y modelo de todos
demanda            <- datos[, (demanda - mean(demanda))/sd(demanda), by = rama]
datos$demanda      <- demanda$V1
precio_gas            <- datos[, (precio_gas - mean(precio_gas))/sd(precio_gas), by = rama]
datos$precio_gas     <- precio_gas$V1
precio_combu            <- datos[, (precio_combu - mean(precio_combu))/sd(precio_combu), by = rama]
datos$precio_combu      <- precio_combu$V1
pib            <- datos[, (pib - mean(pib))/sd(pib), by = rama]
datos$pib      <- pib$V1
#Variables retrasadas
demanda.lag <- datos[, shift(demanda, n=1, fill=NA, type="lag"), by = rama]
datos$demanda.lag <- demanda.lag$V1
precio_gas.lag <- datos[, shift(precio_gas, n=1, fill=NA, type="lag"), by = rama]
datos$precio_gas.lag <- precio_gas.lag$V1
precio_combu.lag <- datos[, shift(precio_combu, n=1, fill=NA, type="lag"), by = rama]
datos$precio_combu.lag <- precio_combu.lag$V1
pib.lag <- datos[, shift(pib, n=1, fill=NA, type="lag"), by = rama]
datos$pib.lag <- pib.lag$V1


#Ramo Variable dummie
datos$rama <- as.factor(datos$rama)
attach(datos)
mod_regresion <- lm(demanda ~ demanda.lag + precio_gas + precio_gas.lag + precio_combu + precio_combu.lag + pib + pib.lag + rama, data = datos)
summary(mod_regresion)

#Durbin Watson test para ver AutoCorrelación
dwtest(demanda ~ demanda.lag + precio_gas + precio_gas.lag + precio_combu + precio_combu.lag + pib + pib.lag + rama, data=datos) 

#Metricas de error
mse <- function(sm) 
  mean(sum(sm$residuals^2))
error <- mse(mod_regresion)
sprintf("El error de este modelo es: %s", error)

########-----------------Resultados--------------------
#Alimentos
sector <-datos[rama=='Alimentos',]
real <- ts(sector$demanda, start=2006, end=2030, frequency=1)
pred <- ts(mod_regresion$fitted[1:25], start=2006, end=2030, frequency=1)
png("./output/graphs/pred_res_alimentos.png")
ts.plot(real, pred, gpars = list(col = c("blue", "red"), ylab="Demanda de gas natural"), xlab="Tiempo", main="Resultados del modelo para Alimentos", type="o")
legend("bottomright", legend = c("real", "modelo"), col = c("blue", "red"), lty = 1)
dev.off()
error2 <- mean(sum(mod_regresion$residuals[1:10]^2))
sprintf("El error en 2006-2015 para Alimentos de este modelo es: %s", error2)
error3 <- mean(sum(mod_regresion$residuals[11:25]^2))
sprintf("El error en 2016-2030 para Alimentos de este modelo es: %s", error3)

#Celulosa y papel
sector <-datos[rama=='Celulosa y papel',]
real <- ts(sector$demanda, start=2006, end=2030, frequency=1)
pred <- ts(mod_regresion$fitted[26:50], start=2006, end=2030, frequency=1)
png("./output/graphs/pred_res_cel.png")
ts.plot(real, pred, gpars = list(col = c("blue", "red"), ylab="Demanda de gas natural"), xlab="Tiempo", main="Resultados del modelo para Celulosa y papel", type="o")
legend("bottomright", legend = c("real", "modelo"), col = c("blue", "red"), lty = 1)
dev.off()
error2 <- mean(sum(mod_regresion$residuals[26:35]^2))
sprintf("El error en 2006-2015 para celulosa y papel de este modelo es: %s", error2)
error3 <- mean(sum(mod_regresion$residuals[36:50]^2))
sprintf("El error en 2016-2030 para celulosa y papel de este modelo es: %s", error3)

#Cemento y vidrio
sector <-datos[rama=='Cemento y vidrio',]
real <- ts(sector$demanda, start=2006, end=2030, frequency=1)
pred <- ts(mod_regresion$fitted[51:75], start=2006, end=2030, frequency=1)
png("./output/graphs/pred_res_cem.png")
ts.plot(real, pred, gpars = list(col = c("blue", "red"), ylab="Demanda de gas natural"), xlab="Tiempo", main="Resultados del modelo para Cemento y vidrio", type="o")
legend("bottomright", legend = c("real", "modelo"), col = c("blue", "red"), lty = 1)
dev.off()
error2 <- mean(sum(mod_regresion$residuals[51:60]^2))
sprintf("El error en 2006-2015 para cemento y vidrio de este modelo es: %s", error2)
error3 <- mean(sum(mod_regresion$residuals[61:75]^2))
sprintf("El error en 2016-2030 para cemento y vidrio de este modelo es: %s", error3)

#Cerveza y malta
sector <-datos[rama=='Cerveza y malta',]
real <- ts(sector$demanda, start=2006, end=2030, frequency=1)
pred <- ts(mod_regresion$fitted[76:100], start=2006, end=2030, frequency=1)
png("./output/graphs/pred_res_cer.png")
ts.plot(real, pred, gpars = list(col = c("blue", "red"), ylab="Demanda de gas natural"), xlab="Tiempo", main="Resultados del modelo para cerveza y malta", type="o")
legend("bottomright", legend = c("real", "modelo"), col = c("blue", "red"), lty = 1)
dev.off()
error2 <- mean(sum(mod_regresion$residuals[76:85]^2))
sprintf("El error en 2006-2015 de este modelo para cerveza y malta es: %s", error2)
error3 <- mean(sum(mod_regresion$residuals[86:100]^2))
sprintf("El error en 2016-2030 de este modelo para cerveza y malta es: %s", error3)

#Metales básicos
sector <-datos[rama=='Metales básicos',]
real <- ts(sector$demanda, start=2006, end=2030, frequency=1)
pred <- ts(mod_regresion$fitted[101:125], start=2006, end=2030, frequency=1)
png("./output/graphs/pred_res_metbas.png")
ts.plot(real, pred, gpars = list(col = c("blue", "red"), ylab="Demanda de gas natural"), xlab="Tiempo", main="Resultados del modelo para metales básicos", type="o")
legend("bottomright", legend = c("real", "modelo"), col = c("blue", "red"), lty = 1)
dev.off()
error2 <- mean(sum(mod_regresion$residuals[101:110]^2))
sprintf("El error en 2006-2015 de este modelo para metales básicos es: %s", error2)
error3 <- mean(sum(mod_regresion$residuals[111:125]^2))
sprintf("El error en 2016-2030 de este modelo para metales básicos es: %s", error3)

#Minería y Productos metálicos, equipo eléctrico y de transporte
sector <-datos[rama=='Minería y Productos metálicos, equipo eléctrico y de transporte',]
real <- ts(sector$demanda, start=2006, end=2030, frequency=1)
pred <- ts(mod_regresion$fitted[126:150], start=2006, end=2030, frequency=1)
png("./output/graphs/pred_res_min.png")
ts.plot(real, pred, gpars = list(col = c("blue", "red"), ylab="Demanda de gas natural"), xlab="Tiempo", main="Resultados del modelo para minería y productos metálicos, equipo eléctrico y de transporte", type="o")
legend("bottomright", legend = c("real", "modelo"), col = c("blue", "red"), lty = 1)
dev.off()
error2 <- mean(sum(mod_regresion$residuals[126:135]^2))
sprintf("El error en 2006-2015 de este modelo para minería y productos metálicos es: %s", error2)
error3 <- mean(sum(mod_regresion$residuals[136:150]^2))
sprintf("El error en 2016-2030 de este modelo para minería y productos metálicos es: %s", error3)

#Productos de minerales no metálicos
sector <-datos[rama=='Productos de minerales no metálicos',]
real <- ts(sector$demanda, start=2006, end=2030, frequency=1)
pred <- ts(mod_regresion$fitted[151:175], start=2006, end=2030, frequency=1)
png("./output/graphs/pred_res_nomet.png")
ts.plot(real, pred, gpars = list(col = c("blue", "red"), ylab="Demanda de gas natural"), xlab="Tiempo", main="Resultados del modelo para productos de minerales no metálicos", type="o")
legend("bottomright", legend = c("real", "modelo"), col = c("blue", "red"), lty = 1)
dev.off()
error2 <- mean(sum(mod_regresion$residuals[151:160]^2))
sprintf("El error en 2006-2015 de este modelo para productos de minerales no metálicos es: %s", error2)
error3 <- mean(sum(mod_regresion$residuals[161:175]^2))
sprintf("El error en 2016-2030 de este modelo para productos de minerales no metálicos es: %s", error3)


#Química
sector <-datos[rama=='Química',]
real <- ts(sector$demanda, start=2006, end=2030, frequency=1)
pred <- ts(mod_regresion$fitted[176:200], start=2006, end=2030, frequency=1)
png("./output/graphs/pred_res_quim.png")
ts.plot(real, pred, gpars = list(col = c("blue", "red"), ylab="Demanda de gas natural"), xlab="Tiempo", main="Resultados del modelo para Química", type="o")
legend("bottomright", legend = c("real", "modelo"), col = c("blue", "red"), lty = 1)
dev.off()
error2 <- mean(sum(mod_regresion$residuals[176:185]^2))
sprintf("El error en 2006-2015 de este modelo para química es: %s", error2)
error3 <- mean(sum(mod_regresion$residuals[186:200]^2))
sprintf("El error en 2016-2030 de este modelo para química es: %s", error3)

#Textil
sector <-datos[rama=='Textil',]
real <- ts(sector$demanda, start=2006, end=2030, frequency=1)
pred <- ts(mod_regresion$fitted[201:225], start=2006, end=2030, frequency=1)
png("./output/graphs/pred_res_textil.png")
ts.plot(real, pred, gpars = list(col = c("blue", "red"), ylab="Demanda de gas natural"), xlab="Tiempo", main="Resultados del modelo para textil", type="o")
legend("bottomright", legend = c("real", "modelo"), col = c("blue", "red"), lty = 1)
dev.off()
error2 <- mean(sum(mod_regresion$residuals[201:210]^2))
sprintf("El error en 2006-2015 de este modelo para textil es: %s", error2)
error3 <- mean(sum(mod_regresion$residuals[211:225]^2))
sprintf("El error en 2016-2030 de este modelo para textil es: %s", error3)

#Resto
sector <-datos[rama=='Resto',]
real <- ts(sector$demanda, start=2006, end=2030, frequency=1)
pred <- ts(mod_regresion$fitted[226:250], start=2006, end=2030, frequency=1)
png("./output/graphs/pred_res_resto.png")
ts.plot(real, pred, gpars = list(col = c("blue", "red"), ylab="Demanda de gas natural"), xlab="Tiempo", main="Resultados del modelo para Resto", type="o")
legend("bottomright", legend = c("real", "modelo"), col = c("blue", "red"), lty = 1)
dev.off()
error2 <- mean(sum(mod_regresion$residuals[226:235]^2))
sprintf("El error en 2006-2015 de este modelo para el resto de subsectores es: %s", error2)
error3 <- mean(sum(mod_regresion$residuals[236:250]^2))
sprintf("El error en 2016-2030 de este modelo para el resto de subsectores es: %s", error3)

##---------------------------------------------------------------------------------------------------------------------

################################
## FUNCION PARA SUBRAMOS
################################


## Leer datos
datos <- fread("datos_quimica.csv")
#Tipo de estructura
str(datos)
datos$PIB <- as.numeric(datos$PIB) 
names(datos)
names(datos) <- c("cat", "rama", "año", "demanda", "precio_gas", "precio_combu","pib", "efi")
attach(datos)
datos <- datos[, c("año", "demanda", "precio_gas", "precio_combu", "pib"), with = FALSE]

#DEMANDA HISTORICA
n <- length(demanda)
datos$demanda.lag <- c(NA,datos$demanda[1:(n-1)])
attach(datos)
png("./output/graphs/hist.png")
hist(demanda, main="Histograma de demanda", xlab="Demanda", ylab="Número de observaciones", col="skyblue", prob=FALSE, breaks=6, plot=TRUE, xlim=c(100,max(demanda)))
dev.off()
png("./output/graphs/demVSdemlag.png")
plot(demanda.lag, datos$demanda, main="Relación entre demanda pasada y actual", xlab="Demanda pasada", ylab="Demanda actual", col="blue")
dev.off()

###################ANALISIS EXPLORATORIO#####################
#Estandarizar variables para quitar unidades
datos$demanda      <- scale(demanda, center = TRUE, scale = TRUE)
datos$precio_gas   <- scale(precio_gas, center = TRUE, scale = TRUE)
datos$precio_combu <- scale(precio_combu, center = TRUE, scale = TRUE)
datos$pib          <- scale(pib, center = TRUE, scale = TRUE)
n <- length(demanda)
datos$demanda.lag <- c(NA,datos$demanda[1:(n-1)])

##Boxplot del subsector
png("./output/graphs/box_plot_quimica.png")
boxplot(datos[-1,-1], main="Boxplot de variables estandarizadas en subsector", col="skyblue")
dev.off()

#RELACIONES ENTRE VARIABLES
#La demanda historica es una variable que tiene una relacion directa con la demanda. No asi el precio del gas. El precio del PIB deja estabiliza su impacto.
png("./output/graphs/scatter.png")
pairs(~demanda+demanda.lag+precio_gas+precio_combu+pib, data=datos, main="Relaciones entre las variables", col="blue")
dev.off()
#CORRELACIONES
M <- cor((datos[-1,-1]))
corrplot(M, method="number")
corrplot(M, method="number", type="upper")

#datos.corr <- abs(cor(datos[-1,-1])) #obtener correlaciones valor absoluto
#datos.col <-dmat.color(datos.corr)  #obtener colores de acuerdo a las correlaciones
#datos.o <- order.single(datos.corr) #ordenar variables para que las más correlacionadas estén más cerca de la diagonal

#CORRELACIONES Y SCATTERPLOTS
#cpairs(datos[-1,-1], datos.o, panel.colors = datos.col, gap = 0.5, main="Variables ordenadas y coloreadas por correlación")

###########################################MODELOS MINERIA#########################################
#-------------REGRESION--------------#

#Modelo sin tomar en cuenta el orden de tiempo
reg_simple <- lm(demanda ~ precio_gas + precio_combu + pib)
summary(reg_simple)
std.rese <- rstandard(reg_simple)
ts.plot(ts(std.rese, start=2006, frequency=1), xlab="Tiempo", ylab="Residuales estandarizados", main="Autocorrelaciones", col="blue")
#Durbin Watson test para ver AutoCorrelación
dwtest(demanda ~ precio_gas + precio_combu + pib)

#Definimos las variables retrasadas
datos$pib.lag <- c(NA,datos$pib[1:(n-1)])
datos$precio_gas.lag <- c(NA,datos$precio_gas[1:(n-1)])
datos$precio_combu.lag <- c(NA,datos$precio_combu[1:(n-1)])
attach(datos)
#Hacemos una matriz de correlaciones
panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}

png("./output/graphs/matriz_corr.png")
pairs(~demanda + demanda.lag + precio_gas + precio_gas.lag + precio_combu + precio_combu.lag + pib + pib.lag , data=datos[-1,-1],
      lower.panel=panel.smooth, upper.panel=panel.cor, pch=20, main="Matriz de correlaciones y scatterplots para todas las variables", col="blue")
dev.off()

#ajustamos un modelo con todos los lags
mod_regresion <- lm(demanda~demanda.lag + precio_gas + precio_gas.lag + precio_combu + precio_combu.lag + pib + pib.lag)
summary(mod_regresion)

#Durbin Watson test para ver AutoCorrelación
dwtest(demanda~demanda.lag + precio_gas + precio_gas.lag + precio_combu + precio_combu.lag + pib + pib.lag)

########-----------------Resultados--------------------
real <- ts(demanda, start=2006, frequency=1)
pred <- ts(fitted(mod_regresion), start=2006, frequency=1)
png("./output/graphs/pred_res.png")
ts.plot(real, pred, gpars = list(col = c("blue", "red"), ylab="Demanda de gas natural"), xlab="Tiempo", main="Resultados del modelo", type="o")
legend("bottomright", legend = c("real", "modelo"), col = c("blue", "red"), lty = 1)
dev.off()
#Metricas de error
mse <- function(sm) 
  mean(sum(sm$residuals^2))
error <- mse(mod_regresion)
sprintf("El error de este modelo es: %s", error)

error2 <- mean(sum(mod_regresion$residuals[1:10]^2))
sprintf("El error en 2006-2015 de este modelo es: %s", error2)
error3 <- mean(sum(mod_regresion$residuals[11:25]^2))
sprintf("El error en 2016-2030 de este modelo es: %s", error3)

} # //modelo
#---------------------------------------------#


################################################MODELOS ARIMA##################################################
####ALIMENTOS####

datos <- datos[, c("demanda")]
datos <- ts(datos, start = 2005, frequency = 1 )
png("./output/graphs/serie_demanda.png")
ts.plot(datos, data = datos, xlab="Tiempo", ylab="Demanda", main="Serie temporal demanda gas natural", col="blue", type="o")
dev.off()
# Leer datos
#datos <- fread("datos_alimentos.csv")
#Tipo de estructura
#str(datos)
#datos$PIB <- as.numeric(datos$PIB) 
#names(datos)
#names(datos) <- c("cat", "rama", "año", "demanda", "precio_gas", "precio_combu","pib", "efi")
#datos <- datos[, c("año", "demanda", "precio_gas", "precio_combu", "pib"), with = FALSE]
#datos <- datos[, c("año", "demanda")]
#datos <- ts(datos)


#Dickey-fuller standard test para estacionariedad
adf.test(datos, alternative="stationary", k=0)
adf.test(diff(datos), alternative="stationary", k=0)
arima_model <- arima(datos, c(0, 1, 1),seasonal = list(order = c(0, 1, 1), period = 1))
pred2 <- predict(arima_model, n.ahead = 10)
png("./output/graphs/arima_pred.png")
ts.plot(datos,pred2$pred, lty = c(1,2), col=c(4,2), main="Resultados modelo ARIMA", xlab="Tiempo", ylab="Demanda de gas natural")
dev.off()

#  ARIMA vs IMP
png("./output/graphs/arima_resul.png")
ts.plot(datos, datos+arima_model$residuals, gpars = list(col = c("blue", "red"), ylab="Demanda de gas natural"), xlab="Tiempo", main="Resultados modelo ARIMA", type="o")
legend("bottomright", legend = c("real", "modelo"), col = c("blue", "red"), lty = 1)
dev.off()

#Error
error <- mse(arima_model)
sprintf("El error cuadrático medio de este modelo es: %s", error)

# Tres modelos
png("./output/graphs/todos_resul.png")
ts.plot(datos[-1], datos[-1]+arima_model$residuals[-1], pred, gpars = list(col = c("black","blue", "red"), ylab="Demanda de gas natural"), xlab="Tiempo", main="Resultados 2 modelos", type="o")
legend("bottomright", legend = c("real", "arima", "minería"), col = c("black","blue", "red"), lty = 1)
dev.off()
