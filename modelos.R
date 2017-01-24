
####################################################################################################
## Este codigo genera los modelos de prediccion de consumo gas natural por subramo industrial
## Las gráficas de los modelos generados pueden consultarse en la carpeta ouput
## Elaborado por Fernanda Mora para la Secretaría de Energía
## Enero/2017
####################################################################################################


################################
# Librerías
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
library(lazyeval)
library(forecast)

################################
# Creación carpetas resultados
################################
system("mkdir output")
system("mkdir output/graphs")
system("mkdir output/graphs/arima")
system("mkdir output/graphs/exploratorio")
system("mkdir output/graphs/mineria")

################################
# Lectura de datos
################################

##Leemos todos los datos
datos        <- fread("./gas_natural.csv")
datos$PIB    <- readr::parse_number(datos$PIB) 
datos$Anio   <- as.numeric(datos$Anio)
names(datos) <- c("cat", "rama", "anio", "demanda", "precio_gas", "precio_combu","pib", "efi")
datos        <- datos[,-c(1,8), with = FALSE]

## Boxplot de todos los datos
png("./output/graphs/exploratorio/box_plot_all.png")
boxplot(scale(datos[,-1:-2, with = FALSE]), col="blue", main="Boxplot de variables estandarizadas")
dev.off()

################################
# Analisis exploratorio 
################################

sector <-datos[rama=='Alimentos',]
alimentos <- ts(sector$demanda, start=2006, end=2030, frequency=1)
sector <-datos[rama=='Celulosa y papel',]
papel <- ts(sector$demanda, start=2006, end=2030, frequency=1)
sector <-datos[rama=='Cemento y vidrio',]
cem_vid <- ts(sector$demanda, start=2006, end=2030, frequency=1)
sector <-datos[rama=='Cerveza y malta',]
cerv <- ts(sector$demanda, start=2006, end=2030, frequency=1)
sector <-datos[rama=='Metales básicos',]
met <- ts(sector$demanda, start=2006, end=2030, frequency=1)
sector <-datos[rama=='Minería y Productos metálicos, equipo eléctrico y de transporte',]
min <- ts(sector$demanda, start=2006, end=2030, frequency=1)
sector <-datos[rama=='Productos de minerales no metálicos',]
nomet <- ts(sector$demanda, start=2006, end=2030, frequency=1)
sector <-datos[rama=='Química',]
quim <- ts(sector$demanda, start=2006, end=2030, frequency=1)
sector <-datos[rama=='Textil',]
textil <- ts(sector$demanda, start=2006, end=2030, frequency=1)
sector <-datos[rama=='Resto',]
resto <- ts(sector$demanda, start=2006, end=2030, frequency=1)

png("./output/graphs/exploratorio/time_series.png")
ts.plot(alimentos, papel, cem_vid, cerv, met, min, nomet, quim, textil, resto, gpars= list(col=rainbow(10)), ylab="Demanda (millones ft cúbicos)", xlab="Tiempo", main="Demanda de gas natural por subsector industrial (IMP) ")
legend("bottomright", legend = c("alimentos", "papel", "cem&vidrio", "cerveza", "metales", "mineria", "no_metales", "química", "textil", "resto"), col = rainbow(10), lty = 1)
dev.off()


################################
# Preprocesamiento
################################

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


########################################################
########################################################
# Modelo Mineria de datos (conjunto)
########################################################
########################################################

#Ramo Variable dummie
datos$rama <- as.factor(datos$rama)
attach(datos)
mod_regresion <- lm(demanda ~ demanda.lag + precio_gas + precio_gas.lag + precio_combu + precio_combu.lag + pib + pib.lag + rama, data = datos)
summary(mod_regresion)

#Durbin Watson test para ver que no hay AutoCorrelación
dwtest(demanda ~ demanda.lag + precio_gas + precio_gas.lag + precio_combu + precio_combu.lag + pib + pib.lag + rama, data=datos) 

#Metricas de error
mse <- function(sm) 
  sqrt(mean(sm$residuals^2))
error <- mse(mod_regresion)

#===========================================
#Alimentos
#===========================================
sector <-datos[rama=='Alimentos',]

#Boxplot del subsector
png("./output/graphs/exploratorio/box_plot_alimentos.png")
boxplot(sector[,-c(1,2)], main="Boxplot de variables estandarizadas en subsector", col="skyblue")
dev.off()

#RELACIONES ENTRE VARIABLES
#La demanda historica es una variable que tiene una relacion directa con la demanda. 
png("./output/graphs/exploratorio/scatter_quimica.png")
pairs(~demanda+demanda.lag+precio_gas+precio_combu+pib, data=datos, main="Relaciones entre las variables", col="blue")
dev.off()
#CORRELACIONES
M <- cor((sector[-1,-c(1, 2)]))
png("./output/graphs/exploratorio/corplot_quimica.png")
corrplot(M, method="number", type="upper")
dev.off()

#MODELO
real <- ts(sector$demanda, start=2006, end=2030, frequency=1)
pred <- ts(mod_regresion$fitted[1:25], start=2006, end=2030, frequency=1)
png("./output/graphs/pred_res_alimentos.png")
ts.plot(real, pred, gpars = list(col = c("blue", "red"), ylab="Demanda de gas natural"), xlab="Tiempo", main="Resultados del modelo para Alimentos", type="o")
legend("bottomright", legend = c("real", "modelo"), col = c("blue", "red"), lty = 1)
dev.off()

#Resultados
error2 <- sqrt(mean(mod_regresion$residuals[1:10]^2))
sprintf("El error en 2006-2015 para Alimentos de este modelo es: %s", error2)
error3 <- sqrt(mean(mod_regresion$residuals[11:25]^2))
sprintf("El error en 2016-2030 para Alimentos de este modelo es: %s", error3)
accuracy(mod_regresion)
tabla_res_tot <- c()
tabla_res <-data.frame("Sector"='Alimentos', "Modelo"='Mineria', "RMSE_2006-2030"=error2+error3, "RMSE_2006-2015"=error2, "RMSE_2016-2030"=error3)
tabla_res_tot <- rbind(tabla_res_tot, tabla_res)

#===========================================
#Celulosa y papel
#===========================================
sector <-datos[rama=='Celulosa y papel',]

#Boxplot del subsector
png("./output/graphs/exploratorio/box_plot_papel.png")
boxplot(sector[,-c(1,2)], main="Boxplot de variables estandarizadas en subsector", col="skyblue")
dev.off()

#RELACIONES ENTRE VARIABLES
#La demanda historica es una variable que tiene una relacion directa con la demanda. 
png("./output/graphs/exploratorio/scatter_papel.png")
pairs(~demanda+demanda.lag+precio_gas+precio_combu+pib, data=datos, main="Relaciones entre las variables", col="blue")
dev.off()

#CORRELACIONES
M <- cor((sector[-1,-c(1, 2)]))
png("./output/graphs/exploratorio/corplot_papel.png")
corrplot(M, method="number", type="upper")
dev.off()

real <- ts(sector$demanda, start=2006, end=2030, frequency=1)
pred <- ts(mod_regresion$fitted[26:50], start=2006, end=2030, frequency=1)
png("./output/graphs/pred_res_cel.png")
ts.plot(real, pred, gpars = list(col = c("blue", "red"), ylab="Demanda de gas natural"), xlab="Tiempo", main="Resultados del modelo para Celulosa y papel", type="o")
legend("bottomright", legend = c("real", "modelo"), col = c("blue", "red"), lty = 1)
dev.off()

#Resultados
error2 <- sqrt(mean(mod_regresion$residuals[26:35]^2))
sprintf("El error en 2006-2015 para celulosa y papel de este modelo es: %s", error2)
error3 <- sqrt(mean(mod_regresion$residuals[36:50]^2))
sprintf("El error en 2016-2030 para celulosa y papel de este modelo es: %s", error3)
tabla_res <-data.frame("Sector"='Celulosa y papel', "Modelo"='Mineria', "RMSE_2006-2030"=error2+error3, "RMSE_2006-2015"=error2, "RMSE_2016-2030"=error3)
tabla_res_tot <- rbind(tabla_res_tot, tabla_res)

#===========================================
#Cemento y vidrio
#===========================================
sector <-datos[rama=='Cemento y vidrio',]

#Boxplot del subsector
png("./output/graphs/exploratorio/box_plot_cemento.png")
boxplot(sector[,-c(1,2)], main="Boxplot de variables estandarizadas en subsector", col="skyblue")
dev.off()

#RELACIONES ENTRE VARIABLES
#La demanda historica es una variable que tiene una relacion directa con la demanda. 
png("./output/graphs/exploratorio/scatter_cemento.png")
pairs(~demanda+demanda.lag+precio_gas+precio_combu+pib, data=datos, main="Relaciones entre las variables", col="blue")
dev.off()

#CORRELACIONES
M <- cor((sector[-1,-c(1, 2)]))
png("./output/graphs/exploratorio/corplot_cemento.png")
corrplot(M, method="number", type="upper")
dev.off()


#Resultados
real <- ts(sector$demanda, start=2006, end=2030, frequency=1)
pred <- ts(mod_regresion$fitted[51:75], start=2006, end=2030, frequency=1)
png("./output/graphs/pred_res_cem.png")
ts.plot(real, pred, gpars = list(col = c("blue", "red"), ylab="Demanda de gas natural"), xlab="Tiempo", main="Resultados del modelo para Cemento y vidrio", type="o")
legend("bottomright", legend = c("real", "modelo"), col = c("blue", "red"), lty = 1)
dev.off()

error2 <- sqrt(mean(mod_regresion$residuals[51:60]^2))
sprintf("El error en 2006-2015 para cemento y vidrio de este modelo es: %s", error2)
error3 <- sqrt(mean(mod_regresion$residuals[61:75]^2))
sprintf("El error en 2016-2030 para cemento y vidrio de este modelo es: %s", error3)
accuracy(mod_regresion)
tabla_res <-data.frame("Sector"='Cemento y vidrio', "Modelo"='Mineria', "RMSE_2006-2030"=error2+error3, "RMSE_2006-2015"=error2, "RMSE_2016-2030"=error3)
tabla_res_tot <- rbind(tabla_res_tot, tabla_res)

#===========================================
#Cerveza y malta
#===========================================
sector <-datos[rama=='Cerveza y malta',]

#Boxplot del subsector
png("./output/graphs/exploratorio/box_plot_cerveza.png")
boxplot(sector[,-c(1,2)], main="Boxplot de variables estandarizadas en subsector", col="skyblue")
dev.off()

#RELACIONES ENTRE VARIABLES
#La demanda historica es una variable que tiene una relacion directa con la demanda. 
png("./output/graphs/exploratorio/scatter_cerveza.png")
pairs(~demanda+demanda.lag+precio_gas+precio_combu+pib, data=datos, main="Relaciones entre las variables", col="blue")
dev.off()

#CORRELACIONES
M <- cor((sector[-1,-c(1, 2)]))
png("./output/graphs/exploratorio/corplot_cerveza.png")
corrplot(M, method="number", type="upper")
dev.off()

#Resultados
real <- ts(sector$demanda, start=2006, end=2030, frequency=1)
pred <- ts(mod_regresion$fitted[76:100], start=2006, end=2030, frequency=1)
png("./output/graphs/pred_res_cer.png")
ts.plot(real, pred, gpars = list(col = c("blue", "red"), ylab="Demanda de gas natural"), xlab="Tiempo", main="Resultados del modelo para cerveza y malta", type="o")
legend("bottomright", legend = c("real", "modelo"), col = c("blue", "red"), lty = 1)
dev.off()

error2 <- sqrt(mean(mod_regresion$residuals[76:85]^2))
sprintf("El error en 2006-2015 de este modelo para cerveza y malta es: %s", error2)
error3 <- sqrt(mean(mod_regresion$residuals[86:100]^2))
sprintf("El error en 2016-2030 de este modelo para cerveza y malta es: %s", error3)
accuracy(mod_regresion)
tabla_res <-data.frame("Sector"='Cerveza y malta', "Modelo"='Mineria', "RMSE_2006-2030"=error2+error3, "RMSE_2006-2015"=error2, "RMSE_2016-2030"=error3)
tabla_res_tot <- rbind(tabla_res_tot, tabla_res)


#===========================================
#Metales básicos
#===========================================
sector <-datos[rama=='Metales básicos',]

#Boxplot del subsector
png("./output/graphs/exploratorio/box_plot_metales.png")
boxplot(sector[,-c(1,2)], main="Boxplot de variables estandarizadas en subsector", col="skyblue")
dev.off()

#RELACIONES ENTRE VARIABLES
#La demanda historica es una variable que tiene una relacion directa con la demanda. 
png("./output/graphs/exploratorio/scatter_metales.png")
pairs(~demanda+demanda.lag+precio_gas+precio_combu+pib, data=datos, main="Relaciones entre las variables", col="blue")
dev.off()

#CORRELACIONES
M <- cor((sector[-1,-c(1, 2)]))
png("./output/graphs/exploratorio/corplot_metales.png")
corrplot(M, method="number", type="upper")
dev.off()

#Resultados
real <- ts(sector$demanda, start=2006, end=2030, frequency=1)
pred <- ts(mod_regresion$fitted[101:125], start=2006, end=2030, frequency=1)
png("./output/graphs/pred_res_metbas.png")
ts.plot(real, pred, gpars = list(col = c("blue", "red"), ylab="Demanda de gas natural"), xlab="Tiempo", main="Resultados del modelo para metales básicos", type="o")
legend("bottomright", legend = c("real", "modelo"), col = c("blue", "red"), lty = 1)
dev.off()

error2 <- sqrt(mean(mod_regresion$residuals[101:110]^2))
sprintf("El error en 2006-2015 de este modelo para metales básicos es: %s", error2)
error3 <- sqrt(mean(mod_regresion$residuals[111:125]^2))
sprintf("El error en 2016-2030 de este modelo para metales básicos es: %s", error3)
accuracy(mod_regresion)
tabla_res <-data.frame("Sector"='Metales básicos', "Modelo"='Mineria', "RMSE_2006-2030"=error2+error3, "RMSE_2006-2015"=error2, "RMSE_2016-2030"=error3)
tabla_res_tot <- rbind(tabla_res_tot, tabla_res)

#===========================================
#Minería y Productos metálicos, equipo eléctrico y de transporte
#===========================================
sector <-datos[rama=='Minería y Productos metálicos, equipo eléctrico y de transporte',]

#Boxplot del subsector
png("./output/graphs/exploratorio/box_plot_mineria.png")
boxplot(sector[,-c(1,2)], main="Boxplot de variables estandarizadas en subsector", col="skyblue")
dev.off()

#RELACIONES ENTRE VARIABLES
#La demanda historica es una variable que tiene una relacion directa con la demanda. 
png("./output/graphs/exploratorio/scatter_mineria.png")
pairs(~demanda+demanda.lag+precio_gas+precio_combu+pib, data=datos, main="Relaciones entre las variables", col="blue")
dev.off()

#CORRELACIONES
M <- cor((sector[-1,-c(1, 2)]))
png("./output/graphs/exploratorio/corplot_mineria.png")
corrplot(M, method="number", type="upper")
dev.off()

#Resultados
real <- ts(sector$demanda, start=2006, end=2030, frequency=1)
pred <- ts(mod_regresion$fitted[126:150], start=2006, end=2030, frequency=1)
png("./output/graphs/pred_res_min.png")
ts.plot(real, pred, gpars = list(col = c("blue", "red"), ylab="Demanda de gas natural"), xlab="Tiempo", main="Resultados para minería y productos metálicos, equipo eléctrico y de transporte", type="o")
legend("bottomright", legend = c("real", "modelo"), col = c("blue", "red"), lty = 1)
dev.off()

error2 <- sqrt(mean(mod_regresion$residuals[126:135]^2))
sprintf("El error en 2006-2015 de este modelo para minería y productos metálicos es: %s", error2)
error3 <- sqrt(mean(mod_regresion$residuals[136:150]^2))
sprintf("El error en 2016-2030 de este modelo para minería y productos metálicos es: %s", error3)
accuracy(mod_regresion)
tabla_res <-data.frame("Sector"='Minería y prod. metálicos', "Modelo"='Mineria', "RMSE_2006-2030"=error2+error3, "RMSE_2006-2015"=error2, "RMSE_2016-2030"=error3)
tabla_res_tot <- rbind(tabla_res_tot, tabla_res)

#===========================================
#Productos de minerales no metálicos
#===========================================
sector <-datos[rama=='Productos de minerales no metálicos',]

#Boxplot del subsector
png("./output/graphs/exploratorio/box_plot_nometal.png")
boxplot(sector[,-c(1,2)], main="Boxplot de variables estandarizadas en subsector", col="skyblue")
dev.off()

#RELACIONES ENTRE VARIABLES
#La demanda historica es una variable que tiene una relacion directa con la demanda. 
png("./output/graphs/exploratorio/scatter_nometal.png")
pairs(~demanda+demanda.lag+precio_gas+precio_combu+pib, data=datos, main="Relaciones entre las variables", col="blue")
dev.off()

#CORRELACIONES
M <- cor((sector[-1,-c(1, 2)]))
png("./output/graphs/exploratorio/corplot_nometal.png")
corrplot(M, method="number", type="upper")
dev.off()

real <- ts(sector$demanda, start=2006, end=2030, frequency=1)
pred <- ts(mod_regresion$fitted[151:175], start=2006, end=2030, frequency=1)
png("./output/graphs/pred_res_nomet.png")
ts.plot(real, pred, gpars = list(col = c("blue", "red"), ylab="Demanda de gas natural"), xlab="Tiempo", main="Resultados del modelo para productos de minerales no metálicos", type="o")
legend("bottomright", legend = c("real", "modelo"), col = c("blue", "red"), lty = 1)
dev.off()

#Resultados
error2 <- sqrt(mean(mod_regresion$residuals[151:160]^2))
sprintf("El error en 2006-2015 de este modelo para productos de minerales no metálicos es: %s", error2)
error3 <- sqrt(mean(mod_regresion$residuals[161:175]^2))
sprintf("El error en 2016-2030 de este modelo para productos de minerales no metálicos es: %s", error3)
accuracy(mod_regresion)

tabla_res <-data.frame("Sector"='Productos de minerales no metálicos', "Modelo"='Mineria', "RMSE_2006-2030"=error2+error3, "RMSE_2006-2015"=error2, "RMSE_2016-2030"=error3)
tabla_res_tot <- rbind(tabla_res_tot, tabla_res)

#===========================================
#Química
#===========================================
sector <-datos[rama=='Química',]

#Boxplot del subsector
png("./output/graphs/exploratorio/box_plot_quimica.png")
boxplot(sector[,-c(1,2)], main="Boxplot de variables estandarizadas en subsector", col="skyblue")
dev.off()

#RELACIONES ENTRE VARIABLES
#La demanda historica es una variable que tiene una relacion directa con la demanda. 
png("./output/graphs/exploratorio/scatter_quimica.png")
pairs(~demanda+demanda.lag+precio_gas+precio_combu+pib, data=datos, main="Relaciones entre las variables", col="blue")
dev.off()

#CORRELACIONES
M <- cor((sector[-1,-c(1, 2)]))
png("./output/graphs/exploratorio/corplot_quimica.png")
corrplot(M, method="number", type="upper")
dev.off()

#Resultados
real <- ts(sector$demanda, start=2006, end=2030, frequency=1)
pred <- ts(mod_regresion$fitted[176:200], start=2006, end=2030, frequency=1)
png("./output/graphs/pred_res_quim.png")
ts.plot(real, pred, gpars = list(col = c("blue", "red"), ylab="Demanda de gas natural"), xlab="Tiempo", main="Resultados del modelo para Química", type="o")
legend("bottomright", legend = c("real", "modelo"), col = c("blue", "red"), lty = 1)
dev.off()

error2 <- sqrt(mean(mod_regresion$residuals[176:185]^2))
sprintf("El error en 2006-2015 de este modelo para química es: %s", error2)
error3 <- sqrt(mean(mod_regresion$residuals[186:200]^2))
sprintf("El error en 2016-2030 de este modelo para química es: %s", error3)
accuracy(mod_regresion)
tabla_res <-data.frame("Sector"='Química', "Modelo"='Mineria', "RMSE_2006-2030"=error2+error3, "RMSE_2006-2015"=error2, "RMSE_2016-2030"=error3)
tabla_res_tot <- rbind(tabla_res_tot, tabla_res)

#===========================================
#Textil
#===========================================
sector <-datos[rama=='Textil',]

#Boxplot del subsector
png("./output/graphs/exploratorio/box_plot_textil.png")
boxplot(sector[,-c(1,2)], main="Boxplot de variables estandarizadas en subsector", col="skyblue")
dev.off()

#RELACIONES ENTRE VARIABLES
#La demanda historica es una variable que tiene una relacion directa con la demanda. 
png("./output/graphs/exploratorio/scatter_textil.png")
pairs(~demanda+demanda.lag+precio_gas+precio_combu+pib, data=datos, main="Relaciones entre las variables", col="blue")
dev.off()

#CORRELACIONES
M <- cor((sector[-1,-c(1, 2)]))
png("./output/graphs/exploratorio/corplot_textil.png")
corrplot(M, method="number", type="upper")
dev.off()

#Resultados
real <- ts(sector$demanda, start=2006, end=2030, frequency=1)
pred <- ts(mod_regresion$fitted[201:225], start=2006, end=2030, frequency=1)
png("./output/graphs/pred_res_textil.png")
ts.plot(real, pred, gpars = list(col = c("blue", "red"), ylab="Demanda de gas natural"), xlab="Tiempo", main="Resultados del modelo para textil", type="o")
legend("bottomright", legend = c("real", "modelo"), col = c("blue", "red"), lty = 1)
dev.off()

error2 <- sqrt(mean(mod_regresion$residuals[201:210]^2))
sprintf("El error en 2006-2015 de este modelo para textil es: %s", error2)
error3 <- sqrt(mean(mod_regresion$residuals[211:225]^2))
sprintf("El error en 2016-2030 de este modelo para textil es: %s", error3)
accuracy(mod_regresion)
tabla_res <-data.frame("Sector"='Textil', "Modelo"='Mineria', "RMSE_2006-2030"=error2+error3, "RMSE_2006-2015"=error2, "RMSE_2016-2030"=error3)
tabla_res_tot <- rbind(tabla_res_tot, tabla_res)

#===========================================
#Resto
#===========================================
sector <-datos[rama=='Resto',]

#Boxplot del subsector
png("./output/graphs/exploratorio/box_plot_resto.png")
boxplot(sector[,-c(1,2)], main="Boxplot de variables estandarizadas en subsector", col="skyblue")
dev.off()

#RELACIONES ENTRE VARIABLES
#La demanda historica es una variable que tiene una relacion directa con la demanda. 
png("./output/graphs/exploratorio/scatter_resto.png")
pairs(~demanda+demanda.lag+precio_gas+precio_combu+pib, data=datos, main="Relaciones entre las variables", col="blue")
dev.off()

#CORRELACIONES
M <- cor((sector[-1,-c(1, 2)]))
png("./output/graphs/exploratorio/corplot_resto.png")
corrplot(M, method="number", type="upper")
dev.off()

#Resultados
real <- ts(sector$demanda, start=2006, end=2030, frequency=1)
pred <- ts(mod_regresion$fitted[226:250], start=2006, end=2030, frequency=1)
png("./output/graphs/pred_res_resto.png")
ts.plot(real, pred, gpars = list(col = c("blue", "red"), ylab="Demanda de gas natural"), xlab="Tiempo", main="Resultados del modelo para resto de subsectores", type="o")
legend("bottomright", legend = c("real", "modelo"), col = c("blue", "red"), lty = 1)
dev.off()

error2 <- sqrt(mean(mod_regresion$residuals[226:235]^2))
sprintf("El error en 2006-2015 de este modelo para el resto de subsectores es: %s", error2)
error3 <- sqrt(mean(mod_regresion$residuals[236:250]^2))
sprintf("El error en 2016-2030 de este modelo para el resto de subsectores es: %s", error3)
accuracy(mod_regresion)
tabla_res <-data.frame("Sector"='Resto', "Modelo"='Mineria', "RMSE_2006-2030"=error2+error3, "RMSE_2006-2015"=error2, "RMSE_2016-2030"=error3)
tabla_res_tot <- rbind(tabla_res_tot, tabla_res)

########################################################
########################################################
# Modelo ARIMA
########################################################
########################################################

#===========================================
#Alimentos
#===========================================
#Serie tiempo
sector <-datos[rama=='Alimentos',]
sector <- ts(sector$demanda, start = 2005, frequency = 1 )
png("./output/graphs/exploratorio/serie_demanda_alimentos.png")
ts.plot(sector, data = sector, xlab="Tiempo", ylab="Demanda", main="Serie temporal demanda gas natural para Alimentos", col="blue", type="o")
dev.off()

#Usar Autoarima: best stationary ARIMA model for the time series (Hyndman-Khandakar) [estacionariedad, seleccion de parametros y mejor modelo]
fit <- auto.arima(sector, trace=TRUE, stationary=TRUE)
arimaorder(fit)
png("./output/graphs/arima/arima_pred_alimentos.png")
plot(forecast(fit,h=10), ylab="Demanda gas natural para Alimentos", xlab="Tiempo")
dev.off()

#Ljung-box test para ver que residuales son ruido blanco, null hypothesis of residuals independence (i.e.correlation is cero), fitdf=p+q
Box.test(residuals(fit), lag=20, fitdf=3, type="Ljung")

#  ARIMA vs IMP
png("./output/graphs/arima/arima_resul_alimentos.png")
ts.plot(fit$x, fitted(fit), gpars = list(col = c("blue", "red"), ylab="Demanda de gas natural"), xlab="Tiempo", main="Resultados modelo ARIMA para alimentos", type="o")
legend("bottomright", legend = c("IMP", "modelo"), col = c("blue", "red"), lty = 1)
dev.off()

error2 <- sqrt(mean(fit$residuals[1:15]^2))
sprintf("El error en 2006-2015 de este modelo para el resto de subsectores es: %s", error2)
error3 <- sqrt(mean(fit$residuals[16:26]^2))
sprintf("El error en 2016-2030 de este modelo para el resto de subsectores es: %s", error3)

tabla_res <-data.frame("Sector"='Alimentos', "Modelo"='ARIMA', "RMSE_2006-2030"=error2+error3, "RMSE_2006-2015"=error2, "RMSE_2016-2030"=error3)
tabla_res_tot <- rbind(tabla_res_tot, tabla_res)

#===========================================
#Celulosa y papel
#===========================================
#Serie tiempo
sector <-datos[rama=='Celulosa y papel',]
sector <- ts(sector$demanda, start = 2005, frequency = 1 )
png("./output/graphs/exploratorio/serie_demanda_papel.png")
ts.plot(sector, data = sector, xlab="Tiempo", ylab="Demanda", main="Serie temporal demanda gas natural para papel", col="blue", type="o")
dev.off()

#Usar Autoarima: best stationary ARIMA model for the time series (Hyndman-Khandakar) [estacionariedad, seleccion de parametros y mejor modelo]
fit <- auto.arima(sector, trace=TRUE, stationary=TRUE)
arimaorder(fit)
png("./output/graphs/arima/arima_pred_papel.png")
plot(forecast(fit,h=10), ylab="Demanda gas natural para Papel", xlab="Tiempo")
dev.off()

#Ljung-box test para ver que residuales son ruido blanco, null hypothesis of residuals independence (i.e.correlation is cero), fitdf=p+q
Box.test(residuals(fit), lag=20, fitdf=1, type="Ljung")

#  ARIMA vs IMP
png("./output/graphs/arima/arima_resul_papel.png")
ts.plot(fit$x, fitted(fit), gpars = list(col = c("blue", "red"), ylab="Demanda de gas natural"), xlab="Tiempo", main="Resultados modelo ARIMA para papel", type="o")
legend("bottomright", legend = c("IMP", "modelo"), col = c("blue", "red"), lty = 1)
dev.off()

error2 <- sqrt(mean(fit$residuals[1:15]^2))
sprintf("El error en 2006-2015 de este modelo para el resto de subsectores es: %s", error2)
error3 <- sqrt(mean(fit$residuals[16:26]^2))
sprintf("El error en 2016-2030 de este modelo para el resto de subsectores es: %s", error3)

tabla_res <-data.frame("Sector"='Celulosa y papel', "Modelo"='ARIMA', "RMSE_2006-2030"=error2+error3, "RMSE_2006-2015"=error2, "RMSE_2016-2030"=error3)
tabla_res_tot <- rbind(tabla_res_tot, tabla_res)

#===========================================
#Cemento y vidrio
#===========================================
sector <-datos[rama=='Cemento y vidrio',]
sector <- ts(sector$demanda, start = 2005, frequency = 1 )
png("./output/graphs/exploratorio/serie_demanda_cemento.png")
ts.plot(sector, data = sector, xlab="Tiempo", ylab="Demanda", main="Serie temporal demanda gas natural para cemento y vidrio", col="blue", type="o")
dev.off()

#Usar Autoarima: best stationary ARIMA model for the time series (Hyndman-Khandakar) [estacionariedad, seleccion de parametros y mejor modelo]
fit <- auto.arima(sector, trace=TRUE, stationary=TRUE)
arimaorder(fit)
png("./output/graphs/arima/arima_pred_cemento.png")
plot(forecast(fit,h=10), ylab="Demanda gas natural para cemento y vidrio", xlab="Tiempo")
dev.off()

#Ljung-box test para ver que residuales son ruido blanco, null hypothesis of residuals independence (i.e.correlation is cero), fitdf=p+q
Box.test(residuals(fit), lag=20, fitdf=1, type="Ljung")

#  ARIMA vs IMP
png("./output/graphs/arima/arima_resul_cemento.png")
ts.plot(fit$x, fitted(fit), gpars = list(col = c("blue", "red"), ylab="Demanda de gas natural"), xlab="Tiempo", main="Resultados modelo ARIMA para cemento y vidrio", type="o")
legend("bottomright", legend = c("real", "modelo"), col = c("blue", "red"), lty = 1)
dev.off()

error2 <- sqrt(mean(fit$residuals[1:15]^2))
sprintf("El error en 2006-2015 de este modelo para el resto de subsectores es: %s", error2)
error3 <- sqrt(mean(fit$residuals[16:26]^2))
sprintf("El error en 2016-2030 de este modelo para el resto de subsectores es: %s", error3)

tabla_res <-data.frame("Sector"='Cemento y vidrio', "Modelo"='ARIMA', "RMSE_2006-2030"=error2+error3, "RMSE_2006-2015"=error2, "RMSE_2016-2030"=error3)
tabla_res_tot <- rbind(tabla_res_tot, tabla_res)

#===========================================
#Cerveza y malta
#===========================================
sector <-datos[rama=='Cerveza y malta',]
sector <- ts(sector$demanda, start = 2005, frequency = 1 )
png("./output/graphs/exploratorio/serie_demanda_cerveza.png")
ts.plot(sector, data = sector, xlab="Tiempo", ylab="Demanda", main="Serie temporal demanda gas natural para cerveza y malta", col="blue", type="o")
dev.off()

#Usar Autoarima: best stationary ARIMA model for the time series (Hyndman-Khandakar) [estacionariedad, seleccion de parametros y mejor modelo]
fit <- auto.arima(sector, trace=TRUE, stationary=TRUE)
arimaorder(fit)
png("./output/graphs/arima/arima_pred_cerv.png")
plot(forecast(fit,h=10), ylab="Demanda gas natural para cerveza y malta", xlab="Tiempo")
dev.off()

#Ljung-box test para ver que residuales son ruido blanco, null hypothesis of residuals independence (i.e.correlation is cero), fitdf=p+q
Box.test(residuals(fit), lag=20, fitdf=1, type="Ljung")

#  ARIMA vs IMP
png("./output/graphs/arima/arima_resul_cerv.png")
ts.plot(fit$x, fitted(fit), gpars = list(col = c("blue", "red"), ylab="Demanda de gas natural"), xlab="Tiempo", main="Resultados modelo ARIMA para cerveza y malta", type="o")
legend("bottomright", legend = c("real", "modelo"), col = c("blue", "red"), lty = 1)
dev.off()

error2 <- sqrt(mean(fit$residuals[1:15]^2))
sprintf("El error en 2006-2015 de este modelo para el resto de subsectores es: %s", error2)
error3 <- sqrt(mean(fit$residuals[16:26]^2))
sprintf("El error en 2016-2030 de este modelo para el resto de subsectores es: %s", error3)

tabla_res <-data.frame("Sector"='Cerveza y malta', "Modelo"='ARIMA', "RMSE_2006-2030"=error2+error3, "RMSE_2006-2015"=error2, "RMSE_2016-2030"=error3)
tabla_res_tot <- rbind(tabla_res_tot, tabla_res)

#===========================================
#Metales básicos
#===========================================
sector <-datos[rama=='Metales básicos',]
sector <- ts(sector$demanda, start = 2005, frequency = 1 )
png("./output/graphs/exploratorio/serie_demanda_metales.png")
ts.plot(sector, data = sector, xlab="Tiempo", ylab="Demanda", main="Serie temporal demanda gas natural para metales básicos", col="blue", type="o")
dev.off()

#Usar Autoarima: best stationary ARIMA model for the time series (Hyndman-Khandakar) [estacionariedad, seleccion de parametros y mejor modelo]
fit <- auto.arima(sector, trace=TRUE, stationary=TRUE)
arimaorder(fit)
png("./output/graphs/arima/arima_pred_metal.png")
plot(forecast(fit,h=10), ylab="Demanda gas natural para metales básicos", xlab="Tiempo")
dev.off()

#Ljung-box test para ver que residuales son ruido blanco, null hypothesis of residuals independence (i.e.correlation is cero), fitdf=p+q
Box.test(residuals(fit), lag=20, fitdf=1, type="Ljung")

#  ARIMA vs IMP
png("./output/graphs/arima/arima_resul_metal.png")
ts.plot(fit$x, fitted(fit), gpars = list(col = c("blue", "red"), ylab="Demanda de gas natural"), xlab="Tiempo", main="Resultados modelo ARIMA para metales básicos", type="o")
legend("bottomright", legend = c("real", "modelo"), col = c("blue", "red"), lty = 1)
dev.off()

error2 <- sqrt(mean(fit$residuals[1:15]^2))
sprintf("El error en 2006-2015 de este modelo para el resto de subsectores es: %s", error2)
error3 <- sqrt(mean(fit$residuals[16:26]^2))
sprintf("El error en 2016-2030 de este modelo para el resto de subsectores es: %s", error3)

tabla_res <-data.frame("Sector"='Metales básicos', "Modelo"='ARIMA', "RMSE_2006-2030"=error2+error3, "RMSE_2006-2015"=error2, "RMSE_2016-2030"=error3)
tabla_res_tot <- rbind(tabla_res_tot, tabla_res)

#===========================================
#Minería y Productos metálicos, equipo eléctrico y de transporte
#===========================================
sector <-datos[rama=='Minería y Productos metálicos, equipo eléctrico y de transporte',]
sector <- ts(sector$demanda, start = 2005, frequency = 1 )
png("./output/graphs/exploratorio/serie_demanda_mineria.png")
ts.plot(sector, data = sector, xlab="Tiempo", ylab="Demanda", main="Serie demanda gas natural para minería y productos metálicos", col="blue", type="o")
dev.off()

#Usar Autoarima: best stationary ARIMA model for the time series (Hyndman-Khandakar) [estacionariedad, seleccion de parametros y mejor modelo]
fit <- auto.arima(sector, trace=TRUE, stationary=TRUE)
arimaorder(fit)
png("./output/graphs/arima/arima_pred_mineria.png")
plot(forecast(fit,h=10), ylab="Demanda gas natural para minería y productos metálicos", xlab="Tiempo")
dev.off()

#Ljung-box test para ver que residuales son ruido blanco, null hypothesis of residuals independence (i.e.correlation is cero), fitdf=p+q
Box.test(residuals(fit), lag=20, fitdf=2, type="Ljung")

#  ARIMA vs IMP
png("./output/graphs/arima/arima_resul_mineria.png")
ts.plot(fit$x, fitted(fit), gpars = list(col = c("blue", "red"), ylab="Demanda de gas natural"), xlab="Tiempo", main="Resultados modelo ARIMA para minería y productos metálicos", type="o")
legend("bottomright", legend = c("real", "modelo"), col = c("blue", "red"), lty = 1)
dev.off()

error2 <- sqrt(mean(fit$residuals[1:15]^2))
sprintf("El error en 2006-2015 de este modelo para el resto de subsectores es: %s", error2)
error3 <- sqrt(mean(fit$residuals[16:26]^2))
sprintf("El error en 2016-2030 de este modelo para el resto de subsectores es: %s", error3)

tabla_res <-data.frame("Sector"='Minería y prod. metálicos', "Modelo"='ARIMA', "RMSE_2006-2030"=error2+error3, "RMSE_2006-2015"=error2, "RMSE_2016-2030"=error3)
tabla_res_tot <- rbind(tabla_res_tot, tabla_res)

#===========================================
#Productos de minerales no metálicos
#===========================================
sector <-datos[rama=='Productos de minerales no metálicos',]
sector <- ts(sector$demanda, start = 2005, frequency = 1 )
png("./output/graphs/exploratorio/serie_demanda_nometal.png")
ts.plot(sector, data = sector, xlab="Tiempo", ylab="Demanda", main="Demanda gas natural para productos de minerales no metálicos", col="blue", type="o")
dev.off()

#Usar Autoarima: best stationary ARIMA model for the time series (Hyndman-Khandakar) [estacionariedad, seleccion de parametros y mejor modelo]
fit <- auto.arima(sector, trace=TRUE, stationary=TRUE)
arimaorder(fit)
png("./output/graphs/arima/arima_pred_nometal.png")
plot(forecast(fit,h=10), ylab="Demanda gas natural para productos de minerales no metálicos", xlab="Tiempo")
dev.off()

#Ljung-box test para ver que residuales son ruido blanco, null hypothesis of residuals independence (i.e.correlation is cero), fitdf=p+q
Box.test(residuals(fit), lag=20, fitdf=2, type="Ljung")

#  ARIMA vs IMP
png("./output/graphs/arima/arima_resul_nometal.png")
ts.plot(fit$x, fitted(fit), gpars = list(col = c("blue", "red"), ylab="Demanda de gas natural"), xlab="Tiempo", main="Resultados modelo ARIMA para productos de minerales no metálicos", type="o")
legend("bottomright", legend = c("real", "modelo"), col = c("blue", "red"), lty = 1)
dev.off()

#Error
error <- mse(fit)
sprintf("El error cuadrático medio de este modelo es: %s", error)
accuracy(fit)

error2 <- sqrt(mean(fit$residuals[1:15]^2))
sprintf("El error en 2006-2015 de este modelo para el resto de subsectores es: %s", error2)
error3 <- sqrt(mean(fit$residuals[16:26]^2))
sprintf("El error en 2016-2030 de este modelo para el resto de subsectores es: %s", error3)

tabla_res <-data.frame("Sector"='Productos de minerales no metálicos', "Modelo"='ARIMA', "RMSE_2006-2030"=error2+error3, "RMSE_2006-2015"=error2, "RMSE_2016-2030"=error3)
tabla_res_tot <- rbind(tabla_res_tot, tabla_res)

#===========================================
#Química
#===========================================
sector <-datos[rama=='Química',]
sector <- ts(sector$demanda, start = 2005, frequency = 1 )
png("./output/graphs/exploratorio/serie_demanda_quimica.png")
ts.plot(sector, data = sector, xlab="Tiempo", ylab="Demanda", main="Serie temporal demanda gas natural para química", col="blue", type="o")
dev.off()

#Usar Autoarima: best stationary ARIMA model for the time series (Hyndman-Khandakar) [estacionariedad, seleccion de parametros y mejor modelo]
fit <- auto.arima(sector, trace=TRUE, stationary=TRUE)
arimaorder(fit)
png("./output/graphs/arima/arima_pred_quimica.png")
plot(forecast(fit,h=10), ylab="Demanda gas natural para quimica", xlab="Tiempo")
dev.off()

#Ljung-box test para ver que residuales son ruido blanco, null hypothesis of residuals independence (i.e.correlation is cero), fitdf=p+q
Box.test(residuals(fit), lag=20, fitdf=1, type="Ljung")

#  ARIMA vs IMP
png("./output/graphs/arima/arima_resul_quimica.png")
ts.plot(fit$x, fitted(fit), gpars = list(col = c("blue", "red"), ylab="Demanda de gas natural"), xlab="Tiempo", main="Resultados modelo ARIMA para química", type="o")
legend("bottomright", legend = c("real", "modelo"), col = c("blue", "red"), lty = 1)
dev.off()

error2 <- sqrt(mean(fit$residuals[1:15]^2))
sprintf("El error en 2006-2015 de este modelo para el resto de subsectores es: %s", error2)
error3 <- sqrt(mean(fit$residuals[16:26]^2))
sprintf("El error en 2016-2030 de este modelo para el resto de subsectores es: %s", error3)

tabla_res <-data.frame("Sector"='Química', "Modelo"='ARIMA', "RMSE_2006-2030"=error2+error3, "RMSE_2006-2015"=error2, "RMSE_2016-2030"=error3)
tabla_res_tot <- rbind(tabla_res_tot, tabla_res)


#===========================================
#Textil
#===========================================
sector <-datos[rama=='Textil',]
sector <- ts(sector$demanda, start = 2005, frequency = 1 )
png("./output/graphs/exploratorio/serie_demanda_textil.png")
ts.plot(sector, data = sector, xlab="Tiempo", ylab="Demanda", main="Serie temporal demanda gas natural para textil", col="blue", type="o")
dev.off()

#Usar Autoarima: best stationary ARIMA model for the time series (Hyndman-Khandakar) [estacionariedad, seleccion de parametros y mejor modelo]
fit <- auto.arima(sector, trace=TRUE, stationary=TRUE)
arimaorder(fit)
png("./output/graphs/arima/arima_pred_textil.png")
plot(forecast(fit,h=10), ylab="Demanda gas natural para quimica", xlab="Tiempo")
dev.off()

#Ljung-box test para ver que residuales son ruido blanco, null hypothesis of residuals independence (i.e.correlation is cero), fitdf=p+q
Box.test(residuals(fit), lag=20, fitdf=3, type="Ljung")

#  ARIMA vs IMP
png("./output/graphs/arima/arima_resul_textil.png")
ts.plot(fit$x, fitted(fit), gpars = list(col = c("blue", "red"), ylab="Demanda de gas natural"), xlab="Tiempo", main="Resultados modelo ARIMA para textil", type="o")
legend("bottomright", legend = c("real", "modelo"), col = c("blue", "red"), lty = 1)
dev.off()

error2 <- sqrt(mean(fit$residuals[1:15]^2))
sprintf("El error en 2006-2015 de este modelo para el resto de subsectores es: %s", error2)
error3 <- sqrt(mean(fit$residuals[16:26]^2))
sprintf("El error en 2016-2030 de este modelo para el resto de subsectores es: %s", error3)

tabla_res <-data.frame("Sector"='Textil', "Modelo"='ARIMA', "RMSE_2006-2030"=error2+error3, "RMSE_2006-2015"=error2, "RMSE_2016-2030"=error3)
tabla_res_tot <- rbind(tabla_res_tot, tabla_res)

#===========================================
#Resto
#===========================================
sector <-datos[rama=='Resto',]
sector <- ts(sector$demanda, start = 2005, frequency = 1 )
png("./output/graphs/exploratorio/serie_demanda_resto.png")
ts.plot(sector, data = sector, xlab="Tiempo", ylab="Demanda", main="Serie temporal demanda gas natural para resto", col="blue", type="o")
dev.off()

#Usar Autoarima: best stationary ARIMA model for the time series (Hyndman-Khandakar) [estacionariedad, seleccion de parametros y mejor modelo]
fit <- auto.arima(sector, trace=TRUE, stationary=TRUE)
arimaorder(fit)
png("./output/graphs/arima/arima_pred_resto.png")
plot(forecast(fit,h=10), ylab="Demanda gas natural para quimica", xlab="Tiempo")
dev.off()

#Ljung-box test para ver que residuales son ruido blanco, null hypothesis of residuals independence (i.e.correlation is cero), fitdf=p+q
Box.test(residuals(fit), lag=20, fitdf=2, type="Ljung")

#  ARIMA vs IMP
png("./output/graphs/arima/arima_resul_resto.png")
ts.plot(fit$x, fitted(fit), gpars = list(col = c("blue", "red"), ylab="Demanda de gas natural"), xlab="Tiempo", main="Resultados modelo ARIMA para resto", type="o")
legend("bottomright", legend = c("real", "modelo"), col = c("blue", "red"), lty = 1)
dev.off()

error2 <- sqrt(mean(fit$residuals[1:15]^2))
sprintf("El error en 2006-2015 de este modelo para el resto de subsectores es: %s", error2)
error3 <- sqrt(mean(fit$residuals[16:26]^2))
sprintf("El error en 2016-2030 de este modelo para el resto de subsectores es: %s", error3)

tabla_res <-data.frame("Sector"='Resto', "Modelo"='ARIMA', "RMSE_2006-2030"=error2+error3, "RMSE_2006-2015"=error2, "RMSE_2016-2030"=error3)
tabla_res_tot <- rbind(tabla_res_tot, tabla_res)

tabla_res_tot <- tabla_res_tot[order(tabla_res_tot[,1]),]
tabla_res_tot
