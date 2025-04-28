#===============================================#
#           Importación de librerías 
#===============================================#
  # Instalación y carga de paquetes necesarios
  install.packages("forecast")
  install.packages("TTR")
  install.packages("tseries")
  install.packages("mFilter")
  install.packages("zoo")
  library(forecast)
  library(TTR)
  library(tseries)
  library(mFilter)
  library("zoo")
  
#===============================================#
#      Limpieza y Presentación del DataSet
#===============================================#
  # Importación del data set (serie de tiempo)
  # Temperatura global en temporalidad mensual a lo largo de 2 siglos aproximadamente
  dataset = read.csv("monthly.csv")
  #Ver los primeros elementos del dataset
  head(dataset)
  
  # Eliminar columna source (no es importante para el análisis de este trabajo)
  dataset <- dataset[ , !(names(dataset) %in% "Source")] 
  # Convertir la columna 'Year' a fecha (suponiendo formato YYYY-MM), manejamos el primer día de cada mes
  dataset$Year <- as.Date(paste0(dataset$Year, "-01"))
  
  # Ver los primeros y últimos elementos del dataset sin columna Source
  head(dataset)
  tail(dataset)
  
  # Graficar la temperatura en el tiempo
  plot(dataset$Year, dataset$Mean,
       type = "l",                  # línea
       col = "blue",
       xlab = "Fecha",
       ylab = "Temperatura media",
       main = "Temperatura global mensual a lo largo del tiempo")

  
#===============================================#
#             Análisis de la Serie 
#===============================================#
  # Transformar el dataset a un objeto de serie de tiempo (ts)
  temp_ts <- ts(dataset$Mean, 
                start = c(as.numeric(format(min(dataset$Year), "%Y")), 
                          as.numeric(format(min(dataset$Year), "%m"))),
                end = c(as.numeric(format(max(dataset$Year), "%Y")), 
                        as.numeric(format(max(dataset$Year), "%m"))),
                frequency = 12)

  head(temp_ts)
  
#======= Análisis de tendencia y periodicidad ========#
  
  # Gráfica de la serie de tiempo
  plot(temp_ts, main = "Serie de Tiempo de Temperatura Global", 
       xlab = "Tiempo", ylab = "Temperatura Media")
  
  #Ya que nuestros datos ya son medias mensuales, buscaremos un promedio anual de temperaturas, pues las tº son variables al promedio, no acumulativas
  aggregate(temp_ts, FUN = mean)
  
  # Pendiente (tendencia)
  plot(aggregate(temp_ts, FUN = mean), main = "Tendencia Anual de Temperatura Global", 
       xlab = "Año", ylab = "Temperatura Total Anual")
  
  # Periodicidad
  boxplot(temp_ts ~ cycle(temp_ts), main = "Boxplot de Periodicidad de Temperatura",
          xlab = "Mes", ylab = "Temperatura Media")
  
  
#====== Tipo de Modelo ¿Aditivo o Multiplicativo ======#
  
  # Descomposición
  decomp_add <- decompose(temp_ts, type = "additive")
  plot(decomp_add)
  
  decomp_mult <- decompose(temp_ts, type = "multiplicative")
  plot(decomp_mult)
  
  # Análisis del modelo:
  
  # Considerando que:
  # Todo modelo posee tendencia, ciclos, error
  # Podemos descomponerlo para determinar si es multiplicativo, aditivo o mixto.
  
  # Modelo aditivo:         x_t = T_t + P_t + E_t
  # Modelo multiplicativo:  x_t = T_t * P_t * E_t
  # Modelo mixto:           x_t = T_t * P_t + E_t
  
  # === Determinando la tendencia ===
  # Usando medias móviles
  plot(aggregate(temp_ts, FUN = mean), main = "Tendencia Anual de Temperatura Global", 
       xlab = "Año", ylab = "Temperatura Total Anual")
  
  
#====== ¿Estacionaria débil? ======# ESTO ESTA EN PROCESO NO SE SI ESTÁ BIEN JSJSJSJ
  acf(temp_ts, main = "ACF de la Temperatura Global")
  pacf(temp_ts, main = "PACF de la Temperatura Global")
  
  library(tseries)
  adf.test(temp_ts)$p.value
  kpss.test(temp_ts)
  
#====== Diferenciación ======#
  diff_temp_ts <- diff(temp_ts)
  plot(diff_temp_ts, main = "Serie Diferenciada de Temperatura", 
       xlab = "Tiempo", ylab = "Diferencia de Temperatura")
  
  acf(diff_temp_ts)
  pacf(diff_temp_ts)
  kpss.test(diff_temp_ts)
  
  # Segunda diferenciación si es necesario
  diff2_temp_ts <- diff(diff_temp_ts)
  plot(diff2_temp_ts, main = "Serie Diferenciada Dos Veces de Temperatura", 
       xlab = "Tiempo", ylab = "Segunda Diferencia de Temperatura")
  
  acf(diff2_temp_ts)
  pacf(diff2_temp_ts)
  kpss.test(diff2_temp_ts)
  

