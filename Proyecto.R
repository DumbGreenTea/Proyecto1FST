#Temperatura global en temporalidad mensual a lo largo de 2 siglos aproximadamente
dataset = read.csv("monthly.csv")
dataset <- dataset[ , !(names(dataset) %in% "Source")]

# Convertir la columna 'Year' a fecha (suponiendo formato YYYY-MM)
dataset$Year <- as.Date(paste0(dataset$Year, "-01"))

# Graficar la temperatura en el tiempo
plot(dataset$Year, dataset$Mean,
     type = "l",                  # línea
     col = "blue",
     xlab = "Fecha",
     ylab = "Temperatura media",
     main = "Temperatura global mensual a lo largo del tiempo")

