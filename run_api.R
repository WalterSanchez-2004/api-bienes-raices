library(plumber)

# Cargar el archivo de la API
pr <- plumber::plumb("api_modelo.R")

# Ejecutar la API en el puerto 8000
pr$run(host = "0.0.0.0", port = 8000)
