# Cargar librerías necesarias
library(plumber)
library(glmnet)  # Para el modelo Ridge
library(jsonlite)

# Cargar el modelo previamente entrenado
modelo <- readRDS("modelo_ridge.rds")

# Función auxiliar para convertir el estado en numérico
convertir_estado <- function(estado) {
  estados_numericos <- c("Nuevo" = 0, "Usado" = 1)
  return(estados_numericos[[estado]])
}

#* @get /predecir
#* @param tamano_m2 El tamaño de la propiedad en m2
#* @param habitaciones Número de habitaciones
#* @param baños Número de baños
#* @param estado Estado de la propiedad (ejemplo: "Nuevo", "Usado")
#* @param estacionamiento Número de estacionamientos
#* @param ubicacion Ubicación de la propiedad (número categórico)
#* @serializer json
function(tamano_m2, habitaciones, baños, estado, estacionamiento, ubicacion = 0) {
  
  # Convertir parámetros a formato numérico con validación
  tamano_m2 <- suppressWarnings(as.numeric(tamano_m2))
  habitaciones <- suppressWarnings(as.integer(habitaciones))
  baños <- suppressWarnings(as.integer(baños))
  estacionamiento <- suppressWarnings(as.integer(estacionamiento))
  ubicacion <- suppressWarnings(as.numeric(ubicacion))  # Nueva variable
  
  # Manejo de errores si hay valores no numéricos o vacíos
  if (any(is.na(c(tamano_m2, habitaciones, baños, estacionamiento, ubicacion)))) {
    return(list(error = "Todos los parámetros deben ser valores numéricos válidos"))
  }
  
  # Convertir estado a valor numérico
  estado_num <- convertir_estado(estado)
  if (is.null(estado_num)) {
    return(list(error = "El estado debe ser 'Nuevo' o 'Usado'"))
  }
  
  # Calcular características faltantes
  precio_por_m2 <- 2000  # ⚠ Ajusta este valor según los datos
  total_baños_habitaciones <- baños + habitaciones
  
  # Crear una matriz con las 9 columnas esperadas por el modelo
  nueva_propiedad <- matrix(c(
    0,  # ID (posiblemente no usado)
    tamano_m2,
    habitaciones,
    baños,
    estacionamiento,
    precio_por_m2,  # Nuevo cálculo
    total_baños_habitaciones,  # Nuevo cálculo
    ubicacion,
    estado_num
  ), nrow = 1)
  
  # Hacer la predicción con el modelo Ridge
  prediccion <- predict(modelo, newx = nueva_propiedad, s = "lambda.min")
  
  # 🚀 NO desescalar, ya que el modelo devuelve valores en la escala correcta
  prediccion_original <- prediccion  
  
  # Retornar el resultado en JSON
  return(list(precio_estimado = prediccion_original[1,1]))
}
