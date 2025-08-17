# Función para leer el archivo de texto y devolver un vector de números
leer_numeros <- function(archivo) {
  if (!file.exists(archivo)) {
    stop("Error: El archivo no existe.")
  }
  numeros <- as.integer(readLines(archivo))
  return(numeros)
}

# Función para calcular estadísticas descriptivas
calcular_estadisticas <- function(numeros) {
  media <- mean(numeros)
  mediana <- median(numeros)
  desviacion <- sd(numeros)
  
  if (desviacion > 10) {
    mensaje_variabilidad <- "Alta variabilidad en los datos."
  } else {
    mensaje_variabilidad <- "Baja variabilidad en los datos."
  }
  
  return(list(media = media, mediana = mediana, desviacion = desviacion, mensaje = mensaje_variabilidad))
}

# Función para calcular el cuadrado de cada número utilizando sapply
calcular_cuadrados <- function(numeros) {
  cuadrados <- sapply(numeros, function(x) x^2)
  return(cuadrados)
}

# Función para guardar los resultados en un archivo de texto
guardar_resultados <- function(estadisticas, cuadrados, archivo) {
  cat("Estadísticas Descriptivas:\n", file = archivo)
  cat("Media: ", estadisticas$media, "\n", file = archivo, append = TRUE)
  cat("Mediana: ", estadisticas$mediana, "\n", file = archivo, append = TRUE)
  cat("Desviación Estándar: ", estadisticas$desviacion, "\n", file = archivo, append = TRUE)
  cat(estadisticas$mensaje, "\n\n", file = archivo, append = TRUE)
  
  cat("Cuadrados de los Números:\n", file = archivo, append = TRUE)
  write(cuadrados, file = archivo, append = TRUE)
}

# Función principal para ejecutar todo el proceso
procesar_numeros <- function() {
  archivo_entrada <- "numeros.txt"
  archivo_salida <- "resultados.txt"
  
  numeros <- leer_numeros(archivo_entrada)
  estadisticas <- calcular_estadisticas(numeros)
  cuadrados <- calcular_cuadrados(numeros)
  guardar_resultados(estadisticas, cuadrados, archivo_salida)
  
  cat("Análisis completado. Los resultados se han guardado en 'resultados.txt'.\n")
}

# Ejecutar el proceso
procesar_numeros()
