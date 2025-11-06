# Módulo de Regresión Lineal Simple
#
# @param id Namespace del módulo
# @return Módulo Shiny

#' @importFrom shiny NS tagList numericInput textInput tableOutput verbatimTextOutput
#' @importFrom shiny renderTable renderPrint
#' @importFrom stats lm cor
mod_regresion_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    h3("Regresión Lineal Simple"),
    
    # Paso 1: Ingreso de datos
    wellPanel(
      h4("Paso 1: Ingresar Datos"),
      textInput(ns("var_x"), "Nombre de la variable independiente (X):", ""),
      textInput(ns("var_y"), "Nombre de la variable dependiente (Y):", ""),
      textAreaInput(ns("datos_x"), "Valores de X (separados por comas o espacios):"),
      textAreaInput(ns("datos_y"), "Valores de Y (separados por comas o espacios):"),
      actionButton(ns("btn_calcular"), "Calcular Regresión")
    ),
    
    # Paso 2: Validación de datos
    conditionalPanel(
      condition = "input.btn_calcular > 0",
      ns = ns,
      wellPanel(
        h4("Paso 2: Validación de Datos"),
        tableOutput(ns("tabla_datos")),
        verbatimTextOutput(ns("validacion"))
      )
    ),
    
    # Paso 3: Resultados
    conditionalPanel(
      condition = "input.btn_calcular > 0",
      ns = ns,
      wellPanel(
        h4("Paso 3: Resultados"),
        h5("Estadísticas Descriptivas"),
        verbatimTextOutput(ns("estadisticas")),
        
        h5("Coeficientes de Correlación"),
        verbatimTextOutput(ns("correlacion")),
        
        h5("Ecuación de Regresión"),
        verbatimTextOutput(ns("ecuacion")),
        
        h5("Análisis de Varianza (ANOVA)"),
        verbatimTextOutput(ns("anova"))
      )
    )
  )
}

#' @rdname mod_regresion_ui
mod_regresion_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Procesar datos de entrada
    datos_react <- eventReactive(input$btn_calcular, {
      req(input$datos_x, input$datos_y)
      
      # Convertir texto a vector numérico
      x <- as.numeric(unlist(strsplit(gsub(",", " ", input$datos_x), "\\s+")))
      y <- as.numeric(unlist(strsplit(gsub(",", " ", input$datos_y), "\\s+")))
      
      # Validar longitudes
      if (length(x) != length(y)) {
        return(list(error = "Error: Las variables deben tener la misma cantidad de observaciones."))
      }
      
      # Eliminar NA's
      valid <- !is.na(x) & !is.na(y)
      x <- x[valid]
      y <- y[valid]
      
      if (length(x) < 2) {
        return(list(error = "Error: Se requieren al menos 2 observaciones válidas."))
      }
      
      # Calcular estadísticas básicas
      n <- length(x)
      sum_x <- sum(x)
      sum_y <- sum(y)
      sum_xy <- sum(x * y)
      sum_x2 <- sum(x^2)
      sum_y2 <- sum(y^2)
      
      # Coeficientes de regresión
      b1 <- (n * sum_xy - sum_x * sum_y) / (n * sum_x2 - sum_x^2)
      b0 <- mean(y) - b1 * mean(x)
      
      # Coeficiente de correlación
      r <- (n * sum_xy - sum_x * sum_y) / 
        sqrt((n * sum_x2 - sum_x^2) * (n * sum_y2 - sum_y^2))
      
      # Predicciones y residuos
      y_pred <- b0 + b1 * x
      residuos <- y - y_pred
      
      # Suma de cuadrados
      SST <- sum((y - mean(y))^2)
      SSR <- sum((y_pred - mean(y))^2)
      SSE <- sum(residuos^2)
      
      list(
        x = x,
        y = y,
        x_name = ifelse(input$var_x == "", "X", input$var_x),
        y_name = ifelse(input$var_y == "", "Y", input$var_y),
        n = n,
        mean_x = mean(x),
        mean_y = mean(y),
        sum_x = sum_x,
        sum_y = sum_y,
        sum_xy = sum_xy,
        sum_x2 = sum_x2,
        sum_y2 = sum_y2,
        b0 = b0,
        b1 = b1,
        r = r,
        r2 = r^2,
        SST = SST,
        SSR = SSR,
        SSE = SSE
      )
    })
    
    # Mostrar tabla de datos
    output$tabla_datos <- renderTable({
      datos <- datos_react()
      if (is.null(datos$error)) {
        data.frame(
          Observación = 1:datos$n,
          X = datos$x,
          Y = datos$y
        )
      }
    }, digits = 4)
    
    # Validación de datos
    output$validacion <- renderPrint({
      datos <- datos_react()
      if (!is.null(datos$error)) {
        cat(datos$error)
      } else {
        cat("Datos validados correctamente.\n")
        cat("Número de observaciones (n):", datos$n, "\n")
        cat("Variable X:", datos$x_name, "\n")
        cat("Variable Y:", datos$y_name, "\n")
      }
    })
    
    # Estadísticas descriptivas
    output$estadisticas <- renderPrint({
      datos <- datos_react()
      if (is.null(datos$error)) {
        cat("Estadísticas descriptivas:\n")
        cat("-------------------------\n")
        cat(sprintf("Media de %s (x̄): %.6f\n", datos$x_name, datos$mean_x))
        cat(sprintf("Media de %s (ȳ): %.6f\n", datos$y_name, datos$mean_y))
        cat(sprintf("Suma de %s (ΣX): %.6f\n", datos$x_name, datos$sum_x))
        cat(sprintf("Suma de %s (ΣY): %.6f\n", datos$y_name, datos$sum_y))
        cat(sprintf("Suma de X² (ΣX²): %.6f\n", datos$sum_x2))
        cat(sprintf("Suma de Y² (ΣY²): %.6f\n", datos$sum_y2))
        cat(sprintf("Suma de XY (ΣXY): %.6f\n", datos$sum_xy))
      }
    })
    
    # Coeficientes de correlación
    output$correlacion <- renderPrint({
      datos <- datos_react()
      if (is.null(datos$error)) {
        cat("Coeficiente de correlación (r):", format(datos$r, digits = 6), "\n")
        cat("Coeficiente de determinación (r²):", format(datos$r2, digits = 6), "\n")
        cat("\nInterpretación: ")
        cat(sprintf("El %.2f%% de la variabilidad en %s es explicado por su relación lineal con %s.", 
                   datos$r2 * 100, datos$y_name, datos$x_name))
      }
    })
    
    # Ecuación de regresión
    output$ecuacion <- renderPrint({
      datos <- datos_react()
      if (is.null(datos$error)) {
        cat("Ecuación de regresión:\n")
        cat("----------------------\n")
        cat(sprintf("ŷ = %.6f + %.6f * %s\n\n", datos$b0, datos$b1, datos$x_name))
        
        cat("Donde:\n")
        cat(sprintf("ŷ = valor predicho de %s\n", datos$y_name))
        cat(sprintf("%s = %s\n", datos$x_name, 
                   ifelse(input$var_x == "", "variable independiente", input$var_x)))
        cat(sprintf("%.6f = intersección con el eje Y\n", datos$b0))
        cat(sprintf("%.6f = pendiente de la recta (cambio en Y por unidad de X)", datos$b1))
      }
    })
    
    # Análisis de varianza
    output$anova <- renderPrint({
      datos <- datos_react()
      if (is.null(datos$error)) {
        cat("Análisis de Varianza (ANOVA):\n")
        cat("---------------------------\n")
        cat(sprintf("Suma de cuadrados total (SST): %.6f\n", datos$SST))
        cat(sprintf("Suma de cuadrados de la regresión (SSR): %.6f\n", datos$SSR))
        cat(sprintf("Suma de cuadrados de los errores (SSE): %.6f\n", datos$SSE))
        cat(sprintf("SST = SSR + SSE: %.6f = %.6f + %.6f\n", 
                   datos$SST, datos$SSR, datos$SSE))
      }
    })
  })
}
