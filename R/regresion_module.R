# Módulo de Regresión Lineal Simple
#
# @param id Namespace del módulo
# @return Módulo Shiny

#' @importFrom shiny NS tagList numericInput textInput tableOutput verbatimTextOutput
#' @importFrom shiny renderTable renderPrint
#' @importFrom stats lm cor anova summary.lm coef
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
      
      # Crear data.frame con los datos
      datos_df <- data.frame(x = x, y = y)
      
      # Ajustar modelo de regresión lineal
      modelo <- lm(y ~ x, data = datos_df)
      
      # Obtener resumen del modelo
      resumen <- summary(modelo)
      
      # Obtener ANOVA
      anova_res <- anova(modelo)
      
      # Calcular correlación
      correlacion <- cor(x, y)
      
      # Obtener coeficientes
      coefs <- coef(modelo)
      
      # Preparar resultados
      list(
        x = x,
        y = y,
        x_name = ifelse(input$var_x == "", "X", input$var_x),
        y_name = ifelse(input$var_y == "", "Y", input$var_y),
        n = length(x),
        modelo = modelo,
        resumen = resumen,
        anova = anova_res,
        coefs = coefs,
        correlacion = correlacion
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
        cat("=========================\n\n")
        
        # Estadísticas para X
        cat(sprintf("Variable %s (X):\n", datos$x_name))
        cat("-------------------------\n")
        cat(sprintf("Media (x̄) = %.6f\n", mean(datos$x)))
        cat(sprintf("Mínimo   = %.6f\n", min(datos$x)))
        cat(sprintf("Máximo   = %.6f\n", max(datos$x)))
        cat(sprintf("Suma (ΣX) = %.6f\n", sum(datos$x)))
        cat(sprintf("ΣX²      = %.6f\n\n", sum(datos$x^2)))
        
        # Estadísticas para Y
        cat(sprintf("Variable %s (Y):\n", datos$y_name))
        cat("-------------------------\n")
        cat(sprintf("Media (ȳ) = %.6f\n", mean(datos$y)))
        cat(sprintf("Mínimo    = %.6f\n", min(datos$y)))
        cat(sprintf("Máximo    = %.6f\n", max(datos$y)))
        cat(sprintf("Suma (ΣY) = %.6f\n", sum(datos$y)))
        cat(sprintf("ΣY²      = %.6f\n\n", sum(datos$y^2)))
        
        # Suma de productos
        cat("Suma de productos (ΣXY) = ", sum(datos$x * datos$y), "\n")
      }
    })
    
    # Coeficientes de correlación
    output$correlacion <- renderPrint({
      datos <- datos_react()
      if (is.null(datos$error)) {
        r <- datos$correlacion
        r2 <- r^2
        
        cat("Coeficiente de correlación de Pearson (r):\n")
        cat("----------------------------------------\n")
        cat(sprintf("r  = %.6f\n", r))
        cat(sprintf("r² = %.6f  (%.2f%% de la variabilidad)\n\n", r2, r2 * 100))
        
        cat("Interpretación de la correlación:\n")
        cat("--------------------------------\n")
        if (abs(r) >= 0.9) {
          cat("Correlación muy fuerte ")
        } else if (abs(r) >= 0.7) {
          cat("Correlación fuerte ")
        } else if (abs(r) >= 0.5) {
          cat("Correlación moderada ")
        } else if (abs(r) >= 0.3) {
          cat("Correlación débil ")
        } else {
          cat("Correlación muy débil o nula ")
        }
        
        cat(ifelse(r > 0, "positiva", ifelse(r < 0, "negativa", "")))
        
        cat("\n\nEl coeficiente de determinación (r²) indica que el ", 
            sprintf("%.2f%% de la variabilidad en %s ", r2 * 100, datos$y_name),
            sprintf("es explicado por su relación lineal con %s.", datos$x_name)
        )
      }
    })
    
    # Ecuación de regresión
    output$ecuacion <- renderPrint({
      datos <- datos_react()
      if (is.null(datos$error)) {
        b0 <- datos$coefs[1]
        b1 <- datos$coefs[2]
        
        cat("Ecuación de regresión lineal:\n")
        cat("============================\n\n")
        
        # Mostrar ecuación con formato
        cat(sprintf("ŷ = %.6f %s %.6f·%s\n\n", 
                   b0, 
                   ifelse(b1 >= 0, "+", "-"), 
                   abs(b1),
                   datos$x_name))
        
        cat("Donde:\n")
        cat("------\n")
        cat(sprintf("• ŷ = valor predicho de %s\n", datos$y_name))
        cat(sprintf("• %s = %s\n", 
                   datos$x_name, 
                   ifelse(input$var_x == "", "variable independiente", input$var_x)))
        cat(sprintf("• %.6f = ordenada al origen (valor de Y cuando X=0)\n", b0))
        cat(sprintf("• %.6f = pendiente de la recta\n\n", b1))
        
        # Interpretación de la pendiente
        cat("Interpretación de la pendiente:\n")
        cat("------------------------------\n")
        cat(sprintf("Por cada unidad que aumenta %s, se espera que %s %s en promedio %.6f unidades.\n",
                   datos$x_name,
                   datos$y_name,
                   ifelse(b1 > 0, "aumente", "disminuya"),
                   abs(b1)))
        
        # Calidad del ajuste
        r2 <- datos$resumen$r.squared
        cat("\nBondad del ajuste:\n")
        cat("-----------------\n")
        cat(sprintf("R² ajustado = %.6f\n", datos$resumen$adj.r.squared))
        cat(sprintf("Error estándar de la estimación = %.6f\n", 
                   summary(datos$modelo)$sigma))
      }
    })
    
    # Análisis de varianza
    output$anova <- renderPrint({
      datos <- datos_react()
      if (is.null(datos$error)) {
        # Mostrar tabla ANOVA
        cat("Análisis de Varianza (ANOVA)\n")
        cat("============================\n\n")
        
        # Crear tabla ANOVA manualmente para mejor formato
        anova_df <- data.frame(
          Fuente = c("Regresión", "Error", "Total"),
          SC = c(datos$anova$`Sum Sq`[1], 
                datos$anova$`Sum Sq`[2], 
                sum(datos$anova$`Sum Sq`)),
          gl = c(datos$anova$Df[1], 
                datos$anova$Df[2], 
                sum(datos$anova$Df)),
          MC = c(datos$anova$`Mean Sq`[1], 
                datos$anova$`Mean Sq`[2],
                NA),
          F = c(datos$anova$`F value`[1], NA, NA),
          p_valor = c(datos$anova$`Pr(>F)`[1], NA, NA)
        )
        
        # Función auxiliar para formatear valores numéricos
        format_num <- function(x, width, digits = 4) {
          if (is.na(x)) {
            return(sprintf(paste0("%", width, "s"), "-"))
          } else {
            return(sprintf(paste0("%", width, ".", digits, "f"), x))
          }
        }
        
        # Encabezado de la tabla
        cat("Fuente     |      SC   | gl |      MC     |    F    |   p-valor\n")
        cat("----------------------------------------------------------\n")
        
        # Contenido de la tabla
        for (i in 1:nrow(anova_df)) {
          cat(sprintf("%-10s|%s |%3d |%11s |%8s |%s\n",
                     anova_df$Fuente[i],
                     format_num(anova_df$SC[i], 10),
                     anova_df$gl[i],
                     ifelse(is.na(anova_df$MC[i]), "           ", format_num(anova_df$MC[i], 11)),
                     ifelse(is.na(anova_df$F[i]), "       ", format_num(anova_df$F[i], 8)),
                     ifelse(is.na(anova_df$p_valor[i]), "       ", 
                           ifelse(anova_df$p_valor[i] < 0.001, "< 0.001", 
                                 format_num(anova_df$p_valor[i], 8, 4)))
          ))
        }
        
        # Interpretación
        cat("\nHipótesis nula (H₀): El modelo no es significativo (β₁ = 0)\n")
        cat(sprintf("Hipótesis alternativa (H₁): El modelo es significativo (β₁ ≠ 0)\n\n"))
        
        if (datos$anova$`Pr(>F)`[1] < 0.05) {
          cat("Conclusión: Rechazamos H₀ (p < 0.05). \n")
          cat("  → Existe evidencia estadísticamente significativa de que el modelo es útil \n")
          cat("    para predecir", datos$y_name, "a partir de", datos$x_name, "\n")
        } else {
          cat("Conclusión: No hay evidencia suficiente para rechazar H₀ (p ≥ 0.05). \n")
          cat("  → No hay evidencia de que el modelo sea útil para predecir", 
              datos$y_name, "a partir de", datos$x_name, "\n")
        }
      }
    })
  })
}
