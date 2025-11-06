# Análisis de Regresión Lineal

Aplicación web interactiva para realizar análisis de regresión lineal simple con R y Shiny.

## 📋 Requisitos

- R (versión 4.0.0 o superior)
- RStudio (recomendado) o cualquier entorno R
- Paquetes: `shiny`, `shinydashboard`

## 🚀 Instalación

1. **Instalar renv** (si no está instalado):
   ```r
   install.packages("renv")
   ```

2. **Restaurar dependencias**:
   ```r
   renv::restore()
   ```

## ▶️ Cómo usar

1. **Iniciar la aplicación**:
   - Abre `app.R` en RStudio y haz clic en "Run App"
   - O ejecuta en la consola: `shiny::runApp()`

2. **Uso de la aplicación**:
   - **Paso 1**: Ingresa los nombres de las variables y sus valores
   - **Paso 2**: Verifica los datos ingresados
   - **Paso 3**: Revisa los resultados del análisis

## 📊 Características

- Análisis de regresión lineal simple
- Cálculo de coeficientes de correlación (r y r²)
- Ecuación de regresión
- Tabla ANOVA
- Validación de datos

## 🗂 Estructura del proyecto

```
pia-disExp/
├── R/                    # Módulos de la aplicación
│   └── regresion_module.R  # Módulo de regresión lineal
├── app.R                # Aplicación principal
├── .Rprofile            # Configuración de R
└── renv/                # Entorno virtual de R
    └── renv.lock        # Versiones de paquetes
```

## 🔄 Control de versiones

1. **Actualizar dependencias**:
   ```r
   renv::snapshot()
   ```

2. **Restaurar entorno**:
   ```r
   renv::restore()
   ```

## 📚 Recursos

- [Documentación de Shiny](https://shiny.rstudio.com/)
- [Tutorial de regresión lineal en R](https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/lm)

## 📝 Notas

- Los datos ingresados no se almacenan en ningún servidor
- Se recomienda usar como máximo 100 observaciones para mejor rendimiento
- La aplicación está optimizada para navegadores modernos
- [Documentación de renv](https://rstudio.github.io/renv/)