# =============================================================================
# EJEMPLO DE IMPLEMENTACIÓN - FLUID DESIGN EN OTROS PANELES
# =============================================================================

# Cómo agregar fluid_design con filtros a cualquier panel:

# 1. En el módulo del panel (ej: patients.R):
create_patients_content_with_filters <- function() {
  tagList(
    # Usar fluid_design en lugar de fluidRow tradicional
    fluid_design("patients_panel", "box1_pat", "box2_pat", "box3_pat", "box4_pat"),
    
    # Otros contenidos...
    fluidRow(
      # Contenido adicional sin filtros
    )
  )
}

# 2. Crear función para generar las cajas con filtros:
create_patients_boxes <- function(output) {
  
  output$box1_pat <- renderUI({
    create_box_with_filters(
      title = "Demografía por Edad",
      status = "info",
      plot_output_id = "demographics_plot",
      filters_list = tagList(
        create_compact_filter("age_range", "Rango Edad", 
                            choices = c(0, 100), selected = c(18, 65), type = "slider"),
        create_compact_filter("gender_demo", "Género",
                            choices = c("Todos", "M", "F"),
                            selected = "Todos", type = "select")
      ),
      description = "Distribución demográfica de pacientes"
    )
  })
  
  output$box2_pat <- renderUI({
    create_box_with_filters(
      title = "Origen de Admisiones",
      status = "success",
      plot_output_id = "origin_plot", 
      filters_list = tagList(
        create_compact_filter("admission_type", "Tipo",
                            choices = c("Todos", "Urgencias", "Programado", "Traslado"),
                            selected = "Todos", type = "select"),
        create_compact_filter("time_admission", "Turno",
                            choices = c("Todos", "Mañana", "Tarde", "Noche"),
                            selected = "Todos", type = "select")
      ),
      description = "Análisis de procedencia de pacientes"
    )
  })
  
  # Box 3 y Box 4 similares...
}

# 3. En el servidor dinámico, agregar el observeEvent:
# observeEvent(input$patients, { 
#   active_panel("patients")
#   create_patients_boxes(output)  # <-- Agregar esta línea
# })

# 4. En plots_data.R, hacer los gráficos reactivos a los filtros:
create_reactive_demographics_plot <- function(output, input) {
  output$demographics_plot <- renderPlot({
    # Obtener valores de filtros
    age_range <- if(!is.null(input$age_range)) input$age_range else c(0, 100)
    gender_filter <- if(!is.null(input$gender_demo)) input$gender_demo else "Todos"
    
    # Simular datos filtrados
    if(gender_filter == "Todos") {
      data <- data.frame(
        edad = runif(100, age_range[1], age_range[2]),
        genero = sample(c("M", "F"), 100, replace = TRUE)
      )
    } else {
      data <- data.frame(
        edad = runif(100, age_range[1], age_range[2]),
        genero = rep(gender_filter, 100)
      )
    }
    
    # Crear el gráfico
    hist(data$edad, 
         main = paste("Demografía -", gender_filter, 
                     "(", age_range[1], "-", age_range[2], "años)"),
         xlab = "Edad", ylab = "Frecuencia",
         col = if(gender_filter == "M") "lightblue" else if(gender_filter == "F") "pink" else "lightgreen",
         breaks = 20)
  })
}

# VENTAJAS DEL SISTEMA FLUID_DESIGN:
# ✓ Filtros integrados directamente en cada gráfico
# ✓ Interfaz compacta y profesional  
# ✓ Filtros contextuales específicos para cada visualización
# ✓ Facilidad para agregar/quitar filtros
# ✓ Reutilizable en cualquier panel
# ✓ Responsive y adaptable