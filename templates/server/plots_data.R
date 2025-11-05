# =============================================================================
# PLOTS AND DATA MODULE
# =============================================================================

create_plots_server <- function(output, input = NULL) {
  
  # Antimicrobials plots con funcionalidad de filtros
  output$antimicrobials_plot <- renderPlot({
    # Datos reactivos basados en filtros
    ab_type <- if(!is.null(input$ab_type_filter) && input$ab_type_filter != "Todos") {
      input$ab_type_filter
    } else "Todos los tipos"
    
    ab_route <- if(!is.null(input$ab_route_filter) && input$ab_route_filter != "Todas") {
      input$ab_route_filter  
    } else "Todas las rutas"
    
    # Simular datos filtrados
    categories <- c("Penicilinas", "Cefalosporinas", "Quinolonas", "Aminoglucósidos", "Macrólidos")
    values <- c(45, 38, 32, 28, 22)
    
    # Filtrar datos según selección
    if(ab_type != "Todos los tipos") {
      idx <- which(categories == ab_type)
      if(length(idx) > 0) {
        categories <- categories[idx]
        values <- values[idx]
      }
    }
    
    barplot(values, names.arg = categories, 
            main = paste("Antimicrobianos -", ab_type),
            ylab = "Número de administraciones",
            col = rainbow(length(categories)),
            las = 2, cex.names = 0.8)
    
    # Agregar subtítulo con filtros activos
    mtext(paste("Ruta:", ab_route), side = 3, line = 0.5, cex = 0.8, col = "gray50")
  })
  
  output$duration_plot <- renderPlot({
    # Datos reactivos para duración
    duration_range <- if(!is.null(input$duration_range)) {
      input$duration_range
    } else c(1, 14)
    
    specialty <- if(!is.null(input$specialty_filter) && input$specialty_filter != "Todas") {
      input$specialty_filter
    } else "Todas las especialidades"
    
    # Generar datos filtrados
    set.seed(123)
    data <- rnorm(100, mean = 7, sd = 2)
    data <- data[data >= duration_range[1] & data <= duration_range[2]]
    
    hist(data, 
         main = paste("Duración del Tratamiento -", specialty),
         xlab = paste("Días (", duration_range[1], "-", duration_range[2], ")"),
         ylab = "Frecuencia", 
         col = "lightcoral", 
         breaks = 15,
         xlim = c(0, 30))
    
    # Línea de media
    abline(v = mean(data), col = "red", lwd = 2, lty = 2)
    text(mean(data) + 1, max(hist(data, plot = FALSE)$counts) * 0.8, 
         paste("Media:", round(mean(data), 1)), col = "red")
  })
  
  # Nuevos gráficos para antimicrobianos
  output$temporal_trends_plot <- renderPlot({
    # Datos temporales
    dates <- seq.Date(from = Sys.Date()-90, to = Sys.Date(), by = "day")
    values <- abs(rnorm(length(dates), 50, 10))
    
    aggregation <- if(!is.null(input$aggregation)) input$aggregation else "Semana"
    
    plot(dates, values, type = "l", 
         main = paste("Tendencia Temporal -", aggregation),
         xlab = "Fecha", ylab = "Uso de antimicrobianos",
         col = "darkgreen", lwd = 2)
    
    # Agregar línea de tendencia
    model <- lm(values ~ as.numeric(dates))
    abline(model, col = "red", lty = 2)
  })
  
  output$resistance_plot <- renderPlot({
    # Datos de resistencia
    pathogens <- c("E. coli", "S. aureus", "K. pneumoniae", "P. aeruginosa", "Enterococcus")
    resistance <- c(25, 15, 35, 45, 20)
    
    pathogen_filter <- if(!is.null(input$pathogen_filter) && input$pathogen_filter != "Todos") {
      input$pathogen_filter
    } else NULL
    
    threshold <- if(!is.null(input$resistance_threshold)) {
      input$resistance_threshold
    } else c(20, 100)
    
    # Filtrar por patógeno
    if(!is.null(pathogen_filter)) {
      idx <- which(pathogens == pathogen_filter)
      if(length(idx) > 0) {
        pathogens <- pathogens[idx]
        resistance <- resistance[idx]
      }
    }
    
    # Filtrar por umbral de resistencia
    valid_idx <- resistance >= threshold[1] & resistance <= threshold[2]
    pathogens <- pathogens[valid_idx]
    resistance <- resistance[valid_idx]
    
    if(length(resistance) > 0) {
      colors <- ifelse(resistance > 30, "red", ifelse(resistance > 20, "orange", "green"))
      
      barplot(resistance, names.arg = pathogens,
              main = "Resistencia Antimicrobiana (%)",
              ylab = "Porcentaje de resistencia",
              col = colors,
              las = 2, cex.names = 0.8,
              ylim = c(0, 100))
      
      # Línea de umbral crítico
      abline(h = 30, col = "red", lty = 2)
      text(1, 32, "Umbral crítico (30%)", col = "red", cex = 0.8)
    } else {
      plot(1, 1, type = "n", xlab = "", ylab = "", 
           main = "Sin datos para los filtros seleccionados")
    }
  })
  
  # Patients plots
  output$demographics_plot <- renderPlot({
    pie(c(30, 45, 25), labels = c("0-18 años", "19-65 años", "65+ años"),
        main = "Distribución por Edad", col = c("lightblue", "lightgreen", "lightyellow"))
  })
  
  output$origin_plot <- renderPlot({
    barplot(c(60, 40), names.arg = c("ER", "Ward"), 
            main = "Procedencia de Pacientes", 
            ylab = "Porcentaje", col = c("orange", "purple"))
  })
  
  # Diagnostics plots
  output$blood_cultures_plot <- renderPlot({
    plot(1:7, c(12, 15, 8, 20, 18, 22, 16), type = "b", 
         main = "Cultivos de Sangre - Últimos 7 días",
         xlab = "Días", ylab = "Número de cultivos", 
         col = "red", pch = 16, lwd = 2)
  })
  
  output$urine_cultures_plot <- renderPlot({
    plot(1:7, c(25, 30, 18, 35, 28, 40, 32), type = "b",
         main = "Cultivos de Orina - Últimos 7 días", 
         xlab = "Días", ylab = "Número de cultivos",
         col = "blue", pch = 16, lwd = 2)
  })
  
  # Outcome plots
  output$los_plot <- renderPlot({
    boxplot(rnorm(50, 8, 3), rnorm(50, 12, 4), rnorm(50, 6, 2),
            names = c("Medicina", "Cirugía", "UCI"),
            main = "Estancia Hospitalaria por Servicio",
            ylab = "Días", col = c("lightgreen", "lightblue", "lightpink"))
  })
  
  output$outcomes_plot <- renderPlot({
    pie(c(70, 20, 10), labels = c("Alta Médica (70%)", "Traslado (20%)", "Fallecimiento (10%)"),
        main = "Resultados Clínicos", 
        col = c("lightgreen", "lightyellow", "lightcoral"))
  })
}

create_tables_server <- function(output) {
  
  # Antimicrobials tables
  output$resistance_table <- DT::renderDataTable({
    data.frame(
      Antimicrobiano = c("Ampicilina", "Ciprofloxacino", "Vancomicina"),
      Resistencia = c("25%", "15%", "5%"),
      Sensibilidad = c("75%", "85%", "95%")
    )
  }, options = list(pageLength = 5))
  
  # Patients tables
  output$specialties_table <- DT::renderDataTable({
    data.frame(
      Especialidad = c("Medicina Interna", "Cirugía", "UCI", "Pediatría"),
      Pacientes = c(120, 85, 45, 60),
      Porcentaje = c("38.7%", "27.4%", "14.5%", "19.4%")
    )
  }, options = list(pageLength = 5))
  
  # Diagnostics tables
  output$microorganisms_table <- DT::renderDataTable({
    data.frame(
      Microorganismo = c("E. coli", "S. aureus", "K. pneumoniae", "P. aeruginosa"),
      Frecuencia = c(45, 32, 28, 15),
      Tipo = c("Gram-", "Gram+", "Gram-", "Gram-")
    )
  }, options = list(pageLength = 5))
  
  # Outcome tables  
  output$mortality_table <- DT::renderDataTable({
    data.frame(
      Factor = c("Edad > 65", "UCI", "Sepsis", "Comorbilidades"),
      Riesgo = c("Alto", "Muy Alto", "Alto", "Medio"),
      Mortalidad = c("15%", "25%", "20%", "8%")
    )
  }, options = list(pageLength = 5))
}