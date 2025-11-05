# =============================================================================
# SERVER FOR ADVANCED PLOTS WITH INTEGRATED FILTERS
# =============================================================================

create_advanced_plots_server <- function(output, input) {
  
  # Gráfico de antimicrobianos con filtros flotantes
  output$antimicrobials_plot_advanced <- renderPlot({
    # Filtrar datos según los controles flotantes
    filter_type <- if(is.null(input$ab_quick_type)) "all" else input$ab_quick_type
    show_trend <- if(is.null(input$ab_show_trend_line)) FALSE else input$ab_show_trend_line
    
    # Datos simulados
    if(filter_type == "pen") {
      data <- c(15, 18, 12, 22, 19, 16, 20)
      title <- "Antimicrobianos - Penicilinas"
      color <- "darkgreen"
    } else if(filter_type == "cef") {
      data <- c(8, 12, 10, 15, 14, 11, 13)
      title <- "Antimicrobianos - Cefalosporinas"
      color <- "darkblue"
    } else {
      data <- c(25, 30, 22, 37, 33, 27, 33)
      title <- "Antimicrobianos - Todos"
      color <- "steelblue"
    }
    
    # Crear el gráfico
    plot(1:7, data, type = "b", 
         main = title,
         xlab = "Días", ylab = "Frecuencia", 
         col = color, pch = 16, lwd = 2,
         cex.main = 1.2, cex.lab = 1.1)
    
    # Agregar línea de tendencia si está activada
    if(show_trend) {
      abline(lm(data ~ I(1:7)), col = "red", lty = 2, lwd = 2)
      legend("topright", legend = "Tendencia", col = "red", lty = 2, lwd = 2, cex = 0.8)
    }
    
    # Agregar grid
    grid(col = "lightgray", lty = 3)
  })
  
  # Gráfico de duración con filtros en header
  output$duration_plot_advanced <- renderPlot({
    filter_type <- if(is.null(input$duration_filter_type)) "Días" else input$duration_filter_type
    threshold <- if(is.null(input$duration_threshold)) 7 else input$duration_threshold
    
    # Generar datos según el tipo de filtro
    if(filter_type == "Semanas") {
      data <- rnorm(100, threshold/7, 1)
      xlab_text <- "Semanas"
      title_text <- paste("Duración (Semanas) - Umbral:", round(threshold/7, 1))
    } else if(filter_type == "Meses") {
      data <- rnorm(100, threshold/30, 0.5)  
      xlab_text <- "Meses"
      title_text <- paste("Duración (Meses) - Umbral:", round(threshold/30, 1))
    } else {
      data <- rnorm(100, threshold, 2)
      xlab_text <- "Días"
      title_text <- paste("Duración (Días) - Umbral:", threshold)
    }
    
    # Crear histograma
    hist(data, main = title_text,
         xlab = xlab_text, ylab = "Frecuencia", 
         col = "lightcoral", breaks = 15, border = "white")
    
    # Agregar línea del umbral
    abline(v = threshold, col = "red", lwd = 3, lty = 2)
    text(threshold, max(hist(data, plot = FALSE)$counts) * 0.8, 
         paste("Umbral:", threshold), 
         pos = 4, col = "red", font = 2)
  })
  
  # Gráfico de resistencia con panel lateral
  output$resistance_plot_advanced <- renderPlot({
    ab_selected <- if(is.null(input$resistance_ab)) "Todos" else input$resistance_ab
    threshold <- if(is.null(input$resistance_threshold)) 20 else input$resistance_threshold
    show_types <- if(is.null(input$resistance_types)) c("sens", "res") else input$resistance_types
    
    # Datos simulados de resistencia
    antimicrobials <- c("Ampicilina", "Ciprofloxacino", "Vancomicina", "Gentamicina", "Ceftriaxona")
    resistance <- c(35, 15, 5, 25, 10)
    sensitive <- 100 - resistance
    
    # Filtrar por antimicrobiano específico si se selecciona
    if(ab_selected != "Todos") {
      idx <- which(antimicrobials == ab_selected)
      if(length(idx) > 0) {
        antimicrobials <- antimicrobials[idx]
        resistance <- resistance[idx] 
        sensitive <- sensitive[idx]
      }
    }
    
    # Crear matriz de datos para el gráfico
    plot_data <- matrix(0, nrow = length(antimicrobials), ncol = 0)
    colors <- c()
    legend_labels <- c()
    
    if("sens" %in% show_types) {
      plot_data <- cbind(plot_data, sensitive)
      colors <- c(colors, "lightgreen")
      legend_labels <- c(legend_labels, "Sensible")
    }
    if("res" %in% show_types) {
      plot_data <- cbind(plot_data, resistance)
      colors <- c(colors, "lightcoral") 
      legend_labels <- c(legend_labels, "Resistente")
    }
    
    # Crear gráfico de barras
    if(ncol(plot_data) > 0) {
      barplot(t(plot_data), names.arg = antimicrobials,
              main = paste("Patrón de Resistencia - Umbral:", threshold, "%"),
              ylab = "Porcentaje (%)",
              col = colors,
              legend.text = legend_labels,
              args.legend = list(x = "topright", cex = 0.8),
              las = 2, cex.names = 0.8)
      
      # Agregar línea del umbral
      abline(h = threshold, col = "red", lwd = 2, lty = 2)
      text(0.5, threshold + 5, paste("Umbral:", threshold, "%"), 
           col = "red", font = 2, cex = 0.9)
    } else {
      plot(1, 1, type = "n", xlab = "", ylab = "", main = "Seleccione al menos un tipo para mostrar")
    }
  })
  
  # Observadores para botones de reset y actualización
  observeEvent(input$ab_reset, {
    updateSelectInput(session, "ab_quick_type", selected = "all")
    updateCheckboxInput(session, "ab_show_trend_line", value = FALSE)
  })
  
  observeEvent(input$update_resistance, {
    showNotification("Gráfico actualizado", type = "message", duration = 2)
  })
}