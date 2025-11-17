# ============================================================================
# SANKEY SERVER LOGIC
# ============================================================================

#' Create Sankey server logic
#' This function sets up all the reactive logic for the Sankey panel
#' 
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#' @param datos_raw Reactive value containing the raw data
#' @param active_panel Reactive value indicating the active panel
create_sankey_server <- function(input, output, session, datos_raw, active_panel) {
  
  # Variable reactiva para almacenar resultados del Sankey
  sankey_data <- reactiveVal(NULL)
  
  # Limpiar memoria al cambiar de pestaña
  observe({
    if (!is.null(active_panel()) && active_panel() != "sankey") {
      sankey_data(NULL)
      gc()
    }
  })
  
  # ========================================================================
  # GENERACIÓN DEL SANKEY cuando se presiona "Generar"
  # ========================================================================
  
  observeEvent(input$apply_filters, {
    # Solo procesar si estamos en el panel de Sankey
    if (is.null(active_panel()) || active_panel() != "sankey") {
      return()
    }
    
    datos <- datos_raw()
    if (is.null(datos)) {
      showNotification("No hay datos cargados", type = "error", duration = 5)
      sankey_data(NULL)
      return()
    }
    
    # Obtener el valor de mostrar_parametro del checkbox en el panel
    mostrar_parametro_actual <- if (!is.null(input$sankey_mostrar_parametro)) {
      input$sankey_mostrar_parametro
    } else {
      FALSE
    }
    
    tryCatch({
      # Por ahora, construir enlaces básicos sin filtros complejos
      # Puedes agregar más filtros del modal aquí
      source("modules/sankey/sankey.R")
      
      enlaces_df <- construir_enlaces_sankey(
        datos = datos,
        centro_gestor = NULL,  # Aquí puedes agregar filtros del modal
        nivel_agregacion = "cac",
        cac_subcac_origen = NULL,
        fases_incluir = c(1, 2, 3),
        excluir_estaticos = TRUE,
        verbose = TRUE
      )
      
      if (is.null(enlaces_df) || nrow(enlaces_df) == 0) {
        showNotification("No se generaron enlaces con los filtros seleccionados", 
                         type = "warning", duration = 5)
        sankey_data(NULL)
        return()
      }
      
      # Agregar enlaces
      if (mostrar_parametro_actual) {
        enlaces_agregados <- enlaces_df %>%
          group_by(source, target, parametro, tipo_reparto) %>%
          summarise(
            value = sum(value, na.rm = TRUE),
            n_registros = n(),
            .groups = "drop"
          ) %>%
          filter(value > 0)
      } else {
        enlaces_agregados <- enlaces_df %>%
          group_by(source, target) %>%
          summarise(
            value = sum(value, na.rm = TRUE),
            n_registros = n(),
            parametro = NA_character_,
            tipo_reparto = if (n_distinct(tipo_reparto) > 1) "AGREGADO" else first(tipo_reparto),
            .groups = "drop"
          ) %>%
          filter(value > 0)
      }
      
      # Crear nodos
      nodos <- unique(c(enlaces_agregados$source, enlaces_agregados$target))
      nodes_df <- data.frame(name = nodos, stringsAsFactors = FALSE)
      
      # Convertir a índices
      links_df <- enlaces_agregados %>%
        mutate(
          source_id = match(source, nodes_df$name) - 1,
          target_id = match(target, nodes_df$name) - 1
        ) %>%
        select(source = source_id, target = target_id, value, parametro, tipo_reparto)
      
      showNotification(
        paste("✓ Sankey generado:", nrow(links_df), "enlaces únicos"), 
        type = "message", 
        duration = 3
      )
      
      # Calcular importe total
      fase_maxima <- max(enlaces_df$fase, na.rm = TRUE)
      importe_total_calculado <- sum(enlaces_df$value[enlaces_df$fase == fase_maxima], na.rm = TRUE)
      
      sankey_data(list(
        nodes = nodes_df,
        links = links_df,
        enlaces_detallados = enlaces_df,
        enlaces_agregados = enlaces_agregados,
        mostrar_parametro = mostrar_parametro_actual,
        info = list(
          n_nodos = nrow(nodes_df),
          n_enlaces = nrow(links_df),
          n_enlaces_detallados = nrow(enlaces_df),
          importe_total = importe_total_calculado,
          fase_maxima = fase_maxima,
          fases_procesadas = unique(enlaces_df$fase),
          tipos_reparto = table(enlaces_df$tipo_reparto)
        )
      ))
      
    }, error = function(e) {
      showNotification(
        paste("Error al construir Sankey:", e$message),
        type = "error",
        duration = 10
      )
      sankey_data(NULL)
    })
  })
  
  # ========================================================================
  # OUTPUTS - INFO BOXES
  # ========================================================================
  
  output$sankey_info_nodos <- renderInfoBox({
    sankey <- sankey_data()
    n <- if (!is.null(sankey)) sankey$info$n_nodos else 0
    infoBox(
      "Nodos",
      formatC(as.integer(n), format = "d", big.mark = " "),
      icon = icon("dot-circle"),
      color = if (n > 0) "blue" else "red"
    )
  })
  
  output$sankey_info_enlaces <- renderInfoBox({
    sankey <- sankey_data()
    n <- if (!is.null(sankey)) sankey$info$n_enlaces else 0
    infoBox(
      "Enlaces Únicos",
      formatC(as.integer(n), format = "d", big.mark = " "),
      icon = icon("arrows-left-right"),
      color = if (n > 0) "purple" else "red"
    )
  })
  
  output$sankey_info_importe <- renderInfoBox({
    sankey <- sankey_data()
    imp <- if (!is.null(sankey)) sankey$info$importe_total else 0
    fase_info <- if (!is.null(sankey)) paste0(" (Fase ", sankey$info$fase_maxima, ")") else ""
    infoBox(
      paste0("Importe Total", fase_info),
      paste0(formatC(imp, format = "f", big.mark = " ", digits = 2), "€"),
      icon = icon("euro-sign"),
      color = if (imp > 0) "yellow" else "red"
    )
  })
  
  # ========================================================================
  # OUTPUT - DIAGRAMA SANKEY
  # ========================================================================
  
  output$sankey_diagram <- renderPlotly({
    sankey <- sankey_data()
    
    if (is.null(sankey)) {
      return(plotly_empty() %>% 
        layout(title = list(
          text = "Configure los filtros y presione 'Generar'",
          font = list(size = 18)
        ))
      )
    }
    
    # Colores básicos por defecto
    link_colors <- "rgba(100, 116, 139, 0.3)"
    node_colors <- "rgba(31, 119, 180, 0.8)"
    
    plot_ly(
      type = "sankey",
      orientation = "h",
      arrangement = "snap",
      node = list(
        label = sankey$nodes$name,
        color = node_colors,
        pad = 18,
        thickness = 25,
        line = list(color = "rgba(255, 255, 255, 0.8)", width = 1.5),
        hovertemplate = "%{label}<extra></extra>"
      ),
      link = list(
        source = sankey$links$source,
        target = sankey$links$target,
        value = sankey$links$value,
        color = link_colors,
        hovertemplate = "Importe: %{value:,.2f}€<extra></extra>"
      )
    ) %>%
      layout(
        font = list(family = "Inter, sans-serif", size = 11, color = "#1e293b"),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        margin = list(l = 10, r = 10, t = 30, b = 10)
      ) %>%
      config(displayModeBar = TRUE, displaylogo = FALSE)
  })
  
  # ========================================================================
  # OUTPUT - INFORMACIÓN DEL SANKEY
  # ========================================================================
  
  output$sankey_info <- renderPrint({
    sankey <- sankey_data()
    
    if (is.null(sankey)) {
      cat("No hay diagrama generado.\n")
      return()
    }
    
    cat("Información del Diagrama de Sankey:\n")
    cat("===================================\n\n")
    cat("Fases mostradas:", paste(unique(sankey$info$fases_procesadas), collapse = ", "), "\n")
    cat("Colorear por parámetro:", if(sankey$mostrar_parametro) "Sí" else "No", "\n\n")
    cat("Estadísticas:\n")
    cat("- Nodos:", sankey$info$n_nodos, "\n")
    cat("- Enlaces detallados:", formatC(sankey$info$n_enlaces_detallados, format = "d", big.mark = " "), "\n")
    cat("- Enlaces únicos:", formatC(sankey$info$n_enlaces, format = "d", big.mark = " "), "\n")
    cat(sprintf("- Importe total (Fase %d): %s€\n", 
                sankey$info$fase_maxima, 
                formatC(sankey$info$importe_total, format = "f", big.mark = " ", digits = 2)))
  })
  
  # ========================================================================
  # OUTPUT - TABLA DE ENLACES
  # ========================================================================
  
  output$sankey_enlaces_table <- DT::renderDataTable({
    sankey <- sankey_data()
    
    if (is.null(sankey)) {
      return(DT::datatable(
        data.frame(Mensaje = "Genera el diagrama de Sankey primero"),
        options = list(dom = 't'),
        rownames = FALSE
      ))
    }
    
    enlaces <- as.data.frame(sankey$enlaces_detallados)
    enlaces$value_formatted <- paste0(formatC(enlaces$value, format = "f", big.mark = " ", digits = 2), "€")
    
    enlaces_display <- enlaces %>%
      select(
        Fase = fase,
        Origen = source,
        Destino = target,
        Tipo = tipo_reparto,
        Parámetro = parametro,
        Importe = value_formatted
      )
    
    DT::datatable(
      enlaces_display,
      options = list(
        scrollX = FALSE,
        responsive = TRUE,
        autoWidth = FALSE,
        pageLength = 10,
        language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')
      ),
      rownames = FALSE,
      class = "stripe hover compact"
    )
  })
  
  # ========================================================================
  # OBSERVADOR PARA EXPANDIR DIAGRAMA
  # ========================================================================
  
  observeEvent(input$expand_sankey, {
    sankey <- sankey_data()
    if (is.null(sankey)) return()
    
    showModal(modalDialog(
      renderPlotly({
        link_colors <- "rgba(100, 116, 139, 0.3)"
        node_colors <- "rgba(31, 119, 180, 0.8)"
        
        plot_ly(
          type = "sankey",
          orientation = "h",
          arrangement = "snap",
          node = list(
            label = sankey$nodes$name,
            color = node_colors,
            pad = 18,
            thickness = 25,
            line = list(color = "rgba(255, 255, 255, 0.8)", width = 1.5),
            hovertemplate = "%{label}<extra></extra>"
          ),
          link = list(
            source = sankey$links$source,
            target = sankey$links$target,
            value = sankey$links$value,
            color = link_colors,
            hovertemplate = "Importe: %{value:,.2f}€<extra></extra>"
          )
        ) %>%
          layout(
            font = list(family = "Inter, sans-serif", size = 14, color = "#1e293b"),
            paper_bgcolor = "rgba(0,0,0,0)",
            plot_bgcolor = "rgba(0,0,0,0)",
            margin = list(l = 10, r = 10, t = 30, b = 10)
          ) %>%
          config(displayModeBar = TRUE, displaylogo = FALSE)
      }, height = 800),
      easyClose = TRUE,
      size = "l",
      footer = NULL
    ))
  })
  
  return(sankey_data)
}
