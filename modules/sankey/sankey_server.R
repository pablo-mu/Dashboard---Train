# ============================================================================
# SANKEY SERVER LOGIC
# ============================================================================

#' Asignar color a parámetro en enlaces Sankey
#' @param parametro Parámetro de reparto
#' @param tipo_reparto Tipo de reparto
#' @return String con color RGBA
asignar_color_parametro <- function(parametro, tipo_reparto) {
  # Sin parámetro: color gris uniforme
  if (is.na(parametro) || parametro == "" || parametro == "NO PARAM") {
    return("rgba(150, 150, 150, 0.25)")
  }
  
  # Colores vibrantes para diferentes parámetros
  parametro_hash <- sum(utf8ToInt(as.character(parametro))) %% 12
  
  colores <- c(
    "rgba(59, 130, 246, 0.5)",   # Azul moderno
    "rgba(16, 185, 129, 0.5)",   # Verde esmeralda
    "rgba(245, 158, 11, 0.5)",   # Ámbar
    "rgba(239, 68, 68, 0.5)",    # Rojo coral
    "rgba(168, 85, 247, 0.5)",   # Púrpura
    "rgba(236, 72, 153, 0.5)",   # Rosa
    "rgba(20, 184, 166, 0.5)",   # Turquesa
    "rgba(251, 146, 60, 0.5)",   # Naranja
    "rgba(139, 92, 246, 0.5)",   # Violeta
    "rgba(34, 197, 94, 0.5)",    # Verde lima
    "rgba(248, 113, 113, 0.5)",  # Rojo claro
    "rgba(96, 165, 250, 0.5)"    # Azul cielo
  )
  
  return(colores[parametro_hash + 1])
}

#' Función auxiliar para crear el plot de Sankey (reutilizable)
#' @param sankey Datos del sankey (lista con nodes, links, mostrar_parametro, etc.)
#' @param font_size Tamaño de fuente para el diagrama
#' @return Objeto plotly
crear_plot_sankey <- function(sankey, font_size = 11) {
  if (is.null(sankey)) {
    return(plotly_empty() %>% 
      layout(title = list(
        text = "Configure los filtros y presione 'Generar'",
        font = list(size = 18)
      ))
    )
  }
  
  # Determinar colores según si se muestra parámetro o no
  if (sankey$mostrar_parametro && "parametro" %in% names(sankey$links)) {
    # Aplicar colores por parámetro
    link_colors <- sapply(1:nrow(sankey$links), function(i) {
      asignar_color_parametro(
        sankey$links$parametro[i],
        sankey$links$tipo_reparto[i]
      )
    })
    
    # Hover template con información de parámetro
    hover_template <- paste0(
      "Importe: %{value:,.2f}€<br>",
      "Parámetro: ", ifelse(is.na(sankey$links$parametro) | sankey$links$parametro == "", 
                           "Sin parámetro", 
                           sankey$links$parametro),
      "<extra></extra>"
    )
  } else {
    # Colores por defecto (gris uniforme)
    link_colors <- "rgba(100, 116, 139, 0.3)"
    hover_template <- "Importe: %{value:,.2f}€<extra></extra>"
  }
  
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
      hovertemplate = hover_template
    )
  ) %>%
    layout(
      font = list(family = "Inter, sans-serif", size = font_size, color = "#1e293b"),
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor = "rgba(0,0,0,0)",
      margin = list(l = 10, r = 10, t = 30, b = 10)
    ) %>%
    config(displayModeBar = TRUE, displaylogo = FALSE)
}

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
    
    # ==========================================================================
    # CAPTURAR FILTROS DEL SIDEBAR
    # ==========================================================================
    
    # Centro Gestor
    centro_gestor_val <- if (!is.null(input$centro_gestor) && input$centro_gestor != "") {
      input$centro_gestor
    } else {
      NULL
    }
    
    # Nivel de agregación
    nivel_agregacion_val <- if (!is.null(input$nivel_agregacion)) {
      input$nivel_agregacion
    } else {
      "cac"
    }
    
    # Origen (CAC/SUBCAC según el nivel)
    cac_subcac_origen_val <- if (!is.null(input$origen) && length(input$origen) > 0 && input$origen[1] != "") {
      input$origen
    } else {
      NULL
    }
    
    # Destino (CAC/SUBCAC según el nivel)
    cac_subcac_destino_val <- if (!is.null(input$destino) && length(input$destino) > 0 && input$destino[1] != "") {
      input$destino
    } else {
      NULL
    }
    
    # Mes
    mes_val <- if (!is.null(input$mes) && length(input$mes) > 0 && input$mes[1] != "") {
      as.numeric(input$mes)
    } else {
      NULL
    }
    
    # Año
    anyo_val <- if (!is.null(input$anyo) && length(input$anyo) > 0 && input$anyo[1] != "") {
      as.numeric(input$anyo)
    } else {
      NULL
    }
    
    # Fases a incluir
    fases_incluir_val <- if (!is.null(input$fases_filter) && length(input$fases_filter) > 0) {
      as.numeric(input$fases_filter)
    } else {
      c(1, 2, 3)
    }
    
    # Mostrar estáticos (del sidebar) - invertir lógica para excluir_estaticos
    # Si mostrar_estaticos = TRUE -> excluir_estaticos = FALSE
    # Si mostrar_estaticos = FALSE -> excluir_estaticos = TRUE
    excluir_estaticos_val <- if (!is.null(input$mostrar_estaticos)) {
      !input$mostrar_estaticos
    } else {
      TRUE  # Por defecto, excluir estáticos
    }
    
    # ==========================================================================
    # CAPTURAR FILTROS DEL MODAL (si existen)
    # ==========================================================================
    
    # El checkbox excluir_estaticos del modal (si existe) sobrescribe el del sidebar
    if (!is.null(input$modal_excluir_estaticos)) {
      # En el modal: "Mostrar Movimientos Estáticos" 
      # Si está marcado (TRUE) -> queremos MOSTRAR -> excluir_estaticos = FALSE
      excluir_estaticos_val <- !input$modal_excluir_estaticos
    }
    
    # El checkbox mostrar_parametro del modal (si existe) sobrescribe el del panel
    if (!is.null(input$modal_mostrar_parametro)) {
      mostrar_parametro_actual <- input$modal_mostrar_parametro
    }
    
    tryCatch({
      source("modules/sankey/sankey.R")
      
      # Construir enlaces con los filtros reales
      resultado <- construir_enlaces_sankey(
        datos = datos,
        centro_gestor = centro_gestor_val,
        nivel_agregacion = nivel_agregacion_val,
        cac_subcac_origen = cac_subcac_origen_val,
        cac_subcac_destino = cac_subcac_destino_val,
        mes = mes_val,
        anyo = anyo_val,
        fases_incluir = fases_incluir_val,
        excluir_estaticos = excluir_estaticos_val,
        verbose = TRUE
      )
      
      if (is.null(resultado)) {
        showNotification("No se generaron enlaces con los filtros seleccionados", 
                         type = "warning", duration = 5)
        sankey_data(NULL)
        return()
      }
      
      # Extraer enlaces y matriz
      enlaces_df <- resultado$enlaces
      matriz_movimientos <- resultado$matriz
      
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
        matriz_movimientos = matriz_movimientos,
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
    crear_plot_sankey(sankey_data(), font_size = 11)
  })
  
  # ========================================================================
  # OUTPUT - DIAGRAMA SANKEY EXPANDIDO
  # ========================================================================
  
  output$sankey_diagram_expanded <- renderPlotly({
    crear_plot_sankey(sankey_data(), font_size = 11)
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
        scrollX = TRUE,
        scrollY = "350px",
        paging = TRUE,
        pageLength = 15,
        responsive = TRUE,
        autoWidth = FALSE,
        dom = 'frtip',
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
    
    # Mostrar el diagrama expandido con ID diferente
    showModal(modalDialog(
      title = NULL,
      plotlyOutput("sankey_diagram_expanded", width = "100%"),
      easyClose = TRUE,
      size = "xl",
      footer = NULL
    ))
  })
  
  # ========================================================================
  # OBSERVADOR PARA EXPANDIR TABLA DE ENLACES
  # ========================================================================
  
  observeEvent(input$expand_enlaces_table, {
    sankey <- sankey_data()
    if (is.null(sankey)) return()
    
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
    
    showModal(modalDialog(
      title = "Enlaces Detallados - Vista Expandida",
      div(
        style = "height: 600px; width: 100%; overflow: auto;",
        DT::dataTableOutput("sankey_enlaces_table_expanded", width = "100%")
      ),
      easyClose = TRUE,
      size = "xl",
      footer = NULL
    ))
    
    output$sankey_enlaces_table_expanded <- DT::renderDataTable({
      DT::datatable(
        enlaces_display,
        options = list(
          scrollX = TRUE,
          responsive = TRUE,
          autoWidth = FALSE,
          pageLength = 25,
          language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')
        ),
        rownames = FALSE,
        class = "stripe hover compact"
      )
    })
  })
  
  # ========================================================================
  # DOWNLOAD HANDLER - DIAGRAMA SANKEY
  # ========================================================================
  
  output$download_sankey <- downloadHandler(
    filename = function() {
      paste0("sankey_diagram_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".html")
    },
    content = function(file) {
      sankey <- sankey_data()
      if (is.null(sankey)) {
        showNotification("No hay diagrama para descargar", type = "warning")
        return()
      }
      
      plot <- crear_plot_sankey(sankey, font_size = 14)
      htmlwidgets::saveWidget(plot, file, selfcontained = TRUE)
    }
  )
  
  return(sankey_data)
}
