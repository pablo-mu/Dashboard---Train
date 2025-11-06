source("R/sankey/sankey.R")

# R/mod_sankey.R - Módulo del Diagrama de Sankey
# Este módulo encapsula toda la lógica del diagrama de Sankey

# ============================================================================
# UI DEL MÓDULO
# ============================================================================

sankeyUI <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    # Filtros
    fluidRow(
      box(
        title = "Filtros para Diagrama de Sankey",
        status = "primary",
        solidHeader = TRUE,
        width = 12,
        fluidRow(
          column(2,
                 uiOutput(ns("centro_gestor_filter")),
                 checkboxInput(ns("mostrar_estaticos"), "Movimientos estáticos", value = TRUE)
          ),
          column(3,
                 radioButtons(ns("nivel_agregacion"), "Nivel de Agregación:",
                              choices = NIVELES_AGREGACION,
                              selected = "cac",
                              inline = FALSE),
                 uiOutput(ns("origen_filter"))
          ),
          column(3,
                 uiOutput(ns("mes_filter")),
                 uiOutput(ns("anyo_filter"))
          ),
          column(2,
                 checkboxGroupInput(ns("fases_filter"), "Fases*:",
                                    choices = FASES_CHOICES,
                                    selected = c("1", "2", "3"),
                                    inline = FALSE),
                 checkboxInput(ns("mostrar_parametro"), "Parámetro Reparto", value = FALSE)
          ),
          column(2,
                 actionButton(ns("generar_sankey"), "Generar Diagrama", 
                              icon = icon("play"),
                              style = sprintf("width: 100%%; background-color: %s; color: white; font-weight: bold;", COLORS$primary)),
                 br(), br(),
                 actionButton(ns("reset_filters"), "Resetear", 
                              icon = icon("rotate-left"),
                              style = "width: 100%;")
          )
        )
      )
    ),
    
    # Info Boxes
    fluidRow(
      infoBoxOutput(ns("info_nodos"), width = 4),
      infoBoxOutput(ns("info_enlaces"), width = 4),
      infoBoxOutput(ns("info_importe"), width = 4)
    ),
    
    # Diagrama
    fluidRow(
      box(
        title = "Flujo de Reparto de Costes",
        status = "primary",
        solidHeader = TRUE,
        width = 12,
        height = "700px",
        plotlyOutput(ns("sankey_diagram"), height = "650px")
      )
    ),
    
    # Información
    fluidRow(
      box(
        title = "Información del Diagrama",
        status = "info",
        solidHeader = TRUE,
        width = 12,
        verbatimTextOutput(ns("sankey_info"))
      )
    ),
    
    # Enlaces detallados
    # Enlaces detallados
    fluidRow(
      box(
        title = "Enlaces Detallados",
        status = "warning",
        solidHeader = TRUE,
        width = 12,
        collapsible = TRUE,
        collapsed = FALSE,  # Changed from TRUE to FALSE
        div(style = "overflow-x:auto;", DT::dataTableOutput(ns("enlaces_table")))
      )
    )
  )
}

# ============================================================================
# SERVER DEL MÓDULO
# ============================================================================

sankeyServer <- function(id, datos_raw, sidebar_state) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Variable reactiva para almacenar resultados del Sankey
    sankey_data <- reactiveVal(NULL)
    
    # Limpiar memoria al cambiar de pestaña
    observe({
      if (!is.null(sidebar_state()) && sidebar_state() != "sankey") {
        sankey_data(NULL)
        gc()
      }
    })
    
    # ========================================================================
    # FILTROS
    # ========================================================================
    
    output$centro_gestor_filter <- crear_filtro_centro_gestor(datos_raw, ns("centro_gestor"))
    
    output$origen_filter <- renderUI({
      crear_filtro_origen(datos_raw, input$nivel_agregacion, input$centro_gestor, ns("origen"))()
    })
    
    output$mes_filter <- crear_filtro_mes(datos_raw, ns("mes"))
    output$anyo_filter <- crear_filtro_anyo(datos_raw, ns("anyo"))
    
    # Resetear filtros
    observeEvent(input$reset_filters, {
      updateSelectInput(session, "centro_gestor", selected = "")
      updateSelectInput(session, "origen", selected = "")
      updateSelectInput(session, "mes", selected = "")
      updateSelectInput(session, "anyo", selected = "")
      updateCheckboxGroupInput(session, "fases_filter", selected = c("1", "2", "3"))
      updateCheckboxInput(session, "mostrar_parametro", value = FALSE)
      updateCheckboxInput(session, "mostrar_estaticos", value = TRUE)
    })
    
    # ========================================================================
    # GENERACIÓN DEL SANKEY
    # ========================================================================
    
    observeEvent(input$generar_sankey, {
      datos <- datos_raw()
      if (is.null(datos)) {
        sankey_data(NULL)
        return()
      }
      
      # Validaciones
      if (is.null(input$centro_gestor) || input$centro_gestor == "") {
        showNotification("Por favor, selecciona un Centro Gestor", type = "error", duration = 5)
        sankey_data(NULL)
        return()
      }
      
      if (is.null(input$fases_filter) || length(input$fases_filter) == 0) {
        showNotification("Por favor, selecciona al menos una fase", type = "error", duration = 5)
        sankey_data(NULL)
        return()
      }
      
      mostrar_carga("Generando Diagrama de Sankey...")
      
      # Preparar filtros
      origen_filter <- if (!is.null(input$origen) && length(input$origen) > 0 && input$origen[1] != "") {
        input$origen
      } else {
        NULL
      }
      
      mes_filter <- if (!is.null(input$mes) && input$mes[1] != "") input$mes else NULL
      anyo_filter <- if (!is.null(input$anyo) && input$anyo[1] != "") as.integer(input$anyo) else NULL
      fases_filter <- as.integer(input$fases_filter)
      
      mostrar_parametro_actual <- input$mostrar_parametro
      excluir_estaticos_actual <- !input$mostrar_estaticos
      
      tryCatch({
        # Construir enlaces
        enlaces_df <- construir_enlaces_sankey(
          datos = datos,
          centro_gestor = input$centro_gestor,
          nivel_agregacion = input$nivel_agregacion,
          cac_subcac_origen = origen_filter,
          fases_incluir = fases_filter,
          excluir_estaticos = excluir_estaticos_actual,
          verbose = TRUE
        )
        
        if (is.null(enlaces_df) || nrow(enlaces_df) == 0) {
          cerrar_carga()
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
        
        cerrar_carga()
        
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
          excluir_estaticos = excluir_estaticos_actual,
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
        cerrar_carga()
        showNotification(
          paste("Error al construir Sankey:", e$message),
          type = "error",
          duration = 10
        )
        sankey_data(NULL)
      })
    })
    
    # ========================================================================
    # OUTPUTS
    # ========================================================================
    
    # Info Boxes
    #output$info_registros <- renderInfoBox({
    #  sankey <- sankey_data()
    #  n <- if (!is.null(sankey)) sankey$info$n_enlaces_detallados else 0
    #  
    #  infoBox(
    #    "Enlaces Detallados",
    #    formatear_numero(n),
    #    icon = icon("list"),
    #    color = if (n > 0) "green" else "red"
    #  )
    #})
    
    # Nodos (enteros)
    output$info_nodos <- renderInfoBox({
      sankey <- sankey_data()
      n <- if (!is.null(sankey)) sankey$info$n_nodos else 0
      infoBox(
        "Nodos",
        formatear_entero(as.integer(n)),
        icon = icon("dot-circle"),  # icon más estándar y compacto
        color = if (n > 0) "blue" else "red"
      )
    })

    # Enlaces únicos (enteros)
    output$info_enlaces <- renderInfoBox({
      sankey <- sankey_data()
      n <- if (!is.null(sankey)) sankey$info$n_enlaces else 0
      infoBox(
        "Enlaces Únicos",
        formatear_entero(as.integer(n)),
        icon = icon("arrows-left-right"),  # icon compacto
        color = if (n > 0) "purple" else "red"
      )
    })

    # Importe total (moneda)
    output$info_importe <- renderInfoBox({
      sankey <- sankey_data()
      imp <- if (!is.null(sankey)) sankey$info$importe_total else 0
      fase_info <- if (!is.null(sankey)) paste0(" (Fase ", sankey$info$fase_maxima, ")") else ""
      infoBox(
        paste0("Importe Total", fase_info),
        formatear_moneda(imp),
        icon = icon("euro-sign"),
        color = if (imp > 0) "yellow" else "red"
      )
    })
    
    # Diagrama
    output$sankey_diagram <- renderPlotly({
      sankey <- sankey_data()
      
      if (is.null(sankey)) {
        return(suppressWarnings(
          plotly_empty() %>% 
            layout(title = list(
              text = "Selecciona filtros y haz click en 'Generar Diagrama'",
              font = list(size = 18)
            ))
        ))
      }
      # Importar función de colores desde utils_ui.R
      source("R/utils_ui.R", local = TRUE)
      
      # Asignar colores
      if (sankey$mostrar_parametro && "parametro" %in% names(sankey$links)) {
        link_colors <- mapply(
          asignar_color_parametro,
          sankey$links$parametro,
          sankey$links$tipo_reparto,
          SIMPLIFY = TRUE,
          USE.NAMES = FALSE
        )
        
        link_labels <- sapply(1:nrow(sankey$links), function(i) {
          param <- sankey$links$parametro[i]
          tipo <- sankey$links$tipo_reparto[i]
          valor <- formatear_moneda(sankey$links$value[i])
          
          if (is.na(param) || param == "" || param == "NO PARAM") {
            paste0("Tipo: ", tipo, "<br>Importe: ", valor)
          } else {
            paste0("Parámetro: ", param, "<br>Tipo: ", tipo, "<br>Importe: ", valor)
          }
        })
      } else {
        link_colors <- "rgba(100, 116, 139, 0.3)"
        link_labels <- NULL
      }
      
      node_colors <- sapply(sankey$nodes$name, asignar_color_nodo, USE.NAMES = FALSE)
      
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
          customdata = if (!is.null(link_labels)) link_labels else NULL,
          hovertemplate = if (!is.null(link_labels)) {
            "%{customdata}<extra></extra>"
          } else {
            "Importe: %{value:,.2f}€<extra></extra>"
          }
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
    
    # Info del Sankey
    output$sankey_info <- renderPrint({
      sankey <- sankey_data()
      
      if (is.null(sankey)) {
        cat("No hay diagrama generado.\n")
        return()
      }
      
      cat("Información del Diagrama de Sankey:\n")
      cat("===================================\n\n")
      
      nivel_texto <- names(NIVELES_AGREGACION)[NIVELES_AGREGACION == input$nivel_agregacion]
      cat("Nivel de agregación:", nivel_texto, "\n")
      cat("Centro Gestor:", input$centro_gestor, "\n")
      
      if (!is.null(input$origen) && length(input$origen) > 0 && input$origen[1] != "") {
        cat("Origen(es):", paste(input$origen, collapse = ", "), "\n")
      } else {
        cat("Origen(es): Todos\n")
      }
      
      cat("Fases mostradas:", paste(unique(sankey$info$fases_procesadas), collapse = ", "), "\n")
      cat("Colorear por parámetro:", if(sankey$mostrar_parametro) "Sí" else "No", "\n")
      cat("Movimientos estáticos excluidos:", if(sankey$excluir_estaticos) "Sí" else "No", "\n\n")
      
      cat("Estadísticas:\n")
      cat("- Nodos:", sankey$info$n_nodos, "\n")
      cat("- Enlaces detallados:", formatear_numero(sankey$info$n_enlaces_detallados), "\n")
      cat("- Enlaces únicos:", formatear_numero(sankey$info$n_enlaces), "\n")
      cat(sprintf("- Importe total (Fase %d): %s\n", 
                  sankey$info$fase_maxima, formatear_moneda(sankey$info$importe_total)))
    })
    
    # Tabla de enlaces
    # Tabla de enlaces
    # Tabla de enlaces (sin columna 'Centro Gestor')
    output$enlaces_table <- DT::renderDataTable({
      sankey <- sankey_data()
      if (is.null(sankey)) {
        DT::datatable(
          data.frame(Mensaje = "Genera el diagrama de Sankey primero"),
          options = list(dom = 't'),
          rownames = FALSE
        )
      } else {
        enlaces <- as.data.frame(sankey$enlaces_detallados)

        enlaces$value_formatted <- formatear_moneda(enlaces$value)

        enlaces_display <- enlaces %>%
          dplyr::select(
            Fase = fase,
            Origen = source,
            Destino = target,
            Tipo = tipo_reparto,
            Parámetro = parametro,
            Importe = value_formatted
          )

        if (nrow(enlaces_display) == 0) {
          DT::datatable(
            data.frame(Mensaje = "No hay enlaces para mostrar"),
            options = list(dom = 't'),
            rownames = FALSE
          )
        } else {
          DT::datatable(
            enlaces_display,
            options = modifyList(
              list(
                scrollX = FALSE,
                responsive = TRUE,
                autoWidth = FALSE,
                pageLength = 10
              ),
              DT_OPTIONS_DEFAULT
            ),
            rownames = FALSE,
            class = "stripe hover compact"
          )
        }
      }
    })
  })
}