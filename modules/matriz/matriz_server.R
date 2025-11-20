# ============================================================================
# MATRIZ SERVER LOGIC
# ============================================================================

#' Create Matriz server logic
#' This function sets up all the reactive logic for the Matriz panel
#' 
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#' @param datos_raw Reactive value containing the raw data
#' @param active_panel Reactive value indicating the active panel
create_matriz_server <- function(input, output, session, datos_raw, active_panel) {
  
  # Variable reactiva para almacenar resultados de la Matriz
  matriz_data <- reactiveVal(NULL)
  
  # Limpiar memoria al cambiar de pestaña
  observe({
    if (!is.null(active_panel()) && active_panel() != "matriz_costes") {
      matriz_data(NULL)
      gc()
    }
  })
  
  # ========================================================================
  # GENERACIÓN DE LA MATRIZ cuando se presiona "Generar"
  # ========================================================================
  
  observeEvent(input$apply_filters, {
    # Solo procesar si estamos en el panel de Matriz
    if (is.null(active_panel()) || active_panel() != "matriz_costes") {
      return()
    }
    
    datos <- datos_raw()
    if (is.null(datos)) {
      showNotification("No hay datos cargados", type = "error", duration = 5)
      matriz_data(NULL)
      return()
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
    
    # Destino (si existe en el sidebar)
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
    
    tryCatch({
      source("modules/matriz/matriz_costes.R")
      
      # Calcular matriz con los filtros reales
      resultado_matriz <- calcular_matriz_costes(
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
      
      if (is.null(resultado_matriz) || is.null(resultado_matriz$matriz)) {
        showNotification("No se generó matriz con los filtros seleccionados", 
                         type = "warning", duration = 5)
        matriz_data(NULL)
        return()
      }
      
      showNotification(
        paste("✓ Matriz generada:", nrow(resultado_matriz$matriz), "movimientos únicos"), 
        type = "message", 
        duration = 3
      )
      
      matriz_data(resultado_matriz)
      
    }, error = function(e) {
      showNotification(
        paste("Error al construir Matriz:", e$message),
        type = "error",
        duration = 10
      )
      matriz_data(NULL)
    })
  })
  
  # ========================================================================
  # OUTPUT - DIAGRAMA MATRIZ (HEATMAP)
  # ========================================================================
  
  output$matriz_diagram <- renderPlotly({
    matriz <- matriz_data()
    
    if (is.null(matriz)) {
      return(suppressWarnings(
        plotly_empty() %>% 
          layout(title = list(
            text = "Configure los filtros y presione 'Generar'",
            font = list(size = 18)
          ))
      ))
    }
    
    tryCatch({
      # Usar la función de matriz_costes.R
      p <- heatmap_matriz_costes(matriz$matriz)
      
      if (is.null(p)) {
        return(suppressWarnings(
          plotly_empty() %>% 
            layout(title = list(
              text = "No hay datos para el heatmap",
              font = list(size = 18)
            ))
        ))
      }
      
      # Convertir ggplot a plotly
      ggplotly(p, tooltip = c("x", "y", "fill")) %>%
        layout(margin = list(l = 100, r = 50, t = 50, b = 150))
        
    }, error = function(e) {
      showNotification(
        paste("Error al generar heatmap:", e$message),
        type = "error",
        duration = 10
      )
      return(suppressWarnings(
        plotly_empty() %>% 
          layout(title = list(
            text = "Error al generar heatmap",
            font = list(size = 18)
          ))
      ))
    })
  })
  
  # ========================================================================
  # OUTPUT - DIAGRAMA MATRIZ EXPANDIDO
  # ========================================================================
  
  output$matriz_diagram_expanded <- renderPlotly({
    matriz <- matriz_data()
    
    if (is.null(matriz)) {
      return(suppressWarnings(
        plotly_empty() %>% 
          layout(title = list(
            text = "No hay datos",
            font = list(size = 18)
          ))
      ))
    }
    
    tryCatch({
      # Usar la función de matriz_costes.R
      p <- heatmap_matriz_costes(matriz$matriz)
      
      if (is.null(p)) {
        return(suppressWarnings(
          plotly_empty() %>% 
            layout(title = list(
              text = "No hay datos para el heatmap",
              font = list(size = 18)
            ))
        ))
      }
      
      # Convertir ggplot a plotly
      ggplotly(p, tooltip = c("x", "y", "fill")) %>%
        layout(margin = list(l = 100, r = 50, t = 50, b = 150))
        
    }, error = function(e) {
      showNotification(
        paste("Error al generar heatmap:", e$message),
        type = "error",
        duration = 10
      )
      return(suppressWarnings(
        plotly_empty() %>% 
          layout(title = list(
            text = "Error al generar heatmap",
            font = list(size = 18)
          ))
      ))
    })
  })
  
  # ========================================================================
  # OUTPUT - TABLA FORMATO LARGO
  # ========================================================================
  
  output$matriz_table_larga <- DT::renderDataTable({
    matriz <- matriz_data()
    
    if (is.null(matriz)) {
      return(DT::datatable(
        data.frame(Mensaje = "Genera la matriz primero"),
        options = list(dom = 't'),
        rownames = FALSE
      ))
    }
    
    tabla_larga <- matriz$matriz %>%
      mutate(importe_formatted = paste0(formatC(importe_total, format = "f", big.mark = " ", digits = 2), "€")) %>%
      select(
        Origen = origen,
        Destino = destino,
        Importe = importe_formatted
      )
    
    DT::datatable(
      tabla_larga,
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
  # OUTPUT - TABLA FORMATO ANCHO
  # ========================================================================
  
  output$matriz_table_ancha <- DT::renderDataTable({
    matriz <- matriz_data()
    
    if (is.null(matriz)) {
      return(DT::datatable(
        data.frame(Mensaje = "Genera la matriz primero"),
        options = list(dom = 't'),
        rownames = FALSE
      ))
    }
    
    tabla_ancha <- matriz$matriz_wide
    
    # Formatear números con separadores de miles
    tabla_ancha_formatted <- tabla_ancha %>%
      mutate(across(where(is.numeric), ~formatC(., format = "f", big.mark = " ", digits = 2)))
    
    DT::datatable(
      tabla_ancha_formatted,
      options = list(
        scrollX = TRUE,
        scrollY = "350px",
        paging = TRUE,
        pageLength = 15,
        responsive = FALSE,
        autoWidth = FALSE,
        dom = 'frtip',
        language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')
      ),
      rownames = FALSE,
      class = "stripe hover compact cell-border"
    )
  })
  
  # ========================================================================
  # OBSERVADOR PARA EXPANDIR DIAGRAMA
  # ========================================================================
  
  observeEvent(input$expand_matriz, {
    matriz <- matriz_data()
    if (is.null(matriz)) return()
    
    # Mostrar el diagrama expandido con ID diferente
    showModal(modalDialog(
      title = NULL,
      plotlyOutput("matriz_diagram_expanded", height = "800px", width = "100%"),
      easyClose = TRUE,
      size = "xl",
      footer = NULL
    ))
  })
  
  # ========================================================================
  # OBSERVADOR PARA EXPANDIR TABLA LARGA
  # ========================================================================
  
  observeEvent(input$expand_matriz_larga, {
    matriz <- matriz_data()
    if (is.null(matriz)) return()
    
    tabla_larga <- matriz$matriz %>%
      mutate(importe_formatted = paste0(formatC(importe_total, format = "f", big.mark = " ", digits = 2), "€")) %>%
      select(
        Origen = origen,
        Destino = destino,
        Importe = importe_formatted
      )
    
    showModal(modalDialog(
      title = "Matriz - Formato Largo (Vista Expandida)",
      div(
        style = "height: 600px; width: 100%; overflow: auto;",
        DT::dataTableOutput("matriz_table_larga_expanded", width = "100%")
      ),
      easyClose = TRUE,
      size = "xl",
      footer = NULL
    ))
    
    output$matriz_table_larga_expanded <- DT::renderDataTable({
      DT::datatable(
        tabla_larga,
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
  # OBSERVADOR PARA EXPANDIR TABLA ANCHA
  # ========================================================================
  
  observeEvent(input$expand_matriz_ancha, {
    matriz <- matriz_data()
    if (is.null(matriz)) return()
    
    tabla_ancha <- matriz$matriz_wide
    
    # Formatear números con separadores de miles
    tabla_ancha_formatted <- tabla_ancha %>%
      mutate(across(where(is.numeric), ~formatC(., format = "f", big.mark = " ", digits = 2)))
    
    showModal(modalDialog(
      title = "Matriz - Formato Ancho (Vista Expandida)",
      div(
        style = "height: 600px; width: 100%; overflow: auto;",
        DT::dataTableOutput("matriz_table_ancha_expanded", width = "100%")
      ),
      easyClose = TRUE,
      size = "xl",
      footer = NULL
    ))
    
    output$matriz_table_ancha_expanded <- DT::renderDataTable({
      DT::datatable(
        tabla_ancha_formatted,
        options = list(
          scrollX = TRUE,
          scrollY = "500px",
          responsive = FALSE,
          autoWidth = FALSE,
          pageLength = 50,
          language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')
        ),
        rownames = FALSE,
        class = "stripe hover compact cell-border"
      )
    })
  })
  
  # ========================================================================
  # DOWNLOAD HANDLER - DIAGRAMA MATRIZ
  # ========================================================================
  
  output$download_matriz <- downloadHandler(
    filename = function() {
      paste0("matriz_costes_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".html")
    },
    content = function(file) {
      matriz <- matriz_data()
      if (is.null(matriz)) {
        showNotification("No hay matriz para descargar", type = "warning")
        return()
      }
      
      tryCatch({
        # Crear el heatmap usando la función de matriz_costes.R
        p <- heatmap_matriz_costes(matriz$matriz)
        
        if (is.null(p)) {
          showNotification("No hay datos para generar el heatmap", type = "warning")
          return()
        }
        
        # Convertir a plotly y guardar como HTML interactivo
        plot_interactive <- ggplotly(p, tooltip = c("x", "y", "fill")) %>%
          layout(margin = list(l = 100, r = 50, t = 50, b = 150))
        
        htmlwidgets::saveWidget(plot_interactive, file, selfcontained = TRUE)
        
      }, error = function(e) {
        showNotification(
          paste("Error al descargar matriz:", e$message),
          type = "error",
          duration = 10
        )
      })
    }
  )
  
  return(matriz_data)
}
