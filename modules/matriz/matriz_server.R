# ============================================================================
# MATRIZ SERVER LOGIC
# ============================================================================

#' Create Matriz server logic
create_matriz_server <- function(input, output, session, datos_raw, active_panel) {

  matriz_data <- reactiveVal(NULL)

  observe({
    if (!is.null(active_panel()) && active_panel() != "matriz_costes") {
      matriz_data(NULL)
      gc()
    }
  })

  # ---- Capturar filtros (helper) ----
  .get_filtros <- function() {
    list(
      centro_gestor      = if (!is.null(input$centro_gestor) && input$centro_gestor != "")
                             input$centro_gestor else NULL,
      nivel_agregacion   = if (!is.null(input$nivel_agregacion)) input$nivel_agregacion else "cac",
      cac_subcac_origen  = if (!is.null(input$origen) && length(input$origen) > 0 &&
                               input$origen[1] != "") input$origen else NULL,
      cac_subcac_destino = if (!is.null(input$destino) && length(input$destino) > 0 &&
                               input$destino[1] != "") input$destino else NULL,
      fase1              = if (!is.null(input$fase1) && length(input$fase1) > 0 &&
                               input$fase1[1] != "") input$fase1 else NULL,
      fase2              = if (!is.null(input$fase2) && length(input$fase2) > 0 &&
                               input$fase2[1] != "") input$fase2 else NULL,
      mes                = if (!is.null(input$mes) && length(input$mes) > 0 &&
                               input$mes[1] != "") as.numeric(input$mes) else NULL,
      anyo               = if (!is.null(input$anyo) && length(input$anyo) > 0 &&
                               input$anyo[1] != "") as.numeric(input$anyo) else NULL,
      fases_incluir      = if (!is.null(input$fases_filter) && length(input$fases_filter) > 0)
                             as.numeric(input$fases_filter) else c(1, 2, 3),
      excluir_estaticos  = {
        base <- if (!is.null(input$mostrar_estaticos)) !input$mostrar_estaticos else TRUE
        if (!is.null(input$modal_excluir_estaticos)) !input$modal_excluir_estaticos else base
      }
    )
  }

  # ========================================================================
  # GENERAR MATRIZ
  # ========================================================================

  observeEvent(input$apply_filters, {
    if (is.null(active_panel()) || active_panel() != "matriz_costes") return()

    datos <- datos_raw()
    if (is.null(datos)) {
      showNotification("No hay datos cargados.", type = "error", duration = 5)
      matriz_data(NULL)
      return()
    }

    f <- .get_filtros()

    # calcular_matriz_costes ya cargada vía dynamic_server.R
    resultado_matriz <- tryCatch(
      calcular_matriz_costes(
        datos              = datos,
        centro_gestor      = f$centro_gestor,
        nivel_agregacion   = f$nivel_agregacion,
        cac_subcac_origen  = f$cac_subcac_origen,
        cac_subcac_destino = f$cac_subcac_destino,
        fase1_filter       = f$fase1,
        fase2_filter       = f$fase2,
        mes                = f$mes,
        anyo               = f$anyo,
        fases_incluir      = f$fases_incluir,
        excluir_estaticos  = f$excluir_estaticos,
        verbose            = TRUE
      ),
      error = function(e) {
        showNotification(paste("Error al construir Matriz:", e$message),
                         type = "error", duration = 10)
        NULL
      }
    )

    if (is.null(resultado_matriz) || is.null(resultado_matriz$matriz)) {
      showNotification("Sin datos de matriz con los filtros seleccionados.", type = "warning", duration = 5)
      matriz_data(NULL)
      return()
    }

    showNotification(paste("✓ Matriz:", nrow(resultado_matriz$matriz), "movimientos únicos"),
                     type = "message", duration = 3)
    matriz_data(resultado_matriz)
  })

  # ========================================================================
  # OUTPUTS — HEATMAP
  # ========================================================================

  .renderHeatmap <- function(altura = 600) {
    renderPlotly({
      matriz <- matriz_data()
      if (is.null(matriz)) {
        return(suppressWarnings(plotly_empty() %>%
          layout(title = list(text = "Configure los filtros y presione 'Generar'",
                              font = list(size = 18)))))
      }
      tryCatch({
        p <- heatmap_matriz_costes(matriz$matriz)
        if (is.null(p)) stop("heatmap_matriz_costes devolvió NULL")
        ggplotly(p, tooltip = c("x", "y", "fill")) %>%
          layout(margin = list(l = 100, r = 50, t = 50, b = 150))
      }, error = function(e) {
        showNotification(paste("Error heatmap:", e$message), type = "error", duration = 10)
        suppressWarnings(plotly_empty() %>%
          layout(title = list(text = "Error al generar heatmap", font = list(size = 18))))
      })
    })
  }

  output$matriz_diagram          <- .renderHeatmap(600)
  output$matriz_diagram_expanded <- .renderHeatmap(800)

  # ========================================================================
  # TABLAS
  # ========================================================================

  .tabla_larga_df <- function(matriz) {
    if (is.null(matriz)) return(data.frame(Mensaje = "Genera la matriz primero"))
    matriz$matriz %>%
      mutate(Importe = paste0(formatC(importe_total, format = "f", big.mark = " ", digits = 2), "€")) %>%
      select(Origen = origen, Destino = destino, Importe)
  }

  output$matriz_table_larga <- DT::renderDataTable({
    DT::datatable(.tabla_larga_df(matriz_data()),
      options = list(scrollX = TRUE, scrollY = "350px", paging = TRUE, pageLength = 15,
                     dom = "frtip",
                     language = list(url = "//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json")),
      rownames = FALSE, class = "stripe hover compact"
    )
  })

  output$matriz_table_ancha <- DT::renderDataTable({
    matriz <- matriz_data()
    if (is.null(matriz)) {
      return(DT::datatable(data.frame(Mensaje = "Genera la matriz primero"),
                           options = list(dom = "t"), rownames = FALSE))
    }
    tabla <- matriz$matriz_wide %>%
      mutate(across(where(is.numeric), ~formatC(., format = "f", big.mark = " ", digits = 2)))
    DT::datatable(tabla,
      options = list(scrollX = TRUE, scrollY = "350px", paging = TRUE, pageLength = 15,
                     responsive = FALSE, autoWidth = FALSE, dom = "frtip",
                     language = list(url = "//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json")),
      rownames = FALSE, class = "stripe hover compact cell-border"
    )
  })

  # ========================================================================
  # MODALES
  # ========================================================================

  observeEvent(input$expand_matriz, {
    if (is.null(matriz_data())) return()
    showModal(modalDialog(
      title = NULL,
      plotlyOutput("matriz_diagram_expanded", height = "800px", width = "100%"),
      easyClose = TRUE, size = "xl", footer = NULL
    ))
  })

  observeEvent(input$expand_matriz_larga, {
    matriz <- matriz_data()
    if (is.null(matriz)) return()
    df <- .tabla_larga_df(matriz)
    showModal(modalDialog(
      title = "Matriz — Formato Largo (Vista Expandida)",
      div(style = "height:600px;overflow:auto;",
          DT::dataTableOutput("matriz_table_larga_expanded", width = "100%")),
      easyClose = TRUE, size = "xl", footer = NULL
    ))
    output$matriz_table_larga_expanded <- DT::renderDataTable({
      DT::datatable(df,
        options = list(scrollX = TRUE, autoWidth = FALSE, pageLength = 25,
                       language = list(url = "//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json")),
        rownames = FALSE, class = "stripe hover compact"
      )
    })
  })

  observeEvent(input$expand_matriz_ancha, {
    matriz <- matriz_data()
    if (is.null(matriz)) return()
    tabla <- matriz$matriz_wide %>%
      mutate(across(where(is.numeric), ~formatC(., format = "f", big.mark = " ", digits = 2)))
    showModal(modalDialog(
      title = "Matriz — Formato Ancho (Vista Expandida)",
      div(style = "height:600px;overflow:auto;",
          DT::dataTableOutput("matriz_table_ancha_expanded", width = "100%")),
      easyClose = TRUE, size = "xl", footer = NULL
    ))
    output$matriz_table_ancha_expanded <- DT::renderDataTable({
      DT::datatable(tabla,
        options = list(scrollX = TRUE, scrollY = "500px", responsive = FALSE, autoWidth = FALSE,
                       pageLength = 50,
                       language = list(url = "//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json")),
        rownames = FALSE, class = "stripe hover compact cell-border"
      )
    })
  })

  # ========================================================================
  # DESCARGA
  # ========================================================================

  output$download_matriz <- downloadHandler(
    filename = function() paste0("matriz_costes_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".html"),
    content  = function(file) {
      matriz <- matriz_data()
      if (is.null(matriz)) { showNotification("Sin matriz.", type = "warning"); return() }
      tryCatch({
        p <- heatmap_matriz_costes(matriz$matriz)
        if (is.null(p)) stop("Sin datos para el heatmap")
        htmlwidgets::saveWidget(
          ggplotly(p, tooltip = c("x","y","fill")) %>%
            layout(margin = list(l=100, r=50, t=50, b=150)),
          file, selfcontained = TRUE
        )
      }, error = function(e) {
        showNotification(paste("Error descarga:", e$message), type = "error", duration = 10)
      })
    }
  )

  matriz_data
}