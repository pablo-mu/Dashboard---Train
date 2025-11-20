# ============================================================================
# DIAGNOSTICO SERVER LOGIC
# ============================================================================

#' Create Diagnostico server logic
#' This function sets up all the reactive logic for the Diagnostico panel
#' 
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#' @param datos_raw Reactive value containing the raw data
#' @param active_panel Reactive value indicating the active panel
create_diagnostico_server <- function(input, output, session, datos_raw, active_panel) {
  
  # Variable reactiva para almacenar opciones de diagnóstico
  diagnostico_options <- reactiveVal(list(
    detailed = TRUE,
    type = "basic"
  ))
  
  # Limpiar memoria al cambiar de pestaña
  observe({
    if (!is.null(active_panel()) && active_panel() != "diagnostics") {
      gc()
    }
  })
  
  # ========================================================================
  # ACTUALIZAR OPCIONES (opcional, sin depender del botón)
  # ========================================================================
  
  # Las opciones se actualizan automáticamente cuando los datos están disponibles
  # El botón del modal no tiene funcionalidad real, solo cierra el modal
  
  # ========================================================================
  # OUTPUT - VISTA PREVIA DE DATOS
  # ========================================================================
  
  output$diagnostico_data_preview <- DT::renderDataTable({
    datos <- datos_raw()
    
    if (is.null(datos)) {
      return(DT::datatable(
        data.frame(Mensaje = "Carga los datos primero"),
        options = list(dom = 't'),
        rownames = FALSE
      ))
    }
    
    datos_dt <- as.data.table(datos)
    
    DT::datatable(
      head(datos_dt, 100),
      options = list(
        scrollX = TRUE,
        scrollY = "450px",
        paging = TRUE,
        pageLength = 25,
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
  # OUTPUT - INFORMACIÓN DE COLUMNAS
  # ========================================================================
  
  output$diagnostico_columns_info <- renderPrint({
    datos <- datos_raw()
    
    if (is.null(datos)) {
      cat("No hay datos cargados.\n")
      cat("Presiona 'CARGAR DATOS' en el panel lateral.")
      return()
    }
    
    datos_dt <- as.data.table(datos)
    
    cat("╔════════════════════════════════════════════╗\n")
    cat("║    INFORMACIÓN DE COLUMNAS                 ║\n")
    cat("╚════════════════════════════════════════════╝\n\n")
    
    cat("📊 Dimensiones:\n")
    cat("   • Total de columnas:", ncol(datos_dt), "\n")
    cat("   • Total de filas:", formatC(nrow(datos_dt), format = "d", big.mark = " "), "\n\n")
    
    cat("🔑 Columnas clave encontradas:\n")
    columnas_clave <- c(
      "ZCENT_GEST" = "Centro Gestor",
      "ZACT_E" = "CAC Origen",
      "ZCT_SUBCAC_E" = "SUBCAC Origen",
      "ZIMPORT" = "Importe",
      "ZMES" = "Mes",
      "ZANYO" = "Año"
    )
    
    for (col in names(columnas_clave)) {
      if (col %in% names(datos_dt)) {
        n_unique <- length(unique(datos_dt[[col]]))
        n_na <- sum(is.na(datos_dt[[col]]))
        cat(sprintf("   • %-15s: %8s valores únicos", 
                    columnas_clave[col], 
                    formatC(n_unique, format = "d", big.mark = " ")))
        if (n_na > 0) {
          cat(sprintf(" (%d NAs)", n_na))
        }
        cat("\n")
      }
    }
    
    # Siempre mostrar columnas de destino (análisis avanzado por defecto)
    cat("\n📋 Columnas de destino (Fases):\n")
    columnas_destino <- c(
      "ZACT_RE12" = "Fase 1 - Receptor 1-2",
      "ZACT_RE34" = "Fase 1 - Receptor 3-4",
      "ZACT_F1SD" = "Fase 1 - Sin Destino",
      "ZACT_2RE12" = "Fase 2 - Receptor 1-2",
      "ZACT_2RE34" = "Fase 2 - Receptor 3-4",
      "ZACT_F2SD" = "Fase 2 - Sin Destino",
      "ZACT_3RE12" = "Fase 3 - Receptor 1-2",
      "ZACT_3RE34" = "Fase 3 - Receptor 3-4",
      "ZACT_F3SD" = "Fase 3 - Sin Destino"
    )
    
    for (col in names(columnas_destino)) {
      if (col %in% names(datos_dt)) {
        n_filled <- sum(datos_dt[[col]] != "" & !is.na(datos_dt[[col]]))
        pct_filled <- (n_filled / nrow(datos_dt)) * 100
        cat(sprintf("   • %-22s: %6s registros (%.1f%%)\n", 
                    columnas_destino[col], 
                    formatC(n_filled, format = "d", big.mark = " "),
                    pct_filled))
      }
    }
  })
  
  # ========================================================================
  # OUTPUT - ANÁLISIS DE DATOS
  # ========================================================================
  
  output$diagnostico_data_issues <- renderPrint({
    datos <- datos_raw()
    
    if (is.null(datos)) {
      cat("No hay datos cargados.\n")
      cat("Presiona 'CARGAR DATOS' en el panel lateral.")
      return()
    }
    
    datos_dt <- as.data.table(datos)
    
    cat("╔════════════════════════════════════════════╗\n")
    cat("║    ANÁLISIS DE DATOS                       ║\n")
    cat("╚════════════════════════════════════════════╝\n\n")
    
    # Análisis de importes
    if ("ZIMPORT" %in% names(datos_dt)) {
      cat("💰 Análisis de Importes:\n")
      cat(sprintf("   • Importe total:    %s€\n", 
                  formatC(sum(datos_dt$ZIMPORT, na.rm = TRUE), 
                         format = "f", big.mark = " ", digits = 2)))
      cat(sprintf("   • Importe medio:    %s€\n", 
                  formatC(mean(datos_dt$ZIMPORT, na.rm = TRUE), 
                         format = "f", big.mark = " ", digits = 2)))
      cat(sprintf("   • Importe máximo:   %s€\n", 
                  formatC(max(datos_dt$ZIMPORT, na.rm = TRUE), 
                         format = "f", big.mark = " ", digits = 2)))
      cat(sprintf("   • Importe mínimo:   %s€\n\n", 
                  formatC(min(datos_dt$ZIMPORT, na.rm = TRUE), 
                         format = "f", big.mark = " ", digits = 2)))
    }
    
    # Valores únicos por dimensión
    cat("📈 Valores únicos por dimensión:\n")
    if ("ZCENT_GEST" %in% names(datos_dt)) {
      cat(sprintf("   • Centros Gestores: %s\n", 
                  formatC(length(unique(datos_dt$ZCENT_GEST)), 
                         format = "d", big.mark = " ")))
    }
    if ("ZACT_E" %in% names(datos_dt)) {
      cacs_unicos <- length(unique(datos_dt$ZACT_E[datos_dt$ZACT_E != "" & !is.na(datos_dt$ZACT_E)]))
      cat(sprintf("   • CAC:              %s\n", 
                  formatC(cacs_unicos, format = "d", big.mark = " ")))
    }
    if ("ZCT_SUBCAC_E" %in% names(datos_dt)) {
      subcacs_unicos <- length(unique(datos_dt$ZCT_SUBCAC_E[datos_dt$ZCT_SUBCAC_E != "" & !is.na(datos_dt$ZCT_SUBCAC_E)]))
      cat(sprintf("   • SubCAC:           %s\n", 
                  formatC(subcacs_unicos, format = "d", big.mark = " ")))
    }
    
    # Análisis temporal
    if ("ZMES" %in% names(datos_dt) && "ZANYO" %in% names(datos_dt)) {
      cat("\n📅 Cobertura temporal:\n")
      meses_unicos <- sort(unique(datos_dt$ZMES[!is.na(datos_dt$ZMES)]))
      anyos_unicos <- sort(unique(datos_dt$ZANYO[!is.na(datos_dt$ZANYO)]))
      cat(sprintf("   • Meses:  %s\n", paste(meses_unicos, collapse = ", ")))
      cat(sprintf("   • Años:   %s\n", paste(anyos_unicos, collapse = ", ")))
    }
    
    
    # Calidad de datos
    cat("\n✅ Calidad de datos:\n")
    n_complete <- sum(complete.cases(datos_dt))
    pct_complete <- (n_complete / nrow(datos_dt)) * 100
    cat(sprintf("   • Registros completos: %s (%.1f%%)\n", 
                formatC(n_complete, format = "d", big.mark = " "),
                pct_complete))
    
    if ("ZIMPORT" %in% names(datos_dt)) {
      n_zero <- sum(datos_dt$ZIMPORT == 0, na.rm = TRUE)
      pct_zero <- (n_zero / nrow(datos_dt)) * 100
      cat(sprintf("   • Importes en cero:    %s (%.1f%%)\n", 
                  formatC(n_zero, format = "d", big.mark = " "),
                  pct_zero))
    }
  })
  
  # ========================================================================
  # OBSERVADOR PARA EXPANDIR VISTA PREVIA
  # ========================================================================
  
  observeEvent(input$expand_diagnostico_preview, {
    datos <- datos_raw()
    if (is.null(datos)) return()
    
    datos_dt <- as.data.table(datos)
    
    showModal(modalDialog(
      title = "Vista Previa de Datos (Vista Expandida)",
      div(
        style = "height: 600px; width: 100%; overflow: auto;",
        DT::dataTableOutput("diagnostico_preview_expanded", width = "100%")
      ),
      easyClose = TRUE,
      size = "xl",
      footer = NULL
    ))
    
    output$diagnostico_preview_expanded <- DT::renderDataTable({
      DT::datatable(
        head(datos_dt, 200),
        options = list(
          scrollX = TRUE,
          scrollY = "500px",
          paging = TRUE,
          pageLength = 50,
          responsive = TRUE,
          autoWidth = FALSE,
          dom = 'frtip',
          language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')
        ),
        rownames = FALSE,
        class = "stripe hover compact"
      )
    })
  })
  
  return(diagnostico_options)
}
