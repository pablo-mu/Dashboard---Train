# ============================================================================
# DATA SERVER - CARGA CENTRALIZADA DE DATOS
# ============================================================================

#' Crear servidor centralizado de datos
#' @param input Input de la sesión
#' @param output Output de la sesión
#' @param session Sesión de Shiny
#' @return Lista con reactive de datos y estado de carga
create_data_server <- function(input, output, session) {
  
  # Estado de carga y mensajes de debug
  loading_state <- reactiveVal("loading")
  debug_messages <- reactiveVal("")
  
  # Datos reactivos
  datos_raw <- reactive({
    cat("\n=== INICIANDO CARGA DE DATOS ===\n")
    
    tryCatch({
      loading_state("loading")
      debug_messages("Iniciando carga de datos...")
      cat("Estado: loading\n")
      
      # Verificar que lib_reparto.R está cargado
      if (!exists("preparar_datos_reparto")) {
        error_msg <- "Función preparar_datos_reparto no encontrada. Verificar modules/lib_reparto.R"
        cat("❌", error_msg, "\n")
        stop(error_msg)
      }
      cat("✓ Función preparar_datos_reparto encontrada\n")
      
      debug_messages("Función preparar_datos_reparto encontrada. Cargando datos...")
      
      # Verificar carpetas de datos
      rutas_datos <- c(".data", "./data", "data", "./.data")
      for (ruta in rutas_datos) {
        if (dir.exists(ruta)) {
          archivos <- list.files(ruta, pattern = "\\.csv$", ignore.case = TRUE)
          cat(sprintf("✓ Carpeta '%s' existe con %d archivos CSV\n", ruta, length(archivos)))
        }
      }
      
      cat("Llamando a preparar_datos_reparto()...\n")
      
      # Usar la función de lib_reparto.R
      datos <- preparar_datos_reparto()
      
      cat("Resultado de preparar_datos_reparto():", class(datos), "\n")
      
      if (is.null(datos)) {
        error_msg <- "Error: preparar_datos_reparto() retornó NULL"
        cat("❌", error_msg, "\n")
        debug_messages(error_msg)
        loading_state("error")
        return(NULL)
      }
      
      if (nrow(datos) == 0) {
        error_msg <- "Error: preparar_datos_reparto() retornó datos vacíos"
        cat("❌", error_msg, "\n")
        debug_messages(error_msg)
        loading_state("error")
        return(NULL)
      }
      
      success_msg <- paste("✓ Datos cargados correctamente:", nrow(datos), "registros,", ncol(datos), "columnas")
      cat(success_msg, "\n")
      debug_messages(success_msg)
      loading_state("loaded")
      return(datos)
      
    }, error = function(e) {
      error_msg <- paste("❌ Error cargando datos:", e$message)
      cat(error_msg, "\n")
      cat("Traceback:\n")
      print(e)
      debug_messages(error_msg)
      loading_state("error")
      return(NULL)
    })
  })
  
  # UI de estado de carga para el sidebar
  output$data_loading_status <- renderUI({
    status <- loading_state()
    messages <- debug_messages()
    
    status_ui <- switch(status,
      "loading" = div(
        class = "loading-status",
        icon("spinner", class = "fa-spin"),
        "Cargando datos..."
      ),
      "loaded" = div(
        class = "loaded-status", 
        icon("check", style = "color: green;"),
        "Datos cargados correctamente"
      ),
      "error" = div(
        class = "error-status",
        icon("exclamation-triangle", style = "color: red;"),
        "Error cargando datos"
      )
    )
    
    # Añadir mensajes de debug
    if (messages != "") {
      tagList(
        status_ui,
        div(
          style = "font-size: 10px; margin-top: 5px; color: #666;",
          messages
        )
      )
    } else {
      status_ui
    }
  })
  
  return(list(
    datos_raw = datos_raw,
    loading_state = loading_state
  ))
}