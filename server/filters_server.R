# ============================================================================
# FILTERS SERVER - RENDERIZACIÓN DINÁMICA DE FILTROS
# ============================================================================

#' Crear servidor de filtros dinámicos
#' @param input Input de la sesión
#' @param output Output de la sesión
#' @param session Sesión de Shiny
#' @param datos_raw Reactive con los datos
#' @return Lista con valores de filtros reactivos
create_filters_server <- function(input, output, session, datos_raw) {
  
  # ========================================
  # FILTRO CENTRO GESTOR
  # ========================================
  output$centro_gestor_filter <- renderUI({
    datos <- datos_raw()
    if (is.null(datos)) return(NULL)
    
    centros <- sort(unique(datos$ZCENT_GEST))
    centros <- centros[!is.na(centros) & centros != ""]
    
    selectInput(
      inputId = "centro_gestor",
      label = "Centro Gestor*:",
      choices = c("Seleccione..." = "", centros),
      selected = ""
    )
  })
  
  # ========================================
  # FILTRO ORIGEN (DEPENDE DE NIVEL Y CENTRO)
  # ========================================
  output$origen_filter <- renderUI({
    datos <- datos_raw()
    if (is.null(datos)) return(NULL)
    
    # Filtrar por centro gestor si está seleccionado
    if (!is.null(input$centro_gestor) && input$centro_gestor != "") {
      datos <- datos[datos$ZCENT_GEST == input$centro_gestor, ]
    }
    
    # Por ahora usar nivel por defecto "cac" (se puede hacer dinámico después)
    nivel_agregacion <- "cac"  # input$nivel_agregacion si existe
    
    if (nivel_agregacion == "subcac") {
      origenes <- sort(unique(datos$ZCT_SUBCAC_E))
      label <- "Subcentro de Actividad (Origen):"
    } else if (nivel_agregacion == "cac2") {
      origenes <- unique(substr(datos$ZACT_E, 1, 2))
      origenes <- sort(origenes)
      label <- "CAC (2 dígitos - Origen):"
    } else if (nivel_agregacion == "cac1") {
      origenes <- unique(substr(datos$ZACT_E, 1, 1))
      origenes <- sort(origenes)
      label <- "CAC (1 dígito - Origen):"
    } else {
      origenes <- sort(unique(datos$ZACT_E))
      label <- "Centro de Actividad (Origen):"
    }
    
    origenes <- origenes[!is.na(origenes) & origenes != ""]
    
    selectInput(
      inputId = "origen",
      label = label,
      choices = c("Todos" = "", origenes),
      selected = "",
      multiple = TRUE
    )
  })
  
  # ========================================
  # FILTRO DESTINO (SIMILAR AL ORIGEN)
  # ========================================
  output$destino_filter <- renderUI({
    datos <- datos_raw()
    if (is.null(datos)) return(NULL)
    
    # Filtrar por centro gestor si está seleccionado
    if (!is.null(input$centro_gestor) && input$centro_gestor != "") {
      datos <- datos[datos$ZCENT_GEST == input$centro_gestor, ]
    }
    
    # Por ahora usar nivel por defecto "cac"
    nivel_agregacion <- "cac"
    
    # Columnas de destino por fase
    cols_destino <- c(
      "ZACT_RE12", "ZACT_RE34", "ZACT_F1SD",
      "ZACT_2RE12", "ZACT_2RE34", "ZACT_F2SD",
      "ZACT_3RE12", "ZACT_3RE34", "ZACT_F3SD"
    )
    
    if (nivel_agregacion == "subcac") {
      destinos <- unique(unlist(lapply(cols_destino, function(col) {
        if (col %in% names(datos)) {
          datos[[col]][datos[[col]] != ""]
        }
      })))
      label <- "Subcentro (Destino):"
    } else if (nivel_agregacion == "cac2") {
      destinos_raw <- unique(c(
        datos$ZACT_E[datos$ZACT_E != ""],
        unlist(lapply(cols_destino, function(col) {
          if (col %in% names(datos)) datos[[col]][datos[[col]] != ""]
        }))
      ))
      destinos <- unique(substr(destinos_raw[!is.na(destinos_raw) & destinos_raw != ""], 1, 2))
      label <- "CAC 2 dígitos (Destino):"
    } else if (nivel_agregacion == "cac1") {
      destinos_raw <- unique(c(
        datos$ZACT_E[datos$ZACT_E != ""],
        unlist(lapply(cols_destino, function(col) {
          if (col %in% names(datos)) datos[[col]][datos[[col]] != ""]
        }))
      ))
      destinos <- unique(substr(destinos_raw[!is.na(destinos_raw) & destinos_raw != ""], 1, 1))
      label <- "CAC 1 dígito (Destino):"
    } else {
      destinos <- unique(c(
        datos$ZACT_E[datos$ZACT_E != ""],
        unlist(lapply(cols_destino, function(col) {
          if (col %in% names(datos)) datos[[col]][datos[[col]] != ""]
        }))
      ))
      label <- "CAC (Destino):"
    }
    
    choices <- sort(destinos[!is.na(destinos) & destinos != ""])
    
    selectInput(
      inputId = "destino",
      label = label,
      choices = c("Todos" = "", choices),
      selected = "",
      multiple = TRUE
    )
  })
  
  # ========================================
  # FILTRO MES
  # ========================================
  output$mes_filter <- renderUI({
    datos <- datos_raw()
    if (is.null(datos)) return(NULL)
    
    meses <- sort(unique(datos$ZMES))
    meses <- meses[!is.na(meses)]
    
    selectInput(
      inputId = "mes",
      label = "Mes:",
      choices = c("Todos" = "", as.character(meses)),
      selected = "",
      multiple = TRUE
    )
  })
  
  # ========================================
  # FILTRO AÑO
  # ========================================
  output$anyo_filter <- renderUI({
    datos <- datos_raw()
    if (is.null(datos)) return(NULL)
    
    anyos <- sort(unique(datos$ZANYO))
    anyos <- anyos[!is.na(anyos)]
    
    selectInput(
      inputId = "anyo",
      label = "Año:",
      choices = c("Todos" = "", as.character(anyos)),
      selected = "",
      multiple = TRUE
    )
  })
  
  # ========================================
  # BOTÓN RESET FILTROS
  # ========================================
  observeEvent(input$reset_filters, {
    updateSelectInput(session, "centro_gestor", selected = "")
    updateSelectInput(session, "origen", selected = "")
    updateSelectInput(session, "destino", selected = "")
    updateSelectInput(session, "mes", selected = "")
    updateSelectInput(session, "anyo", selected = "")
  })
  
  # ========================================
  # RETURN VALORES DE FILTROS REACTIVOS
  # ========================================
  return(list(
    centro_gestor = reactive({ input$centro_gestor }),
    origen = reactive({ input$origen }),
    destino = reactive({ input$destino }),
    mes = reactive({ input$mes }),
    anyo = reactive({ input$anyo })
  ))
}