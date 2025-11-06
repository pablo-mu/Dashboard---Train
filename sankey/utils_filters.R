
# ============================================================================
# FUNCIONES DE FILTROS REUTILIZABLES
# ============================================================================

#' Crear filtro de Centro Gestor
#' @param datos_raw Reactive con los datos
#' @param input_id ID del input
#' @param label Etiqueta del filtro
#' @return UI element
crear_filtro_centro_gestor <- function(datos_raw, inputId, label = "Centro Gestor*:") {
  shiny::renderUI({
    datos <- datos_raw()
    if (is.null(datos)) return(NULL)
    centros <- sort(unique(datos$ZCENT_GEST))
    centros <- centros[!is.na(centros)]
    selectInput(inputId, label, choices = c("Seleccione..." = "", centros), selected = "")
  })
}

crear_filtro_mes <- function(datos_raw, inputId) {
  shiny::renderUI({
    datos <- datos_raw()
    if (is.null(datos)) return(NULL)
    meses <- sort(unique(datos$ZMES))
    meses <- meses[!is.na(meses)]
    selectInput(inputId, "Mes:", choices = c("Todos" = "", as.character(meses)), selected = "", multiple = TRUE)
  })
}

crear_filtro_anyo <- function(datos_raw, inputId) {
  shiny::renderUI({
    datos <- datos_raw()
    if (is.null(datos)) return(NULL)
    anyos <- sort(unique(datos$ZANYO))
    anyos <- anyos[!is.na(anyos)]
    selectInput(inputId, "Año:", choices = c("Todos" = "", as.character(anyos)), selected = "", multiple = TRUE)
  })
}

crear_filtro_origen <- function(datos_raw, nivel_agregacion, centro_gestor, inputId) {
  function() {
    datos <- datos_raw()
    if (is.null(datos)) return(NULL)
    if (!is.null(centro_gestor) && centro_gestor != "") {
      datos <- datos %>% filter(ZCENT_GEST == centro_gestor)
    }
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
    shiny::selectInput(inputId, label, choices = c("Todos" = "", origenes), selected = "", multiple = TRUE)
  }
}

crear_filtro_destino <- function(datos_raw, nivel_agregacion, centro_gestor, inputId) {
  function() {
    datos <- datos_raw()
    if (is.null(datos)) return(NULL)
    if (!is.null(centro_gestor) && centro_gestor != "") {
      datos <- datos %>% filter(ZCENT_GEST == centro_gestor)
    }
    if (nivel_agregacion == "subcac") {
      destinos <- sort(unique(datos$ZCT_SUBCAC_E))
      label <- "Subcentro (Destino):"
    } else if (nivel_agregacion == "cac2") {
      destinos <- unique(substr(datos$ZACT_E, 1, 2))
      destinos <- sort(destinos)
      label <- "CAC 2 dígitos (Destino):"
    } else if (nivel_agregacion == "cac1") {
      destinos <- unique(substr(datos$ZACT_E, 1, 1))
      destinos <- sort(destinos)
      label <- "CAC 1 dígito (Destino):"
    } else {
      destinos <- sort(unique(datos$ZACT_E))
      label <- "CAC (Destino):"
    }
    destinos <- destinos[!is.na(destinos) & destinos != ""]
    shiny::selectInput(inputId, label, choices = c("Todos" = "", destinos), selected = "", multiple = TRUE)
  }
}
# ============================================================================
# FUNCIONES AUXILIARES INTERNAS PARA FILTROS
# ============================================================================

#' Obtener choices para filtro de origen
#' @param datos_dt Data.table con los datos
#' @param nivel Nivel de agregación
#' @return List con choices y label
obtener_choices_origen <- function(datos_dt, nivel) {
  if (nivel == "subcac") {
    choices <- sort(unique(datos_dt$ZCT_SUBCAC_E[datos_dt$ZCT_SUBCAC_E != ""]))
    label <- "Subcentro de Actividad (Origen):"
  } else if (nivel == "cac2") {
    choices <- sort(unique(substr(datos_dt$ZACT_E[datos_dt$ZACT_E != ""], 1, 2)))
    label <- "CAC (2 dígitos - Origen):"
  } else if (nivel == "cac1") {
    choices <- sort(unique(substr(datos_dt$ZACT_E[datos_dt$ZACT_E != ""], 1, 1)))
    label <- "CAC (1 dígito - Origen):"
  } else {
    choices <- sort(unique(datos_dt$ZACT_E[datos_dt$ZACT_E != ""]))
    label <- "Centro de Actividad (Origen):"
  }
  
  list(choices = choices, label = label)
}

#' Obtener choices para filtro de destino
#' @param datos_dt Data.table con los datos
#' @param nivel Nivel de agregación
#' @return List con choices y label
obtener_choices_destino <- function(datos_dt, nivel) {
  # Columnas de destino por fase
  cols_destino <- c(
    "ZACT_RE12", "ZACT_RE34", "ZACT_F1SD",
    "ZACT_2RE12", "ZACT_2RE34", "ZACT_F2SD",
    "ZACT_3RE12", "ZACT_3RE34", "ZACT_F3SD"
  )
  
  if (nivel == "subcac") {
    destinos <- unique(unlist(lapply(cols_destino, function(col) {
      if (col %in% names(datos_dt)) {
        datos_dt[[col]][datos_dt[[col]] != ""]
      }
    })))
    label <- "Subcentro (Destino):"
  } else if (nivel == "cac2") {
    destinos_raw <- unique(c(
      datos_dt$ZACT_E[datos_dt$ZACT_E != ""],
      unlist(lapply(cols_destino, function(col) {
        if (col %in% names(datos_dt)) datos_dt[[col]][datos_dt[[col]] != ""]
      }))
    ))
    destinos <- unique(substr(destinos_raw[!is.na(destinos_raw) & destinos_raw != ""], 1, 2))
    label <- "CAC 2 dígitos (Destino):"
  } else if (nivel == "cac1") {
    destinos_raw <- unique(c(
      datos_dt$ZACT_E[datos_dt$ZACT_E != ""],
      unlist(lapply(cols_destino, function(col) {
        if (col %in% names(datos_dt)) datos_dt[[col]][datos_dt[[col]] != ""]
      }))
    ))
    destinos <- unique(substr(destinos_raw[!is.na(destinos_raw) & destinos_raw != ""], 1, 1))
    label <- "CAC 1 dígito (Destino):"
  } else {
    destinos <- unique(c(
      datos_dt$ZACT_E[datos_dt$ZACT_E != ""],
      unlist(lapply(cols_destino, function(col) {
        if (col %in% names(datos_dt)) datos_dt[[col]][datos_dt[[col]] != ""]
      }))
    ))
    label <- "CAC (Destino):"
  }
  
  choices <- sort(destinos[!is.na(destinos) & destinos != ""])
  list(choices = choices, label = label)
}
