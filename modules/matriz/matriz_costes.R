# R/matriz/matriz_costes.R
# lib_reparto.R ya está cargado por dynamic_server.R — no re-source aquí.
library(ggplot2)
library(scales)
library(tidyr)

# ============================================================================
# CÁLCULO DE MATRIZ DE COSTES
# ============================================================================

#' Calcular matriz de costes origen-destino
#'
#' @param datos            data.frame con datos de reparto
#' @param centro_gestor    Centro gestor a filtrar
#' @param nivel_agregacion "cac" | "cac2" | "cac1" | "subcac"
#' @param cac_subcac_origen   Vector de orígenes  (NULL = todos)
#' @param cac_subcac_destino  Vector de destinos  (NULL = todos)
#' @param fase1_filter     Filtro por CAC en Fase 1
#' @param fase2_filter     Filtro por CAC en Fase 2
#' @param mes              Vector de meses
#' @param anyo             Vector de años
#' @param fases_incluir    Fases a incluir (NULL = 1:3)
#' @param excluir_estaticos Excluir costes directos (sin movimiento)
#' @param verbose          Mensajes de progreso
#' @return Lista con `matriz`, `matriz_wide`, `movimientos_detallados`, `resumen`
calcular_matriz_costes <- function(
  datos,
  centro_gestor      = NULL,
  nivel_agregacion   = "cac",
  cac_subcac_origen  = NULL,
  cac_subcac_destino = NULL,
  fase1_filter       = NULL,
  fase2_filter       = NULL,
  mes                = NULL,
  anyo               = NULL,
  fases_incluir      = NULL,
  excluir_estaticos  = TRUE,
  verbose            = TRUE
) {
  # Validaciones
  stopifnot(length(nivel_agregacion) == 1,
            nivel_agregacion %in% c("cac","cac2","cac1","subcac"),
            length(verbose) == 1, is.logical(verbose))

  if (verbose) message("=== CALCULANDO MATRIZ DE COSTES ===")

  # 1. Filtrar (sin filtro por origen/destino todavía)
  datos_filtrados <- filtrar_datos_reparto(
    datos, centro_gestor = centro_gestor,
    cacs_origen = NULL, subcacs_origen = NULL,
    mes = mes, anyo = anyo, nivel_agregacion = nivel_agregacion
  )
  if (is.null(datos_filtrados) || nrow(datos_filtrados) == 0) {
    warning("Sin datos tras aplicar filtros."); return(NULL)
  }

  datos_dt <- as_dt(datos_filtrados)
  datos_dt <- datos_dt[!is.na(ZIMPORT) & ZIMPORT != 0]
  if (nrow(datos_dt) == 0) { warning("Sin registros válidos."); return(NULL) }

  if (verbose) message(sprintf("Procesando %s registros...",
                               formatC(nrow(datos_dt), format = "d", big.mark = " ")))

  # 2. Extraer movimientos
  matriz_completa <- extraer_movimientos(
    datos = datos_dt, nivel_agregacion = nivel_agregacion,
    fases_incluir = fases_incluir,
    incluir_tipo_param = FALSE, verbose = verbose, debug = FALSE
  )
  if (is.null(matriz_completa) || nrow(matriz_completa) == 0) {
    warning("Sin movimientos."); return(NULL)
  }

  fases_a_procesar <- if (is.null(fases_incluir)) 1:3 else sort(unique(as.integer(fases_incluir)))

  # Expandir selecciones de fase/origen/destino
  if (!is.null(fase1_filter) && "fase_1" %in% names(matriz_completa))
    fase1_filter <- expandir_seleccion_cac(fase1_filter, unique(matriz_completa$fase_1))
  if (!is.null(fase2_filter) && "fase_2" %in% names(matriz_completa))
    fase2_filter <- expandir_seleccion_cac(fase2_filter, unique(matriz_completa$fase_2))
  if (!is.null(cac_subcac_origen))
    cac_subcac_origen <- expandir_seleccion_cac(cac_subcac_origen, unique(matriz_completa$fase_0))
  if (!is.null(cac_subcac_destino)) {
    posibles <- unlist(lapply(fases_a_procesar, function(f) {
      col <- paste0("fase_", f)
      if (col %in% names(matriz_completa)) unique(matriz_completa[[col]])
    }))
    cac_subcac_destino <- expandir_seleccion_cac(cac_subcac_destino, unique(posibles))
  }

  # Aplicar filtros de fase
  if (!is.null(fase1_filter) && length(fase1_filter) > 0 && "fase_1" %in% names(matriz_completa)) {
    matriz_completa <- matriz_completa[fase_1 %in% fase1_filter]
    if (nrow(matriz_completa) == 0) return(NULL)
  }
  if (!is.null(fase2_filter) && length(fase2_filter) > 0 && "fase_2" %in% names(matriz_completa)) {
    matriz_completa <- matriz_completa[fase_2 %in% fase2_filter]
    if (nrow(matriz_completa) == 0) return(NULL)
  }

  # 3. Construir lista de movimientos (formato largo) — data.table
  lista_mov <- vector("list", length(fases_a_procesar))
  for (i in seq_along(fases_a_procesar)) {
    fase_num  <- fases_a_procesar[i]
    col_dest  <- paste0("fase_", fase_num)
    col_orig  <- paste0("fase_", fase_num - 1)
    if (!col_dest %in% names(matriz_completa)) next

    mov <- data.table(
      origen  = matriz_completa[[col_orig]],
      destino = matriz_completa[[col_dest]],
      importe = matriz_completa$importe,
      fase    = fase_num
    )
    lista_mov[[i]] <- mov[origen != destino]
  }

  # Costes directos (opcionales)
  if (!excluir_estaticos && length(fases_a_procesar) > 0) {
    uf  <- max(fases_a_procesar)
    ufc <- paste0("fase_", uf)
    cd  <- data.table(
      origen  = matriz_completa$fase_0,
      destino = matriz_completa[[ufc]],
      importe = matriz_completa$importe,
      fase    = uf
    )[origen == destino]
    if (nrow(cd) > 0) {
      if (verbose) message(sprintf("  Incluyendo %s costes directos en fase %d",
                                   formatC(nrow(cd), format = "d", big.mark = " "), uf))
      lista_mov <- c(lista_mov, list(cd))
    }
  }

  lista_mov <- Filter(function(x) !is.null(x) && nrow(x) > 0, lista_mov)
  if (length(lista_mov) == 0) { warning("Sin movimientos entre CACs."); return(NULL) }

  matriz_mov <- rbindlist(lista_mov)

  # 4. Matriz cuadrada (solo niveles CAC sin filtros de origen/destino)
  if (nivel_agregacion %in% c("cac","cac2","cac1") &&
      is.null(cac_subcac_origen) && is.null(cac_subcac_destino)) {

    cacs <- unique(c(matriz_mov$origen, matriz_mov$destino))
    if (verbose) message(sprintf("Construyendo matriz cuadrada: %d CACs", length(cacs)))

    # CJ es O(n²) — solo hacerlo si el número de CACs es razonable
    if (length(cacs) <= 500) {
      completa <- CJ(origen = cacs, destino = cacs, sorted = FALSE)[, importe := 0][, fase := NA_integer_]
      matriz_mov <- merge(completa, matriz_mov,
                          by = c("origen","destino"), all.x = TRUE,
                          suffixes = c("_def","_real"))
      matriz_mov[, importe := fifelse(!is.na(importe_real), importe_real, importe_def)]
      matriz_mov[, fase    := fifelse(!is.na(fase_real),    fase_real,    fase_def)]
      matriz_mov[, c("importe_def","importe_real","fase_def","fase_real") := NULL]
    } else if (verbose) {
      message(sprintf("  ℹ Omitida expansión cuadrada (%d CACs > 500 — rendimiento)", length(cacs)))
    }
  }

  # 5. Filtros finales por origen / destino
  if (!is.null(cac_subcac_origen) && length(cac_subcac_origen) > 0) {
    matriz_mov <- matriz_mov[origen %in% cac_subcac_origen]
    if (verbose) message(sprintf("  Filtro origen: %s movimientos",
                                 formatC(nrow(matriz_mov), format = "d", big.mark = " ")))
    if (nrow(matriz_mov) == 0) { warning("Sin movimientos desde los orígenes seleccionados."); return(NULL) }
  }
  if (!is.null(cac_subcac_destino) && length(cac_subcac_destino) > 0) {
    matriz_mov <- matriz_mov[destino %in% cac_subcac_destino]
    if (verbose) message(sprintf("  Filtro destino: %s movimientos",
                                 formatC(nrow(matriz_mov), format = "d", big.mark = " ")))
    if (nrow(matriz_mov) == 0) { warning("Sin movimientos hacia los destinos seleccionados."); return(NULL) }
  }

  # 6. Agregación — data.table para velocidad
  matriz_agr <- matriz_mov[, .(importe_total = sum(importe, na.rm = TRUE)), by = .(origen, destino)]
  matriz_agr[, importe := importe_total]   # alias para compatibilidad

  # 7. Formato ancho (pivot) — tidyr para simplicidad
  matriz_wide <- as.data.frame(matriz_agr) %>%
    select(origen, destino, importe_total) %>%
    tidyr::pivot_wider(names_from = destino, values_from = importe_total, values_fill = 0)

  # 8. Resumen
  resumen <- list(
    n_origenes           = uniqueN(matriz_agr$origen),
    n_destinos           = uniqueN(matriz_agr$destino),
    n_movimientos_unicos = nrow(matriz_agr),
    n_movimientos_totales = nrow(matriz_mov),
    importe_total        = sum(matriz_agr$importe_total, na.rm = TRUE),
    importe_promedio     = mean(matriz_agr$importe_total, na.rm = TRUE),
    movimientos_por_fase = as.data.frame(matriz_mov)[, c("fase","importe")] %>%
      dplyr::count(fase) %>% dplyr::arrange(fase),
    top_origen  = as.data.frame(matriz_agr) %>%
      dplyr::group_by(origen) %>%
      dplyr::summarise(total = sum(importe_total), .groups = "drop") %>%
      dplyr::arrange(dplyr::desc(total)) %>% dplyr::slice_head(n = 5),
    top_destino = as.data.frame(matriz_agr) %>%
      dplyr::group_by(destino) %>%
      dplyr::summarise(total = sum(importe_total), .groups = "drop") %>%
      dplyr::arrange(dplyr::desc(total)) %>% dplyr::slice_head(n = 5)
  )

  if (verbose) {
    message(sprintf("\n=== RESUMEN: %d orígenes × %d destinos | %s movimientos | %s",
                    resumen$n_origenes, resumen$n_destinos,
                    formatC(resumen$n_movimientos_unicos, format = "d", big.mark = " "),
                    formatear_importe(resumen$importe_total)))
  }

  list(
    matriz                = as.data.frame(matriz_agr),
    matriz_wide           = matriz_wide,
    movimientos_detallados = as.data.frame(matriz_mov),
    resumen               = resumen
  )
}

# ============================================================================
# VISUALIZACIÓN — HEATMAP
# ============================================================================

#' Generar heatmap interactivo de la matriz de costes
#' @param matriz_agregada data.frame con columnas: origen, destino, importe_total
#' @return ggplot object (para convertir con ggplotly)
heatmap_matriz_costes <- function(matriz_agregada) {
  if (is.null(matriz_agregada) || nrow(matriz_agregada) == 0) {
    warning("Sin datos para el heatmap."); return(NULL)
  }

  # Convertir a character para seguridad
  df <- matriz_agregada %>%
    mutate(origen  = as.character(origen),
           destino = as.character(destino))

  # Ordenar niveles numéricamente si es posible, alfabéticamente si no
  order_fn <- function(x) {
    nums <- suppressWarnings(as.numeric(x))
    if (all(!is.na(nums))) sort(nums) else sort(x)
  }
  niveles_dest <- as.character(order_fn(unique(df$destino)))
  niveles_orig <- as.character(order_fn(unique(df$origen)))

  df <- df %>%
    mutate(
      destino      = factor(destino, levels = niveles_dest),
      origen       = factor(origen,  levels = niveles_orig),
      importe_plot = ifelse(importe_total == 0, NA, importe_total)
    )

  ggplot(df, aes(x = destino, y = origen, fill = importe_plot)) +
    geom_tile(color = "white", linewidth = 0.5) +
    scale_fill_gradient(low = "lightblue", high = "darkred",
                        name = "Importe (€)", labels = scales::comma,
                        na.value = "white") +
    scale_y_discrete(limits = rev) +
    theme_minimal() +
    theme(
      axis.text.x   = element_text(angle = 45, hjust = 1, size = 10),
      axis.text.y   = element_text(size = 10),
      panel.grid    = element_blank()
    ) +
    labs(title = "Matriz de Costes entre CACs",
         x = "CAC Destino", y = "CAC Origen")
}