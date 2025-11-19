# R/matriz/matriz.R - Funciones auxiliares para matriz de costes

# Cargar funciones de negocio
source("modules/lib_reparto.R")

# ============================================================================
# CÁLCULO DE MATRIZ DE COSTES
# ============================================================================

#' Calcular matriz de costes origen-destino
#'
#' @param datos data.frame con los datos de reparto
#' @param centro_gestor Centro gestor a filtrar
#' @param nivel_agregacion Nivel de agregación ("cac", "cac2", "cac1", "subcac")
#' @param cac_subcac_origen Vector de orígenes a filtrar (NULL = todos)
#' @param cac_subcac_destino Vector de destinos a filtrar (NULL = todos)
#' @param mes Vector de meses a filtrar (NULL = todos)
#' @param anyo Vector de años a filtrar (NULL = todos)
#' @param fases_incluir Vector de fases a incluir (NULL = todas)
#' @param excluir_estaticos Excluir movimientos estáticos (costes directos)
#' @param verbose Mostrar mensajes de progreso
#'
#' @return Lista con 'matriz' (formato largo) y 'matriz_wide' (formato pivotado)
calcular_matriz_costes <- function(
  datos,
  centro_gestor = NULL,
  nivel_agregacion = "cac",
  cac_subcac_origen = NULL,
  cac_subcac_destino = NULL,
  mes = NULL,
  anyo = NULL,
  fases_incluir = NULL,
  excluir_estaticos = TRUE,
  verbose = TRUE
){
  # Validaciones de argumentos
  if (length(nivel_agregacion) != 1) {
    stop("nivel_agregacion debe ser de longitud 1: 'cac', 'cac2', 'cac1' o 'subcac'")
  }
  if (!nivel_agregacion %in% c("cac", "cac2", "cac1", "subcac")) {
    stop("nivel_agregacion debe ser 'cac', 'cac2', 'cac1' o 'subcac'")
  }
  if (length(verbose) != 1 || !is.logical(verbose)) {
    stop("verbose debe ser un único valor lógico (TRUE/FALSE)")
  }

  if (verbose){
    message("=== CALCULANDO MATRIZ DE COSTES ===\n")
  }

  # 1. Filtrar datos solo por centro_gestor, mes y anyo
  # NO filtrar por cac_subcac_origen aquí, lo haremos al final sobre los movimientos
  datos_filtrados <- filtrar_datos_reparto(
    datos,
    centro_gestor = centro_gestor,
    cacs_origen = NULL,  # No filtrar por origen aquí
    subcacs_origen = NULL,  # No filtrar por origen aquí
    mes = mes,
    anyo = anyo,
    nivel_agregacion = nivel_agregacion
  )

  if (is.null(datos_filtrados) || nrow(datos_filtrados) == 0) {
    warning("No hay datos después de aplicar filtros")
    return(NULL)
  }

  # Convertir a data.table para mejor rendimiento
  if (!is.data.table(datos_filtrados)) {
    datos_dt <- as.data.table(datos_filtrados)
  } else {
    datos_dt <- copy(datos_filtrados)
  }

  # Filtrar registros sin importe
  datos_dt <- datos_dt[!is.na(ZIMPORT) & ZIMPORT != 0]

  if (nrow(datos_dt) == 0) {
    warning("No hay registros válidos después de filtrar")
    return(NULL)
  }

  if (verbose) {
    message(sprintf("Procesando %s registros...", 
                    formatC(nrow(datos_dt), format = "d", big.mark = " ")))
  }

  # 2. Extraer movimientos usando la función de lib_reparto.R
  matriz_movimientos_completa <- extraer_movimientos(
    datos = datos_dt,
    nivel_agregacion = nivel_agregacion,
    fases_incluir = fases_incluir,
    incluir_tipo_param = FALSE,
    verbose = verbose,
    debug = FALSE
  )

  if (is.null(matriz_movimientos_completa) || nrow(matriz_movimientos_completa) == 0) {
    warning("No se pudieron extraer movimientos")
    return(NULL)
  }

  # 3. Convertir matriz de movimientos a formato largo (origen-destino-importe-fase)
  fases_a_procesar <- if (is.null(fases_incluir)) 1:3 else sort(unique(fases_incluir))
  
  lista_movimientos <- list()
  
  for (fase_num in fases_a_procesar) {
    fase_col <- paste0("fase_", fase_num)
    
    if (!fase_col %in% names(matriz_movimientos_completa)) {
      next
    }
    
    # Obtener origen de la fase anterior
    if (fase_num == 1) {
      origen_col <- "fase_0"
    } else {
      origen_col <- paste0("fase_", fase_num - 1)
    }
    
    # Crear data.table con movimientos de esta fase
    movimientos_fase <- data.table(
      origen = matriz_movimientos_completa[[origen_col]],
      destino = matriz_movimientos_completa[[fase_col]],
      importe = matriz_movimientos_completa$importe,
      fase = fase_num
    )
    
    # Filtrar solo movimientos reales (donde hubo cambio)
    movimientos_fase <- movimientos_fase[origen != destino]
    
    if (nrow(movimientos_fase) > 0) {
      lista_movimientos[[length(lista_movimientos) + 1]] <- movimientos_fase
    }
  }

  # Si excluir_estaticos = FALSE, agregar costes directos en la última fase
  if (!excluir_estaticos && length(fases_a_procesar) > 0) {
    ultima_fase <- max(fases_a_procesar)
    ultima_fase_col <- paste0("fase_", ultima_fase)
    
    # Costes directos: registros donde fase_final == fase_0
    costes_directos <- data.table(
      origen = matriz_movimientos_completa$fase_0,
      destino = matriz_movimientos_completa[[ultima_fase_col]],
      importe = matriz_movimientos_completa$importe,
      fase = ultima_fase
    )
    
    # Filtrar solo donde origen == destino (no hubo movimiento)
    costes_directos <- costes_directos[origen == destino]
    
    if (nrow(costes_directos) > 0) {
      if (verbose) {
        message(sprintf("  Incluyendo %s costes directos (estáticos) en fase %d", 
                        formatC(nrow(costes_directos), format = "d", big.mark = " "),
                        ultima_fase))
      }
      lista_movimientos[[length(lista_movimientos) + 1]] <- costes_directos
    }
  }

  # 4. Combinar todos los movimientos
  if (length(lista_movimientos) == 0) {
    warning("No se encontraron movimientos para las fases seleccionadas")
    return(NULL)
  }

  matriz_movimientos <- rbindlist(lista_movimientos)

  if (nrow(matriz_movimientos) == 0) {
    warning("No hay movimientos entre CACs diferentes")
    return(NULL)
  }

  # 5. HACER MATRIZ CUADRADA si es nivel CAC (cualquier nivel) y NO hay filtros de origen/destino
  # Esto se hace ANTES de los filtros para tener la matriz completa
  if (nivel_agregacion %in% c("cac", "cac2", "cac1") && 
      is.null(cac_subcac_origen) && 
      is.null(cac_subcac_destino)) {
    
    # Obtener todos los CACs únicos (tanto origen como destino)
    cacs_unicos <- unique(c(matriz_movimientos$origen, matriz_movimientos$destino))
    
    if (verbose) {
      message(sprintf("Haciendo matriz cuadrada: %d CACs únicos", length(cacs_unicos)))
    }
    
    # Crear todas las combinaciones posibles origen-destino con importe 0
    combinaciones_completas <- CJ(
      origen = cacs_unicos,
      destino = cacs_unicos,
      sorted = FALSE
    )
    combinaciones_completas[, importe := 0]
    combinaciones_completas[, fase := NA_integer_]
    
    # Hacer un merge para mantener los importes reales donde existen
    # y añadir las combinaciones faltantes con importe 0
    matriz_movimientos <- merge(
      combinaciones_completas,
      matriz_movimientos,
      by = c("origen", "destino"),
      all.x = TRUE,
      suffixes = c("_default", "_real")
    )
    
    # Usar el importe real si existe, sino el default (0)
    matriz_movimientos[, importe := ifelse(!is.na(importe_real), importe_real, importe_default)]
    matriz_movimientos[, fase := ifelse(!is.na(fase_real), fase_real, fase_default)]
    
    # Limpiar columnas auxiliares
    matriz_movimientos[, c("importe_default", "importe_real", "fase_default", "fase_real") := NULL]
    
    if (verbose) {
      n_movimientos_reales <- matriz_movimientos[importe > 0, .N]
      n_movimientos_cero <- matriz_movimientos[importe == 0, .N]
      message(sprintf("Matriz cuadrada: %d movimientos reales + %d combinaciones con importe 0", 
                      n_movimientos_reales, n_movimientos_cero))
    }
  }

  # 6. AHORA sí filtrar por cac_subcac_origen si se especifica
  # Filtrar solo los movimientos que ORIGINAN desde los CACs/SUBCACs seleccionados
  if (!is.null(cac_subcac_origen) && length(cac_subcac_origen) > 0) {
    matriz_movimientos <- matriz_movimientos[origen %in% cac_subcac_origen]
    
    if (verbose) {
      message(sprintf("Filtrando movimientos por origen: %s", paste(cac_subcac_origen, collapse = ", ")))
      message(sprintf("Movimientos después del filtro: %s", formatC(nrow(matriz_movimientos), format = "d", big.mark = " ")))
    }
    
    if (nrow(matriz_movimientos) == 0) {
      warning("No hay movimientos desde los orígenes seleccionados")
      return(NULL)
    }
  }

  # 7. Filtrar por cac_subcac_destino si se especifica
  # Filtrar solo los movimientos que VAN HACIA los CACs/SUBCACs seleccionados
  if (!is.null(cac_subcac_destino) && length(cac_subcac_destino) > 0) {
    matriz_movimientos <- matriz_movimientos[destino %in% cac_subcac_destino]
    
    if (verbose) {
      message(sprintf("Filtrando movimientos por destino: %s", paste(cac_subcac_destino, collapse = ", ")))
      message(sprintf("Movimientos después del filtro: %s", formatC(nrow(matriz_movimientos), format = "d", big.mark = " ")))
    }
    
    if (nrow(matriz_movimientos) == 0) {
      warning("No hay movimientos hacia los destinos seleccionados")
      return(NULL)
    }
  }

  # 8. Agregar movimientos por origen-destino
  matriz_agregada <- matriz_movimientos %>%
    dplyr::group_by(origen, destino) %>%
    dplyr::summarise(
      importe_total = sum(importe, na.rm = TRUE),
      .groups = "drop"
    )
  
  # 9. Crear matriz en formato ancho (para heatmap/visualización)
  matriz_wide <- matriz_agregada %>%
    dplyr::select(origen, destino, importe_total) %>%
    tidyr::pivot_wider(
      names_from = destino,
      values_from = importe_total,
      values_fill = 0
    )

  # 10. Calcular resumen estadístico
  resumen <- list(
    n_origenes = dplyr::n_distinct(matriz_agregada$origen),
    n_destinos = dplyr::n_distinct(matriz_agregada$destino),
    n_movimientos_unicos = nrow(matriz_agregada),
    n_movimientos_totales = nrow(matriz_movimientos),
    importe_total = sum(matriz_agregada$importe_total, na.rm = TRUE),
    importe_promedio = mean(matriz_agregada$importe_total, na.rm = TRUE),
    movimientos_por_fase = matriz_movimientos %>%
      dplyr::count(fase) %>%
      dplyr::arrange(fase),
    top_origen = matriz_agregada %>%
      dplyr::group_by(origen) %>%
      dplyr::summarise(total = sum(importe_total), .groups = "drop") %>%
      dplyr::arrange(dplyr::desc(total)) %>%
      dplyr::slice_head(n = 5),
    top_destino = matriz_agregada %>%
      dplyr::group_by(destino) %>%
      dplyr::summarise(total = sum(importe_total), .groups = "drop") %>%
      dplyr::arrange(dplyr::desc(total)) %>%
      dplyr::slice_head(n = 5)
  )

  if (verbose) {
    message("\n=== RESUMEN DE MATRIZ DE COSTES ===")
    message(sprintf("Orígenes únicos: %d", resumen$n_origenes))
    message(sprintf("Destinos únicos: %d", resumen$n_destinos))
    message(sprintf("Movimientos únicos (origen-destino): %d", resumen$n_movimientos_unicos))
    message(sprintf("Movimientos totales (todas las fases): %d", resumen$n_movimientos_totales))
    message(sprintf("Importe total: %s", formatear_importe(resumen$importe_total)))
    message(sprintf("Importe promedio por movimiento: %s", formatear_importe(resumen$importe_promedio)))

    message("\nMovimientos por fase:")
    print(resumen$movimientos_por_fase)

    message("\nTop 5 CACs origen (mayor importe saliente):")
    print(resumen$top_origen)

    message("\nTop 5 CACs destino (mayor importe entrante):")
    print(resumen$top_destino)
  }

  # Para compatibilidad: añadir columna 'importe' igual a 'importe_total' (como en lib_reparto.R)
  matriz_agregada <- matriz_agregada %>% mutate(importe = importe_total)

  return(list(
    matriz = matriz_agregada,
    matriz_wide = matriz_wide,
    movimientos_detallados = matriz_movimientos,
    resumen = resumen
  ))
}






# ============================================================================
# VISUALIZACIÓN - HEATMAP
# ============================================================================


#' Generar heatmap de matriz de costes (versión ggplot2)
#' El eje X son los CACs destino, el eje Y los CACs origen
#' El color indica el importe, de frío (bajo) a caliente (alto)
#' Los valores 0 se muestran en blanco
#' @param matriz_agregada data.frame con columnas: origen, destino, importe_total
#' @return Objeto ggplot2
heatmap_matriz_costes <- function(matriz_agregada){
  if (is.null(matriz_agregada) || nrow(matriz_agregada) == 0) {
    warning("No hay datos para generar heatmap")
    return(NULL)
  }
  
  # Asegurar que origen y destino sean caracteres primero
  matriz_agregada <- matriz_agregada %>%
    mutate(
      origen = as.character(origen),
      destino = as.character(destino)
    )
  
  # Ordenar niveles: numéricos si es posible, sino alfabético
  tryCatch({
    destinos_sorted <- sort(unique(as.numeric(matriz_agregada$destino)))
    origenes_sorted <- sort(unique(as.numeric(matriz_agregada$origen)), decreasing = FALSE)
    matriz_agregada <- matriz_agregada %>%
      mutate(
        destino = factor(destino, levels = as.character(destinos_sorted)),
        origen = factor(origen, levels = as.character(origenes_sorted), ordered = TRUE)
      )
  }, warning = function(w) {
    matriz_agregada <- matriz_agregada %>%
      mutate(
        destino = factor(destino, levels = sort(unique(destino))),
        origen = factor(origen, levels = sort(unique(origen), decreasing = FALSE), ordered = TRUE)
      )
  })
  
  # En modular: la columna se llama 'importe_total', no 'importe'
  matriz_agregada <- matriz_agregada %>%
    mutate(importe_plot = ifelse(importe_total == 0, NA, importe_total))
  
  heatmap_plot <- ggplot(matriz_agregada, aes(x = destino, y = origen, fill = importe_plot)) +
    geom_tile(color = "white", linewidth = 0.5) +
    scale_fill_gradient(low = "lightblue", high = "darkred", 
                        name = "Importe (€)", 
                        labels = scales::comma,
                        na.value = "white") +
    scale_y_discrete(limits = rev) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
      axis.text.y = element_text(size = 10),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    ) +
    labs(title = "Matriz de Costes entre CACs",
         x = "CAC Destino",
         y = "CAC Origen")
  
  return(heatmap_plot)
}