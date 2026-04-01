# Exportamos scripts necesarios

source("modules/lib_reparto.R")


#' Construir enlaces de reparto para Sankey usando la matriz de extraer_movimientos
#'
#' @param datos data.frame con los datos de reparto (tras preparar_datos_reparto)
#' @param centro_gestor Centro gestor a filtrar. NULL = todos
#' @param nivel_agregacion "cac" (3 dígitos), "cac2" (2 dígitos), "cac1" (1 dígito) o "subcac"
#' @param cac_subcac_origen Vector de CACs/SUBCACs origen a incluir. NULL = todos
#' @param cac_subcac_destino Vector de CACs/SUBCACs destino a incluir. NULL = todos
#' @param mes Vector de meses a filtrar (NULL = todos)
#' @param anyo Vector de años a filtrar (NULL = todos)
#' @param fases_incluir Vector con fases a incluir (1, 2, 3). NULL = todas
#' @param excluir_estaticos Si TRUE, excluye registros que no se movieron de fase_0 a la última fase
#' @param verbose Mostrar mensajes de progreso (TRUE/FALSE)
#'
#' @return data.frame con enlaces: source, target, value, origen, fase, tipo_reparto, parametro
#'         o NULL si no hay datos
#'
construir_enlaces_sankey <- function(
  datos,
  centro_gestor = NULL,
  nivel_agregacion = "cac",
  cac_subcac_origen = NULL,
  cac_subcac_destino = NULL,
  fase1_filter = NULL,
  fase2_filter = NULL,
  mes = NULL,
  anyo = NULL,
  fases_incluir = NULL,
  excluir_estaticos = TRUE,
  verbose = TRUE,
  debug = FALSE
)  {
  
  if (is.null(datos)){
    warning("No hay datos para construir enlaces")
    return(NULL)
  }

    # 1) Filtrar datos segun parametros (NO filtrar por origen aquí)
  datos_dt <- filtrar_datos_reparto(
    datos,
    centro_gestor = centro_gestor,
    cacs_origen   = NULL,  # No filtrar por origen aquí
    subcacs_origen= NULL,  # No filtrar por origen aquí
    mes = mes,
    anyo = anyo,
    nivel_agregacion = nivel_agregacion
  )

  if (!is.data.table(datos_dt)) datos_dt <- as.data.table(datos_dt)

  if (nrow(datos_dt) == 0) {
    warning("No hay datos después de aplicar filtros")
    return(NULL)
  }

  # Fases a procesar
  fases_a_procesar <- if (is.null(fases_incluir)) c(1, 2, 3) else sort(unique(fases_incluir))

  # Extraer matriz de movimientos
  matriz <- extraer_movimientos(
    datos = datos_dt,
    nivel_agregacion = nivel_agregacion,
    fases_incluir = fases_a_procesar,
    incluir_tipo_param = TRUE,
    verbose = verbose
  )

  if (is.null(matriz) || nrow(matriz) == 0) {
    warning("No se generó matriz de movimientos")
    return(NULL)
  }
  if (!is.data.table(matriz)) matriz <- as.data.table(matriz)

  # Expandir selecciones de origen si existen grupos
  if (!is.null(cac_subcac_origen) && length(cac_subcac_origen) > 0) {
    cac_subcac_origen <- expandir_seleccion_cac(cac_subcac_origen, unique(matriz$fase_0))
  }

  # Filtrar por origen (fase_0) si se especifica
  if (!is.null(cac_subcac_origen) && length(cac_subcac_origen) > 0) {
    n_antes <- nrow(matriz)
    matriz <- matriz[fase_0 %in% cac_subcac_origen]
    n_despues <- nrow(matriz)
    
    if (verbose && n_antes > n_despues) {
      message(sprintf("  Filtrados %s registros por origen: %s",
                      formatC(n_antes - n_despues, format = "d", big.mark = " "),
                      paste(cac_subcac_origen, collapse = ", ")))
    }
    
    if (nrow(matriz) == 0) {
      warning("No hay registros con los orígenes seleccionados")
      return(NULL)
    }
  }

  # Expandir selecciones de fase si existen grupos (terminan en ..)
  if (!is.null(fase1_filter) && "fase_1" %in% names(matriz)) {
    fase1_filter <- expandir_seleccion_cac(fase1_filter, unique(matriz$fase_1))
  }
  if (!is.null(fase2_filter) && "fase_2" %in% names(matriz)) {
    fase2_filter <- expandir_seleccion_cac(fase2_filter, unique(matriz$fase_2))
  }
  if (!is.null(cac_subcac_destino)) {
    # Recopilar todos los posibles destinos para la expansión
    posibles_destinos <- c()
    for (fase_num in fases_a_procesar) {
      col_destino <- paste0("fase_", fase_num)
      if (col_destino %in% names(matriz)) {
        posibles_destinos <- c(posibles_destinos, unique(matriz[[col_destino]]))
      }
    }
    cac_subcac_destino <- expandir_seleccion_cac(cac_subcac_destino, unique(posibles_destinos))
  }

  # Filtrar por Fase 1
  if (!is.null(fase1_filter) && length(fase1_filter) > 0 && "fase_1" %in% names(matriz)) {
    matriz <- matriz[fase_1 %in% fase1_filter]
    if (nrow(matriz) == 0) return(NULL)
  }

  # Filtrar por Fase 2
  if (!is.null(fase2_filter) && length(fase2_filter) > 0 && "fase_2" %in% names(matriz)) {
    matriz <- matriz[fase_2 %in% fase2_filter]
    if (nrow(matriz) == 0) return(NULL)
  }

  # Filtrar por destino si se especifica
  # Buscar en todas las fases de destino posibles
  if (!is.null(cac_subcac_destino) && length(cac_subcac_destino) > 0) {
    n_antes <- nrow(matriz)
    
    # Determinar qué fases están presentes en los datos
    fases_a_procesar <- if (is.null(fases_incluir)) c(1, 2, 3) else sort(unique(fases_incluir))
    
    # Crear condición OR para todas las fases destino
    condicion_destino <- rep(FALSE, nrow(matriz))
    for (fase_num in fases_a_procesar) {
      col_destino <- paste0("fase_", fase_num)
      if (col_destino %in% names(matriz)) {
        condicion_destino <- condicion_destino | (matriz[[col_destino]] %in% cac_subcac_destino)
      }
    }
    
    matriz <- matriz[condicion_destino]
    n_despues <- nrow(matriz)
    
    if (verbose && n_antes > n_despues) {
      message(sprintf("  Filtrados %s registros por destino: %s",
                      formatC(n_antes - n_despues, format = "d", big.mark = " "),
                      paste(cac_subcac_destino, collapse = ", ")))
    }
    
    if (nrow(matriz) == 0) {
      warning("No hay registros con los destinos seleccionados")
      return(NULL)
    }
  }

  # Filtramos los movimientos estáticos si se solicita
  if (excluir_estaticos) {
    # Determinar la última fase procesada (máxima fase en fases_a_procesar)
    fase_final_num <- max(fases_a_procesar)
    col_fase_final <- paste0("fase_", fase_final_num)

    # Determinar la primera fase procesada (mínima fase en fases_a_procesar - 1)
    fase_inicial_num <- min(fases_a_procesar) - 1
    col_fase_inicial <- paste0("fase_", fase_inicial_num)

    if (col_fase_final %in% names(matriz)) {
      n_antes <- nrow(matriz)
      # Mantener solo aquellos donde fase_0 != fase_final
      matriz <- matriz[get(col_fase_inicial) != get(col_fase_final)]
      n_despues <- nrow(matriz)

      if (verbose && n_antes > n_despues) {
        message(sprintf("  Excluidos %s registros estáticos (sin movimiento de F0 a F%d)",
                        formatC(n_antes - n_despues, format = "d", big.mark = " "),
                        fase_final_num))
      }
      
      if (nrow(matriz) == 0) {
        warning("No hay registros después de excluir movimientos estáticos")
        return(NULL)
      }
    }
  }

  # Construcción vectorizada de enlaces
  tipos_con_sufijo <- c("RE1","RE2","RE3","RE4","SD")

  if (verbose) {
    message(sprintf("Construyendo enlaces para %s registros...",
                    formatC(nrow(matriz), format = "d", big.mark = " ")))
  }

  if (debug) {
    # DEBUG: Inspeccionar matriz recibida
    print("DEBUG: Primeras 20 filas de matriz:")
    print(head(matriz, 20))
    print("DEBUG: Estructura de matriz:")
    str(matriz)
    print("DEBUG: Resumen de tipos_fase:")
    if ("tipo_fase_1" %in% names(matriz)) {
      print(table(matriz$tipo_fase_1, useNA = "ifany"))
    }
    if ("tipo_fase_2" %in% names(matriz)) {
      print(table(matriz$tipo_fase_2, useNA = "ifany"))
    }
    if ("tipo_fase_3" %in% names(matriz)) {
      print(table(matriz$tipo_fase_3, useNA = "ifany"))
    }
  }

  enlaces_por_fase <- list()

    for (fase in fases_a_procesar) {
    col_origen  <- paste0("fase_", fase - 1)
    col_destino <- paste0("fase_", fase)
    
    # Verificar que las columnas existen
    if (!(col_origen %in% names(matriz)) || !(col_destino %in% names(matriz))) {
      next
    }
    
    # Crear un subset de la matriz solo con registros válidos para esta fase
    # CORRECCIÓN: Permitir importes negativos y valores vacíos (para cuadrar con Traza)
    fase_dt <- matriz[!is.na(get(col_origen)) & 
                      !is.na(get(col_destino)) & 
                      !is.na(importe) & importe != 0]
    
    if (nrow(fase_dt) == 0) next
    
    # Reemplazar cadenas vacías con "Sin Dato" para visualización
    fase_dt[get(col_origen) == "", (col_origen) := "Sin Dato"]
    fase_dt[get(col_destino) == "", (col_destino) := "Sin Dato"]
    
    # Obtener columnas de tipo y parámetro
    nombre_tipo   <- paste0("tipo_fase_", fase)
    nombre_param  <- paste0("param_fase_", fase)
    
    # Tipo de la fase anterior (para el sufijo del source)
    # Solo usar si la fase anterior está en las fases procesadas
    fase_anterior <- fase - 1
    nombre_tipo_prev <- if (fase_anterior > 0 && fase_anterior %in% fases_a_procesar) {
      paste0("tipo_fase_", fase_anterior)
    } else {
      NULL
    }
    tiene_tipo_prev <- !is.null(nombre_tipo_prev) && nombre_tipo_prev %in% names(fase_dt)
    
    # CONSTRUIR ETIQUETAS SOURCE (con continuidad del flujo)
    # El source debe coincidir exactamente con el target de la fase anterior
    if (fase == min(fases_a_procesar)) {
      # Para la primera fase procesada: usar el origen de fase anterior
      fase_dt[, source_label := paste0("F", fase - 1, ": ", get(col_origen))]
    } else {
      # Para fases posteriores: el source debe tener el sufijo de cómo llegó en la fase anterior
      fase_dt[, source_label := paste0("F", fase - 1, ": ", get(col_origen))]
      
      # Si hay tipo previo y está en tipos_con_sufijo, añadir sufijo al source
      if (tiene_tipo_prev) {
        fase_dt[get(nombre_tipo_prev) %in% tipos_con_sufijo & !is.na(get(nombre_tipo_prev)),
                source_label := paste0("F", fase - 1, "-", get(nombre_tipo_prev), ": ", get(col_origen))]
      }
    }
    
    # CONSTRUIR ETIQUETAS TARGET (con el tipo de movimiento actual)
    # Comparar si origen == destino para determinar si hay movimiento
    fase_dt[, hay_movimiento := get(col_origen) != get(col_destino)]
    
    # Por defecto: target sin sufijo (para SIN_MOVIMIENTO o cuando origen == destino)
    fase_dt[, target_label := paste0("F", fase, ": ", get(col_destino))]
    
    # Si hay movimiento Y el tipo está en tipos_con_sufijo, añadir sufijo al target
    if (nombre_tipo %in% names(fase_dt)) {
      fase_dt[hay_movimiento == TRUE & 
              get(nombre_tipo) %in% tipos_con_sufijo & 
              !is.na(get(nombre_tipo)),
              target_label := paste0("F", fase, "-", get(nombre_tipo), ": ", get(col_destino))]
    }
    
    # Limpiar columna auxiliar
    fase_dt[, hay_movimiento := NULL]
    
    # Crear data.frame de enlaces para esta fase
    enlaces_fase <- data.frame(
      source = fase_dt$source_label,
      target = fase_dt$target_label,
      value  = fase_dt$importe,
      origen = fase_dt$fase_0,
      fase   = fase,
      tipo_reparto = if (nombre_tipo %in% names(fase_dt)) {
        ifelse(is.na(fase_dt[[nombre_tipo]]), NA_character_, as.character(fase_dt[[nombre_tipo]]))
      } else {
        NA_character_
      },
      parametro = if (nombre_param %in% names(fase_dt)) {
        ifelse(is.na(fase_dt[[nombre_param]]), NA_character_, as.character(fase_dt[[nombre_param]]))
      } else {
        NA_character_
      },
      stringsAsFactors = FALSE
    )
    
    enlaces_por_fase[[length(enlaces_por_fase) + 1]] <- enlaces_fase
  }

  if (length(enlaces_por_fase) == 0) {
    warning("No se encontraron flujos válidos")
    return(NULL)
  }

  enlaces_df <- dplyr::bind_rows(enlaces_por_fase)

  if (verbose) {
    message(sprintf("  Enlaces construidos: %s",
                    formatC(nrow(enlaces_df), format = "d", big.mark = " ")))

  }

  # Retornar tanto los enlaces como la matriz original
  return(list(
    enlaces = enlaces_df,
    matriz = matriz
  ))
}

#' Agregar enlaces duplicados
#' 
#' Suma los valores de enlaces que tienen el mismo origen y destino.
#' 
#' @param enlaces data.frame con enlaces individuales
#' 
#' @return data.frame con enlaces agregados
#' 
#' @export
agregar_enlaces <- function(enlaces) {
  
  if (is.null(enlaces) || nrow(enlaces) == 0) {
    return(NULL)
  }
  
  n_original <- nrow(enlaces)
  
  enlaces_agregados <- enlaces %>%
    group_by(source, target) %>%
    summarise(
      value = sum(value, na.rm = TRUE),
      n_registros = n(),
      .groups = "drop"
    ) %>%
    filter(value > 0)
  
  n_agregado <- nrow(enlaces_agregados)
  
  message(sprintf("✓ Agregación: %s → %s enlaces únicos", 
                  formatC(n_original, format = "d", big.mark = " "),
                  formatC(n_agregado, format = "d", big.mark = " ")))
  
  return(enlaces_agregados)
}



# =============================================================================
# 5. PREPARACIÓN PARA SANKEY
# =============================================================================

#' Preparar datos para diagrama de Sankey
#' 
#' Convierte enlaces agregados en formato requerido por Plotly Sankey
#' (nodos + links con índices numéricos).
#' 
#' @param enlaces_agregados data.frame con enlaces agregados
#' 
#' @return Lista con componentes 'nodes' y 'links' listos para Plotly
#' 
#' @export
preparar_sankey <- function(enlaces_agregados) {
  
  if (is.null(enlaces_agregados) || nrow(enlaces_agregados) == 0) {
    return(NULL)
  }
  
  # Extraer nodos únicos
  nodos_unicos <- unique(c(enlaces_agregados$source, enlaces_agregados$target))
  nodes_df <- data.frame(name = nodos_unicos, stringsAsFactors = FALSE)
  
  # Crear índices para links
  links_df <- enlaces_agregados %>%
    mutate(
      source_id = match(source, nodes_df$name) - 1,
      target_id = match(target, nodes_df$name) - 1
    ) %>%
    select(source = source_id, target = target_id, value)
  
  message(sprintf("✓ Sankey preparado: %d nodos, %d enlaces", 
                  nrow(nodes_df), nrow(links_df)))
  
  return(list(
    nodes = nodes_df,
    links = links_df
  ))
}
