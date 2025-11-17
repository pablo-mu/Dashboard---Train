library(dplyr)
library(dtplyr)
library(data.table)

# =============================================================================
# 1. CONFIGURACIÓN DE COLUMNAS SIMPLIFICADA
# =============================================================================

# Definición centralizada de columnas por categorías
COLUMNAS_BASE <- list(
  # Metadatos básicos
  meta = c("ZCENT_GEST", "ZANYO", "ZMES", "ZIMPORT"),
  
  # Fase 0 (origen)
  fase_0 = c("ZCT_SUBCAC_E", "ZACT_E"),
  
  # Fase 1
  fase_1 = c("ZACT_RE12", "ZACT_RE34", "ZACT_F1SD", "ZPARAM_RE34", "ZPARAM_F1SD"),
  
  # Fase 2  
  fase_2 = c("ZACT_2RE12", "ZACT_2RE34", "ZACT_F2SD", "ZPARAM_2RE34", "ZPARAM_F2SD"),
  
  # Fase 3
  fase_3 = c("ZACT_3RE12", "ZACT_3RE34", "ZACT_F3SD", "ZPARAM_3RE34", "ZPARAM_F3SD")
)

# Columnas que necesitamos leer (excluyendo las ignoradas)
COLUMNAS_LECTURA <- c(
  COLUMNAS_BASE$meta,
  COLUMNAS_BASE$fase_0,
  COLUMNAS_BASE$fase_1,
  COLUMNAS_BASE$fase_2,
  COLUMNAS_BASE$fase_3
)

# Tipos de datos para cada columna
TIPOS_COLUMNAS <- c(
  # Meta
  ZCENT_GEST = "character", ZANYO = "integer", ZMES = "integer", ZIMPORT = "double",
  # Fase 0
  ZCT_SUBCAC_E = "character", ZACT_E = "character",
  # Fase 1
  ZACT_RE12 = "character", ZACT_RE34 = "character", ZACT_F1SD = "character",
  ZPARAM_RE34 = "character", ZPARAM_F1SD = "character",
  # Fase 2
  ZACT_2RE12 = "character", ZACT_2RE34 = "character", ZACT_F2SD = "character",
  ZPARAM_2RE34 = "character", ZPARAM_F2SD = "character",
  # Fase 3
  ZACT_3RE12 = "character", ZACT_3RE34 = "character", ZACT_F3SD = "character",
  ZPARAM_3RE34 = "character", ZPARAM_F3SD = "character"
)

# Configuración de fases para procesamiento
CONFIG_FASES <- list(
  "1" = list(re12 = "ZACT_RE12", re34 = "ZACT_RE34", sd = "ZACT_F1SD", 
             param_re34 = "ZPARAM_RE34", param_sd = "ZPARAM_F1SD"),
  "2" = list(re12 = "ZACT_2RE12", re34 = "ZACT_2RE34", sd = "ZACT_F2SD",
             param_re34 = "ZPARAM_2RE34", param_sd = "ZPARAM_F2SD"),
  "3" = list(re12 = "ZACT_3RE12", re34 = "ZACT_3RE34", sd = "ZACT_F3SD",
             param_re34 = "ZPARAM_3RE34", param_sd = "ZPARAM_F3SD")
)

# =============================================================================
# 2. FUNCIONES AUXILIARES
# =============================================================================

#' Aplicar transformación de tipos a las columnas
aplicar_tipos_columnas <- function(datos) {
  for (col_name in names(datos)) {
    if (col_name %in% names(TIPOS_COLUMNAS)) {
      tipo <- TIPOS_COLUMNAS[col_name]
      
      if (tipo == "integer") {
        datos[[col_name]] <- as.integer(datos[[col_name]])
      } else if (tipo == "double") {
        # Limpiar formato europeo: espacios, puntos de miles, coma decimal
        datos[[col_name]] <- gsub("\\s+", "", datos[[col_name]])
        datos[[col_name]] <- gsub("\\.", "", datos[[col_name]])
        datos[[col_name]] <- gsub(",", ".", datos[[col_name]])
        datos[[col_name]] <- as.numeric(datos[[col_name]])
        datos[[col_name]][is.na(datos[[col_name]])] <- 0
      }
      # Para character no hacemos nada
    }
  }
  return(datos)
}

#' Corregir valores problemáticos en columnas CAC
corregir_valores_cac <- function(datos) {
  columnas_cac <- grep("^ZACT_", names(datos), value = TRUE)
  
  for (col in columnas_cac) {
    if (col %in% names(datos)) {
      # Corregir notación científica problemática: "8,00E+02" -> "8E2"
      n_correcciones <- sum(grepl("^8[,\\.]00E\\+0?2$", datos[[col]], ignore.case = TRUE))
      
      if (n_correcciones > 0) {
        datos[[col]] <- gsub("^8[,\\.]00E\\+0?2$", "8E2", datos[[col]], ignore.case = TRUE)
        message(sprintf("  ⚠ Corregidos %d valores '8,00E+02' → '8E2' en %s", n_correcciones, col))
      }
    }
  }
  return(datos)
}

#' Agregar CACs según nivel
agregar_cac <- function(cac_vector, nivel) {
  if (nivel == "cac1") return(substr(cac_vector, 1, 1))
  if (nivel == "cac2") return(substr(cac_vector, 1, 2))
  return(cac_vector) # "cac" o "subcac" mantienen valor original
}

#' Crear fase 0 según nivel de agregación
crear_fase_origen <- function(datos, nivel_agregacion) {
  if (nivel_agregacion == "subcac") {
    datos$fase_0 <- ifelse(!is.na(datos$ZCT_SUBCAC_E) & datos$ZCT_SUBCAC_E != "", 
                           datos$ZCT_SUBCAC_E, datos$ZACT_E)
  } else {
    datos$fase_0 <- agregar_cac(datos$ZACT_E, nivel_agregacion)
  }
  return(datos)
}

# =============================================================================
# 3. CARGA DE DATOS SIMPLIFICADA
# =============================================================================

#' Carga datos de reparto desde archivos CSV
#' 
#' @param rutas Vector de rutas donde buscar archivos CSV
#' @param sep Separador de columnas
#' @param encodings Vector de encodings a probar
#' @return data.frame con datos cargados
carga_datos_reparto <- function(
  rutas = c(".data", "./data", "data", "./.data"),
  sep = ";",
  encodings = c("UTF-8", "latin1", "ISO-8859-1", "windows-1252")
) {
  
  # Buscar archivos CSV
  archivos_encontrados <- NULL
  ruta_encontrada <- NULL
  
  for (ruta in rutas) {
    if (dir.exists(ruta)) {
      archivos <- list.files(ruta, pattern = "\\.csv$", full.names = TRUE, ignore.case = TRUE)
      if (length(archivos) > 0) {
        archivos_encontrados <- archivos
        ruta_encontrada <- ruta
        break
      }
    }
  }
  
  if (is.null(archivos_encontrados)) {
    warning("No se encontraron archivos CSV en las rutas especificadas.")
    return(NULL)
  }
  
  message(sprintf("✓ Encontrados %d archivo(s) CSV en: %s", 
                  length(archivos_encontrados), ruta_encontrada))
  
  # Leer archivos
  datos_list <- lapply(archivos_encontrados, function(archivo) {
    leer_archivo_csv(archivo, sep, encodings)
  })
  
  # Filtrar elementos NULL y combinar
  datos_list <- datos_list[!sapply(datos_list, is.null)]
  if (length(datos_list) == 0) {
    warning("No se pudo cargar ningún archivo CSV válido")
    return(NULL)
  }
  
  datos_combinados <- dplyr::bind_rows(datos_list)
  message(sprintf("✓ Total de datos combinados: %s registros", 
                  formatC(nrow(datos_combinados), format = "d", big.mark = " ")))
  
  return(datos_combinados)
}

#' Leer un archivo CSV con manejo de encoding
leer_archivo_csv <- function(archivo, sep, encodings) {
  for (enc in encodings) {
    resultado <- try({
      # Leer como character inicialmente
      datos <- read.csv(archivo, sep = sep, stringsAsFactors = FALSE,
                       fileEncoding = enc, check.names = FALSE, colClasses = "character")
      
      # Limpiar nombres y seleccionar columnas
      names(datos) <- trimws(names(datos))
      columnas_disponibles <- intersect(COLUMNAS_LECTURA, names(datos))
      datos <- datos[, columnas_disponibles, drop = FALSE]
      
      # Aplicar correcciones y tipos
      datos <- corregir_valores_cac(datos)
      datos <- aplicar_tipos_columnas(datos)
      
      # Limpiar NAs en caracteres
      datos <- datos %>% mutate(across(where(is.character), ~replace(., is.na(.), "")))
      
      message(sprintf("✓ Archivo '%s' leído con encoding '%s' (%s filas)", 
                      basename(archivo), enc, formatC(nrow(datos), format = "d", big.mark = " ")))
      return(datos)
    }, silent = TRUE)
    
    if (!inherits(resultado, "try-error")) return(resultado)
  }
  
  warning(sprintf("No se pudo leer el archivo '%s'", basename(archivo)))
  return(NULL)
}

# =============================================================================
# 4. LIMPIEZA Y FILTRADO
# =============================================================================

#' Limpia y agrupa los datos de reparto
limpiar_datos_reparto <- function(datos) {
  if (is.null(datos) || nrow(datos) == 0) {
    warning("No hay datos para limpiar")
    return(NULL)
  }
  
  columnas_agrupacion <- setdiff(names(datos), "ZIMPORT")
  
  datos_limpios <- datos %>%
    mutate(ZIMPORT = ifelse(is.na(ZIMPORT), 0, ZIMPORT)) %>%
    group_by(across(all_of(columnas_agrupacion))) %>%
    summarise(ZIMPORT = sum(ZIMPORT, na.rm = TRUE), .groups = "drop")
  
  message(sprintf("✓ Datos limpiados: %s registros finales", 
                  formatC(nrow(datos_limpios), format = "d", big.mark = " ")))
  return(datos_limpios)
}

#' Filtrar datos de reparto
filtrar_datos_reparto <- function(datos, centro_gestor = NULL, cacs_origen = NULL, 
                                 subcacs_origen = NULL, mes = NULL, anyo = NULL, 
                                 nivel_agregacion = "cac") {
  if (is.null(datos)) return(NULL)
  
  datos_filtrados <- datos
  
  # Aplicar filtros uno por uno
  if (!is.null(centro_gestor) && centro_gestor != "") {
    datos_filtrados <- datos_filtrados %>% filter(ZCENT_GEST == centro_gestor)
  }
  
  if (nivel_agregacion == "subcac" && !is.null(subcacs_origen)) {
    if ("ZCT_SUBCAC_E" %in% names(datos_filtrados)) {
      datos_filtrados <- datos_filtrados %>% filter(ZCT_SUBCAC_E %in% subcacs_origen)
    }
  } else if (!is.null(cacs_origen)) {
    datos_filtrados <- datos_filtrados %>% filter(ZACT_E %in% cacs_origen)
  }
  
  if (!is.null(mes) && "ZMES" %in% names(datos_filtrados)) {
    datos_filtrados <- datos_filtrados %>% filter(ZMES %in% mes)
  }
  
  if (!is.null(anyo) && "ZANYO" %in% names(datos_filtrados)) {
    datos_filtrados <- datos_filtrados %>% filter(ZANYO %in% anyo)
  }
  
  return(datos_filtrados)
}


# =============================================================================
# FUNCIONES DE EXTRACCIÓN DE MOVIMIENTOS - VERSIÓN CORREGIDA
# =============================================================================

#' Extraer datos de una fase específica
#' 
#' @param datos data.table con todos los datos
#' @param fase_num Número de fase (1, 2 o 3)
#' @return data.table con columnas relevantes para esa fase
extraer_fase <- function(datos, fase_num) {
  fase_config <- CONFIG_FASES[[as.character(fase_num)]]
  fase_actual <- paste0("fase_", fase_num)
  
  if (is.null(fase_config)) {
    stop(sprintf("Fase %d no está configurada", fase_num))
  }
  
  # Columnas a seleccionar
  columnas_seleccionar <- c(COLUMNAS_BASE$meta, COLUMNAS_BASE$fase_0)
  
  # Añadir columnas de fases previas si no es fase 1
  if (fase_num > 1) {
    for (f in 1:(fase_num - 1)) {
      fase_previa <- paste0("fase_", f)
      if (fase_previa %in% names(COLUMNAS_BASE)) {
        columnas_seleccionar <- c(columnas_seleccionar, COLUMNAS_BASE[[fase_previa]])
      }
    }
  }
  
  # Añadir columnas de la fase actual
  columnas_seleccionar <- c(columnas_seleccionar, COLUMNAS_BASE[[fase_actual]])
  
  # Filtrar solo columnas que existen en datos
  columnas_existentes <- intersect(columnas_seleccionar, names(datos))
  
  movimientos_fase <- datos[, ..columnas_existentes]
  return(movimientos_fase)
}

#' Determinar tipo de movimiento prioritario en una fase
#' 
#' @param movimientos_fase data.table con datos de la fase
#' @param fase_num Número de fase
#' @param origen_cac Vector con valores CAC de origen (3 dígitos)
#' @return character vector con tipo de movimiento ("RE12", "RE34", "SD", "NINGUNO")
detectar_tipo_movimiento <- function(movimientos_fase, fase_num, origen_cac) {
  fase_config <- CONFIG_FASES[[as.character(fase_num)]]
  
  tipo_movimiento <- rep("NINGUNO", nrow(movimientos_fase))
  
  # Prioridad 1: RE12
  if (fase_config$re12 %in% names(movimientos_fase)) {
    valores_re12 <- movimientos_fase[[fase_config$re12]]
    hay_re12 <- !is.na(valores_re12) & 
                valores_re12 != "" & 
                valores_re12 != origen_cac
    tipo_movimiento[hay_re12] <- "RE12"
  }
  
  # Prioridad 2: RE34 (solo si no hay RE12)
  if (fase_config$re34 %in% names(movimientos_fase)) {
    valores_re34 <- movimientos_fase[[fase_config$re34]]
    hay_re34 <- tipo_movimiento == "NINGUNO" &
                !is.na(valores_re34) & 
                valores_re34 != "" & 
                valores_re34 != origen_cac
    tipo_movimiento[hay_re34] <- "RE34"
  }
  
  # Prioridad 3: SD (solo si no hay RE12 ni RE34)
  if (fase_config$sd %in% names(movimientos_fase)) {
    valores_sd <- movimientos_fase[[fase_config$sd]]
    hay_sd <- tipo_movimiento == "NINGUNO" &
              !is.na(valores_sd) & 
              valores_sd != "" & 
              valores_sd != origen_cac
    tipo_movimiento[hay_sd] <- "SD"
  }
  
  return(tipo_movimiento)
}

#' Determinar estrategia de origen para SUBCAC
#' 
#' Esta función implementa la lógica corregida:
#' - Si el primer movimiento (en cualquier fase) es RE12: mantener SUBCAC hasta esa fase
#' - Si el primer movimiento es RE34 o SD: acumular a CAC en la fase previa al movimiento
#' 
#' @param datos data.table con todos los datos
#' @return data.table con dos columnas:
#'   - primera_fase_movimiento: fase donde ocurre el primer movimiento (1-3 o NA)
#'   - tipo_primer_movimiento: tipo del primer movimiento ("RE12", "RE34", "SD", "NINGUNO")
detectar_estrategia_subcac <- function(datos) {
  n_registros <- nrow(datos)
  
  estrategia <- data.table(
    primera_fase_movimiento = rep(NA_integer_, n_registros),
    tipo_primer_movimiento = rep("NINGUNO", n_registros)
  )
  
  # Solo procesar registros con SUBCAC origen
  tiene_subcac <- !is.na(datos$ZCT_SUBCAC_E) & datos$ZCT_SUBCAC_E != ""
  
  if (!any(tiene_subcac)) {
    return(estrategia)
  }
  
  # CAC padre del SUBCAC (primeros 3 dígitos)
  cac_padre <- substr(datos$ZCT_SUBCAC_E, 1, 3)
  
  # Revisar cada fase en orden
  for (fase_num in 1:3) {
    fase_config <- CONFIG_FASES[[as.character(fase_num)]]
    
    # Identificar tipo de movimiento en esta fase
    tipo_mov <- detectar_tipo_movimiento(datos, fase_num, cac_padre)
    
    # Para registros que aún no tienen movimiento detectado
    sin_movimiento_previo <- tiene_subcac & estrategia$tipo_primer_movimiento == "NINGUNO"
    
    # Si hay movimiento en esta fase
    hay_movimiento <- sin_movimiento_previo & tipo_mov != "NINGUNO"
    
    if (any(hay_movimiento)) {
      estrategia$primera_fase_movimiento[hay_movimiento] <- fase_num
      estrategia$tipo_primer_movimiento[hay_movimiento] <- tipo_mov[hay_movimiento]
    }
  }
  
  return(estrategia)
}

#' Aplicar agregación a nivel CAC1 o CAC2
#' 
#' @param movimientos_fase data.table con datos
#' @param nivel_agregacion "cac1" o "cac2"
#' @return data.table modificado
aplicar_agregacion_cac <- function(movimientos_fase, nivel_agregacion) {
  columnas_cac <- grep("^ZACT_", names(movimientos_fase), value = TRUE)
  
  for (col in columnas_cac) {
    movimientos_fase[[col]] <- agregar_cac(movimientos_fase[[col]], nivel_agregacion)
  }
  
  return(movimientos_fase)
}

#' Extraer movimientos de una fase específica - VERSIÓN CORREGIDA
#' 
#' @param movimientos_fase data.table con datos de la fase
#' @param fase_num Número de fase (1, 2 o 3)
#' @param nivel_agregacion Nivel: "subcac", "cac", "cac1", "cac2"
#' @param origen_previo Vector con origen de la fase anterior (NULL para fase 1)
#' @param estrategia_subcac data.table con estrategia para registros SUBCAC
#' @param incluir_tipo_param Incluir tipo y parámetro de reparto
#' @param debug Mostrar información de debugging
#' @return data.table con movimientos procesados
extraer_movimientos_fase <- function(
  movimientos_fase,
  fase_num,
  nivel_agregacion = "cac",
  origen_previo = NULL,
  estrategia_subcac = NULL,
  incluir_tipo_param = TRUE,
  debug = TRUE
) {
  
  fase_config <- CONFIG_FASES[[as.character(fase_num)]]
  
  # ---- 1. PREPARAR NIVEL DE AGREGACIÓN ----
  
  if (nivel_agregacion %in% c("cac1", "cac2")) {
    movimientos_fase <- aplicar_agregacion_cac(movimientos_fase, nivel_agregacion)
  }
  
  # ---- 2. DETERMINAR ORIGEN SEGÚN FASE Y ESTRATEGIA ----
  
  if (fase_num == 1) {
    # Fase 1: usar origen inicial
    if (nivel_agregacion == "subcac") {
      if ("ZCT_SUBCAC_E" %in% names(movimientos_fase)) {
        # Origen base: SUBCAC si existe, sino CAC
        origen_base <- ifelse(
          !is.na(movimientos_fase$ZCT_SUBCAC_E) & movimientos_fase$ZCT_SUBCAC_E != "",
          movimientos_fase$ZCT_SUBCAC_E,
          movimientos_fase$ZACT_E
        )
        
        # LÓGICA CORREGIDA PARA FASE 1:
        # Si hay estrategia y el primer movimiento NO es RE12, o es en fase > 1,
        # entonces acumular a CAC en fase 1
        if (!is.null(estrategia_subcac)) {
          # Acumular a CAC si:
          # - Primer movimiento es RE34 o SD (requieren acumulación)
          # - O primer movimiento es en fase 2 o 3 y NO es RE12
          necesita_acumulacion <- (estrategia_subcac$tipo_primer_movimiento %in% c("RE34", "SD")) |
                                  (estrategia_subcac$primera_fase_movimiento > 1 & 
                                   estrategia_subcac$tipo_primer_movimiento != "RE12")
          
          cac_padre <- substr(origen_base, 1, 3)
          origen <- ifelse(necesita_acumulacion & nchar(origen_base) == 4,
                          cac_padre,
                          origen_base)
          
          if (debug && any(necesita_acumulacion, na.rm = TRUE)) {
            n_acum <- sum(necesita_acumulacion, na.rm = TRUE)
            message(sprintf("     [DEBUG] Fase 1: %d registros SUBCAC acumulados a CAC (requieren acumulación previa)", n_acum))
          }
        } else {
          origen <- origen_base
        }
      } else {
        origen <- movimientos_fase$ZACT_E
      }
    } else {
      # Para CAC, CAC1, CAC2: usar ZACT_E agregado
      origen <- agregar_cac(movimientos_fase$ZACT_E, nivel_agregacion)
    }
  } else {
    # Fases 2 y 3: usar origen de fase anterior
    if (is.null(origen_previo)) {
      stop(sprintf("Para fase %d se requiere origen_previo", fase_num))
    }
    
    # LÓGICA CORREGIDA PARA FASES 2 Y 3:
    # Si estamos en SUBCAC y hay movimiento no-RE12 en esta fase,
    # acumular a CAC ANTES de aplicar el movimiento
    if (nivel_agregacion == "subcac" && !is.null(estrategia_subcac)) {
      # Detectar registros que necesitan acumulación en esta fase
      necesita_acumulacion_ahora <- (estrategia_subcac$primera_fase_movimiento == fase_num) &
                                    (estrategia_subcac$tipo_primer_movimiento %in% c("RE34", "SD"))
      
      if (any(necesita_acumulacion_ahora, na.rm = TRUE)) {
        # Para estos registros, si el origen es SUBCAC, acumularlo a CAC
        origen <- ifelse(
          necesita_acumulacion_ahora & nchar(origen_previo) == 4,
          substr(origen_previo, 1, 3),  # Acumular a CAC
          origen_previo  # Mantener origen anterior
        )
        
        if (debug) {
          n_acum <- sum(necesita_acumulacion_ahora & nchar(origen_previo) == 4, na.rm = TRUE)
          if (n_acum > 0) {
            message(sprintf("     [DEBUG] Fase %d: %d registros SUBCAC acumulados a CAC antes de movimiento RE34/SD", 
                           fase_num, n_acum))
          }
        }
      } else {
        origen <- origen_previo
      }
    } else {
      origen <- origen_previo
    }
  }
  
  if (debug && fase_num == 1) {
    n_subcac <- sum(nchar(origen) == 4)
    n_cac <- sum(nchar(origen) == 3)
    message(sprintf("     [DEBUG] Orígenes Fase %d: %d SUBCAC, %d CAC", fase_num, n_subcac, n_cac))
  }
  
  # ---- 3. INICIALIZAR COLUMNAS DE RESULTADO ----
  
  movimientos_fase$destino <- origen  # Por defecto, sin movimiento
  movimientos_fase$movimiento_usado <- "SIN_MOVIMIENTO"
  
  if (incluir_tipo_param) {
    movimientos_fase$tipo_reparto <- NA_character_
    movimientos_fase$parametro <- NA_character_
  }
  
  # ---- 4. PREPARAR COLUMNAS DESTINO SEGÚN AGREGACIÓN ----
  
  if (nivel_agregacion %in% c("cac1", "cac2")) {
    col_re12_proc <- paste0("temp_re12")
    col_re34_proc <- paste0("temp_re34")
    col_sd_proc <- paste0("temp_sd")
    
    movimientos_fase[[col_re12_proc]] <- agregar_cac(
      movimientos_fase[[fase_config$re12]], nivel_agregacion
    )
    movimientos_fase[[col_re34_proc]] <- agregar_cac(
      movimientos_fase[[fase_config$re34]], nivel_agregacion
    )
    movimientos_fase[[col_sd_proc]] <- agregar_cac(
      movimientos_fase[[fase_config$sd]], nivel_agregacion
    )
  } else {
    col_re12_proc <- fase_config$re12
    col_re34_proc <- fase_config$re34
    col_sd_proc <- fase_config$sd
  }
  
  # ---- 5. APLICAR PRIORIDADES DE MOVIMIENTO ----
  
  # Para comparaciones: extraer CAC del origen (si es SUBCAC)
  origen_cac <- ifelse(nchar(origen) == 4, substr(origen, 1, 3), origen)
  
  # Prioridad 1: RE12
  if (col_re12_proc %in% names(movimientos_fase)) {
    idx_re12 <- !is.na(movimientos_fase[[col_re12_proc]]) &
                movimientos_fase[[col_re12_proc]] != "" &
                movimientos_fase[[col_re12_proc]] != origen_cac
    
    if (any(idx_re12)) {
      movimientos_fase$destino[idx_re12] <- movimientos_fase[[col_re12_proc]][idx_re12]
      movimientos_fase$movimiento_usado[idx_re12] <- "RE12"
      
      if (debug) {
        n_re12 <- sum(idx_re12)
        n_desde_subcac <- sum(idx_re12 & nchar(origen) == 4)
        message(sprintf("     [DEBUG] Aplicados %d movimientos RE12 (%d desde SUBCAC)", 
                       n_re12, n_desde_subcac))
      }
    }
  }
  
  # Prioridad 2: RE34
  if (col_re34_proc %in% names(movimientos_fase)) {
    idx_re34 <- movimientos_fase$movimiento_usado == "SIN_MOVIMIENTO" &
                !is.na(movimientos_fase[[col_re34_proc]]) &
                movimientos_fase[[col_re34_proc]] != "" &
                movimientos_fase[[col_re34_proc]] != origen_cac
    
    if (any(idx_re34)) {
      movimientos_fase$destino[idx_re34] <- movimientos_fase[[col_re34_proc]][idx_re34]
      movimientos_fase$movimiento_usado[idx_re34] <- "RE34"
      
      if (debug) {
        n_re34 <- sum(idx_re34)
        n_desde_subcac <- sum(idx_re34 & nchar(origen) == 4)
        message(sprintf("     [DEBUG] Aplicados %d movimientos RE34 (%d desde SUBCAC - debería ser 0)", 
                       n_re34, n_desde_subcac))
        if (n_desde_subcac > 0) {
          message("     [ADVERTENCIA] RE34 desde SUBCAC detectado - revisar lógica de acumulación")
        }
      }
    }
  }
  
  # Prioridad 3: SD
  if (col_sd_proc %in% names(movimientos_fase)) {
    idx_sd <- movimientos_fase$movimiento_usado == "SIN_MOVIMIENTO" &
              !is.na(movimientos_fase[[col_sd_proc]]) &
              movimientos_fase[[col_sd_proc]] != "" &
              movimientos_fase[[col_sd_proc]] != origen_cac
    
    if (any(idx_sd)) {
      movimientos_fase$destino[idx_sd] <- movimientos_fase[[col_sd_proc]][idx_sd]
      movimientos_fase$movimiento_usado[idx_sd] <- "SD"
      
      if (debug) {
        n_sd <- sum(idx_sd)
        n_desde_subcac <- sum(idx_sd & nchar(origen) == 4)
        message(sprintf("     [DEBUG] Aplicados %d movimientos SD (%d desde SUBCAC - debería ser 0)", 
                       n_sd, n_desde_subcac))
        if (n_desde_subcac > 0) {
          message("     [ADVERTENCIA] SD desde SUBCAC detectado - revisar lógica de acumulación")
        }
      }
    }
  }
  
  # ---- 6. ASIGNAR TIPOS Y PARÁMETROS ----
  
  if (incluir_tipo_param) {
    # RE1/RE2: Basado en presencia de parámetro RE34
    idx_re12 <- movimientos_fase$movimiento_usado == "RE12"
    if (any(idx_re12)) {
      tiene_param_re34 <- !is.na(movimientos_fase[[fase_config$param_re34]]) &
                         movimientos_fase[[fase_config$param_re34]] != "" &
                         movimientos_fase[[fase_config$param_re34]] != "NO PARAM"
      
      movimientos_fase$tipo_reparto[idx_re12] <- ifelse(
        tiene_param_re34[idx_re12], "RE2", "RE1"
      )
      movimientos_fase$parametro[idx_re12] <- ifelse(
        tiene_param_re34[idx_re12],
        movimientos_fase[[fase_config$param_re34]][idx_re12],
        "NO PARAM"
      )
    }
    
    # RE3/RE4: Basado en presencia de parámetro RE34
    idx_re34 <- movimientos_fase$movimiento_usado == "RE34"
    if (any(idx_re34)) {
      tiene_param_re34 <- !is.na(movimientos_fase[[fase_config$param_re34]]) &
                         movimientos_fase[[fase_config$param_re34]] != "" &
                         movimientos_fase[[fase_config$param_re34]] != "NO PARAM"
      
      movimientos_fase$tipo_reparto[idx_re34] <- ifelse(
        tiene_param_re34[idx_re34], "RE4", "RE3"
      )
      movimientos_fase$parametro[idx_re34] <- ifelse(
        tiene_param_re34[idx_re34],
        movimientos_fase[[fase_config$param_re34]][idx_re34],
        "NO PARAM"
      )
    }
    
    # SD: Usar parámetro SD
    idx_sd <- movimientos_fase$movimiento_usado == "SD"
    if (any(idx_sd)) {
      movimientos_fase$tipo_reparto[idx_sd] <- "SD"
      movimientos_fase$parametro[idx_sd] <- movimientos_fase[[fase_config$param_sd]][idx_sd]
    }
    
    # SIN_MOVIMIENTO
    idx_sin <- movimientos_fase$movimiento_usado == "SIN_MOVIMIENTO"
    if (any(idx_sin)) {
      movimientos_fase$tipo_reparto[idx_sin] <- "SIN_MOVIMIENTO"
      movimientos_fase$parametro[idx_sin] <- NA_character_
    }
  }
  
  # ---- 7. LIMPIAR COLUMNAS TEMPORALES ----
  
  if (nivel_agregacion %in% c("cac1", "cac2")) {
    movimientos_fase[, c("temp_re12", "temp_re34", "temp_sd") := NULL]
  }
  
  # ---- 8. RETORNAR RESULTADO ----
  
  columnas_resultado <- c("destino", "movimiento_usado")
  if (incluir_tipo_param) {
    columnas_resultado <- c(columnas_resultado, "tipo_reparto", "parametro")
  }
  
  return(movimientos_fase[, ..columnas_resultado])
}

#' Extraer todos los movimientos a través de las fases - VERSIÓN CORREGIDA
#' 
#' @param datos data.frame o data.table con datos limpios
#' @param nivel_agregacion Nivel de agregación: "subcac", "cac", "cac1", "cac2"
#' @param fases_incluir Vector de fases a incluir (por defecto 1:3)
#' @param incluir_tipo_param Incluir columnas de tipo y parámetro
#' @param verbose Mostrar mensajes de progreso
#' @param debug Mostrar información detallada de debugging
#' @return data.table con matriz de movimientos
extraer_movimientos <- function(
  datos,
  nivel_agregacion = "cac",
  fases_incluir = NULL,
  incluir_tipo_param = TRUE,
  verbose = TRUE,
  debug = FALSE
) {
  
  # ---- 1. VALIDACIONES INICIALES ----
  
  if (is.null(datos) || nrow(datos) == 0) {
    warning("No hay datos para extraer movimientos")
    return(NULL)
  }
  
  # Convertir a data.table si es necesario
  datos_dt <- if (!is.data.table(datos)) as.data.table(datos) else copy(datos)
  
  # Filtrar registros válidos
  datos_dt <- datos_dt[!is.na(ZIMPORT) & ZIMPORT != 0]
  if (nrow(datos_dt) == 0) {
    warning("No hay registros con importe válido")
    return(NULL)
  }
  
  # Determinar fases a procesar
  fases_a_procesar <- if (is.null(fases_incluir)) 1:3 else sort(unique(fases_incluir))
  max_fase <- max(fases_a_procesar)
  
  if (verbose) {
    message(sprintf("Extrayendo movimientos para %s registros, fases: %s", 
                    formatC(nrow(datos_dt), format = "d", big.mark = " "),
                    paste(fases_a_procesar, collapse = ", ")))
    message(sprintf("  Nivel de agregación: %s", nivel_agregacion))
  }
  
  # ---- 2. CREAR FASE ORIGEN (FASE 0) ----
  
  datos_dt <- crear_fase_origen(datos_dt, nivel_agregacion)
  
  # ---- 3. DETECTAR ESTRATEGIA PARA SUBCAC ----
  
  estrategia_subcac <- NULL
  
  if (nivel_agregacion == "subcac" && "ZCT_SUBCAC_E" %in% names(datos_dt)) {
    if (verbose) {
      message("  Detectando estrategia de flujo para registros SUBCAC...")
    }
    
    estrategia_subcac <- detectar_estrategia_subcac(datos_dt)
    
    n_con_movimiento <- sum(estrategia_subcac$tipo_primer_movimiento != "NINGUNO")
    
    if (verbose && n_con_movimiento > 0) {
      message(sprintf("    → %s registros SUBCAC con movimientos detectados", 
                     formatC(n_con_movimiento, format = "d", big.mark = " ")))
      
      # Estadísticas por tipo
      tabla_tipos <- table(estrategia_subcac$tipo_primer_movimiento)
      for (tipo in names(tabla_tipos)) {
        if (tipo != "NINGUNO") {
          n_tipo <- tabla_tipos[tipo]
          message(sprintf("       · %s: %s registros", tipo, 
                         formatC(n_tipo, format = "d", big.mark = " ")))
          
          # Distribución por fases para este tipo
          idx_tipo <- estrategia_subcac$tipo_primer_movimiento == tipo
          tabla_fases <- table(estrategia_subcac$primera_fase_movimiento[idx_tipo])
          for (fase in names(tabla_fases)) {
            message(sprintf("         - Fase %s: %s", fase, 
                           formatC(tabla_fases[fase], format = "d", big.mark = " ")))
          }
        }
      }
    }
    
    if (debug && n_con_movimiento > 0) {
      message("\n[DEBUG] Ejemplos de estrategias detectadas:")
      idx_ejemplos <- which(estrategia_subcac$tipo_primer_movimiento != "NINGUNO")[1:min(5, n_con_movimiento)]
      for (i in idx_ejemplos) {
        subcac <- datos_dt$ZCT_SUBCAC_E[i]
        fase <- estrategia_subcac$primera_fase_movimiento[i]
        tipo <- estrategia_subcac$tipo_primer_movimiento[i]
        message(sprintf("  Registro %d: SUBCAC %s → Primer movimiento: %s en Fase %d", 
                       i, subcac, tipo, fase))
      }
    }
  }
  
  # ---- 4. PROCESAR CADA FASE SECUENCIALMENTE ----
  
  resultado <- data.table(
    fase_0 = datos_dt$fase_0,
    importe = datos_dt$ZIMPORT
  )
  
  origen_actual <- datos_dt$fase_0
  
  for (fase_num in 1:max_fase) {
    if (verbose) {
      message(sprintf("  Procesando Fase %d...", fase_num))
    }
    
    # Extraer datos de la fase
    movimientos_fase <- extraer_fase(datos_dt, fase_num)
    
    # Procesar movimientos de la fase
    fase_resultado <- extraer_movimientos_fase(
      movimientos_fase = movimientos_fase,
      fase_num = fase_num,
      nivel_agregacion = nivel_agregacion,
      origen_previo = if (fase_num == 1) NULL else origen_actual,
      estrategia_subcac = estrategia_subcac,
      incluir_tipo_param = incluir_tipo_param,
      debug = debug
    )
    
    # Agregar columnas al resultado
    nombre_fase <- paste0("fase_", fase_num)
    resultado[[nombre_fase]] <- fase_resultado$destino
    
    if (incluir_tipo_param) {
      resultado[[paste0("tipo_fase_", fase_num)]] <- fase_resultado$tipo_reparto
      resultado[[paste0("param_fase_", fase_num)]] <- fase_resultado$parametro
    }
    
    # Actualizar origen para siguiente fase
    origen_actual <- fase_resultado$destino
    
    # Estadísticas de la fase
    if (verbose) {
      n_movimientos <- sum(fase_resultado$movimiento_usado != "SIN_MOVIMIENTO")
      pct_movimientos <- 100 * n_movimientos / nrow(fase_resultado)
      
      message(sprintf("    → %s movimientos (%.1f%%)", 
                     formatC(n_movimientos, format = "d", big.mark = " "),
                     pct_movimientos))
      
      if (debug) {
        tipos_movimiento <- table(fase_resultado$movimiento_usado)
        message("       Detalle por tipo:")
        for (tipo in names(tipos_movimiento)) {
          message(sprintf("         · %s: %d", tipo, tipos_movimiento[tipo]))
        }
      }
    }
  }
  
  # ---- 5. ELIMINAR FASES NO INCLUIDAS ----
  
  if (!is.null(fases_incluir)) {
    fases_todas <- 1:3
    fases_eliminar <- setdiff(fases_todas, fases_incluir)
    
    for (fase_num in fases_eliminar) {
      cols_eliminar <- c(
        paste0("fase_", fase_num),
        paste0("tipo_fase_", fase_num),
        paste0("param_fase_", fase_num)
      )
      cols_eliminar <- intersect(cols_eliminar, names(resultado))
      if (length(cols_eliminar) > 0) {
        resultado[, (cols_eliminar) := NULL]
      }
    }
  }
  
  # ---- 6. RESUMEN FINAL ----
  
  if (verbose) {
    message(sprintf("✓ Matriz generada: %s registros, importe total: %s €", 
                    formatC(nrow(resultado), format = "d", big.mark = " "),
                    formatC(sum(resultado$importe, na.rm = TRUE), format = "f", 
                            big.mark = " ", decimal.mark = ",", digits = 2)))
  }
  
  if (debug) {
    message("\n[DEBUG] Resumen final de la matriz (primeras 20 filas):")
    print(head(resultado, 20))
  }
  
  return(resultado)
}

# =============================================================================
# 6. FUNCIONES AUXILIARES DE UTILIDAD
# =============================================================================

#' Pipeline completo de carga y limpieza
preparar_datos_reparto <- function(...) {
  datos_raw <- carga_datos_reparto(...)
  datos_limpios <- limpiar_datos_reparto(datos_raw)
  return(datos_limpios)
}

#' Obtener valores únicos para filtros
obtener_valores_filtros <- function(datos) {
  if (is.null(datos) || nrow(datos) == 0) return(list())
  
  list(
    centros_gestores = sort(unique(datos$ZCENT_GEST)),
    anyos = sort(unique(datos$ZANYO)),
    meses = sort(unique(datos$ZMES)),
    cacs_origen = sort(unique(datos$ZACT_E)),
    subcacs_origen = sort(unique(datos$ZCT_SUBCAC_E))
  )
}

#' Formatear número para visualización
formatear_numero <- function(x, decimales = 2) {
  formatC(x, format = "f", big.mark = " ", decimal.mark = ",", digits = decimales)
}

#' Formatear importe en euros
formatear_importe <- function(x) {
  paste(formatear_numero(x), "€")
}

#' Formatear entero con separadores de miles
formatear_entero <- function(x) {
  formatC(x, format = "d", big.mark = " ")
}
