library(dplyr)
library(dtplyr)
library(data.table)
library(readr)

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
  # Proteger contra NAs
  cac_vector <- ifelse(is.na(cac_vector), "", cac_vector)
  
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
# 3. FUNCIONES PARA ARCHIVOS SIE
# =============================================================================

#' Detectar si un archivo es formato SIE
#' @param nombres_columnas Vector con nombres de columnas del archivo
#' @return Lógico indicando si es formato SIE
es_formato_sie <- function(nombres_columnas) {
  columnas_sie <- c("CENTRO", "ANYO", "MESCF", "CACCF", "CACA0", "CACA1", "CACA2", "CACA3", "IMPORTE")
  sum(columnas_sie %in% nombres_columnas) >= 7  # Al menos 7 de 9 columnas clave
}

#' Transformar datos SIE al formato estándar
#' 
#' Convierte la estructura SIE a la estructura compatible con el código actual.
#' Cada movimiento se trata como "Se Distribuye" (SD) ya que no hay información
#' de tipo de reparto en los archivos SIE.
#' 
#' @param datos_sie data.frame con estructura SIE
#' @return data.frame con estructura estándar
transformar_sie_a_estandar <- function(datos_sie) {
  
  message("  Transformando formato SIE a formato estándar...")
  message(sprintf("    [DEBUG] Dimensiones datos_sie: %d filas, %d columnas", nrow(datos_sie), ncol(datos_sie)))
  message(sprintf("    [DEBUG] Columnas disponibles: %s", paste(names(datos_sie)[1:min(10, ncol(datos_sie))], collapse=", ")))
  
  # Convertir IMPORTE de forma segura
  message("    [DEBUG] Convirtiendo IMPORTE...")
  importe_raw <- as.character(datos_sie$IMPORTE)
  importe_limpio <- gsub("\\s+", "", importe_raw)  # Quitar espacios
  importe_limpio <- gsub("\\.", "", importe_limpio)  # Quitar separadores de miles
  importe_limpio <- gsub(",", ".", importe_limpio)  # Cambiar coma decimal por punto
  importe_numerico <- as.numeric(importe_limpio)
  importe_numerico[is.na(importe_numerico)] <- 0
  message(sprintf("    [DEBUG] IMPORTE convertido: %d valores, suma: %.2f", length(importe_numerico), sum(importe_numerico)))
  
  # Mapear columnas básicas
  message("    [DEBUG] Creando data.frame estándar...")
  datos_estandar <- data.frame(
    ZCENT_GEST = as.character(datos_sie$CENTRO),
    ZANYO = as.integer(datos_sie$ANYO),
    ZMES = as.integer(datos_sie$MESCF),
    ZIMPORT = importe_numerico,
    ZCT_SUBCAC_E = as.character(datos_sie$CACCF),
    ZACT_E = as.character(datos_sie$CACA0),
    stringsAsFactors = FALSE
  )
  message("    [DEBUG] Data.frame básico creado correctamente")
  
  # Extraer CACs (siempre existen en archivos SIE válidos)
  message("    [DEBUG] Extrayendo CACs...")
  caca0 <- as.character(datos_sie$CACA0)
  caca1 <- as.character(datos_sie$CACA1)
  caca2 <- as.character(datos_sie$CACA2)
  caca3 <- as.character(datos_sie$CACA3)
  message("    [DEBUG] CACs extraídos correctamente")
  message("    [DEBUG] CACs extraídos correctamente")
  
  # Fase 1: CACA0 → CACA1
  message("    [DEBUG] Configurando Fase 1...")
  datos_estandar$ZACT_RE12 <- caca0      # Mantener origen = no movimiento
  datos_estandar$ZACT_RE34 <- caca0      # Mantener origen = no movimiento
  datos_estandar$ZACT_F1SD <- caca1      # Mover a CACA1 via SD
  datos_estandar$ZPARAM_RE34 <- "NO PARAM"
  datos_estandar$ZPARAM_F1SD <- "SIE"
  message("    [DEBUG] Fase 1 configurada")
  
  # Fase 2: CACA1 → CACA2
  message("    [DEBUG] Configurando Fase 2...")
  datos_estandar$ZACT_2RE12 <- caca1     # Mantener fase anterior = no movimiento
  datos_estandar$ZACT_2RE34 <- caca1     # Mantener fase anterior = no movimiento
  datos_estandar$ZACT_F2SD <- caca2      # Mover a CACA2 via SD
  datos_estandar$ZPARAM_2RE34 <- "NO PARAM"
  datos_estandar$ZPARAM_F2SD <- "SIE"
  message("    [DEBUG] Fase 2 configurada")
  
  # Fase 3: CACA2 → CACA3
  message("    [DEBUG] Configurando Fase 3...")
  datos_estandar$ZACT_3RE12 <- caca2     # Mantener fase anterior = no movimiento
  datos_estandar$ZACT_3RE34 <- caca2     # Mantener fase anterior = no movimiento
  datos_estandar$ZACT_F3SD <- caca3      # Mover a CACA3 via SD
  datos_estandar$ZPARAM_3RE34 <- "NO PARAM"
  datos_estandar$ZPARAM_F3SD <- "SIE"
  message("    [DEBUG] Fase 3 configurada")
  
  # Limpiar NAs en columnas de caracteres
  message("    [DEBUG] Limpiando NAs...")
  cols_char <- c("ZCENT_GEST", "ZCT_SUBCAC_E", "ZACT_E",
                 "ZACT_RE12", "ZACT_RE34", "ZACT_F1SD", "ZPARAM_RE34", "ZPARAM_F1SD",
                 "ZACT_2RE12", "ZACT_2RE34", "ZACT_F2SD", "ZPARAM_2RE34", "ZPARAM_F2SD",
                 "ZACT_3RE12", "ZACT_3RE34", "ZACT_F3SD", "ZPARAM_3RE34", "ZPARAM_F3SD")
  
  for (col in cols_char) {
    datos_estandar[[col]][is.na(datos_estandar[[col]])] <- ""
  }
  message("    [DEBUG] NAs limpiados")
  
  n_registros <- nrow(datos_estandar)
  importe_total <- sum(datos_estandar$ZIMPORT, na.rm = TRUE)
  
  message(sprintf("  ✓ Transformación SIE completada: %s registros, importe total: %s €",
                  formatC(n_registros, format = "d", big.mark = " "),
                  formatC(importe_total, format = "f", big.mark = " ", decimal.mark = ",", digits = 2)))
  
  return(datos_estandar)
}

#' Leer un archivo CSV con detección automática de formato
#' 
#' @param archivo Ruta al archivo CSV
#' @param sep Separador de columnas
#' @param encodings Vector de encodings a probar
#' @return data.frame con estructura estándar (transformado si es SIE)
leer_archivo_csv_auto <- function(archivo, sep, encodings) {
  
  message(sprintf("[DEBUG] Intentando leer archivo: %s", basename(archivo)))
  
  for (enc in encodings) {
    message(sprintf("[DEBUG] Probando encoding: %s", enc))
    
    resultado <- try({
      # Leer con read_delim que es más robusto
      message("[DEBUG] Llamando a read_delim...")
      datos <- read_delim(
        archivo, 
        delim = sep, 
        locale = locale(
          encoding = enc,
          decimal_mark = ",",
          grouping_mark = ""
        ),
        col_types = cols(.default = "c"),  # Todo como character
        trim_ws = TRUE,
        show_col_types = FALSE
      )
      
      # Convertir tibble a data.frame
      datos <- as.data.frame(datos, stringsAsFactors = FALSE)
      message(sprintf("[DEBUG] read_delim completado: %d filas, %d columnas", nrow(datos), ncol(datos)))
      
      # Limpiar nombres de columnas
      message("[DEBUG] Limpiando nombres de columnas...")
      names(datos) <- trimws(names(datos))
      message(sprintf("[DEBUG] Columnas: %s", paste(head(names(datos), 5), collapse=", ")))
      
      # Detectar formato
      message("[DEBUG] Detectando formato...")
      es_sie <- es_formato_sie(names(datos))
      message(sprintf("[DEBUG] ¿Es formato SIE?: %s", es_sie))
      
      if (es_sie) {
        # Transformar SIE a estándar
        message(sprintf("✓ Archivo SIE detectado: '%s' (encoding '%s')", basename(archivo), enc))
        datos_estandar <- transformar_sie_a_estandar(datos)
        return(datos_estandar)
        
      } else {
        # Formato estándar: procesar como antes
        message("[DEBUG] Es formato estándar, procesando...")
        columnas_disponibles <- intersect(COLUMNAS_LECTURA, names(datos))
        
        if (length(columnas_disponibles) == 0) {
          warning(sprintf("No se encontraron columnas reconocibles en '%s'", basename(archivo)))
          return(NULL)
        }
        
        datos <- datos[, columnas_disponibles, drop = FALSE]
        
        # Aplicar correcciones y tipos
        datos <- corregir_valores_cac(datos)
        datos <- aplicar_tipos_columnas(datos)
        
        # Limpiar NAs en caracteres (CRÍTICO para evitar errores substr)
        for (col in names(datos)) {
          if (is.character(datos[[col]])) {
            datos[[col]][is.na(datos[[col]])] <- ""
          }
        }
        
        # Asegurar que ZCT_SUBCAC_E existe y no tiene NAs
        if (!"ZCT_SUBCAC_E" %in% names(datos)) {
          datos$ZCT_SUBCAC_E <- ""
        } else {
          datos$ZCT_SUBCAC_E[is.na(datos$ZCT_SUBCAC_E)] <- ""
        }
        
        message(sprintf("✓ Archivo estándar leído: '%s' con encoding '%s' (%s filas)", 
                        basename(archivo), enc, formatC(nrow(datos), format = "d", big.mark = " ")))
        return(datos)
      }
      
    }, silent = TRUE)
    
    if (!inherits(resultado, "try-error")) {
      message("[DEBUG] Lectura exitosa, retornando datos")
      return(resultado)
    } else {
      message(sprintf("[DEBUG] Error con encoding %s: %s", enc, as.character(resultado)))
    }
  }
  
  warning(sprintf("No se pudo leer el archivo '%s'", basename(archivo)))
  return(NULL)
}

# =============================================================================
# 4. CARGA DE DATOS SIMPLIFICADA
# =============================================================================

#' Listar archivos disponibles por tipo
#' @param tipo "CASA" o "SIE"
#' @return Vector con rutas de archivos
listar_archivos_reparto <- function(tipo = "CASA") {
  rutas_base <- c(".data", "./data", "data", "./.data")
  rutas_tipo <- file.path(rutas_base, tipo)
  
  archivos_encontrados <- c()
  
  for (ruta in rutas_tipo) {
    if (dir.exists(ruta)) {
      archivos <- list.files(ruta, pattern = "\\.csv$", full.names = TRUE, ignore.case = TRUE)
      if (length(archivos) > 0) {
        # Normalizar rutas para evitar duplicados
        archivos_norm <- normalizePath(archivos, winslash = "/", mustWork = FALSE)
        archivos_encontrados <- c(archivos_encontrados, archivos_norm)
      }
    }
  }
  
  return(unique(archivos_encontrados))
}

#' Carga datos de reparto desde archivos CSV
#' 
#' @param rutas Vector de rutas donde buscar archivos CSV
#' @param archivo_especifico Ruta a un archivo específico (opcional)
#' @param sep Separador de columnas
#' @param encodings Vector de encodings a probar
#' @return data.frame con datos cargados
carga_datos_reparto <- function(
  rutas = c(".data", "./data", "data", "./.data"),
  archivo_especifico = NULL,
  sep = ";",
  encodings = c("UTF-8", "latin1", "ISO-8859-1", "windows-1252")
) {
  
  # Buscar archivos CSV
  archivos_encontrados <- NULL
  ruta_encontrada <- NULL
  
  if (!is.null(archivo_especifico) && archivo_especifico != "") {
    if (file.exists(archivo_especifico)) {
      archivos_encontrados <- archivo_especifico
      ruta_encontrada <- dirname(archivo_especifico)
      message(sprintf("✓ Cargando archivo específico: %s", basename(archivo_especifico)))
    } else {
      warning(sprintf("El archivo específico no existe: %s", archivo_especifico))
      return(NULL)
    }
  } else {
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
  }
  
  # Leer archivos
  datos_list <- lapply(archivos_encontrados, function(archivo) {
    leer_archivo_csv_auto(archivo, sep, encodings)
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

#' Leer un archivo CSV con manejo de encoding (MÉTODO ANTIGUO - MANTENER POR COMPATIBILIDAD)
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
# 5. LIMPIEZA Y FILTRADO
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

#' Expandir selección de CACs incluyendo grupos
#' @param seleccion Vector de selecciones (puede incluir "1..", "2..")
#' @param todos_los_cacs Vector con todos los CACs disponibles
#' @return Vector con todos los CACs expandidos
expandir_seleccion_cac <- function(seleccion, todos_los_cacs) {
  if (is.null(seleccion) || length(seleccion) == 0) return(NULL)
  if (any(seleccion == "")) return(NULL) # "Todos"
  
  # Identificar grupos (terminan en ..)
  es_grupo <- grepl("\\.\\.$", seleccion)
  
  if (!any(es_grupo)) {
    return(seleccion)
  }
  
  grupos <- seleccion[es_grupo]
  individuales <- seleccion[!es_grupo]
  
  cacs_expandidos <- individuales
  
  for (grupo in grupos) {
    prefijo <- substr(grupo, 1, nchar(grupo) - 2) # Quitar ".."
    matches <- todos_los_cacs[startsWith(as.character(todos_los_cacs), prefijo)]
    cacs_expandidos <- c(cacs_expandidos, matches)
  }
  
  return(unique(cacs_expandidos))
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
      # Expandir selección si hay grupos
      subcacs_expandidos <- expandir_seleccion_cac(subcacs_origen, unique(datos_filtrados$ZCT_SUBCAC_E))
      datos_filtrados <- datos_filtrados %>% filter(ZCT_SUBCAC_E %in% subcacs_expandidos)
    }
  } else if (!is.null(cacs_origen)) {
    # Expandir selección si hay grupos
    cacs_expandidos <- expandir_seleccion_cac(cacs_origen, unique(datos_filtrados$ZACT_E))
    datos_filtrados <- datos_filtrados %>% filter(ZACT_E %in% cacs_expandidos)
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
# 6. FUNCIONES DE EXTRACCIÓN DE MOVIMIENTOS - VERSIÓN CORREGIDA
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
  # Proteger contra NAs antes de substr
  subcac_seguro <- ifelse(tiene_subcac, datos$ZCT_SUBCAC_E, "")
  cac_padre <- substr(subcac_seguro, 1, 3)
  
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
  
  # ---- 1. NO MODIFICAR movimientos_fase AQUÍ ----
  # La agregación se aplica solo a columnas específicas según sea necesario
  
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
          
          # Proteger substr contra NAs
          origen_base_seguro <- ifelse(is.na(origen_base), "", origen_base)
          cac_padre <- substr(origen_base_seguro, 1, 3)
          origen <- ifelse(necesita_acumulacion & nchar(origen_base_seguro) == 4,
                          cac_padre,
                          origen_base_seguro)
          
          if (debug && any(necesita_acumulacion, na.rm = TRUE)) {
            n_acum <- sum(necesita_acumulacion, na.rm = TRUE)
            message(sprintf("     [DEBUG] Fase 1: %d registros SUBCAC acumulados a CAC (requieren acumulación previa)", n_acum))
          }
        } else {
          # Proteger contra NAs incluso sin estrategia
          origen <- ifelse(is.na(origen_base), "", origen_base)
        }
      } else {
        # Proteger contra NAs
        origen <- ifelse(is.na(movimientos_fase$ZACT_E), "", movimientos_fase$ZACT_E)
      }
    } else {
      # Para CAC, CAC1, CAC2: usar ZACT_E agregado (agregar_cac ya protege contra NAs)
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
        # Proteger contra NAs
        origen_previo_seguro <- ifelse(is.na(origen_previo), "", origen_previo)
        origen <- ifelse(
          necesita_acumulacion_ahora & nchar(origen_previo_seguro) == 4,
          substr(origen_previo_seguro, 1, 3),  # Acumular a CAC
          origen_previo_seguro  # Mantener origen anterior
        )
        
        if (debug) {
          n_acum <- sum(necesita_acumulacion_ahora & nchar(origen_previo_seguro) == 4, na.rm = TRUE)
          if (n_acum > 0) {
            message(sprintf("     [DEBUG] Fase %d: %d registros SUBCAC acumulados a CAC antes de movimiento RE34/SD", 
                           fase_num, n_acum))
          }
        }
      } else {
        # Proteger contra NAs
        origen <- ifelse(is.na(origen_previo), "", origen_previo)
      }
    } else {
      # Proteger contra NAs
      origen <- ifelse(is.na(origen_previo), "", origen_previo)
    }
  }
  
  if (debug && fase_num == 1) {
    # Proteger contra NAs antes de nchar
    origen_para_debug <- ifelse(is.na(origen), "", origen)
    n_subcac <- sum(nchar(origen_para_debug) == 4)
    n_cac <- sum(nchar(origen_para_debug) == 3)
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
  # Proteger contra NAs
  origen_seguro <- ifelse(is.na(origen), "", origen)
  origen_cac <- ifelse(nchar(origen_seguro) == 4, substr(origen_seguro, 1, 3), origen_seguro)
  
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
        # Proteger contra NAs
        origen_para_debug <- ifelse(is.na(origen), "", origen)
        n_desde_subcac <- sum(idx_re12 & nchar(origen_para_debug) == 4)
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
        # Proteger contra NAs
        origen_para_debug <- ifelse(is.na(origen), "", origen)
        n_desde_subcac <- sum(idx_re34 & nchar(origen_para_debug) == 4)
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
        # Proteger contra NAs
        origen_para_debug <- ifelse(is.na(origen), "", origen)
        n_desde_subcac <- sum(idx_sd & nchar(origen_para_debug) == 4)
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
  
  # ---- 5. NO ELIMINAR FASES INTERMEDIAS ----
  
  # IMPORTANTE: NO eliminamos las fases intermedias aunque no estén en fases_incluir
  # porque los módulos de Sankey y Matriz necesitan la cadena completa de movimientos
  # desde fase_0 hasta la última fase procesada.
  # 
  # Por ejemplo, si fases_incluir = [2, 3]:
  # - Necesitamos: fase_0, fase_1, fase_2, fase_3
  # - NO podemos eliminar fase_1 porque es el puente entre fase_0 y fase_2
  #
  # Solo eliminaríamos fases que están DESPUÉS de max(fases_incluir), pero como
  # solo procesamos hasta max_fase, ya no existen columnas posteriores.
  
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
# 7. FUNCIONES AUXILIARES DE UTILIDAD
# =============================================================================

#' Pipeline completo de carga y limpieza
preparar_datos_reparto <- function(..., archivo_especifico = NULL) {
  datos_raw <- carga_datos_reparto(..., archivo_especifico = archivo_especifico)
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
