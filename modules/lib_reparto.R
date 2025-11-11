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

COLUMNAS_CAC <- c("ZCT_SUBCAC_E", "ZACT_E", "ZACT_RE12", "ZACT_RE34", 
                  "ZACT_2RE12", "ZACT_2RE34", "ZACT_3RE12", "ZACT_3RE34")

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

agregar_cac <- function(cac_vector, nivel) {
  if (nivel == "cac1") return(substr(cac_vector, 1, 1))
  if (nivel == "cac2") return(substr(cac_vector, 1, 2))
  return(cac_vector) # "cac" o "subcac" mantienen valor original
}


extraer_fase <- function(datos, fase_num){
  # Extraer dataframe origen y destino
  fase_config <- CONFIG_FASES[[as.character(fase_num)]]
  fase_actual <- paste0("fase_", fase_num)
  fase_previa <- paste0("fase_", fase_num - 1)

  if (is.null(fase_config)) {
    stop(sprintf("Fase %d no está configurada", fase_num))
  }

  # Crear dataframe de movimientos para la fase
  # Seleccionamos meta, columnas de la fase previa (fase - 1) y columnas actuales
  # incluir también fase 0 si fase != 1. 
  movimientos_fase <- datos %>%
    select(
      all_of(COLUMNAS_BASE$meta),
      all_of(COLUMNAS_BASE$fase_0),
      all_of(if (fase_num > 1) COLUMNAS_BASE[[fase_previa]] else NULL),
      all_of(COLUMNAS_BASE[[fase_actual]])
    )
  return(movimientos_fase)
}

extraer_movimientos_fase <- function(movimientos_fase, fase, nivel_agregacion = "cac", 
  incluir_tipo_param = TRUE){
  
  # Preparar columnas destino según nivel de agregación 
  if (nivel_agregacion %in% c("cac1", "cac2")){
    # Cambiamos agrupación de CACs en las columnas de movimientos_fase
    # que estén en columnas CAC
    columnas_cac <- grep("^ZACT_", names(movimientos_fase), value = TRUE)
    for (col in columnas_cac) {
      movimientos_fase[[col]] <- agregar_cac(movimientos_fase[[col]], nivel_agregacion)
    }
  }
  elif (nivel_agregacion == "subcac"){
    # Para subcac, la fase 0 ya está creada correctamente
    # Quitamos la columna ZACT_E si existe y cambiamos ZCT_SUBCAC_E a origen
    if ("ZACT_E" %in% names(movimientos_fase)){
      movimientos_fase <- movimientos_fase %>% select(-ZACT_E)
    }
    # Renombrar ZCT_SUBCAC_E a origen
    movimientos_fase <- movimientos_fase %>%
      rename(ORIGEN = ZCT_SUBCAC_E)
  }
  else {
    # Nivel cac completo
    # Renombrar ZACT_E a ORIGEN
    movimientos_fase <- movimientos_fase %>%
      rename(ORIGEN = ZACT_E) %>%
      select(-ZCT_SUBCAC_E)
  }
  }




extraer_movimientos <- function(
  datos,
  nivel_agregacion,
  fases_incluir = NULL,
  incluir_tipo_param = TRUE,
  verbose = TRUE
){
  if (is.null(datos) || nrow(datos) == 0) {
    warning("No hay datos para extraer movimientos")
    return(NULL)
  }
  
  # Convertir a data.table
  datos_dt <- if (!is.data.table(datos)) as.data.table(datos) else copy(datos)
  
  # Filtrar registros válidos
  datos_dt <- datos_dt[!is.na(ZIMPORT) & ZIMPORT != 0]
  if (nrow(datos_dt) == 0) {
    warning("No hay registros con importe válido")
    return(NULL)
  }
  
  # Determinar fases a procesar
  fases_a_procesar <- if (is.null(fases_incluir)) 1:3 else 1:max(fases_incluir)
  
  if (verbose) {
    message(sprintf("Extrayendo movimientos para %s registros, fases: %s", 
                    formatC(nrow(datos_dt), format = "d", big.mark = " "),
                    paste(fases_a_procesar, collapse = ", ")))
  }
}