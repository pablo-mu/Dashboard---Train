library(dplyr)
library(dtplyr)
library(data.table)



COLUMNAS_REPARTO <- c(
  ZCENT_GEST = "character",
  ZANYO = "integer",
  ZMES = "integer",
  ZIND = "integer",
  ZFASE = "character",
  ZCG_CRC = "character",
  ZCT_SUBCAC_E = "character",
  ZACT_E = "character",
  ZFASE_RE12 = "character",
  ZCG_CRC_RE12 = "character",
  ZACT_RE12 = "character",
  ZFASE_RE34 = "character",
  ZCG_CRC_RE34 = "character",
  ZACT_RE34 = "character",
  ZPARAM_RE34 = "character",
  ZFASE_F1SD = "character",
  ZCG_CRC_F1SD = "character",
  ZACT_F1SD = "character",
  ZPARAM_F1SD = "character",
  ZFASE_2RE12 = "character",
  ZCG_CRC_2RE12 = "character",
  ZACT_2RE12 = "character",
  ZCG_CRC_2RE34 = "character",
  ZFASE_2RE34 = "character",
  ZACT_2RE34 = "character",
  ZPARAM_2RE34 = "character",
  ZFASE_F2SD = "character",
  ZCG_CRC_F2SD = "character",
  ZACT_F2SD = "character",
  ZPARAM_F2SD = "character",
  ZFASE_3RE12 = "character",
  ZCG_CRC_3RE12 = "character",
  ZACT_3RE12 = "character",
  ZCG_CRC_3RE34 = "character",
  ZACT_3RE34 = "character",
  ZPARAM_3RE34 = "character",
  ZFASE_3RE34 = "character",
  ZCG_CRC_F3SD = "character",
  ZACT_F3SD = "character",
  ZPARAM_F3SD = "character",
  ZFASE_F3SD = "character",
  ZC_ODC = "character",
  ZTA_NOC = "integer",
  ZTA_NOC2 = "integer",
  ZIMPORT = "double",
  ZMONEDA = "character",
  V_ORIGEN_D = "character"
)

COLUMNAS_IGNORADAS <- c("ZTA_NOC2", "V_ORIGEN_D", "ZTA_NOC", "ZMONEDA", "ZIND",
  "ZCG_CRC", "ZCG_CRC_RE12", "ZCG_CRC_RE34", "ZCG_CRC_2RE12", "ZCG_CRC_2RE34", "ZCG_CRC_3RE12", "ZCG_CRC_3RE34",
  "ZCG_CRC_F1SD", "ZCG_CRC_F2SD", "ZCG_CRC_F3SD", "ZFASE", "ZFASE_RE12", "ZFASE_RE34",
  "ZFASE_F1SD", "ZFASE_2RE12", "ZFASE_2RE34", "ZFASE_F2SD", "ZFASE_3RE12", "ZFASE_3RE34",
  "ZFASE_F3SD", "ZC_ODC")

COLUMNAS_LECTURA <- COLUMNAS_REPARTO[!names(COLUMNAS_REPARTO) %in% COLUMNAS_IGNORADAS]

FASE_0 <- c("ZCT_SUBCAC_E", "ZACT_E")
FASE_1 <- c("ZACT_RE12", "ZACT_RE34", "ZACT_F1SD", "ZPARAM_RE34", "ZPARAM_F1SD")
FASE_2 <- c("ZACT_2RE12", "ZACT_2RE34", "ZACT_F2SD", "ZPARAM_2RE34", "ZPARAM_F2SD")
FASE_3 <- c("ZACT_3RE12", "ZACT_3RE34", "ZACT_F3SD", "ZPARAM_3RE34", "ZPARAM_F3SD")


#' Carga datos de reparto desde archivos CSV
#'
#' Busca archivos CSV en múltiples rutas posibles y los carga,
#' aplicando limpieza básica de formato.
#'
#' @param rutas Vector de rutas donde buscar archivos CSV.
#'              Por defecto: c(".data", "./data", "data", "./.data")
#' @param sep Separador de columnas. Por defecto: ";"
#' @param encodings Vector de encodings a probar. Por defecto: c("UTF-8")
#' @param separador_miles Caracter usado como separador de miles. Defecto: ","
#' @param columnas_lectura Vector con nombres de las columnas a leer y sus tipo.
#' @return lazy dt con los datos cargados y limpiados.
#'
carga_datos_reparto <- function(
  rutas = c(".data", "./data", "data", "./.data"),
  sep = ";",
  encodings = c("UTF-8", "latin1", "ISO-8859-1", "windows-1252"),
  separador_miles = ",",
  columnas_lectura = COLUMNAS_LECTURA
){
  archivos_encontrados <- NULL
  ruta_encontrada <- NULL

  for (ruta in rutas){
    if (dir.exists(ruta)){
      archivos <- list.files(ruta,
        pattern = "\\.csv$",
        full.names = TRUE,
        ignore.case = TRUE
      )
      if (length(archivos) > 0){
        archivos_encontrados <- archivos
        ruta_encontrada <- ruta
        break
      }
    }
  }

  if (is.null(archivos_encontrados) || length(archivos_encontrados) == 0){
    warning("No se encontraron archivos CSV en las rutas especificadas.")
    return(NULL)
  }

  message(sprintf("✓ Encontrados %d archivo(s) CSV en: %s",
                  length(archivos_encontrados), ruta_encontrada))

  # Leemos y combinamos los archivos encontrados
  datos_list <- lapply(archivos_encontrados, function(archivo){
    ultimo_error <- NULL
    
    for (enc in encodings){
      resultado <- try({
        # Leer CSV FORZANDO tipos de columnas según columnas_lectura
        # Esto evita que CACs como "800" se lean como números
        datos <- read.csv(
          archivo,
          sep = sep,
          stringsAsFactors = FALSE,
          fileEncoding = enc,
          check.names = FALSE,  # No modificar nombres de columnas
          colClasses = "character"  # LEER TODO COMO CARACTERES inicialmente
        )
        
        # Limpiar nombres de columnas (quitar espacios)
        names(datos) <- trimws(names(datos))
        
        # Seleccionar solo las columnas definidas en columnas_lectura
        columnas_disponibles <- names(columnas_lectura)
        columnas_faltantes <- setdiff(columnas_disponibles, names(datos))
        columnas_a_seleccionar <- intersect(columnas_disponibles, names(datos))
        
        if (length(columnas_faltantes) > 0) {
          warning(sprintf("Columnas no encontradas en '%s': %s", 
                          basename(archivo), 
                          paste(columnas_faltantes, collapse = ", ")))
        }
        
        # Filtrar solo las columnas que queremos
        datos <- datos[, columnas_a_seleccionar, drop = FALSE]
        
        # CORRECCIÓN: Reemplazar el caso específico "8,00E+02" o "8.00E+02" por "8E2"
        # Solo para este caso particular que causa problemas
        columnas_cac <- c("ZACT_E", "ZACT_RE12", "ZACT_RE34", "ZACT_F1SD",
                          "ZACT_2RE12", "ZACT_2RE34", "ZACT_F2SD",
                          "ZACT_3RE12", "ZACT_3RE34", "ZACT_F3SD")
        
        for (col_cac in columnas_cac) {
          if (col_cac %in% names(datos)) {
            # Buscar y reemplazar solo "8,00E+02" o "8.00E+02" (case insensitive)
            n_reemplazos <- sum(grepl("^8[,\\.]00E\\+0?2$", datos[[col_cac]], ignore.case = TRUE))
            
            if (n_reemplazos > 0) {
              datos[[col_cac]] <- gsub("^8[,\\.]00E\\+0?2$", "8E2", datos[[col_cac]], ignore.case = TRUE)
              message(sprintf("  ⚠ Corregidos %d valores '8,00E+02' → '8E2' en columna '%s'",
                              n_reemplazos, col_cac))
            }
          }
        }
        
        # Ahora convertir tipos según columnas_lectura
        for (col_name in names(datos)) {
          tipo_esperado <- columnas_lectura[col_name]
          
          if (tipo_esperado == "integer") {
            datos[[col_name]] <- as.integer(datos[[col_name]])
          } else if (tipo_esperado == "double") {
            # Para ZIMPORT: manejar formato europeo de números
            datos[[col_name]] <- gsub("\\s+", "", datos[[col_name]])  # Quitar espacios
            datos[[col_name]] <- gsub("\\.", "", datos[[col_name]])   # Quitar separador de miles
            datos[[col_name]] <- gsub(",", ".", datos[[col_name]])    # Cambiar coma decimal a punto
            datos[[col_name]] <- as.numeric(datos[[col_name]])
            datos[[col_name]][is.na(datos[[col_name]])] <- 0
          }
          # Para "character" no hacemos nada, ya es character
        }
        
        # Reemplazar NA con cadenas vacías en columnas de caracteres
        datos <- datos %>%
          mutate(across(where(is.character), ~replace(., is.na(.), "")))
        
        message(sprintf("✓ Archivo '%s' leído con encoding '%s' (%s filas, %s columnas seleccionadas)",
                        basename(archivo), enc, 
                        formatC(nrow(datos), format = "d", big.mark = " "),
                        ncol(datos))
        )
        return(datos)
      }, silent = TRUE)
      
      if (!inherits(resultado, "try-error")) {
        return(resultado)
      } else {
        ultimo_error <- attr(resultado, "condition")$message
      }
    }
    
    # Si llegamos aquí, ningún encoding funcionó
    warning(
      sprintf("No se pudo leer el archivo '%s' con los encodings: %s\nÚltimo error: %s",
              basename(archivo), 
              paste(encodings, collapse = ", "),
              if (!is.null(ultimo_error)) ultimo_error else "desconocido")
    )
    NULL
  })

  if (length(datos_list) == 0) {
    warning("No se pudo cargar ningún archivo CSV")
    return(NULL)
  }

  # Filtrar elementos NULL
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

#' Limpia y transforma los datos de reparto.
#' Agrupa los datos por las columnas de reparto y suma los importes.
#' @param datos Data frame con los datos de reparto a limpiar.
#' @return Data frame limpio y transformado.
#' 
limpiar_datos_reparto <- function(datos){
  
  if(is.null(datos) || nrow(datos) == 0){
    warning("No hay datos para limpiar")
    return(NULL)
  }

  # Obtenemos las columnas para agrupar (todas excepto ZIMPORT)
  columnas_agrupacion <- setdiff(names(datos), "ZIMPORT")
  
  #Reemplazamos valores NA en zimport por 0
  #Acumulamos los datos por cada combinación de reparto

  datos_limpios <- datos %>%
    dplyr::mutate(
        ZIMPORT = ifelse(is.na(ZIMPORT), 0, ZIMPORT)
    ) %>%
    dplyr::group_by(across(all_of(columnas_agrupacion))) %>%
    dplyr::summarise(
      ZIMPORT = sum(ZIMPORT, na.rm = TRUE),
      .groups = "drop"
    )

  message(sprintf("✓ Datos limpiados y agrupados: %s registros finales", 
                  formatC(nrow(datos_limpios), format = "d", big.mark = " ")))
  return(datos_limpios)
}


#' Pipeline completo de carga y limpieza
#' 
#' Ejecuta la carga y limpieza de datos en un solo paso.
#' 
#' @param ... Argumentos pasados a cargar_datos_reparto()
#' 
#' @return data.frame con datos listos para análisis
#' 
#' @export
#' @examples
#' datos <- preparar_datos_reparto()
preparar_datos_reparto <- function(...) {
  datos_raw <- carga_datos_reparto(...)
  datos_limpios <- limpiar_datos_reparto(datos_raw)
  return(datos_limpios)
}


# =============================================================================
# 3. FILTRADO DE DATOS
# =============================================================================

#' Filtrar datos de reparto por criterios
#' 
#' Aplica filtros múltiples a los datos de reparto.
#' 
#' @param datos data.frame con datos de reparto
#' @param centro_gestor Centro gestor a filtrar. NULL = todos
#' @param cacs_origen Vector de CACs de origen. NULL = todos
#' @param subcacs_origen Vector de SUBCACs de origen. NULL = todos
#' @param mes vector de meses a filtrar. NULL = todos
#' @param anyo vector de años a filtrar. NULL = todos
#' @param nivel_agregacion "cac" (3 dígitos), "cac2" (2 dígitos), "cac1" (1 dígito) o "subcac"
#' 
#' @return data.frame filtrado
#' 
#' @export
#' @examples
#' datos_filtrados <- filtrar_datos_reparto(
#'   datos, 
#'   centro_gestor = "CENTRO_001",
#'   cacs_origen = c("CAC001", "CAC002")
#' )
filtrar_datos_reparto <- function(datos,
                                  centro_gestor = NULL,
                                  cacs_origen = NULL,
                                  subcacs_origen = NULL,
                                  mes = NULL,
                                  anyo = NULL,
                                  nivel_agregacion = "cac") {
  
  if (is.null(datos)) return(NULL)
  
  datos_filtrados <- datos
  
  # Filtro por centro gestor
  if (!is.null(centro_gestor) && centro_gestor != "") {
    datos_filtrados <- datos_filtrados %>%
      filter(ZCENT_GEST == centro_gestor)
  }
  
  # Filtro por origen según nivel de agregación
  if (nivel_agregacion == "subcac" && !is.null(subcacs_origen)) {
    if ("ZCT_SUBCAC_E" %in% names(datos_filtrados)) {
      datos_filtrados <- datos_filtrados %>%
        filter(ZCT_SUBCAC_E %in% subcacs_origen)
    }
  } else if (!is.null(cacs_origen)) {
    datos_filtrados <- datos_filtrados %>%
      filter(ZACT_E %in% cacs_origen)
  }
  
  # Filtro por mes
  if (!is.null(mes) && "ZMES" %in% names(datos_filtrados)) {
    datos_filtrados <- datos_filtrados %>%
    filter(ZMES %in% mes)
  }

  # Filtro por año
  if (!is.null(anyo) && "ZANYO" %in% names(datos_filtrados)) {
    datos_filtrados <- datos_filtrados %>%
    filter(ZANYO %in% anyo)
  }
  
  # Nota: No usamos lazy_dt aquí para evitar problemas de materialización
  # Los datos ya vienen filtrados como data.frame normal
  
  return(datos_filtrados)
}