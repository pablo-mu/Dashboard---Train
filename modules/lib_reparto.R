library(dplyr)
library(dtplyr)
library(data.table)
library(readr)

# =============================================================================
# 1. CONFIGURACIÓN DE COLUMNAS
# =============================================================================

COLUMNAS_BASE <- list(
  meta   = c("ZCENT_GEST", "ZANYO", "ZMES", "ZIMPORT"),
  fase_0 = c("ZCT_SUBCAC_E", "ZACT_E"),
  fase_1 = c("ZACT_RE12",  "ZACT_RE34",  "ZACT_F1SD",  "ZPARAM_RE34",  "ZPARAM_F1SD"),
  fase_2 = c("ZACT_2RE12", "ZACT_2RE34", "ZACT_F2SD",  "ZPARAM_2RE34", "ZPARAM_F2SD"),
  fase_3 = c("ZACT_3RE12", "ZACT_3RE34", "ZACT_F3SD",  "ZPARAM_3RE34", "ZPARAM_F3SD")
)

# Columnas necesarias para cálculos (se procesan siempre que estén disponibles)
COLUMNAS_LECTURA <- c(
  COLUMNAS_BASE$meta,
  COLUMNAS_BASE$fase_0,
  COLUMNAS_BASE$fase_1,
  COLUMNAS_BASE$fase_2,
  COLUMNAS_BASE$fase_3
)

# Mínimo obligatorio para que la app funcione
COLUMNAS_ESENCIALES <- c("ZCENT_GEST", "ZANYO", "ZMES", "ZIMPORT", "ZACT_E")

TIPOS_COLUMNAS <- c(
  ZCENT_GEST = "character", ZANYO = "integer", ZMES = "integer", ZIMPORT = "double",
  ZCT_SUBCAC_E = "character", ZACT_E = "character",
  ZACT_RE12 = "character", ZACT_RE34 = "character", ZACT_F1SD = "character",
  ZPARAM_RE34 = "character", ZPARAM_F1SD = "character",
  ZACT_2RE12 = "character", ZACT_2RE34 = "character", ZACT_F2SD = "character",
  ZPARAM_2RE34 = "character", ZPARAM_F2SD = "character",
  ZACT_3RE12 = "character", ZACT_3RE34 = "character", ZACT_F3SD = "character",
  ZPARAM_3RE34 = "character", ZPARAM_F3SD = "character"
)

CONFIG_FASES <- list(
  "1" = list(re12 = "ZACT_RE12",  re34 = "ZACT_RE34",  sd = "ZACT_F1SD",
             param_re34 = "ZPARAM_RE34",  param_sd = "ZPARAM_F1SD"),
  "2" = list(re12 = "ZACT_2RE12", re34 = "ZACT_2RE34", sd = "ZACT_F2SD",
             param_re34 = "ZPARAM_2RE34", param_sd = "ZPARAM_F2SD"),
  "3" = list(re12 = "ZACT_3RE12", re34 = "ZACT_3RE34", sd = "ZACT_F3SD",
             param_re34 = "ZPARAM_3RE34", param_sd = "ZPARAM_F3SD")
)

# =============================================================================
# 2. FUNCIONES DE VALIDACIÓN Y UTILIDAD
# =============================================================================

#' Verifica que columnas requeridas existen; lanza error informativo si no
validar_columnas <- function(datos, columnas_req, nombre_fn = "función") {
  if (is.null(datos)) stop(sprintf("[%s] 'datos' es NULL.", nombre_fn))
  faltantes <- setdiff(columnas_req, names(datos))
  if (length(faltantes) > 0) {
    stop(sprintf("[%s] Columnas requeridas faltantes: %s",
                 nombre_fn, paste(faltantes, collapse = ", ")))
  }
  invisible(TRUE)
}

#' Convierte a data.table sin copia innecesaria
as_dt <- function(x) {
  if (is.data.table(x)) x else as.data.table(x)
}

#' Aplica transformación de tipos a las columnas conocidas
aplicar_tipos_columnas <- function(datos) {
  for (col_name in names(datos)) {
    tipo <- TIPOS_COLUMNAS[col_name]
    if (is.na(tipo)) next  # columna extra → no tocar
    if (tipo == "integer") {
      datos[[col_name]] <- suppressWarnings(as.integer(datos[[col_name]]))
    } else if (tipo == "double") {
      v <- gsub("\\s+", "", as.character(datos[[col_name]]))
      v <- gsub("\\.", "", v)
      v <- gsub(",", ".", v)
      v <- suppressWarnings(as.numeric(v))
      v[is.na(v)] <- 0
      datos[[col_name]] <- v
    }
    # character → sin cambios
  }
  datos
}

#' Corrige notación científica problemática en columnas CAC
corregir_valores_cac <- function(datos) {
  columnas_cac <- grep("^ZACT_", names(datos), value = TRUE)
  for (col in columnas_cac) {
    n <- sum(grepl("^8[,\\.]00E\\+0?2$", datos[[col]], ignore.case = TRUE))
    if (n > 0) {
      datos[[col]] <- gsub("^8[,\\.]00E\\+0?2$", "8E2", datos[[col]], ignore.case = TRUE)
      message(sprintf("  ⚠ Corregidos %d valores '8,00E+02' → '8E2' en %s", n, col))
    }
  }
  datos
}

#' Agrega CACs según nivel
agregar_cac <- function(cac_vector, nivel) {
  cac_vector <- ifelse(is.na(cac_vector), "", cac_vector)
  if (nivel == "cac1") return(substr(cac_vector, 1, 1))
  if (nivel == "cac2") return(substr(cac_vector, 1, 2))
  cac_vector
}

#' Crea columna fase_0 según nivel de agregación
crear_fase_origen <- function(datos, nivel_agregacion) {
  if (nivel_agregacion == "subcac") {
    datos$fase_0 <- ifelse(
      !is.na(datos$ZCT_SUBCAC_E) & datos$ZCT_SUBCAC_E != "",
      datos$ZCT_SUBCAC_E, datos$ZACT_E
    )
  } else {
    datos$fase_0 <- agregar_cac(datos$ZACT_E, nivel_agregacion)
  }
  datos
}

# =============================================================================
# 3. DETECCIÓN Y TRANSFORMACIÓN DE ARCHIVOS SIE
# =============================================================================

#' Detecta si un archivo es formato SIE
es_formato_sie <- function(nombres_columnas) {
  columnas_sie <- c("CENTRO","ANYO","MESCF","CACCF","CACA0","CACA1","CACA2","CACA3","IMPORTE")
  sum(columnas_sie %in% nombres_columnas) >= 7
}

#' Transforma datos SIE al formato estándar (sin mensajes DEBUG en producción)
transformar_sie_a_estandar <- function(datos_sie, debug = FALSE) {
  if (debug) message(sprintf("  [SIE] Iniciando transformación: %d filas", nrow(datos_sie)))

  # Convertir IMPORTE de forma robusta
  importe_raw  <- as.character(datos_sie$IMPORTE)
  importe_limpio <- gsub("\\s+", "", importe_raw)
  importe_limpio <- gsub("\\.", "", importe_limpio)
  importe_limpio <- gsub(",", ".", importe_limpio)
  importe_numerico <- suppressWarnings(as.numeric(importe_limpio))
  importe_numerico[is.na(importe_numerico)] <- 0

  caca0 <- as.character(datos_sie$CACA0)
  caca1 <- as.character(datos_sie$CACA1)
  caca2 <- as.character(datos_sie$CACA2)
  caca3 <- as.character(datos_sie$CACA3)

  datos_estandar <- data.frame(
    ZCENT_GEST    = as.character(datos_sie$CENTRO),
    ZANYO         = suppressWarnings(as.integer(datos_sie$ANYO)),
    ZMES          = suppressWarnings(as.integer(datos_sie$MESCF)),
    ZIMPORT       = importe_numerico,
    ZCT_SUBCAC_E  = as.character(datos_sie$CACCF),
    ZACT_E        = caca0,
    # Fase 1
    ZACT_RE12     = caca0, ZACT_RE34 = caca0, ZACT_F1SD = caca1,
    ZPARAM_RE34   = "NO PARAM", ZPARAM_F1SD = "SIE",
    # Fase 2
    ZACT_2RE12    = caca1, ZACT_2RE34 = caca1, ZACT_F2SD = caca2,
    ZPARAM_2RE34  = "NO PARAM", ZPARAM_F2SD = "SIE",
    # Fase 3
    ZACT_3RE12    = caca2, ZACT_3RE34 = caca2, ZACT_F3SD = caca3,
    ZPARAM_3RE34  = "NO PARAM", ZPARAM_F3SD = "SIE",
    stringsAsFactors = FALSE
  )

  # Limpiar NAs en columnas character
  cols_char <- names(datos_estandar)[sapply(datos_estandar, is.character)]
  for (col in cols_char) {
    datos_estandar[[col]][is.na(datos_estandar[[col]])] <- ""
  }

  message(sprintf("  ✓ Transformación SIE: %s registros, importe total: %s €",
                  formatC(nrow(datos_estandar), format = "d", big.mark = " "),
                  formatC(sum(datos_estandar$ZIMPORT), format = "f",
                          big.mark = " ", decimal.mark = ",", digits = 2)))
  datos_estandar
}

# =============================================================================
# 4. LECTURA DE ARCHIVOS CSV — FLEXIBLE (acepta columnas extra)
# =============================================================================

#' Lee un archivo CSV detectando formato automáticamente.
#' Mantiene TODAS las columnas del archivo; solo procesa las conocidas.
#'
#' @param archivo Ruta al archivo
#' @param sep Separador
#' @param encodings Vector de encodings a probar
#' @return data.frame con columnas de cálculo procesadas + columnas extra sin tocar
leer_archivo_csv_auto <- function(archivo, sep, encodings) {
  nombre <- basename(archivo)

  for (enc in encodings) {
    resultado <- tryCatch({
      datos <- read_delim(
        archivo, delim = sep,
        locale = locale(encoding = enc, decimal_mark = ",", grouping_mark = ""),
        col_types = cols(.default = "c"),
        trim_ws = TRUE, show_col_types = FALSE
      )
      datos <- as.data.frame(datos, stringsAsFactors = FALSE)
      names(datos) <- trimws(names(datos))

      # ---- Detección de formato ----
      if (es_formato_sie(names(datos))) {
        message(sprintf("✓ Archivo SIE detectado: '%s' (encoding '%s')", nombre, enc))
        return(transformar_sie_a_estandar(datos))
      }

      # ---- Formato estándar ----
      cols_calc  <- intersect(COLUMNAS_LECTURA, names(datos))
      cols_extra <- setdiff(names(datos), COLUMNAS_LECTURA)

      # Validar mínimo de columnas esenciales
      esenciales_disponibles <- intersect(COLUMNAS_ESENCIALES, cols_calc)
      if (length(esenciales_disponibles) < length(COLUMNAS_ESENCIALES)) {
        faltantes <- setdiff(COLUMNAS_ESENCIALES, cols_calc)
        warning(sprintf("'%s': columnas esenciales faltantes: %s",
                        nombre, paste(faltantes, collapse = ", ")))
        return(NULL)
      }

      if (length(cols_extra) > 0) {
        message(sprintf("  ℹ '%s': %d columna(s) extra conservadas (solo lectura): %s",
                        nombre, length(cols_extra),
                        paste(head(cols_extra, 5), collapse = ", "),
                        if (length(cols_extra) > 5) "..." else ""))
      }

      # Procesar columnas de cálculo
      datos_calc <- datos[, cols_calc, drop = FALSE]
      datos_calc <- corregir_valores_cac(datos_calc)
      datos_calc <- aplicar_tipos_columnas(datos_calc)

      # Limpiar NAs en columnas character de cálculo
      for (col in cols_calc) {
        if (is.character(datos_calc[[col]])) {
          datos_calc[[col]][is.na(datos_calc[[col]])] <- ""
        }
      }

      if (!"ZCT_SUBCAC_E" %in% names(datos_calc)) {
        datos_calc$ZCT_SUBCAC_E <- ""
      } else {
        datos_calc$ZCT_SUBCAC_E[is.na(datos_calc$ZCT_SUBCAC_E)] <- ""
      }

      # Adjuntar columnas extra al resultado
      if (length(cols_extra) > 0) {
        datos_extra <- datos[, cols_extra, drop = FALSE]
        datos_resultado <- cbind(datos_calc, datos_extra)
        attr(datos_resultado, "columnas_extra") <- cols_extra
      } else {
        datos_resultado <- datos_calc
      }

      message(sprintf("✓ Archivo estándar: '%s' (encoding '%s', %s filas%s)",
                      nombre, enc,
                      formatC(nrow(datos_resultado), format = "d", big.mark = " "),
                      if (length(cols_extra) > 0)
                        sprintf(", %d cols. extra", length(cols_extra)) else ""))
      return(datos_resultado)

    }, error = function(e) {
      structure(class = "try-error", conditionMessage(e))
    })

    if (!inherits(resultado, "try-error")) return(resultado)
  }

  warning(sprintf("No se pudo leer el archivo '%s' con ningún encoding", nombre))
  NULL
}

# =============================================================================
# 5. CARGA DE DATOS
# =============================================================================

#' Lista archivos CSV por tipo (CASA / SIE)
listar_archivos_reparto <- function(tipo = "CASA") {
  rutas_base <- c(".data", "./data", "data", "./.data")
  archivos <- c()
  for (ruta in file.path(rutas_base, tipo)) {
    if (dir.exists(ruta)) {
      found <- list.files(ruta, pattern = "\\.csv$", full.names = TRUE, ignore.case = TRUE)
      if (length(found) > 0)
        archivos <- c(archivos, normalizePath(found, winslash = "/", mustWork = FALSE))
    }
  }
  unique(archivos)
}

#' Carga datos de reparto desde archivos CSV
carga_datos_reparto <- function(
  rutas = c(".data", "./data", "data", "./.data"),
  archivo_especifico = NULL,
  sep = ";",
  encodings = c("UTF-8", "latin1", "ISO-8859-1", "windows-1252")
) {
  # Resolver archivos a leer
  if (!is.null(archivo_especifico) && nchar(trimws(archivo_especifico)) > 0) {
    if (!file.exists(archivo_especifico)) {
      warning(sprintf("El archivo especificado no existe: %s", archivo_especifico))
      return(NULL)
    }
    archivos <- archivo_especifico
    message(sprintf("✓ Cargando archivo específico: %s", basename(archivo_especifico)))
  } else {
    archivos <- NULL
    for (ruta in rutas) {
      if (dir.exists(ruta)) {
        found <- list.files(ruta, pattern = "\\.csv$", full.names = TRUE, ignore.case = TRUE)
        if (length(found) > 0) { archivos <- found; break }
      }
    }
    if (is.null(archivos)) {
      warning("No se encontraron archivos CSV en las rutas especificadas.")
      return(NULL)
    }
    message(sprintf("✓ Encontrados %d archivo(s) CSV", length(archivos)))
  }

  # Leer archivos
  datos_list <- lapply(archivos, leer_archivo_csv_auto, sep = sep, encodings = encodings)
  datos_list <- Filter(Negate(is.null), datos_list)

  if (length(datos_list) == 0) {
    warning("No se pudo cargar ningún archivo CSV válido.")
    return(NULL)
  }

  datos_combinados <- tryCatch(
    dplyr::bind_rows(datos_list),
    error = function(e) {
      warning(sprintf("Error al combinar archivos: %s. Intentando rbindlist...", e$message))
      as.data.frame(data.table::rbindlist(datos_list, fill = TRUE))
    }
  )

  message(sprintf("✓ Total combinado: %s registros",
                  formatC(nrow(datos_combinados), format = "d", big.mark = " ")))
  datos_combinados
}

# Mantener por compatibilidad con código antiguo
leer_archivo_csv <- function(archivo, sep, encodings) {
  leer_archivo_csv_auto(archivo, sep, encodings)
}

# =============================================================================
# 6. LIMPIEZA Y FILTRADO  (data.table para máxima velocidad)
# =============================================================================

#' Limpia y agrupa los datos de reparto
#' Solo agrega por columnas de cálculo; las columnas extra se descartan del resultado
limpiar_datos_reparto <- function(datos) {
  if (is.null(datos) || nrow(datos) == 0) {
    warning("No hay datos para limpiar")
    return(NULL)
  }

  datos_dt <- as_dt(datos)
  datos_dt[is.na(ZIMPORT), ZIMPORT := 0]

  # Solo agrupar por columnas de cálculo (evita que columnas extra rompan la agregación)
  cols_disponibles   <- intersect(COLUMNAS_LECTURA, names(datos_dt))
  cols_agrupacion    <- setdiff(cols_disponibles, "ZIMPORT")

  if (length(cols_agrupacion) == 0) {
    warning("No hay columnas de agrupación válidas.")
    return(datos_dt)
  }

  datos_limpios <- datos_dt[, .(ZIMPORT = sum(ZIMPORT, na.rm = TRUE)),
                             by = cols_agrupacion]

  message(sprintf("✓ Datos limpiados: %s registros",
                  formatC(nrow(datos_limpios), format = "d", big.mark = " ")))
  datos_limpios
}

#' Expande selección de CACs (soporta grupos con "..")
expandir_seleccion_cac <- function(seleccion, todos_los_cacs) {
  if (is.null(seleccion) || length(seleccion) == 0) return(NULL)
  if (any(seleccion == "")) return(NULL)

  es_grupo <- grepl("\\.\\.$", seleccion)
  if (!any(es_grupo)) return(seleccion)

  grupos      <- seleccion[es_grupo]
  individuales <- seleccion[!es_grupo]
  todos_str   <- as.character(todos_los_cacs)

  expandidos <- individuales
  for (grupo in grupos) {
    prefijo  <- substr(grupo, 1, nchar(grupo) - 2)
    matches  <- todos_str[startsWith(todos_str, prefijo)]
    expandidos <- c(expandidos, matches)
  }
  unique(expandidos)
}

#' Filtra datos de reparto — operaciones en data.table
filtrar_datos_reparto <- function(datos, centro_gestor = NULL, cacs_origen = NULL,
                                  subcacs_origen = NULL, mes = NULL, anyo = NULL,
                                  nivel_agregacion = "cac") {
  if (is.null(datos)) return(NULL)

  datos_dt <- as_dt(datos)

  # Centro Gestor
  if (!is.null(centro_gestor) && nchar(trimws(centro_gestor)) > 0 &&
      "ZCENT_GEST" %in% names(datos_dt)) {
    datos_dt <- datos_dt[ZCENT_GEST == centro_gestor]
    if (nrow(datos_dt) == 0) return(datos_dt)
  }

  # Origen
  if (nivel_agregacion == "subcac" && !is.null(subcacs_origen) &&
      length(subcacs_origen) > 0 && "ZCT_SUBCAC_E" %in% names(datos_dt)) {
    exp <- expandir_seleccion_cac(subcacs_origen, unique(datos_dt$ZCT_SUBCAC_E))
    if (!is.null(exp)) {
      datos_dt <- datos_dt[ZCT_SUBCAC_E %in% exp]
      if (nrow(datos_dt) == 0) return(datos_dt)
    }
  } else if (!is.null(cacs_origen) && length(cacs_origen) > 0 &&
             "ZACT_E" %in% names(datos_dt)) {
    exp <- expandir_seleccion_cac(cacs_origen, unique(datos_dt$ZACT_E))
    if (!is.null(exp)) {
      datos_dt <- datos_dt[ZACT_E %in% exp]
      if (nrow(datos_dt) == 0) return(datos_dt)
    }
  }

  # Mes
  if (!is.null(mes) && length(mes) > 0 && "ZMES" %in% names(datos_dt)) {
    datos_dt <- datos_dt[ZMES %in% mes]
    if (nrow(datos_dt) == 0) return(datos_dt)
  }

  # Año
  if (!is.null(anyo) && length(anyo) > 0 && "ZANYO" %in% names(datos_dt)) {
    datos_dt <- datos_dt[ZANYO %in% anyo]
  }

  datos_dt
}

# =============================================================================
# 7. EXTRACCIÓN DE MOVIMIENTOS
# =============================================================================

extraer_fase <- function(datos, fase_num) {
  fase_config <- CONFIG_FASES[[as.character(fase_num)]]
  if (is.null(fase_config)) stop(sprintf("Fase %d no configurada", fase_num))

  cols_sel <- c(COLUMNAS_BASE$meta, COLUMNAS_BASE$fase_0)
  if (fase_num > 1) {
    for (f in 1:(fase_num - 1)) {
      clave <- paste0("fase_", f)
      if (clave %in% names(COLUMNAS_BASE)) cols_sel <- c(cols_sel, COLUMNAS_BASE[[clave]])
    }
  }
  cols_sel <- c(cols_sel, COLUMNAS_BASE[[paste0("fase_", fase_num)]])
  cols_sel <- intersect(cols_sel, names(datos))
  datos[, ..cols_sel]
}

detectar_tipo_movimiento <- function(movimientos_fase, fase_num, origen_cac) {
  fase_config   <- CONFIG_FASES[[as.character(fase_num)]]
  tipo_movimiento <- rep("NINGUNO", nrow(movimientos_fase))

  # Prioridad 1: RE12
  if (fase_config$re12 %in% names(movimientos_fase)) {
    v <- movimientos_fase[[fase_config$re12]]
    tipo_movimiento[!is.na(v) & v != "" & v != origen_cac] <- "RE12"
  }

  # Prioridad 2: RE34
  if (fase_config$re34 %in% names(movimientos_fase)) {
    v <- movimientos_fase[[fase_config$re34]]
    idx <- tipo_movimiento == "NINGUNO" & !is.na(v) & v != "" & v != origen_cac
    tipo_movimiento[idx] <- "RE34"
  }

  # Prioridad 3: SD
  if (fase_config$sd %in% names(movimientos_fase)) {
    v <- movimientos_fase[[fase_config$sd]]
    idx <- tipo_movimiento == "NINGUNO" & !is.na(v) & v != "" & v != origen_cac
    tipo_movimiento[idx] <- "SD"
  }

  tipo_movimiento
}

detectar_estrategia_subcac <- function(datos) {
  n <- nrow(datos)
  estrategia <- data.table(
    primera_fase_movimiento = rep(NA_integer_, n),
    tipo_primer_movimiento  = rep("NINGUNO", n)
  )

  tiene_subcac <- !is.na(datos$ZCT_SUBCAC_E) & datos$ZCT_SUBCAC_E != ""
  if (!any(tiene_subcac)) return(estrategia)

  subcac_seg  <- ifelse(tiene_subcac, datos$ZCT_SUBCAC_E, "")
  cac_padre   <- substr(subcac_seg, 1, 3)

  for (fase_num in 1:3) {
    tipo_mov          <- detectar_tipo_movimiento(datos, fase_num, cac_padre)
    sin_prev          <- tiene_subcac & estrategia$tipo_primer_movimiento == "NINGUNO"
    hay_mov           <- sin_prev & tipo_mov != "NINGUNO"
    if (any(hay_mov)) {
      estrategia$primera_fase_movimiento[hay_mov] <- fase_num
      estrategia$tipo_primer_movimiento[hay_mov]  <- tipo_mov[hay_mov]
    }
  }
  estrategia
}

extraer_movimientos_fase <- function(
  movimientos_fase,
  fase_num,
  nivel_agregacion = "cac",
  origen_previo    = NULL,
  estrategia_subcac = NULL,
  incluir_tipo_param = TRUE,
  debug = FALSE
) {
  fase_config <- CONFIG_FASES[[as.character(fase_num)]]

  # ---- Determinar ORIGEN ----
  if (fase_num == 1) {
    if (nivel_agregacion == "subcac" && "ZCT_SUBCAC_E" %in% names(movimientos_fase)) {
      origen_base <- ifelse(
        !is.na(movimientos_fase$ZCT_SUBCAC_E) & movimientos_fase$ZCT_SUBCAC_E != "",
        movimientos_fase$ZCT_SUBCAC_E, movimientos_fase$ZACT_E
      )
      if (!is.null(estrategia_subcac)) {
        necesita_acum  <- (estrategia_subcac$tipo_primer_movimiento %in% c("RE34", "SD")) |
                          (estrategia_subcac$primera_fase_movimiento > 1 &
                           estrategia_subcac$tipo_primer_movimiento != "RE12")
        ob_seg         <- ifelse(is.na(origen_base), "", origen_base)
        origen <- ifelse(necesita_acum & nchar(ob_seg) == 4,
                         substr(ob_seg, 1, 3), ob_seg)
        if (debug && any(necesita_acum, na.rm = TRUE))
          message(sprintf("     [D] Fase 1: %d SUBCAC → CAC (pre-acumulación)",
                          sum(necesita_acum, na.rm = TRUE)))
      } else {
        origen <- ifelse(is.na(origen_base), "", origen_base)
      }
    } else {
      origen <- agregar_cac(movimientos_fase$ZACT_E, nivel_agregacion)
    }
  } else {
    if (is.null(origen_previo)) stop(sprintf("Fase %d requiere origen_previo", fase_num))
    if (nivel_agregacion == "subcac" && !is.null(estrategia_subcac)) {
      necesita_acum_ahora <- (estrategia_subcac$primera_fase_movimiento == fase_num) &
                             (estrategia_subcac$tipo_primer_movimiento %in% c("RE34", "SD"))
      if (any(necesita_acum_ahora, na.rm = TRUE)) {
        op_seg <- ifelse(is.na(origen_previo), "", origen_previo)
        origen <- ifelse(necesita_acum_ahora & nchar(op_seg) == 4,
                         substr(op_seg, 1, 3), op_seg)
        if (debug) {
          n_a <- sum(necesita_acum_ahora & nchar(ifelse(is.na(origen_previo),"",origen_previo)) == 4, na.rm = TRUE)
          if (n_a > 0)
            message(sprintf("     [D] Fase %d: %d SUBCAC → CAC (RE34/SD)", fase_num, n_a))
        }
      } else {
        origen <- ifelse(is.na(origen_previo), "", origen_previo)
      }
    } else {
      origen <- ifelse(is.na(origen_previo), "", origen_previo)
    }
  }

  # ---- Inicializar resultado ----
  movimientos_fase$destino          <- origen
  movimientos_fase$movimiento_usado <- "SIN_MOVIMIENTO"
  if (incluir_tipo_param) {
    movimientos_fase$tipo_reparto <- NA_character_
    movimientos_fase$parametro    <- NA_character_
  }

  # ---- Preparar columnas de destino (con agregación si aplica) ----
  usa_temp <- nivel_agregacion %in% c("cac1", "cac2")
  if (usa_temp) {
    movimientos_fase[["temp_re12"]] <- agregar_cac(movimientos_fase[[fase_config$re12]], nivel_agregacion)
    movimientos_fase[["temp_re34"]] <- agregar_cac(movimientos_fase[[fase_config$re34]], nivel_agregacion)
    movimientos_fase[["temp_sd"]]   <- agregar_cac(movimientos_fase[[fase_config$sd]],   nivel_agregacion)
    col_re12_proc <- "temp_re12"; col_re34_proc <- "temp_re34"; col_sd_proc <- "temp_sd"
  } else {
    col_re12_proc <- fase_config$re12
    col_re34_proc <- fase_config$re34
    col_sd_proc   <- fase_config$sd
  }

  # CAC padre del origen para comparaciones
  origen_seg  <- ifelse(is.na(origen), "", origen)
  origen_cac  <- ifelse(nchar(origen_seg) == 4, substr(origen_seg, 1, 3), origen_seg)

  # ---- Prioridades ----
  if (col_re12_proc %in% names(movimientos_fase)) {
    v    <- movimientos_fase[[col_re12_proc]]
    idx  <- !is.na(v) & v != "" & v != origen_cac
    if (any(idx)) {
      movimientos_fase$destino[idx]          <- v[idx]
      movimientos_fase$movimiento_usado[idx] <- "RE12"
      if (debug) message(sprintf("     [D] %d RE12 (Fase %d)", sum(idx), fase_num))
    }
  }

  if (col_re34_proc %in% names(movimientos_fase)) {
    v    <- movimientos_fase[[col_re34_proc]]
    idx  <- movimientos_fase$movimiento_usado == "SIN_MOVIMIENTO" &
            !is.na(v) & v != "" & v != origen_cac
    if (any(idx)) {
      movimientos_fase$destino[idx]          <- v[idx]
      movimientos_fase$movimiento_usado[idx] <- "RE34"
      if (debug) message(sprintf("     [D] %d RE34 (Fase %d)", sum(idx), fase_num))
    }
  }

  if (col_sd_proc %in% names(movimientos_fase)) {
    v    <- movimientos_fase[[col_sd_proc]]
    idx  <- movimientos_fase$movimiento_usado == "SIN_MOVIMIENTO" &
            !is.na(v) & v != "" & v != origen_cac
    if (any(idx)) {
      movimientos_fase$destino[idx]          <- v[idx]
      movimientos_fase$movimiento_usado[idx] <- "SD"
      if (debug) message(sprintf("     [D] %d SD (Fase %d)", sum(idx), fase_num))
    }
  }

  # ---- Tipos y parámetros ----
  if (incluir_tipo_param) {
    param_re34_col <- fase_config$param_re34
    param_sd_col   <- fase_config$param_sd

    tiene_param_re34 <- if (param_re34_col %in% names(movimientos_fase)) {
      v <- movimientos_fase[[param_re34_col]]
      !is.na(v) & v != "" & v != "NO PARAM"
    } else rep(FALSE, nrow(movimientos_fase))

    idx_re12 <- movimientos_fase$movimiento_usado == "RE12"
    if (any(idx_re12)) {
      movimientos_fase$tipo_reparto[idx_re12] <- ifelse(tiene_param_re34[idx_re12], "RE2", "RE1")
      movimientos_fase$parametro[idx_re12]    <- ifelse(
        tiene_param_re34[idx_re12],
        movimientos_fase[[param_re34_col]][idx_re12], "NO PARAM"
      )
    }

    idx_re34 <- movimientos_fase$movimiento_usado == "RE34"
    if (any(idx_re34)) {
      movimientos_fase$tipo_reparto[idx_re34] <- ifelse(tiene_param_re34[idx_re34], "RE4", "RE3")
      movimientos_fase$parametro[idx_re34]    <- ifelse(
        tiene_param_re34[idx_re34],
        movimientos_fase[[param_re34_col]][idx_re34], "NO PARAM"
      )
    }

    idx_sd <- movimientos_fase$movimiento_usado == "SD"
    if (any(idx_sd) && param_sd_col %in% names(movimientos_fase)) {
      movimientos_fase$tipo_reparto[idx_sd] <- "SD"
      movimientos_fase$parametro[idx_sd]    <- movimientos_fase[[param_sd_col]][idx_sd]
    }

    idx_sin <- movimientos_fase$movimiento_usado == "SIN_MOVIMIENTO"
    if (any(idx_sin)) {
      movimientos_fase$tipo_reparto[idx_sin] <- "SIN_MOVIMIENTO"
      movimientos_fase$parametro[idx_sin]    <- NA_character_
    }
  }

  # Limpiar columnas temporales
  if (usa_temp) {
    movimientos_fase[, c("temp_re12", "temp_re34", "temp_sd") := NULL]
  }

  cols_ret <- c("destino", "movimiento_usado")
  if (incluir_tipo_param) cols_ret <- c(cols_ret, "tipo_reparto", "parametro")
  movimientos_fase[, ..cols_ret]
}

#' Extrae todos los movimientos a través de las fases
#'
#' @param datos          data.frame / data.table con datos limpios
#' @param nivel_agregacion "subcac" | "cac" | "cac2" | "cac1"
#' @param fases_incluir  NULL = 1:3
#' @param incluir_tipo_param Añadir columnas tipo_fase_N y param_fase_N
#' @param verbose        Mensajes de progreso
#' @param debug          Mensajes detallados de debugging
#' @return data.table con columnas fase_0 ... fase_N + importe [+ tipo/param]
extraer_movimientos <- function(
  datos,
  nivel_agregacion    = "cac",
  fases_incluir       = NULL,
  incluir_tipo_param  = TRUE,
  verbose             = TRUE,
  debug               = FALSE
) {
  if (is.null(datos) || nrow(datos) == 0) {
    warning("No hay datos para extraer movimientos")
    return(NULL)
  }

  datos_dt <- if (!is.data.table(datos)) as.data.table(datos) else copy(datos)
  datos_dt <- datos_dt[!is.na(ZIMPORT) & ZIMPORT != 0]

  if (nrow(datos_dt) == 0) {
    warning("Sin registros con importe válido distinto de cero.")
    return(NULL)
  }

  fases_a_procesar <- if (is.null(fases_incluir)) 1:3 else sort(unique(as.integer(fases_incluir)))
  max_fase <- max(fases_a_procesar)

  if (verbose) {
    message(sprintf("Extrayendo movimientos: %s registros | fases %s | nivel '%s'",
                    formatC(nrow(datos_dt), format = "d", big.mark = " "),
                    paste(fases_a_procesar, collapse = "-"),
                    nivel_agregacion))
  }

  datos_dt <- crear_fase_origen(datos_dt, nivel_agregacion)

  # Estrategia SUBCAC
  estrategia_subcac <- NULL
  if (nivel_agregacion == "subcac" && "ZCT_SUBCAC_E" %in% names(datos_dt)) {
    if (verbose) message("  Detectando estrategia SUBCAC...")
    estrategia_subcac <- detectar_estrategia_subcac(datos_dt)
    if (verbose) {
      n_mov <- sum(estrategia_subcac$tipo_primer_movimiento != "NINGUNO")
      if (n_mov > 0) {
        message(sprintf("    → %s registros SUBCAC con movimiento",
                        formatC(n_mov, format = "d", big.mark = " ")))
        for (tipo in setdiff(names(table(estrategia_subcac$tipo_primer_movimiento)), "NINGUNO")) {
          message(sprintf("       · %s: %d", tipo,
                          sum(estrategia_subcac$tipo_primer_movimiento == tipo)))
        }
      }
    }
  }

  # Resultado base
  resultado <- data.table(fase_0 = datos_dt$fase_0, importe = datos_dt$ZIMPORT)
  origen_actual <- datos_dt$fase_0

  for (fase_num in 1:max_fase) {
    if (verbose) message(sprintf("  Procesando Fase %d...", fase_num))

    movimientos_fase <- extraer_fase(datos_dt, fase_num)
    fase_resultado   <- extraer_movimientos_fase(
      movimientos_fase   = movimientos_fase,
      fase_num           = fase_num,
      nivel_agregacion   = nivel_agregacion,
      origen_previo      = if (fase_num == 1) NULL else origen_actual,
      estrategia_subcac  = estrategia_subcac,
      incluir_tipo_param = incluir_tipo_param,
      debug              = debug
    )

    nombre_fase <- paste0("fase_", fase_num)
    resultado[[nombre_fase]] <- fase_resultado$destino
    if (incluir_tipo_param) {
      resultado[[paste0("tipo_fase_",  fase_num)]] <- fase_resultado$tipo_reparto
      resultado[[paste0("param_fase_", fase_num)]] <- fase_resultado$parametro
    }
    origen_actual <- fase_resultado$destino

    if (verbose) {
      n_mov <- sum(fase_resultado$movimiento_usado != "SIN_MOVIMIENTO")
      message(sprintf("    → %s movimientos (%.1f%%)",
                      formatC(n_mov, format = "d", big.mark = " "),
                      100 * n_mov / nrow(fase_resultado)))
    }
  }

  if (verbose) {
    message(sprintf("✓ Matriz: %s registros | importe total: %s €",
                    formatC(nrow(resultado), format = "d", big.mark = " "),
                    formatC(sum(resultado$importe, na.rm = TRUE),
                            format = "f", big.mark = " ", decimal.mark = ",", digits = 2)))
  }

  resultado
}

# =============================================================================
# 8. FUNCIONES AUXILIARES DE UTILIDAD
# =============================================================================

#' Pipeline completo de carga y limpieza
preparar_datos_reparto <- function(..., archivo_especifico = NULL) {
  datos_raw    <- carga_datos_reparto(..., archivo_especifico = archivo_especifico)
  datos_limpios <- limpiar_datos_reparto(datos_raw)
  datos_limpios
}

#' Obtener valores únicos para los selectores de filtros
obtener_valores_filtros <- function(datos) {
  if (is.null(datos) || nrow(datos) == 0) return(list())
  datos_dt <- as_dt(datos)
  list(
    centros_gestores = sort(unique(datos_dt$ZCENT_GEST)),
    anyos            = sort(unique(datos_dt$ZANYO)),
    meses            = sort(unique(datos_dt$ZMES)),
    cacs_origen      = sort(unique(datos_dt$ZACT_E)),
    subcacs_origen   = sort(unique(datos_dt$ZCT_SUBCAC_E))
  )
}

#' Columnas extra presentes en un dataset cargado
obtener_columnas_extra <- function(datos) {
  if (is.null(datos)) return(character(0))
  setdiff(names(datos), COLUMNAS_LECTURA)
}

# Formateo de números
formatear_numero  <- function(x, decimales = 2)
  formatC(x, format = "f", big.mark = " ", decimal.mark = ",", digits = decimales)

formatear_importe <- function(x)
  paste(formatear_numero(x), "€")

formatear_entero  <- function(x)
  formatC(x, format = "d", big.mark = " ")