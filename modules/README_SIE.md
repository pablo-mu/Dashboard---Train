# Integración de Archivos SIE

## Descripción General

El sistema ahora soporta **dos formatos de archivos CSV**:

1. **Formato Estándar** (original): Con columnas detalladas de tipos de reparto (RE12, RE34, SD)
2. **Formato SIE** (nuevo): Con estructura simplificada de movimientos CAC secuenciales

## Formato SIE

### Estructura de Columnas

```
CENTRO;ANYO;CTRABCF;PERIODO;MESCF;CACCF;OCCF;CTRABA0;CACA0;OCA0;CTRABA1;CACA1;OCA1;CTRABA2;CACA2;OCA2;CTRABA3;CACA3;OCA3;IMPORTE;ESTIMADO
```

### Mapeo de Columnas

| Columna SIE | Columna Estándar | Descripción |
|-------------|------------------|-------------|
| `CENTRO` | `ZCENT_GEST` | Centro gestor |
| `ANYO` | `ZANYO` | Año |
| `MESCF` | `ZMES` | Mes |
| `CACCF` | `ZCT_SUBCAC_E` | SUBCAC de origen (4 dígitos) |
| `CACA0` | `ZACT_E` | CAC de origen (3 dígitos) |
| `CACA1` | `ZACT_F1SD` | CAC destino fase 1 |
| `CACA2` | `ZACT_F2SD` | CAC destino fase 2 |
| `CACA3` | `ZACT_F3SD` | CAC destino fase 3 |
| `IMPORTE` | `ZIMPORT` | Importe del movimiento |

### Transformación Aplicada

Dado que los archivos SIE **no distinguen tipos de reparto**, la transformación aplica la siguiente estrategia:

1. **Cada movimiento CACAx se trata como "Se Distribuye" (SD)**
2. **Las columnas RE12 y RE34 mantienen el CAC anterior** (mismo valor = sin movimiento):
   - `ZACT_RE12` = `CACA0` (CAC anterior - no hay movimiento)
   - `ZACT_RE34` = `CACA0` (CAC anterior - no hay movimiento)
   - `ZACT_F1SD` = `CACA1` ← **Movimiento SD al nuevo destino**

3. **Los parámetros se configuran como**:
   - `ZPARAM_RE34` = "NO PARAM"
   - `ZPARAM_FxSD` = "SIE" (identificador de origen SIE)

4. **Efecto en la lógica de prioridades**:
   - Prioridad 1 (RE12): Valor igual al origen → No aplica
   - Prioridad 2 (RE34): Valor igual al origen → No aplica
   - Prioridad 3 (SD): Valor diferente al origen → **Se aplica SD**
   - Resultado: Los movimientos SIE se detectan correctamente como **SD**

## Uso

### Detección Automática

El sistema **detecta automáticamente** el formato del archivo:

```r
# Funciona para ambos formatos sin cambios en el código
datos <- carga_datos_reparto()
```

### Criterio de Detección

Un archivo se identifica como SIE si contiene **al menos 7 de estas 9 columnas clave**:
- `CENTRO`
- `ANYO`
- `MESCF`
- `CACCF`
- `CACA0`
- `CACA1`
- `CACA2`
- `CACA3`
- `IMPORTE`

### Mezcla de Formatos

Puedes tener **archivos de ambos formatos en la misma carpeta** `.data/`:
- Los archivos SIE serán transformados automáticamente
- Los archivos estándar se cargan sin modificación
- Ambos se combinan en un único dataset compatible

## Ventajas de la Estrategia

### ✅ Compatibilidad Total
- **Sin cambios en módulos existentes**: Sankey, Matriz, Diagnóstico funcionan igual
- **Aprovecha todo el código actual**: `extraer_movimientos()`, filtros, agregaciones

### ✅ Trazabilidad SUBCAC
- Los archivos SIE incluyen SUBCAC origen (`CACCF`)
- La lógica de acumulación funciona correctamente
- Se respeta la nueva casuística de movimientos RE1 desde SUBCAC

### ✅ Claridad en Resultados

- Los movimientos SIE se identifican claramente:
  - `tipo_reparto = "SD"` (Se Distribuye)
  - `parametro = "SIE"` en columnas SD

### ✅ Simplicidad
- Un solo punto de transformación (`transformar_sie_a_estandar`)
- Detección automática sin configuración manual
- Mensajes claros en consola sobre tipo de archivo detectado

## Ejemplo de Flujo

```r
# 1. Cargar datos (mezcla de formatos)
datos <- carga_datos_reparto()
# ✓ Encontrados 3 archivo(s) CSV en: .data
# ✓ Archivo SIE detectado: 'datos_sie_2024.csv' (encoding 'UTF-8')
#   Transformando formato SIE a formato estándar...
#   ✓ Transformación SIE completada: 15,234 registros
# ✓ Archivo estándar leído: 'datos_sap_2024.csv' (UTF-8, 8,456 filas)
# ✓ Total de datos combinados: 23,690 registros

# 2. Limpiar
datos_limpios <- limpiar_datos_reparto(datos)

# 3. Filtrar
datos_filtrados <- filtrar_datos_reparto(
  datos_limpios,
  centro_gestor = "1001",
  anyo = 2024,
  nivel_agregacion = "subcac"
)

# 4. Extraer movimientos (funciona igual para ambos formatos)
matriz <- extraer_movimientos(
  datos_filtrados,
  nivel_agregacion = "subcac",
  verbose = TRUE,
  debug = FALSE
)

# La matriz resultante es idéntica en estructura,
# independientemente del formato de origen
```

## Notas Técnicas

### Comportamiento de Prioridades con SIE

Al mantener el CAC anterior en RE12/RE34 y el nuevo destino solo en SD:

```r
# Para movimiento de CACA0="800" a CACA1="810"
ZACT_RE12 = "800"  # Igual al origen - Prioridad 1 (no movimiento)
ZACT_RE34 = "800"  # Igual al origen - Prioridad 2 (no movimiento)
ZACT_F1SD = "810"  # Diferente del origen - Prioridad 3 (movimiento SD)
```

La lógica evalúa en orden:

1. ¿Hay valor en RE12 distinto del origen "800"? → **NO** (800 == 800) → Continuar
2. ¿Hay valor en RE34 distinto del origen "800"? → **NO** (800 == 800) → Continuar
3. ¿Hay valor en SD distinto del origen "800"? → **SÍ** (810 != 800) → Aplicar SD

**Resultado**: Movimiento detectado correctamente como `SD` con parámetro `"SIE"`

### Limitaciones Conocidas

- **No se distinguen tipos de reparto originales** en datos SIE
- Todos los movimientos SIE aparecen como `SD` en los resultados
- Para análisis que requieran tipo de reparto real, usar formato estándar

### Columnas Ignoradas

Las siguientes columnas SIE se leen pero no se utilizan:
- `CTRABCF`, `PERIODO` (información de contexto)
- `OCCF`, `OCA0`, `OCA1`, `OCA2`, `OCA3` (orden de operación)
- `CTRABA0`, `CTRABA1`, `CTRABA2`, `CTRABA3` (centros de trabajo)
- `ESTIMADO` (indicador de estimación)

Estas columnas podrían incorporarse en futuras versiones si se requiere trazabilidad adicional.

## Verificación de Transformación

Para verificar que la transformación es correcta:

```r
# Cargar archivo SIE directamente
datos_sie <- read.csv(".data/archivo_sie.csv", sep = ";")

# Ver estructura original
str(datos_sie)

# Transformar manualmente
datos_transformados <- transformar_sie_a_estandar(datos_sie)

# Verificar columnas resultantes
names(datos_transformados)
# [1] "ZCENT_GEST"   "ZANYO"        "ZMES"         "ZIMPORT"     
# [5] "ZCT_SUBCAC_E" "ZACT_E"       "ZACT_RE12"    "ZACT_RE34"   
# [9] "ZACT_F1SD"    "ZPARAM_RE34"  "ZPARAM_F1SD"  ...

# Verificar muestra
head(datos_transformados[, c("ZCT_SUBCAC_E", "ZACT_E", "ZACT_RE12", "ZACT_F1SD")])
```

## Soporte y Mantenimiento

- **Función principal**: `transformar_sie_a_estandar()` en `modules/lib_reparto.R`
- **Detección**: `es_formato_sie()` en `modules/lib_reparto.R`
- **Lectura unificada**: `leer_archivo_csv_auto()` en `modules/lib_reparto.R`
