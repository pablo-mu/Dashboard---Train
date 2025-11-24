# Módulo Traza

## Descripción

El módulo **Traza** permite visualizar la matriz de movimientos de costes entre CACs (Centros de Actividad de Costes) a través de las diferentes fases de reparto, con la capacidad de configurar el nivel de agregación de forma independiente para cada fase.

## Características

### Agregación Flexible por Fase

- **Fase 0 (Origen)**: Puede configurarse en cualquier nivel:
  - SUBCAC (4 dígitos)
  - CAC (3 dígitos)
  - CAC1 (1 dígito)
  - CAC2 (2 dígitos)

- **Fases 1, 2 y 3**: Pueden configurarse independientemente en:
  - CAC (3 dígitos)
  - CAC1 (1 dígito)
  - CAC2 (2 dígitos)

### Vista Simplificada

- **Sin columnas de Tipo y Parámetro**: A diferencia del diagrama Sankey, la traza se enfoca únicamente en los movimientos de CAC a CAC por fase, sin mostrar el tipo de reparto ni los parámetros.
- **Agregación automática**: Los importes se agrupan automáticamente según los niveles de agregación seleccionados.

## Uso

1. **Cargar datos**: Primero carga los datos desde el botón "CARGAR DATOS" en el sidebar
2. **Configurar filtros generales**: En el sidebar, configura:
   - Centro Gestor
   - Mes y Año
   - Fases a incluir (1, 2, 3)
   - Origen y Destino (opcional)

3. **Configurar agregación por fase**: Haz clic en el botón "GENERAR TRAZA" para abrir el modal de configuración donde puedes:
   - Seleccionar el nivel de agregación para Fase 0 (Origen)
   - Seleccionar el nivel de agregación para cada fase (1, 2, 3)

4. **Generar**: Presiona "Generar Diagrama" en el modal

## Outputs

- **Info Boxes**: Muestra estadísticas clave:
  - Número de registros
  - Número de fases
  - Importe total

- **Matriz de Movimientos**: Tabla interactiva con:
  - Columna Origen (Fase 0)
  - Columnas por cada fase incluida (Fase 1, 2, 3)
  - Columna Importe
  - Capacidad de búsqueda, ordenamiento y exportación

## Controles

- **Expandir tabla**: Ver la tabla en pantalla completa
- **Descargar CSV**: Exportar la matriz a formato CSV

## Diferencias con Sankey

| Característica | Sankey | Traza |
|---------------|--------|-------|
| Visualización | Diagrama de flujo | Tabla de datos |
| Columnas Tipo | ✓ Sí | ✗ No |
| Columnas Parámetro | ✓ Sí | ✗ No |
| Agregación por fase | ✗ No (global) | ✓ Sí (independiente) |
| Enlaces detallados | ✓ Sí | ✗ No |

## Casos de Uso

### Ejemplo 1: Análisis Detallado de Origen, Resumen de Destino

```
Fase 0: SUBCAC (máximo detalle en origen)
Fase 1: CAC2 (resumen medio)
Fase 2: CAC2 (resumen medio)
Fase 3: CAC1 (máximo resumen en destino final)
```

### Ejemplo 2: Vista Resumida Completa

```
Fase 0: CAC1
Fase 1: CAC1
Fase 2: CAC1
Fase 3: CAC1
```

Esta configuración proporciona una vista muy agregada del flujo de costes.

## Archivos del Módulo

```
modules/traza/
├── traza_ui.R         # Interfaz de usuario (UI)
├── traza_server.R     # Lógica del servidor
└── README.md          # Esta documentación
```

## Integración

El módulo está completamente integrado con:
- `config.R`: Definición del panel
- `ui/navbar.R`: Botón de navegación
- `app.R`: Panel en la UI principal
- `server/dynamic_server.R`: Inicialización del servidor
