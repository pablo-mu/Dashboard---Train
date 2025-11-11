# Guía de Implementación del Sistema de Filtros Dinámicos

## Resumen del Sistema

Este sistema resuelve el problema de renderizar filtros que dependen de datos de manera centralizada y escalable.

## Arquitectura

### 1. Carga Centralizada de Datos (`server/data_server.R`)
- **Función**: `create_data_server(input, output, session)`
- **Responsabilidad**: Cargar datos usando `lib_reparto.R` y proporcionar estado de carga
- **Retorna**: 
  - `datos_raw`: Reactive con los datos
  - `loading_state`: Estado de la carga (loading/loaded/error)

### 2. Renderización de Filtros (`server/filters_server.R`)
- **Función**: `create_filters_server(input, output, session, datos_raw)`
- **Responsabilidad**: Renderizar filtros dinámicos basados en los datos
- **Filtros implementados**:
  - Centro Gestor (obligatorio)
  - Origen (depende de nivel de agregación y centro gestor)
  - Destino (depende de nivel de agregación y centro gestor)
  - Mes
  - Año
- **Retorna**: Lista con valores de filtros como reactives

### 3. Integración en UI (`ui/sidebar.R`)
- Los filtros se renderizan usando `uiOutput()` en las posiciones correspondientes
- Estado de carga visible para el usuario
- Estructura modular mantenida

### 4. Coordinación Central (`server/dynamic_server.R`)
- Integra data_server y filters_server
- Proporciona datos y filtros a los módulos
- Mantiene la lógica existente de paneles dinámicos

## Próximos pasos para implementar:

### Paso 1: Verificar Dependencias
```r
# Asegúrate de que existe el archivo modules/lib_reparto.R
# y que tiene la función preparar_datos_reparto()
# Verifica que existe la carpeta .data/ con los archivos CSV
```

### Paso 2: Probar el Sistema
```r
# Ejecuta la aplicación para probar:
# 1. Que los datos se cargan correctamente
# 2. Que los filtros aparecen en el sidebar
# 3. Que los filtros se actualizan según las selecciones
```

### Paso 3: Adaptar Módulos Existentes
Usa el patrón del archivo `modules/ejemplo_filtros.R`:

```r
# En tu módulo existente:
mi_modulo_Server <- function(id, datos_raw, filters) {
  moduleServer(id, function(input, output, session) {
    
    # Aplicar filtros a los datos
    datos_filtrados <- reactive({
      datos <- datos_raw()
      
      # Aplicar filtros según necesidad
      if (!is.null(filters$centro_gestor()) && filters$centro_gestor() != "") {
        datos <- datos[datos$ZCENT_GEST == filters$centro_gestor(), ]
      }
      
      # ... más filtros según necesidad
      return(datos)
    })
    
    # Usar datos_filtrados() en tus outputs
    # ...
  })
}
```

### Paso 4: Integrar con Sankey
Para integrar con tu módulo sankey existente:

```r
# En app.R, después de crear server_components:
# Llamar al módulo sankey pasando los datos y filtros
sankeyServer("sankey_module", 
             datos_raw = server_components$datos_raw,
             filters = server_components$filters)
```

## Ventajas del Sistema

1. **Centralizado**: Los datos se cargan una sola vez
2. **Reactivo**: Los filtros se actualizan automáticamente según los datos
3. **Escalable**: Fácil añadir nuevos filtros o módulos
4. **Mantenible**: Lógica de filtros separada de la lógica de módulos
5. **Reutilizable**: Los filtros están disponibles para todos los módulos

## Gestión de IDs

Los IDs de los filtros son fijos y se manejan en `filters_server.R`:
- `centro_gestor`
- `origen`  
- `destino`
- `mes`
- `anyo`

No necesitas usar `NS()` para estos filtros ya que se renderizan directamente en el sidebar principal.

## Extensibilidad

Para añadir un nuevo filtro:

1. Agregar `uiOutput("nuevo_filtro")` en `sidebar.R`
2. Implementar `output$nuevo_filtro <- renderUI({...})` en `filters_server.R`
3. Añadir el reactive del filtro al return de `create_filters_server()`
4. Usar el filtro en los módulos como `filters$nuevo_filtro()`

¿Te parece bien esta implementación? ¿Quieres que te ayude a integrar algún módulo específico con este sistema de filtros?