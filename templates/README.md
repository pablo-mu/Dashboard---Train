# Dashboard Modular - Template Structure

Este template proporciona una estructura modular "plug and play" para crear dashboards Shiny dinámicos.

## 📁 Estructura de Archivos

```
templates/
├── app.R                    # Archivo principal - ensambla todos los módulos
├── config.R                 # Configuración general y definiciones de paneles
├── ui/
│   ├── header.R            # Módulo del header del dashboard  
│   ├── sidebar.R           # Módulo del sidebar
│   └── main_buttons.R      # Módulo de botones principales
├── modules/
│   ├── antimicrobials.R    # Módulo del panel de antimicrobianos
│   ├── patients.R          # Módulo del panel de pacientes
│   ├── diagnostics.R       # Módulo del panel de diagnósticos
│   └── outcome.R           # Módulo del panel de outcomes
└── server/
    ├── dynamic_server.R    # Lógica del servidor dinámico
    └── plots_data.R        # Generación de gráficos y tablas
```

## 🚀 Cómo usar

### 1. Ejecutar el dashboard
```r
# Desde el directorio principal
source("templates/app.R")
```

### 2. Agregar un nuevo panel (Plug & Play)

#### Paso 1: Configurar el panel
Editar `config.R` y agregar la definición del nuevo panel:
```r
PANEL_DEFINITIONS <- list(
  # ... paneles existentes ...
  mi_nuevo_panel = list(
    name = "MI NUEVO PANEL",
    label = "MI NUEVO PANEL", 
    icon = "chart-bar",
    button_style = "success",
    button_label = "FILTROS MI PANEL"
  )
)
```

#### Paso 2: Crear el módulo del panel
Crear `templates/modules/mi_nuevo_panel.R`:
```r
create_mi_nuevo_panel_content <- function() {
  tagList(
    fluidRow(
      introBox(
        box(title = "Mi Contenido", status = "primary", solidHeader = TRUE, width = 12,
            plotOutput("mi_grafico"),
            "Descripción de mi contenido"
        ),
        data.step = 3, data.intro = "Mi panel personalizado."
      )
    )
  )
}

create_mi_nuevo_panel_filters <- function() {
  list(
    h4("Filtros para Mi Panel"),
    selectInput("mi_filtro", "Mi Filtro:", choices = c("Opción 1", "Opción 2"))
  )
}
```

#### Paso 3: Agregar al servidor dinámico
Editar `templates/server/dynamic_server.R` y agregar:
```r
# En observeEvent
observeEvent(input$mi_nuevo_panel, { active_panel("mi_nuevo_panel") })

# En switch del contenido
switch(panel,
  # ... casos existentes ...
  "mi_nuevo_panel" = create_mi_nuevo_panel_content(),
  
# En switch de filtros  
switch(panel,
  # ... casos existentes ...
  "mi_nuevo_panel" = create_mi_nuevo_panel_filters(),
```

#### Paso 4: Agregar outputs (opcional)
Editar `templates/server/plots_data.R` y agregar:
```r
# En create_plots_server
output$mi_grafico <- renderPlot({
  plot(1:10, 1:10, main = "Mi Gráfico Personalizado")
})
```

#### Paso 5: Agregar botón principal
Editar `templates/ui/main_buttons.R`:
```r
bsButton("mi_nuevo_panel", label = panel_definitions$mi_nuevo_panel$name, 
         icon = icon(panel_definitions$mi_nuevo_panel$icon), style = "success"),
```

#### Paso 6: Cargar el módulo
Editar `templates/app.R` y agregar:
```r
source("templates/modules/mi_nuevo_panel.R")
```

## ⚙️ Características

- **Modular**: Cada panel es un módulo independiente
- **Plug & Play**: Fácil agregar/quitar paneles sin afectar el resto
- **Configuración centralizada**: Un solo archivo para toda la configuración
- **Dinámico**: Botones de filtros y contenido cambian automáticamente
- **Escalable**: Estructura preparada para dashboards grandes
- **Mantenible**: Código organizado y separado por funcionalidades

## 🎨 Personalización

### Cambiar colores de botones
Editar `config.R` y modificar `button_style` de cada panel:
- `"danger"` = Rojo
- `"primary"` = Azul  
- `"success"` = Verde
- `"warning"` = Naranja
- `"info"` = Cian

### Cambiar iconos
Editar `config.R` y modificar `icon` de cada panel usando iconos de FontAwesome.

### Modificar layout
Editar los archivos en `ui/` para cambiar la estructura del dashboard.

## 📋 Dependencias

```r
library(shinydashboard)
library(shiny) 
library(shinyjs)
library(rintrojs)
library(shinyBS)
library(shinyWidgets)
library(DT)
```

## 🔧 Solución de problemas

1. **Error de funciones no encontradas**: Verificar que todas las librerías estén instaladas
2. **Panel no aparece**: Verificar que el módulo esté cargado en `app.R`
3. **Botón no funciona**: Verificar que el observeEvent esté agregado en `dynamic_server.R`