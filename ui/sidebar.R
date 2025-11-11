create_dashboard_sidebar <- function(config) {
  shinydashboard::dashboardSidebar(
    width = config$sidebar_width,
    
    introBox(
      data.step = 4, 
      data.intro = "Panel de filtros para generar y personalizar los gráficos de reparto de costes.",
      div(class = "inlay", style = "height:15px;width:100%;background-color: #ecf0f5;"),
      
      
    # --- Botón de carga de datos / filtros dinámico ---
    introBox(
      data.step = 5, 
      data.intro = "Primero carga los datos, después configura los filtros para generar los gráficos.",
      div(id = "sidebar_button_top",
          uiOutput("dynamic_action_btn")
      )
    ),

    # (Opcional) Separador visual
    div(class = "inlay", style = "height:15px;width:100%;background-color: #ecf0f5;"),

      sidebarMenu(
        id = "sidebar_tab",        
        # Centro Gestor - Filtro principal
        menuItem("CENTRO GESTOR", tabName = "centro_gestor", icon = icon("building"),
          uiOutput("centro_gestor_filter"),
          helpText("Selecciona el centro gestor principal para el análisis")
        ),
        
        # Nivel de Agregación
        menuItem("NIVEL AGREGACIÓN", tabName = "agregacion", icon = icon("layer-group"),
          radioButtons("nivel_agregacion", "Nivel:", 
                       choices = list(
                         "CAC Completo" = "cac",
                         "CAC (1 dígito)" = "cac1", 
                         "CAC (2 dígitos)" = "cac2",
                         "Subcentro" = "subcac"
                       ), 
                       selected = "cac",
                       inline = FALSE)
        ),
        
        # Filtros de Origen y Destino
        menuItem("ORIGEN & DESTINO", tabName = "origen_destino", icon = icon("arrows-left-right"),
          uiOutput("origen_filter"),
          div(style = "margin-top: 10px;"),
          uiOutput("destino_filter"),
          div(style = "margin-top: 10px;"),
          checkboxInput("mostrar_estaticos", "Incluir movimientos estáticos", value = TRUE)
        ),
        
        # Filtros Temporales
        menuItem("PERÍODO", tabName = "periodo", icon = icon("calendar-alt"),
          uiOutput("mes_filter"),
          uiOutput("anyo_filter")
        ),
        
        # Fases del Reparto
        menuItem("FASES REPARTO", tabName = "fases", icon = icon("list-ol"),
          checkboxGroupInput("fases_filter", "Fases a incluir:",
                             choices = list(
                               "Fase 1" = "1",
                               "Fase 2" = "2", 
                               "Fase 3" = "3"
                             ),
                             selected = c("1", "2", "3"),
                             inline = FALSE),
          helpText("Selecciona las fases del proceso de reparto")
        ),
        
        # Opciones de Visualización
        #menuItem("VISUALIZACIÓN", tabName = "visualizacion", icon = icon("palette"),
        #  checkboxInput("mostrar_parametro", "Colorear por parámetro", value = FALSE),
        #  helpText("Muestra colores diferentes según el parámetro de reparto")
        #),
        
        # Acciones
        #menuItem("ACCIONES", tabName = "acciones", icon = icon("gear"),
        #  div(style = "padding: 10px;",
        #    actionButton("generar_sankey", "Generar Diagrama", 
        #                 icon = icon("play"),
        #                 style = "width: 100%; margin-bottom: 10px; font-weight: bold;",
        #                 class = "btn-primary"),
        #    actionButton("reset_filters", "Resetear Filtros", 
        #                 icon = icon("rotate-left"),
        #                 style = "width: 100%;",
        #                 class = "btn-default")
        #  )
        #),
        
        # Información de Ayuda
        menuItem("AYUDA", tabName = "ayuda", icon = icon("question-circle"),
          div(style = "padding: 10px; font-size: 12px;",
            h5("Instrucciones:", style = "font-weight: bold; margin-bottom: 10px;"),
            p("1. Selecciona un Centro Gestor"),
            p("2. Configura el nivel de agregación"),
            p("3. Ajusta filtros temporales (opcional)"),
            p("4. Selecciona las fases a mostrar"),
            p("5. Haz clic en 'Generar Diagrama'"),
            hr(),
            p(strong("Nota:"), "El Centro Gestor es obligatorio para generar el diagrama.",
              style = "font-size: 11px; color: #666;")
          )
        )
      )
    )
  )
}
