library(shiny)
library(shinydashboard)

source("global.R")
source("R/lib_reparto/lib_reparto.R")
source("R/sankey/mod_sankey.R")
source("R/matriz/mod_matriz.R")
source("R/diagnostico/mod_diagnostico.R")
source("R/utils_ui.R")
source("R/utils_filters.R")


ui <- dashboardPage(
  dashboardHeader(title = "Dashboard Reparto de Costes"),
  dashboardSidebar(
      sidebarMenu(
        id = "sidebar",
        menuItem("Diagrama de Sankey", tabName = "sankey", icon = icon("diagram-project")),
        menuItem("Matriz de Costes", tabName = "matriz", icon = icon("table")),
        menuItem("Diagnóstico de Datos", tabName = "diagnostico", icon = icon("stethoscope")),
        hr(),
        uiOutput("file_selector")
      )
    ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
      tags$style(HTML("
        /* Ajustes compactos para KPIs */
        .info-box {
          padding: 0px 0px;
          min-height: 46px;
        }
        .info-box .info-box-icon {
          width: 46px;
          height: 46px;
          display: flex;
          align-items: center;
          justify-content: center;
          font-size: 18px; /* icon size */
          margin-right: 1px; /* <- reducir separación */
          border-radius: 1px;
        }
        .info-box .info-box-content {
          padding: 4px 6px;
          margin-left: 0; /* asegurar que no haya margen extra */
          display: flex;
          flex-direction: column;
          justify-content: center;
        }
        .info-box .info-box-text {
          font-size: 12px;
          line-height: 1;
          margin: 0 0 2px 0;
        }
        .info-box .info-box-number {
          font-size: 16px;
          font-weight: 600;
          margin: 0;
        }
      ")),
      tags$style(HTML("
        /* Permitir salto de línea dentro de botones para que el texto no se desborde */
        .action-button, .btn {
          white-space: normal !important;
          overflow-wrap: anywhere !important;
          word-break: break-word !important;
          -ms-word-break: break-word !important;
        }

        /* Asegurar que el wrapper de DataTables use todo el ancho disponible */
        .dataTables_wrapper { width: 100% !important; }

        /* Si usas cajas con padding/aplanado, evita overflow horizontal no deseado */
        .box { overflow-x: visible !important; }
      ")),
      tags$script(HTML("
        $(document).on('shiny:connected', function() {
          function adjustAllDT(){
            try {
              $('.dataTable').each(function() {
                try {
                  var tbl = $(this).DataTable();
                  if (tbl && typeof tbl.columns === 'function') {
                    tbl.columns.adjust();
                  }
                } catch(e) { /* ignore per-table errors */ }
              });
            } catch(e){}
          }

          var resizeTimer;
          $(window).on('resize', function(){
            clearTimeout(resizeTimer);
            resizeTimer = setTimeout(function(){ adjustAllDT(); }, 150);
          });

          // sidebar toggle (shinydashboard)
          $(document).on('click', '.sidebar-toggle, .main-sidebar', function(){
            setTimeout(function(){ adjustAllDT(); }, 300);
          });

          // DataTable specific events that can change header alignment
          $(document).on('draw.dt page.dt length.dt column-visibility.dt', '.dataTable', function(){
            try {
              var tbl = $(this).DataTable();
              if (tbl && typeof tbl.columns === 'function') tbl.columns.adjust();
            } catch(e){}
          });

          // Also adjust after DataTable init complete
          $(document).on('init.dt', '.dataTable', function(){
            try {
              var tbl = $(this).DataTable();
              if (tbl && typeof tbl.columns === 'function') tbl.columns.adjust();
            } catch(e){}
          });

          // Inicial
          setTimeout(adjustAllDT, 500);
        });
      "))
    ),
    tabItems(
      tabItem(tabName = "sankey", sankeyUI("sankey_module")),
      tabItem(tabName = "matriz", matrizUI("matriz_module")),
      tabItem(tabName = "diagnostico", diagnosticoUI("diagnostico_module"))
    )
  )
)

# Server Principal
server <- function(input, output, session) {
  
  # Carga de datos centralizada
  datos_raw <- reactive({
    input$refresh_data
    input$refresh_data_matriz
    
    tryCatch({
      datos <- preparar_datos_reparto()
      
      if (!is.null(datos)) {
        datos_dt <- datos %>% as.data.table()
        showNotification(
          paste("✓ Datos cargados:", formatC(nrow(datos_dt), format = "d", big.mark = " "), "registros únicos"),
          type = "message",
          duration = 5
        )
        return(datos)
      } else {
        showNotification(
          paste("No se encontraron archivos CSV. Directorio:", getwd()),
          type = "error",
          duration = 10
        )
        return(NULL)
      }
    }, error = function(e) {
      showNotification(
        paste("Error al cargar datos:", e$message),
        type = "error",
        duration = 10
      )
      return(NULL)
    })
  })
  
  # File selector común
  output$file_selector <- renderUI({
    datos <- datos_raw()
    
    if (is.null(datos)) {
      return(div(
        style = "padding: 15px;",
        p("⚠️ No se encontraron archivos CSV", style = "color: red; font-weight: bold;"),
        p(paste("Directorio actual:", getwd()), style = "font-size: 11px; color: #666;"),
        p("Coloca tus archivos CSV en: .data/ o data/", style = "font-size: 11px;")
      ))
    }
    
    n_registros <- datos %>% as.data.table() %>% nrow()
    
    return(NULL)
    #return(div(
    #  style = "padding: 15px;",
    #  p(paste("✓ Datos limpios:", formatC(n_registros, format = "d", big.mark = " "), "registros únicos"), 
    #    style = "color: green; font-weight: bold;"),
    #  p("(Después de agrupar y sumar)", style = "font-size: 11px; color: #666;")
    #))
  })
  
  # Llamar a los módulos con datos compartidos
  sankeyServer("sankey_module", datos_raw, reactive(input$sidebar))
  matrizServer("matriz_module", datos_raw, reactive(input$sidebar))
  diagnosticoServer("diagnostico_module", datos_raw)
}

# Ejecutar la aplicación
shinyApp(ui = ui, server = server)