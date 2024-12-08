# Importar librerías

library(scales)
library(shiny)         
library(sf)            
library(shinythemes)  
library(RColorBrewer)
library(colourpicker) 
library(ggplot2)
library(bslib)
library(dplyr)       
library(readxl)  
library(plotly)
library(promises)
library(future)
library(tidyr)
library(scales)        
library(leaflet)       

# Leer los datos generados en el archivo recursos/limpieza.R
datos_agregados <- readRDS("recursos/datos_agregados.rds")
datos_delegaciones <- readRDS("recursos/datos_delegaciones.rds")



# UI
ui <- fluidPage(
  theme = bs_theme(
    bootswatch = "flatly",  
    bg = "#FFFFFF",          # Color del fondo
    fg = "#386641",    # Color del texto
    primary = "#386641",
    secondary = "#386641"
  ),
  navbarPage(
    title = "AgroMap",
    
    # Pestaña Home
    tabPanel(
      "Home",
      fluidPage(
        titlePanel(h2("Herramientas para la visualización de cultivos")),
        mainPanel(
          h4("Objetivo de AgroMap"),
          p("AgroMap ha sido diseñado con el propósito de facilitar la visualización de datos geográficos y de variables relacionadas con cultivos. Muchas áreas dentro de la empresa X requieren de gráficos y representaciones visuales, pero su acceso y comprensión se ve limitada debido a la falta de conocimiento en programación o en el manejo de herramientas especializadas en geometrías espaciales."),
          
          h4("Secciones de la Aplicación"),
          p(strong("1. Visualizador:"),
            "Esta sección permite a los usuarios visualizar los datos provenientes de la Secretaría de Agricultura, Ganadería y Pesca (SAGyP), organizados por delegación. Los usuarios pueden seleccionar un cultivo y una variable de interés, como Producción, Superficie Sembrada o Superficie Cosechada, y analizar los datos correspondientes."),
          p(strong("2. Graficador delegaciones SAGyP:"),
            "En esta pestaña, el usuario puede cargar un archivo del tipo 'archivo modelo', seleccionar la variable a graficar, ajustar las opciones estéticas del gráfico y finalmente exportar la visualización generada."),
          p(strong("3. Graficador regiones GEA:"),
            "La segmentación de regiones GEA es una herramienta interna de la empresa, utilizada principalmente en las hojas de balance de las commodities. Los valores de las tres regiones (Centro, Norte y Sur) son ingresados manualmente por el usuario. Luego de seleccionar las opciones estéticas, el gráfico puede ser exportado para su uso en informes y presentaciones."),
          p(strong("4. Información sobre el desarrollo"),
            "En esta sección se comenta la selección y el proceso de limpieza de los datos."),
          br(),
          p("Para generar los gráficos en la pestaña Delegaciones SAGyP, es necesario utilizar un archivo con el siguiente formato:"),
          downloadButton("download_template", "Descargar archivo modelo"),
          br(),
          br(),
          p("Es importante destacar que las dos primeras columnas del archivo no deben ser modificadas. Sin embargo, el resto de las columnas puede ser editado, eliminado o ampliado con nuevos datos, según lo requiera el análisis.")
          
          
        )
      )
    ),
    
    # Pestaña Visualización
    tabPanel(
      "Visualizador",
      fluidPage(
        titlePanel("Análisis de Delegaciones"),
        
        # Texto explicativo
        fluidRow(
          column(12,
                 p("Seleccione un cultivo y una variable para analizar. Use la tabla para explorar las delegaciones según su ranking, y seleccione una o varias para ver su evolución temporal.")
          )
        ),
        
        # Filtros
        fluidRow(
          column(6,
                 selectInput("cultivo", "Seleccione el Cultivo:",
                             choices = unique(datos_agregados$Cultivo),  
                             selected = unique(datos_agregados$Cultivo)[1])
          ),
          column(6,
                 selectInput(
                   "variable", 
                   "Seleccione la Variable:",
                   choices = c(
                     "Producción" = "Produccion",
                     "Superficie Sembrada" = "Sup_Sembrada",
                     "Superficie Cosechada" = "Sup_Cosechada"
                   ),
                   selected = "Produccion"
                 )
          )
        ),
        
        # Tabla DT
        fluidRow(
          column(6,
                 htmlOutput("tabla_titulo"), 
                 DT::dataTableOutput("tabla_ranking") 
          ),
          column(6,
                 plotlyOutput("serie_tiempo_plot", height = "510px")  # Serie de tiempo
          )
        )
      )
    ),
    
    # Pestaña Delegaciones SAGyP
    tabPanel(
      "Graficador delegaciones SAGyP",
      fluidPage(
        titlePanel(h2("Graficador de Mapas - Delegaciones SAGyP")),
        fluidRow(
          column(12, 
                 p("Utilice esta herramienta para graficar datos sobre las delegaciones SAGyP. Cargue un archivo basado en el modelo proporcionado, seleccione las opciones estéticas, el título y la leyenda deseados, y finalmente exporte el gráfico generado en formato PNG.")
          )
        ),
        fluidRow(
          column(
            width = 4,
            fileInput("v_fileinput", label = "Cargue su archivo"),
            uiOutput("variable_selector"),  
            colourpicker::colourInput(inputId = "color_low", label = "Color más bajo", value = "#e0ffa3"),
            colourpicker::colourInput(inputId = "color_mid", label = "Color medio", value = "#95d070"),
            colourpicker::colourInput(inputId = "color_high", label = "Color más alto", value = "#5a7648"),
            colourpicker::colourInput(inputId = "color_na", label = "Color para los valores NA", value = "#f2f2f2"),
            textInput(inputId = "label_name", label = "Leyenda", value = "Escriba una leyenda"),
            textInput(inputId = "title_name", label = "Título", value = "Escriba un título"),
            downloadButton('downloadPlot', 'Descargar gráfico')
          ),
          column(
            width = 8,
            plotOutput("map", height = "600px")
          )
        )
      )
    ),
    # Pestaña Regiones GEA
    tabPanel(
      "Graficador regiones GEA",
      fluidPage(
        titlePanel(h2("Graficador de Mapas - Regiones GEA")),
        fluidRow(
          column(12, 
                 p("Utilice esta herramienta para graficar datos de las regiones GEA (Centro, Norte y Sur). Ingrese manualmente los valores correspondientes a cada región, personalice las opciones estéticas, y genere un mapa que puede ser exportado en formato PNG.")
          )
        ),
        fluidRow(
          column(
            width = 4,  
            numericInput("centro_val", "Valor para región CENTRO", value = 2, min = 0, step = 0.05),
            numericInput("norte_val", "Valor para región NORTE", value = 4, min = 0, step = 0.05),
            numericInput("sur_val", "Valor para región SUR", value = 8, min = 0, step = 0.05),
            actionButton("graficar_gea", "Graficar"),
            colourpicker::colourInput(inputId = "gea_color_low", label = "Color más bajo", value = "#e0ffa3"),
            colourpicker::colourInput(inputId = "gea_color_mid", label = "Color medio", value = "#95d070"),
            colourpicker::colourInput(inputId = "gea_color_high", label = "Color más alto", value = "#5a7648"),
            colourpicker::colourInput(inputId = "gea_color_na", label = "Color para los valores NA", value = "#f2f2f2"),
            textInput(inputId = "gea_label_name", label = "Leyenda", value = "Escriba una leyenda"),
            textInput(inputId = "gea_title_name", label = "Título", value = "Escriba un título"),
            downloadButton('downloadPlot_gea', 'Descargar gráfico')
          ),
          column(
            width = 8,  
            plotOutput("map_gea", height = "600px")
          )
        )
      )
    ),
    # Pestaña "Info"
    tabPanel(
      "Información sobre el desarrollo",
      fluidPage(
        titlePanel(h3("Información General")),
        fluidRow(
          column(
            width = 12,
            h4("Selección de Datos"),
            p("Este trabajo fue desarrollado en respuesta a las necesidades expresadas por los usuarios de datos de la empresa, quienes destacaron la importancia de contar con herramientas simples y efectivas para la visualización de mapas y delegaciones."),
            p("Los datos de geometría utilizados para los mapas fueron obtenidos del Instituto Nacional de Estadística y Censos (INDEC). La información por departamento y provincia está disponible en el siguiente enlace: ",
              a("INDEC Codgeo", href = "https://www.indec.gob.ar/indec/web/Institucional-Indec-Codgeo", target = "_blank"), "."),
            p("La serie de tiempo de la Secretaría de Agricultura, Ganadería y Pesca (SAGyP) se obtuvo de su plataforma oficial, a través de este enlace: ",
              a("Estimaciones Agrícolas", href = "https://datosestimaciones.magyp.gob.ar/reportes.php?reporte=Estimaciones", target = "_blank"), ". Sólo se seleccionaron los cultivos relevantes para el seguimiento que realiza la empresa."),
            p("SAGyP cuenta con delegaciones geográficas para la estimación de cultivos, conocidas en el ámbito agropecuario. Más información sobre estas delegaciones puede encontrarse en: ",
              a("Delegaciones SAGyP", href = "https://www.magyp.gob.ar/sitio/areas/estimaciones/delegaciones/", target = "_blank"), "."),
            p("Finalmente, se utilizó un archivo interno de la empresa que contiene las asignaciones de departamentos o partidos a las delegaciones de SAGyP, para vincular los datos geográficos con las variables analizadas.")
          ),
          column(
            width = 12,
            h4("Manipulación de Datos"),
            p("La manipulación y limpieza de los datos se realizó en un archivo interno denominado `limpieza.R`, donde se llevaron a cabo las siguientes tareas principales:"),
            tags$ul(
              tags$li("Unir el archivo auxiliar que contiene la asignación de departamentos a delegaciones."),
              tags$li("Filtrar provincias y variables irrelevantes, heredadas del censo de INDEC."),
              tags$li("Realizar la asignación de los departamentos a las delegaciones para los datos históricos de SAGyP, ajustando los nombres de algunos departamentos para garantizar la coincidencia exacta. Posteriormente, se calcularon los totales por delegación para cada cultivo y campaña.")
            ),
            p("Estos procesos se realizaron previamente en R para reducir la carga de trabajo dentro de la aplicación Shiny, optimizando así su rendimiento.")
          )
        )
      )
    )
  )
)

# SERVER
server <- function(input, output) {
  
  # Botón para la descarga del archivo modelo
  output$download_template <- downloadHandler(
    filename = function() {
      "modelo_uso.xlsx"
    },
    content = function(file) {
      file.copy("recursos/modelo_uso.xlsx", file)
    }
  )
  
  # PESTAÑA DE VISUALIZADOR
  
  #datos_ranking va a tener los datos con los filtros que seleccione el usuario
  datos_ranking <- reactive({
    req(input$cultivo, input$variable)
    
    # Determinar la última campaña disponible, esto sirve para actualizar en un futuro
    # y que no tenga un valor fijo como 2022/23
    ultima_campana <- max(datos_agregados$Campaña, na.rm = TRUE)
    
    # Filtrar datos para la última campaña
    datos_agregados %>%
      filter(Cultivo == input$cultivo, Campaña == ultima_campana) %>%
      arrange(desc(.data[[input$variable]])) %>%
      select(DELEGACION, PROVINCIA, !!sym(input$variable)) %>%
      rename(Valor = !!sym(input$variable)) 
  })
  
  output$tabla_ranking <- DT::renderDataTable({
    req(datos_ranking())
    
    DT::datatable(
      datos_ranking(),
      selection = list(mode = "multiple", selected = 1),  # Selección múltiple, fila 1 seleccionada por defecto
      rownames = FALSE,
      options = list(pageLength = 8, scrollX = TRUE)
    ) %>%
      DT::formatCurrency(
        columns = "Valor",           
        currency = "",               
        interval = 3,                
        mark = ".",                  
        digits = 0                   
      )
  })
  
  # título reactivo de la tabla en base a la variable y cultivo
  output$tabla_titulo <- renderUI({
    req(input$variable, input$cultivo)
    variable_amigable <- switch(input$variable,
                                "Produccion" = "Producción",
                                "Sup_Sembrada" = "Superficie Sembrada",
                                "Sup_Cosechada" = "Superficie Cosechada")
    
    # Crear el HTML del título
    HTML(
      paste0(
        "<p style='text-align:center; font-weight:bold; color:#386641; font-size:18px; margin-bottom:10px;'>",
        "Ranking de Delegaciones según <i>", variable_amigable, "</i> para el cultivo <i>", input$cultivo, "</i> en la última campaña",
        "</p>"
      )
    )
  })
  
  # En base a lo que el usuario seleccione en la tabla DT, debemos elegir los datos
  # para luego el gráfico de serie de tiempo
  datos_serie <- reactive({
    req(input$cultivo, input$variable)
    # Obtener las delegaciones seleccionadas
    seleccion <- input$tabla_ranking_rows_selected
    if (is.null(seleccion) || length(seleccion) == 0) return(NULL)
    delegaciones <- datos_ranking()$DELEGACION[seleccion]
    # Filtrar datos de las delegaciones seleccionadas
    datos_agregados %>%
      filter(Cultivo == input$cultivo, DELEGACION %in% delegaciones) %>%
      select(DELEGACION, Campaña, !!sym(input$variable)) %>%
      rename(Valor = !!sym(input$variable))
  })
  
  # Gráfico serie de tiempo
  output$serie_tiempo_plot <- renderPlotly({
    datos <- datos_serie()
    validate(need(!is.null(datos), "Seleccione al menos una delegación de la tabla."))
    
    plot_ly(
      data = datos,
      x = ~Campaña,
      y = ~Valor,
      color = ~DELEGACION,
      type = 'scatter',
      mode = 'lines+markers',
      text = ~paste(
        "Campaña:", Campaña, "<br>",
        "Delegación:", DELEGACION, "<br>",
        "Valor:", format(Valor, big.mark = ".", decimal.mark = ",")
      ),
      hoverinfo = "text"
    ) %>%
      layout(
        title = list(
          text = paste("Evolución de", input$variable, "en Delegaciones Seleccionadas"),
          font = list(color = "#386641", size = 16)
        ),
        xaxis = list(title = "Campaña"),
        yaxis = list(title = input$variable)
      )
  })
  
  # PESTAÑA GRAFICADOR DELEGACIONES GEA
  
  # Cargar el archivo del tipo "modelo_uso" de manera reactiva
  modelo_uso <- reactive({
    req(input$v_fileinput)
    read_excel(input$v_fileinput$datapath)
  })
  
  # Crear el selector dinámico de variables continuas del archivo, para que no 
  # aparezca provincia, delegación como graficables
  output$variable_selector <- renderUI({
    req(modelo_uso())
    selectInput(
      inputId = "selected_var",
      label = "Seleccione la columna a graficar",
      choices = colnames(modelo_uso() %>% select_if(is.numeric)),  
      selected = colnames(modelo_uso() %>% select_if(is.numeric))[1]  
    )
  })
  
  # Datos necesarios para el mapa, uniendo con los datos de geometría
  mapa_data <- reactive({
    req(modelo_uso(), input$selected_var)
    
    modelo_uso_filtrado <- modelo_uso() %>%
      select(DELEGACIONES,PROVINCIA, input$selected_var)
    
    datos_combinados <- datos_delegaciones %>%
      left_join(modelo_uso_filtrado, by = c("DELEGACION" = "DELEGACIONES"))
    
    return(datos_combinados)
  })
  
  # Generar el mapa
  mapa <- reactive({
    req(mapa_data(), input$selected_var, input$color_low, input$color_mid, input$color_high)
    
    ggplot(mapa_data()) +
      geom_sf(aes(fill = !!sym(input$selected_var)), color = "grey", size = 0.1) +  
      scale_fill_gradientn(
        colours = c(input$color_low, input$color_mid, input$color_high),  
        name = input$label_name,  
        na.value = input$color_na, 
        guide = guide_colorbar(direction = "vertical"),  
        labels = label_number(big.mark = ".", decimal.mark = ",")  
      ) +
      labs(
        title = input$title_name,
        x = NULL,  # Eliminar la etiqueta de "Longitud"
        y = NULL   # Eliminar la etiqueta de "Latitud"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 24, face = "bold"),
        legend.position = "right",  
        axis.text = element_blank(),  
        axis.ticks = element_blank(),  
        panel.grid = element_blank()   
      )
  })
  
  # Renderizar el mapa
  output$map <- renderPlot({
    mapa()
  })
  
  # Descargar el gráfico
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste0(input$title_name, ".png")
    },
    content = function(file) {
      ggsave(file, plot = mapa(), device = "png", bg = "transparent")  # Fondo transparente - solicitado por el usuario
    }
  )
  
  # PESTAÑA GRAFICADOR REGIONES GEA
  
  gea_data <- reactive({
    req(input$graficar_gea)  # Es necesario que el usuario apriete el botón graficar
    
    # Se crea un dataframe con los valores que el usuario ingresa para CENTRO, NORTE y SUR
    valores <- data.frame(
      ZONA = c("CENTRO", "NORTE", "SUR"),
      valor = c(input$centro_val, input$norte_val, input$sur_val)
    )
    
    # Unimos esos valores con la geometría, ahora por la variable ZONA
    datos_gea <- datos_delegaciones %>%
      left_join(valores, by = "ZONA")  
    
    # Asignar un color neutro a las zonas que no sean CENTRO, NORTE o SUR
    datos_gea <- datos_gea %>%
      mutate(valor = ifelse(ZONA %in% c("CENTRO", "NORTE", "SUR"), valor, NA))  # Asignar NA a las zonas no seleccionadas
    
    return(datos_gea)
  })
  
  # Generar el mapa para las Regiones GEA
  mapa_gea <- reactive({
    req(gea_data(), input$gea_color_low, input$gea_color_mid, input$gea_color_high, input$gea_color_na)
    
    ggplot(gea_data()) +
      geom_sf(aes(fill = valor), color = "grey", size = 0.1) +  
      scale_fill_gradientn(
        colours = c(input$gea_color_low, input$gea_color_mid, input$gea_color_high),  
        name = input$gea_label_name,  
        na.value = input$gea_color_na,  
        guide = guide_colorbar(direction = "vertical"),  
        labels = label_number(big.mark = ".", decimal.mark = ",")  
      ) +
      labs(
        title = input$gea_title_name,
        x = NULL,  
        y = NULL   
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 24, face = "bold"),
        legend.position = "right",  # Colocar la leyenda a la derecha
        axis.text = element_blank(),  # Eliminar los valores del eje
        axis.ticks = element_blank(),  # Eliminar los ticks del eje
        panel.grid = element_blank()   # Eliminar las líneas de la cuadrícula
      )
  })
  
  # Renderizar el mapa de GEA
  output$map_gea <- renderPlot({
    mapa_gea()
  })
  # Descargar el gráfico de GEA
  output$downloadPlot_gea <- downloadHandler(
    filename = function() {
      paste0(input$gea_title_name, ".png")
    },
    content = function(file) {
      ggsave(file, plot = mapa_gea(), device = "png", bg = "transparent")  # Fondo transparente - solicitado por el usuario
    }
  )
}

# Ejecutar la aplicación Shiny
shinyApp(ui = ui, server = server)

