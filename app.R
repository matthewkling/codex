
library(shiny)
library(shinythemes)
library(leaflet)
library(raster)
library(tidyverse)
library(shinydashboard)


d <- readRDS("data/lookup.rds")
counties <- readRDS("data/counties.rds")


vars <- readRDS("data/variables.rds")


ui <- navbarPage("CODEX [county data explorer]",
                 theme = shinytheme("slate"),
                 tabPanel("map",
                          
                          tags$style(type = "text/css", 
                                     "#map {height: calc(100vh - 80px) !important;}
                                     
                                      #image img {max-width: 100%; width: 100%; height: auto}
                                     
                                     .leaflet-container { background: #333333; }
                                     .leaflet-control { background: #333333; color: #ffffff; }
                                     .legend svg text { fill: #ffffff; }
                                     .legend svg line { stroke: #ffffff; }
                                     
                                     .shiny-notification {position:fixed;
                                     height: 50px;width: 250px;
                                     top: calc(50% - 50px);; left: calc(50% - 50px);;}
                                     "),
                          
                          column(2,
                                 selectInput("dataset", "dataset",
                                             unique(d$source_dataset),
                                             "Schlenker climate"),
                                 selectInput("variable", "variable",
                                             sort(unique(d$variable))),
                                 selectInput("normalizer", "normalizing variable",
                                             c("none", sort(unique(d$variable))),
                                             "none"),
                                 selectInput("year", "year",
                                             unique(d$year[d$variable == d$variable[1]]),
                                             unique(d$year[1])),
                                 checkboxInput("logtrans", "log-transform"),
                                 actionButton("go", "Go"),
                                 br(),
                                 br(),
                                 plotOutput("histogram", height = "200px")
                          ),
                          
                          column(10,
                                 leafletOutput("map")
                          )
                 ),
                 tabPanel("metadata",
                          dataTableOutput("metadata"))
                 
)

server <- function(input, output, session) {
  
  output$metadata <- renderDataTable({vars})
  
  observe({
    updateSelectInput(session, "variable",
                      choices = sort(unique(d$variable[d$source_dataset == input$dataset])))
    updateSelectInput(session, "normalizer",
                      choices = c("none", sort(unique(d$variable[d$source_dataset == input$dataset]))))
  })
  
  observe({
    updateSelectInput(session, "year",
                      choices = sort(unique(d$year[d$variable == input$variable])))
  })
  
  title <- reactive({
    ifelse(input$normalizer == "none",
           input$variable,
           paste(input$variable, input$normalizer, sep = " /\n"))
  })
  
  
  p <- reactive({
    # input = list(variable = "population", normalizer = "population", year = "2012")
    
    y <- counties
    dd <- d %>% 
      filter(variable == input$variable) %>%
      filter(year == input$year) %>%
      pull(id) %>%
      paste0("data/", ., ".rds") %>%
      readRDS()
    y@data <- left_join(y@data, dd)
    
    if(input$normalizer != "none"){
      dd <- d %>% 
        filter(variable == input$normalizer) %>%
        filter(year == input$year) %>%
        pull(id) %>%
        paste0("data/", ., ".rds") %>%
        readRDS() %>%
        rename(norm = value)
      y@data <- y@data %>%
        left_join(dd) %>%
        mutate(value = value / norm) %>%
        select(-norm)
    }
    
    if(input$logtrans) y$value <- log(y$value)
    y
  })
  
  colors <- c("dodgerblue", "purple", "red", "yellow")
  
  observeEvent(input$go, {
    # p = function(){y}
    
    v <- na.omit(p()$value)
    v <- v[is.finite(v)]
    
    output$histogram <- renderPlot({
      p()@data %>%
        mutate(bin = cut(value, 20)) %>%
        group_by(bin) %>%
        summarize(freq = sum(farmland, na.rm = T)) %>%
        mutate(bin = str_remove_all(bin, "\\(|\\]")) %>%
        separate(bin, c("low", "high"), ",", convert = T) %>%
        mutate(value = (low + high) / 2) %>%
        ggplot(aes(value, freq, fill = value)) +
        geom_bar(stat = "identity") +
        scale_fill_gradientn(colors = colors) +
        scale_x_continuous(breaks = signif(range(v), 3)) +
        labs(x = title(),
             y = "ag land area") +
        theme(legend.position = "none",
              panel.background = element_blank(),
              panel.grid = element_blank(),
              axis.text.y = element_blank(),
              axis.text.x = element_text(color = "white"),
              axis.title = element_text(color = "white"),
              plot.background = element_rect(fill = "gray17", color = NA))
    })
  })
  
  
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 2, maxZoom = 7)) %>%
      setView(lng=-100, lat=40, zoom=5) %>%
      addProviderTiles(providers$CartoDB.DarkMatter)
  })
  
  observeEvent(input$go, {
    
    v <- na.omit(p()$value)
    v <- v[is.finite(v)]
    
    pal <- colorNumeric(colors,
                        domain = range(v),
                        na.color = "gray")
    
    labels <- sprintf(
      "<strong>%s county</strong><br/>%g",
      p()$NAME, p()$value) %>% 
      lapply(htmltools::HTML)
    
    leafletProxy("map") %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(data = p(),
                  stroke = F,
                  fillColor = ~pal(value),
                  fillOpacity = .75,
                  smoothFactor = 1.5,
                  label = labels,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "10px",
                    direction = "auto")) %>%
      addLegend(title = title(), 
                pal = pal, bins = 10, opacity = .75,
                values = seq(min(v), max(v), length.out = 100))
  })
  
  
}

shinyApp(ui = ui, server = server)