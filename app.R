library(shinypanels)
library(parmesan)
library(shinyinvoer)
library(hotr)
library(dsmodules)
library(homodatum)
library(brickr)


ui <- panelsPage(panel(title = "Upload Data", 
                       width = 400,
                       body = imageInputUI("initial_data",
                                           choices = list("Sample data" = "sampleData",
                                                          "JPEG/PNG upload" = "fileUpload",
                                                          "Image addres (url)" = "url"),
                                           selected = "sampleData")),
                 panel(title = "Dataset",
                       width = 400,
                       body = imageOutput("data_preview")),
                 panel(title = "Options",
                       width = 400,
                       body = div(uiOutput("controls0"),
                                  uiOutput("controls1"),
                                  uiOutput("controls2"),
                                  uiOutput("controls3")),
                       
                       footer =  div(style = "text-align: center; display: flex; align-items: baseline;",
                                     `data-for-btn` = "generate",
                                     actionButton("generate", "Generate", style = "margin: 0;"),
                                     span(class = "btn-loading-container",
                                          # img(style = "display: none; position: relative; right: 110px;",
                                          img(style = "display: none; margin-left: 18px;",
                                              class = "btn-loading-indicator",
                                              src = "data:image/gif;base64,R0lGODlhEAALAPQAAP///wAAANra2tDQ0Orq6gYGBgAAAC4uLoKCgmBgYLq6uiIiIkpKSoqKimRkZL6+viYmJgQEBE5OTubm5tjY2PT09Dg4ONzc3PLy8ra2tqCgoMrKyu7u7gAAAAAAAAAAACH/C05FVFNDQVBFMi4wAwEAAAAh/hpDcmVhdGVkIHdpdGggYWpheGxvYWQuaW5mbwAh+QQJCwAAACwAAAAAEAALAAAFLSAgjmRpnqSgCuLKAq5AEIM4zDVw03ve27ifDgfkEYe04kDIDC5zrtYKRa2WQgAh+QQJCwAAACwAAAAAEAALAAAFJGBhGAVgnqhpHIeRvsDawqns0qeN5+y967tYLyicBYE7EYkYAgAh+QQJCwAAACwAAAAAEAALAAAFNiAgjothLOOIJAkiGgxjpGKiKMkbz7SN6zIawJcDwIK9W/HISxGBzdHTuBNOmcJVCyoUlk7CEAAh+QQJCwAAACwAAAAAEAALAAAFNSAgjqQIRRFUAo3jNGIkSdHqPI8Tz3V55zuaDacDyIQ+YrBH+hWPzJFzOQQaeavWi7oqnVIhACH5BAkLAAAALAAAAAAQAAsAAAUyICCOZGme1rJY5kRRk7hI0mJSVUXJtF3iOl7tltsBZsNfUegjAY3I5sgFY55KqdX1GgIAIfkECQsAAAAsAAAAABAACwAABTcgII5kaZ4kcV2EqLJipmnZhWGXaOOitm2aXQ4g7P2Ct2ER4AMul00kj5g0Al8tADY2y6C+4FIIACH5BAkLAAAALAAAAAAQAAsAAAUvICCOZGme5ERRk6iy7qpyHCVStA3gNa/7txxwlwv2isSacYUc+l4tADQGQ1mvpBAAIfkECQsAAAAsAAAAABAACwAABS8gII5kaZ7kRFGTqLLuqnIcJVK0DeA1r/u3HHCXC/aKxJpxhRz6Xi0ANAZDWa+kEAA7AAAAAAAAAAAA"),
                                          # HTML("<i class = 'btn-done-indicator fa fa-check' style = 'display: none; position: relative; right: 110px;'> </i>"))
                                          HTML("<i class = 'btn-done-indicator fa fa-check' style = 'display: none; margin-left: 18px;'> </i>"))
                       )),
                 panel(title = "Viz",
                       width = 464,
                       body = div(plotOutput("result"),
                                  shinypanels::modal(id = "test",
                                                     title = "Download plot",
                                                     dsmodules::downloadImagesUI("download_data_button", "Descarga", c( "jpeg", "pdf", "png", "svg"))),
                                  shinypanels::modalButton(label = "Download image", modal_id = "test"))))


config_path <- "parmesan"
# Reactive part
input_ids <- parmesan_input_ids(section = NULL, config_path = "parmesan")
input_ids_values <- lapply(input_ids, function(i) {
  NA
})
names(input_ids_values) <- input_ids


server <- function(input, output, session) {
  
  react_env <- new.env()
  
  # reactivo que almacena el plot y la imagen procesada
  plot_lego <- reactiveValues(img = NULL,
                              plt = NULL)
  
  datasetInput <- callModule(imageInput,
                             "initial_data",
                             sampleFile = list("Tapete persa" = "www/99028399493-1.jpg",
                                               "Madera tejida" = "www/h1_t.png",
                                               "Rincón encontrado" = "www/pero.png"))
  
  # renderizando los parámetros
  output$controls0 <- renderUI({
    parmesan_render_ui(sections = "Colors", config_path = config_path, input = input, env = react_env)
  })
  
  output$controls1 <- renderUI({
    parmesan_render_ui(sections = "Mosaic method", config_path = config_path, input = input, env = react_env)
  })
  
  output$controls2 <- renderUI({
    parmesan_render_ui(sections = "Image properties", config_path = config_path, input = input, env = react_env)
  })
  
  output$controls3 <- renderUI({
    parmesan_render_ui(sections = "Image size", config_path = config_path, input = input, env = react_env)
  })
  
  # renderizando lo importado
  output$data_preview <- renderImage({
    req(datasetInput())
    dt <- datasetInput()
    dt$width <- "100%"
    dt
  }, deleteFile = FALSE)
  
  # imágen leída
  observeEvent(datasetInput(), {
    req(datasetInput()$src)
    lg <- datasetInput()$src
    r0 <- tryCatch(png::readPNG(lg), error = function(e) e)
    r1 <- tryCatch(jpeg::readJPEG(lg), error = function(e) e)
    w0 <- which(c(!any(grepl("error", class(r0))), !any(grepl("error", class(r1))))) - 1
    r2 <- get(paste0("r", w0))
    plot_lego$img <- r2
  })
  
  # valores iniciales de parámetros dependientes del input del usuarioe
  # almacenando el width y height de la imágen en reactivos para inicializar los sliders
  width <- reactive({
    # req(plot_lego$img)
    r0 <- dim(plot_lego$img)[1:2]
    # escalar las dimensiones
    scl <- 78
    w1 <- which(r0 == max(r0))
    if (w1 == 1) {
      (r0[2] * scl) / r0[1]
    } else {
      scl
    }
  })
  
  height <- reactive({
    r0 <- dim(plot_lego$img)[1:2]
    # escalar las dimensiones
    scl <- 78
    w1 <- which(r0 == max(r0))
    if (w1 == 1) {
      scl
    } else {
      (r0[1] * scl) / r0[2]
    }
  })
  
  # si cambia la imagen, el mosaico se reinicializa
  observeEvent(datasetInput(), {
    plot_lego$plt <- NULL
  })
  
  # gráfica de lego
  observeEvent(input$generate, {
    session$sendCustomMessage("setButtonState", c("loading", "generate"))
    plt <- plot_lego$img %>%
      image_to_mosaic(img_size = c(input$width, input$height),
                      # color_table = input$color_table_img,
                      method = input$method,
                      color_palette = input$color_palette,
                      dithering = input$dithering,
                      contrast = input$contrast,
                      brightness = input$brightness) %>%
      build_mosaic()
    plot_lego$plt <- plt
    session$sendCustomMessage("setButtonState", c("done", "generate"))
  })
  
  # renderizando mosaico ggplot
  output$result <- renderPlot({
    plot_lego$plt
  })
  
  # descargas
  callModule(downloadImages, "download_data_button", graph = plot_lego$plt, lib = "ggplot", formats = c("jpeg", "pdf", "svg", "png"))
  
}


shinyApp(ui, server)