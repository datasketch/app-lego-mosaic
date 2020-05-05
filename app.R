library(shinypanels)
library(parmesan)
library(shinyinvoer)
library(dsmodules)
library(purrr)
library(brickr)

# Internacionalización
# Arreglar código
# Parámetros width y height no son tan independientes

ui <- panelsPage(panel(title = "Upload Data", 
                       width = 200,
                       body = imageInputUI("initial_data",
                                           choices = list("Sample data" = "sampleData",
                                                          "JPEG/PNG upload" = "fileUpload",
                                                          "Image from URL" = "url"),
                                           selected = "sampleData")),
                 panel(title = "Dataset",
                       width = 300,
                       body = imageOutput("data_preview")),
                 panel(title = "Options",
                       width = 250,
                       body = uiOutput("controls"),
                       footer =  div(style = "text-align: center; display: flex; align-items: baseline;",
                                     `data-for-btn` = "generate",
                                     actionButton("generate", "Generate", style = "margin: 0;"),
                                     span(class = "btn-loading-container",
                                          img(style = "display: none; margin-left: 18px;",
                                              class = "btn-loading-indicator",
                                              src = "data:image/gif;base64,R0lGODlhEAALAPQAAP///wAAANra2tDQ0Orq6gYGBgAAAC4uLoKCgmBgYLq6uiIiIkpKSoqKimRkZL6+viYmJgQEBE5OTubm5tjY2PT09Dg4ONzc3PLy8ra2tqCgoMrKyu7u7gAAAAAAAAAAACH/C05FVFNDQVBFMi4wAwEAAAAh/hpDcmVhdGVkIHdpdGggYWpheGxvYWQuaW5mbwAh+QQJCwAAACwAAAAAEAALAAAFLSAgjmRpnqSgCuLKAq5AEIM4zDVw03ve27ifDgfkEYe04kDIDC5zrtYKRa2WQgAh+QQJCwAAACwAAAAAEAALAAAFJGBhGAVgnqhpHIeRvsDawqns0qeN5+y967tYLyicBYE7EYkYAgAh+QQJCwAAACwAAAAAEAALAAAFNiAgjothLOOIJAkiGgxjpGKiKMkbz7SN6zIawJcDwIK9W/HISxGBzdHTuBNOmcJVCyoUlk7CEAAh+QQJCwAAACwAAAAAEAALAAAFNSAgjqQIRRFUAo3jNGIkSdHqPI8Tz3V55zuaDacDyIQ+YrBH+hWPzJFzOQQaeavWi7oqnVIhACH5BAkLAAAALAAAAAAQAAsAAAUyICCOZGme1rJY5kRRk7hI0mJSVUXJtF3iOl7tltsBZsNfUegjAY3I5sgFY55KqdX1GgIAIfkECQsAAAAsAAAAABAACwAABTcgII5kaZ4kcV2EqLJipmnZhWGXaOOitm2aXQ4g7P2Ct2ER4AMul00kj5g0Al8tADY2y6C+4FIIACH5BAkLAAAALAAAAAAQAAsAAAUvICCOZGme5ERRk6iy7qpyHCVStA3gNa/7txxwlwv2isSacYUc+l4tADQGQ1mvpBAAIfkECQsAAAAsAAAAABAACwAABS8gII5kaZ7kRFGTqLLuqnIcJVK0DeA1r/u3HHCXC/aKxJpxhRz6Xi0ANAZDWa+kEAA7AAAAAAAAAAAA"),
                                          HTML("<i class = 'btn-done-indicator fa fa-check' style = 'display: none; margin-left: 18px;'> </i>")))),
                 panel(title = "Viz",
                       can_collapse = FALSE,
                       body = div(uiOutput("result", style = "text-align: center;"),
                                  shinypanels::modal(id = "test",
                                                     title = "Download image",
                                                     dsmodules::downloadImageUI("download_data_button", "Download", c("jpeg", "png", "svg", "pdf")))),
                       footer = shinypanels::modalButton(label = "Download image", modal_id = "test")))


server <- function(input, output, session) {

  path <- "parmesan"
  parmesan <- parmesan_load(path)
  parmesan_input <- parmesan_watch(input, parmesan)
  parmesan_alert(parmesan, env = environment())
  output_parmesan("controls",
                  parmesan = parmesan,
                  input = input,
                  output = output)
  
  # reactivo que almacena el plot y la imagen procesada
  plot_lego <- reactiveValues(img = NULL,
                              plt = NULL)
  
  datasetInput <- callModule(imageInput,
                             "initial_data",
                             sampleFile = list("Tapete persa" = "www/99028399493-1.jpg",
                                               "Madera tejida" = "www/h1_t.png",
                                               "Rincón encontrado" = "www/pero.png"))
  
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
    r0 <- dim(plot_lego$img)[1:2]
    # escalar las dimensiones
    # scl <- 78
    scl <- 150
    w1 <- which(r0 == max(r0))
    if (w1 == 1) {
      floor((r0[2] * scl) / r0[1])
    } else {
      scl
    }
    # r0[1]
  })
  
  height <- reactive({
    r0 <- dim(plot_lego$img)[1:2]
    assign("r0", r0, envir = globalenv())
    # escalar las dimensiones
    scl <- 150
    w1 <- which(r0 == max(r0))
    if (w1 == 1) {
      scl
    } else {
      floor((r0[1] * scl) / r0[2])
    }
    # r0[2]
  })
  
  
  # gráfica de lego
  observeEvent(input$generate, {
    lapply(c("jpeg", "png", "svg", "pdf"), function(z) {
      buttonId <- paste0("download_data_button-DownloadImg", z)
      session$sendCustomMessage("setButtonState", c("none", buttonId)) 
    })
    
    session$sendCustomMessage("setButtonState", c("loading", "generate"))
    plt <- plot_lego$img %>%
      image_to_mosaic(
        img_size = c(input$width, input$height),
                      # color_table = input$color_table_img,
                      # dithering = input$dithering,
                      method = input$method,
                      color_palette = input$color_palette,
                      contrast = input$contrast,
        brightness = input$brightness
                      ) %>%
      build_mosaic()
    plot_lego$plt <- plt
    session$sendCustomMessage("setButtonState", c("done", "generate"))
  })
  
  # renderizando mosaico ggplot
  output$result <- renderUI({
    if (is.null(plot_lego$plt)) {
      # img(src = "plt_in.jpeg")
      img(src = "plt_in.png")
    } else {
      plotOutput("plot", height = "61vh")
    }
  })
  
  output$plot <- renderPlot({
    plot_lego$plt
  })
  
  # quitando el chulo de generado cuando cualquier parámetro cambia
  observeEvent(list(parmesan_input(), datasetInput()), {
    session$sendCustomMessage("setButtonState", c("none", "generate"))
  })
  
  # descargas
  callModule(downloadImage, "download_data_button", graph = reactive(plot_lego$plt), lib = "ggplot", formats = c("jpeg", "png", "svg", "pdf"))
  
}


shinyApp(ui, server)