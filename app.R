library(shinypanels)
library(parmesan)
library(shinyinvoer)
library(shi18ny)
library(dsmodules)
library(purrr)
library(brickr)

# Internacionalización
# Arreglar código
# Parámetros width y height no son tan independientes

ui <- panelsPage(useShi18ny(),
                 panel(title = ui_("upload_data"),
                       width = 200,
                       body = uiOutput("image_input")),
                 panel(title = ui_("dataset"),
                       width = 300,
                       body = imageOutput("data_preview")),
                 panel(title = ui_("options"),
                       width = 250,
                       color = "chardonnay",
                       body = uiOutput("controls"),
                       footer =  div(style = "text-align: center; display: flex; align-items: baseline;",
                                     `data-for-btn` = "generate",
                                     uiOutput("generate_bt"),
                                     span(class = "btn-loading-container",
                                          img(style = "display: none; margin-left: 18px;",
                                              class = "btn-loading-indicator",
                                              src = "data:image/gif;base64,R0lGODlhEAALAPQAAP///wAAANra2tDQ0Orq6gYGBgAAAC4uLoKCgmBgYLq6uiIiIkpKSoqKimRkZL6+viYmJgQEBE5OTubm5tjY2PT09Dg4ONzc3PLy8ra2tqCgoMrKyu7u7gAAAAAAAAAAACH/C05FVFNDQVBFMi4wAwEAAAAh/hpDcmVhdGVkIHdpdGggYWpheGxvYWQuaW5mbwAh+QQJCwAAACwAAAAAEAALAAAFLSAgjmRpnqSgCuLKAq5AEIM4zDVw03ve27ifDgfkEYe04kDIDC5zrtYKRa2WQgAh+QQJCwAAACwAAAAAEAALAAAFJGBhGAVgnqhpHIeRvsDawqns0qeN5+y967tYLyicBYE7EYkYAgAh+QQJCwAAACwAAAAAEAALAAAFNiAgjothLOOIJAkiGgxjpGKiKMkbz7SN6zIawJcDwIK9W/HISxGBzdHTuBNOmcJVCyoUlk7CEAAh+QQJCwAAACwAAAAAEAALAAAFNSAgjqQIRRFUAo3jNGIkSdHqPI8Tz3V55zuaDacDyIQ+YrBH+hWPzJFzOQQaeavWi7oqnVIhACH5BAkLAAAALAAAAAAQAAsAAAUyICCOZGme1rJY5kRRk7hI0mJSVUXJtF3iOl7tltsBZsNfUegjAY3I5sgFY55KqdX1GgIAIfkECQsAAAAsAAAAABAACwAABTcgII5kaZ4kcV2EqLJipmnZhWGXaOOitm2aXQ4g7P2Ct2ER4AMul00kj5g0Al8tADY2y6C+4FIIACH5BAkLAAAALAAAAAAQAAsAAAUvICCOZGme5ERRk6iy7qpyHCVStA3gNa/7txxwlwv2isSacYUc+l4tADQGQ1mvpBAAIfkECQsAAAAsAAAAABAACwAABS8gII5kaZ7kRFGTqLLuqnIcJVK0DeA1r/u3HHCXC/aKxJpxhRz6Xi0ANAZDWa+kEAA7AAAAAAAAAAAA"),
                                          HTML("<i class = 'btn-done-indicator fa fa-check' style = 'display: none; margin-left: 18px;'> </i>")))),
                 panel(title = ui_("viz"),
                       color = "chardonnay",
                       can_collapse = FALSE,
                       body = div(langSelectorInput("lang", position = "fixed"),
                                  uiOutput("result", style = "text-align: center;"),
                                  shinypanels::modal(id = "test",
                                                     title = ui_("download_image"),
                                                     uiOutput("modal"))),
                       footer = shinypanels::modalButton(label = "Download image", modal_id = "test")))


server <- function(input, output, session) {

  i18n <- list(defaultLang = "en", availableLangs = c("es", "en", "pt"))
  lang <- callModule(langSelector, "lang", i18n = i18n, showSelector = TRUE)
  observeEvent(lang(), {
    uiLangUpdate(input$shi18ny_ui_classes, lang())
  })  
  
  output$image_input <- renderUI({
    choices <- c("sampleData", "fileUpload", "url")
    names(choices) <- i_(c("sample", "upload", "url"), lang = lang())
    imageInputUI("initial_data",
                 choices = choices,
                 selected = ifelse(is.null(input$`initial_data-imageInput`), "sampleData", input$`initial_data-imageInput`))
  })
  
  path <- "parmesan"
  parmesan <- parmesan_load(path)
  parmesan_input <- parmesan_watch(input, parmesan)
  parmesan_alert(parmesan, env = environment())
  parmesan_lang <- reactive({i_(parmesan, lang(), keys = c("label", "choices"))})
  output_parmesan("controls",
                  parmesan = parmesan_lang,
                  input = input,
                  output = output, 
                  env = environment())
  
  output$generate_bt <- renderUI({
    gn <- i_("generate", lang())
    actionButton("generate", gn, style = "margin: 0;")
  })
  
  output$modal <- renderUI({
    dw <- i_("download", lang())
    downloadImageUI("download_data_button", dw, formats = c("jpeg", "png", "svg", "pdf"))
  })
  
  # reactivo que almacena el plot y la imagen procesada
  plot_lego <- reactiveValues(dtin = NULL,
                              img = NULL,
                              plt = NULL)
  
  labels <- reactive({
    list(sampleLabel = i_("sample_lb", lang()), 
         sampleFiles = list("Tapete persa" = "www/99028399493-1.jpg",
                            "Madera tejida" = "www/h1_t.png",
                            "Rincón encontrado" = "www/pero.png"),
         uploadLabel = i_("upload_lb", lang()), uploadButtonLabel = i_("upload_bt_lb", lang()), uploadPlaceholder = i_("upload_pl", lang()),
         urlLabel = i_("url_lb", lang()))
  })
  
  observe({
    plot_lego$dtin <- do.call(callModule,
                              c(imageInput,
                                "initial_data",
                                labels()))
  })
  
  # renderizando lo importado
  output$data_preview <- renderImage({
  req(plot_lego$dtin())
  dt <- plot_lego$dtin()
  dt$width <- "100%"
  dt
  }, deleteFile = FALSE)
  
  # imágen leída
  observeEvent(plot_lego$dtin(), {
    req(plot_lego$dtin()$src)
    lg <- plot_lego$dtin()$src
    r0 <- tryCatch(png::readPNG(lg), error = function(e) e)
    r1 <- tryCatch(jpeg::readJPEG(lg), error = function(e) e)
    w0 <- which(c(!any(grepl("error", class(r0))), !any(grepl("error", class(r1))))) - 1
    r2 <- get(paste0("r", w0))
    plot_lego$img <- r2
  })
  
  # valores iniciales de parámetros dependientes del input del usuarioe
  # almacenando el width y height de la imágen en reactivos para inicializar los sliders
  width <- reactive({
    req(plot_lego$img)
    r0 <- dim(plot_lego$img)[1:2]
    # escalar las dimensiones
    scl <- 78
    scl <- 110
    w1 <- which(r0 == max(r0))
    if (w1 == 1) {
      floor((r0[2] * scl) / r0[1])
    } else {
      scl
    }
    # r0[1]
  })
  
  height <- reactive({
    req(plot_lego$img)
    r0 <- dim(plot_lego$img)[1:2]
    # escalar las dimensiones
    scl <- 110
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
    
    session$sendCustomMessage("setButtonState", c("loading", "generate_bt"))
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
    session$sendCustomMessage("setButtonState", c("done", "generate_bt"))
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
    session$sendCustomMessage("setButtonState", c("none", "generate_bt"))
  })
  
  # descargas
  callModule(downloadImage, "download_data_button", graph = reactive(plot_lego$plt), lib = "ggplot", formats = c("jpeg", "png", "svg", "pdf"))
  
}


shinyApp(ui, server)