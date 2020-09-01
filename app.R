library(shinypanels)
library(parmesan)
library(shinyinvoer)
library(shi18ny)
library(V8)
library(dsmodules)
library(dspins)
library(brickr)
library(shinycustomloader)
library(magick)
library(ggplot2)

# Parámetros width y height no son tan independientes
# loader gif style
st <- "
.loader-img {
  margin-top: 27%;
  width: 27% !important;
}
"

ui <- panelsPage(includeScript(paste0(system.file("js/", package = "dsmodules"), "downloadGen.js")),
                 useShi18ny(),
                 showDebug(),
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
                       footer =  div(style = "text-align: center; display: flex; align-items: center;",
                                     `data-for-btn` = "generate",
                                     uiOutput("generate_bt"),
                                     span(class = "btn-loading-container",
                                          img(style = "display: none; margin-left: 18px;",
                                              class = "btn-loading-indicator",
                                              src = "data:image/gif;base64,R0lGODlhEAALAPQAAP///wAAANra2tDQ0Orq6gYGBgAAAC4uLoKCgmBgYLq6uiIiIkpKSoqKimRkZL6+viYmJgQEBE5OTubm5tjY2PT09Dg4ONzc3PLy8ra2tqCgoMrKyu7u7gAAAAAAAAAAACH/C05FVFNDQVBFMi4wAwEAAAAh/hpDcmVhdGVkIHdpdGggYWpheGxvYWQuaW5mbwAh+QQJCwAAACwAAAAAEAALAAAFLSAgjmRpnqSgCuLKAq5AEIM4zDVw03ve27ifDgfkEYe04kDIDC5zrtYKRa2WQgAh+QQJCwAAACwAAAAAEAALAAAFJGBhGAVgnqhpHIeRvsDawqns0qeN5+y967tYLyicBYE7EYkYAgAh+QQJCwAAACwAAAAAEAALAAAFNiAgjothLOOIJAkiGgxjpGKiKMkbz7SN6zIawJcDwIK9W/HISxGBzdHTuBNOmcJVCyoUlk7CEAAh+QQJCwAAACwAAAAAEAALAAAFNSAgjqQIRRFUAo3jNGIkSdHqPI8Tz3V55zuaDacDyIQ+YrBH+hWPzJFzOQQaeavWi7oqnVIhACH5BAkLAAAALAAAAAAQAAsAAAUyICCOZGme1rJY5kRRk7hI0mJSVUXJtF3iOl7tltsBZsNfUegjAY3I5sgFY55KqdX1GgIAIfkECQsAAAAsAAAAABAACwAABTcgII5kaZ4kcV2EqLJipmnZhWGXaOOitm2aXQ4g7P2Ct2ER4AMul00kj5g0Al8tADY2y6C+4FIIACH5BAkLAAAALAAAAAAQAAsAAAUvICCOZGme5ERRk6iy7qpyHCVStA3gNa/7txxwlwv2isSacYUc+l4tADQGQ1mvpBAAIfkECQsAAAAsAAAAABAACwAABS8gII5kaZ7kRFGTqLLuqnIcJVK0DeA1r/u3HHCXC/aKxJpxhRz6Xi0ANAZDWa+kEAA7AAAAAAAAAAAA"),
                                          HTML("<i class = 'btn-done-indicator fa fa-check' style = 'display: none; margin-left: 18px;'> </i>")))),
                 panel(title = ui_("viz"),
                       style = st,
                       title_plugin = uiOutput("download"),
                       color = "chardonnay",
                       can_collapse = FALSE,
                       body = div(langSelectorInput("lang", position = "fixed"),
                                  withLoader(uiOutput("result", style = "text-align: center;"), type = "image", loader = "loading_gris.gif"))))


server <- function(input, output, session) {
  
  i18n <- list(defaultLang = "en", availableLangs = c("es", "en", "pt_BR"))
  lang <- callModule(langSelector, "lang", i18n = i18n, showSelector = FALSE)
  observeEvent(lang(), {uiLangUpdate(input$shi18ny_ui_classes, lang())})  
  
  output$image_input <- renderUI({
    choices <- c("sampleData", "fileUpload", "url")
    names(choices) <- i_(c("sample", "upload", "url"), lang = lang())
    imageInputUI("initial_data",
                 choices = choices,
                 selected = ifelse(is.null(input$`initial_data-imageInput`), "sampleData", input$`initial_data-imageInput`))
  })
  
  # reactivo que almacena el plot y la imagen procesada
  plot_lego <- reactiveValues(dtin = NULL, img = NULL, plt = NULL)
  
  labels <- reactive({
    
    sm_f <- c("www/abs.jpg", "www/book_rug.jpg", "www/horse.png")
    names(sm_f) <- i_(c("sample_ch_nm_0", "sample_ch_nm_1", "sample_ch_nm_2"), lang())
    
    list(sampleLabel = i_("sample_lb", lang()), 
         sampleFiles = sm_f,
         
         uploadLabel = i_("upload_lb", lang()),
         uploadButtonLabel = i_("upload_bt_lb", lang()),
         uploadPlaceholder = i_("upload_pl", lang()),
         
         urlLabel = i_("url_lb", lang()))
  })
  
  observe({
    plot_lego$dtin <- do.call(callModule, c(imageInput, "initial_data", labels()))
  })
  
  path <- "parmesan"
  parmesan <- parmesan_load(path)
  parmesan_input <- parmesan_watch(input, parmesan)
  parmesan_alert(parmesan, env = environment())
  parmesan_lang <- reactive({i_(parmesan, lang(), keys = c("label", "choices", "text"))})
  output_parmesan("controls",
                  parmesan = parmesan_lang,
                  input = input,
                  output = output, 
                  env = environment())
  
  output$generate_bt <- renderUI({
    gn <- i_("generate", lang())
    actionButton("generate", gn, style = "margin: 0;")
  })
  
  # updating names choices of inputs depending on language
  observeEvent(lang(), {
    ch <- as.character(parmesan$properties$inputs[[1]]$input_params$choices)
    names(ch) <- i_(ch, lang())
    updateRadioButtons(session, "color_palette", choices = ch, selected = input$color_palette)
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
    # r2 <- image_read(lg)
    plot_lego$img <- r2
  })
  
  # valores iniciales de parámetros dependientes del input del usuario
  # almacenando el width y height de la imágen en reactivos para inicializar los sliders
  width <- reactive({
    req(plot_lego$img)
    r0 <- dim(plot_lego$img)[1:2]
    # escalar las dimensiones
    scl <- 63
    scl <- 90
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
    scl <- 90
    w1 <- which(r0 == max(r0))
    if (w1 == 1) {
      scl
    } else {
      floor((r0[1] * scl) / r0[2])
    }
  })
  
  # gráfica de lego
  observeEvent(input$generate, {
    session$sendCustomMessage("setButtonState", c("loading", "generate_bt"))
    safe_image_to_mosaic <- purrr::safely(image_to_mosaic)
    assign("w1", input$height, envir = globalenv())
    assign("w0", input$width, envir = globalenv())
    plt <- plot_lego$img %>%
      safe_image_to_mosaic(img_size = c(input$width, input$height),
                           # color_table = input$color_table_img,
                           # dithering = input$dithering,
                           method = input$method,
                           color_palette = input$color_palette,
                           contrast = input$contrast,
                           brightness = input$brightness)
    
    if (!is.null(plt$result)) plt$result <- build_mosaic(plt$result) 
    plot_lego$plt <- plt
    session$sendCustomMessage("setButtonState", c("done", "generate_bt"))
    # }
  })
  # }, ignoreNULL = FALSE)
  
  output$download <- renderUI({
    lb <- i_("download_image", lang())
    dw <- i_("download", lang())
    gl <- i_("get_link", lang())
    mb <- list(textInput("name", i_("gl_name", lang())),
               textInput("description", i_("gl_description", lang())),
               selectInput("license", i_("gl_license", lang()), choices = c("CC0", "CC-BY")),
               selectizeInput("tags", i_("gl_tags", lang()), choices = list("No tag" = "no-tag"), multiple = TRUE, options = list(plugins= list('remove_button', 'drag_drop'))),
               selectizeInput("category", i_("gl_category", lang()), choices = list("No category" = "no-category")))
    downloadDsUI("download_data_button", dropdownLabel = lb, text = dw, formats = c("jpeg", "png"),# "svg", "pdf"), 
                 display = "dropdown", dropdownWidth = 180, getLinkLabel = gl, modalTitle = gl, modalBody = mb,
                 modalButtonLabel = i_("gl_save", lang()), modalLinkLabel = i_("gl_url", lang()), modalIframeLabel = i_("gl_iframe", lang()),
                 modalFormatChoices = c("PNG" = "png", "SVG" = "svg"))
  })
  
  # renderizando mosaico ggplot
  output$result <- renderUI({
    ls <- list("www/abs.jpg" = "abs_lg.png",
               "www/book_rug.jpg" = "book_rug_lg.jpeg",
               "www/horse.png" = "horse_lg.jpeg")
    if (input$`initial_data-imageInput` == "sampleData" && input$generate == 0) {
      img(src = ls[[plot_lego$dtin()$src]])
    } else {
      res <- plot_lego$plt
      if (is.null(res$result)) {
        infomessage(p(res$error$message))
      } else {
        plotOutput("plot", height = "61vh")
      }
    }
  })
  
  output$plot <- renderPlot({
    plot_lego$plt$result
  })
  
  # quitando el chulo de generado cuando cualquier parámetro cambia
  observeEvent(list(parmesan_input(), plot_lego$dtin), {
    session$sendCustomMessage("setButtonState", c("none", "generate_bt"))
  })
  
  # url params
  par <- list(user_name = "brandon", org_name = NULL)
  url_par <- reactive({
    url_params(par, session)
  })
  
  # prepare element for pining (for htmlwidgets or ggplots)
  # función con user board connect y set locale
  pin_ <- function(x, bkt, ...) {
    x <- dsmodules:::eval_reactives(x)
    bkt <- dsmodules:::eval_reactives(bkt)
    nm <- input$`download_data_button-modal_form-name`
    if (!nzchar(input$`download_data_button-modal_form-name`)) {
      nm <- paste0("saved", "_", gsub("[ _:]", "-", substr(as.POSIXct(Sys.time()), 1, 19)))
      updateTextInput(session, "download_data_button-modal_form-name", value = nm)
    }
    dv <- dsviz(x,
                name = nm,
                description = input$`download_data_button-modal_form-description`,
                license = input$`download_data_button-modal_form-license`,
                tags = input$`download_data_button-modal_form-tags`,
                category = input$`download_data_button-modal_form-category`)
    dspins_user_board_connect(bkt)
    Sys.setlocale(locale = "en_US.UTF-8")
    pin(dv, bucket_id = bkt)
  }
  
  # descargas
  observe({
    req(input$`initial_data-inputDataSample`)
    # ls <- list("www/abs.jpg" = "abs_lg.svg")#,
    ls <- list("www/abs.jpg" = "abs_lg.jpeg",
               "www/book_rug.jpg" = "book_rug_lg.jpeg",
               "www/horse.png" = "horse_lg.jpeg")
    if (input$`initial_data-imageInput` == "sampleData" & input$generate == 0) {
      # r0 <- image_read_svg(paste0("www/", ls[[plot_lego$dtin()$src]]))
      r0 <- jpeg::readJPEG(paste0("www/", ls[[plot_lego$dtin()$src]]))
      lapply(1:2, function(z) {
        fn <- c("jpeg::writeJPEG", "png::writePNG", "rsvg::rsvg_svg", "rsvg::rsvg_pdf")[z]
        fr <- c("jpeg", "png", "svg", "pdf")[z]
        buttonId <- paste0("download_data_button-download_data_button-DownloadImg", fr)
        output[[paste0("download_data_button-download_data_button-DownloadImg", fr)]] <- shiny::downloadHandler(filename = function() {
          paste0("plot-", gsub(" ", "_", substr(as.POSIXct(Sys.time()), 1, 19)), ".", fr)
        }, content = function(file) {
          # if (z %in% 3) {
          #   r0 <- image_read_svg(paste0("www/", ls[[plot_lego$dtin()$src]]))
          #   
          #   # tmp <- paste(tempdir(), "svg", sep = ".")
          #   # svglite::svglite(tmp, width = 10, height = 7)
          #   # plt0
          #   # dev.off()
          # } else {
          #   r0 <- image_read(paste0("www/", ls[[plot_lego$dtin()$src]]))
          # }
          # image_write(r0, file, format = fr)
          do.call(eval(parse(text = fn)), list(r0, file))
        })
      })
    } else {
      downloadDsServer("download_data_button", element = reactive(plot_lego$plt$result), formats = c("jpeg", "png"),
                       errorMessage = NULL,# i_("gl_error", lang()),
                       modalFunction = pin_, reactive(plot_lego$plt$result),
                       bkt = url_par()$inputs$user_name)
    }
  })
  
}



shinyApp(ui, server)