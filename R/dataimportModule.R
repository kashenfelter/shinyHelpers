# TODO:
# - change naming to underscore
# - think of a way to make the user not have to duplicate the UI and server code

#' @export
dataimportModule <- function(id = "dataimport",
                             fileExt = c("csv", "txt", "xlsx", "ods"),
                             sampleDataPackage = NULL,
                             sampleDatasets = NULL) {
  list(
    ui = function() dataimportUI(id, fileExt, sampleDataPackage, sampleDatasets),
    server = function() dataimportServer(id, fileExt, sampleDataPackage, sampleDatasets)
  )
}

#' @export
doit <- function() {
  fileExt = c("csv", "txt", "xlsx", "ods")
  
  expl_data <- list("Ebola" = outbreaks::ebola_sim$linelist, 
                 "MERS" = outbreaks::mers_korea_2015$linelist,
                 "SARS Canada 2003" = outbreaks::sars_canada_2003)
  
  dataimport <- dataimportModule(
    "vv", fileExt = fileExt, sampleDataPackage = "outbreaks",
    sampleDatasets = expl_data
  )
  
  ui <- fluidPage(
    dataimport$ui(),
    br(),
    DT::dataTableOutput("table")
  )
  
  server <- function(input, output, session) {
    mydata <- dataimport$server()
    output$table <- DT::renderDataTable({
      mydata()
    })
  }
  
  runApp(shinyApp(ui = ui, server = server))
}



SUPPORTED_FILES <- c("csv", "txt", "xlsx", "ods")

check_file_formats <- function(fileExt) {
  if (!all(fileExt %in% SUPPORTED_FILES) ) {
    stop("Unsupported file formats. Supported formats are: ",
         paste(SUPPORTED_FILES, collapse = ", "),
         call. = FALSE)
  }
}

dataimportCSS <- "
.dataimport-module .upload-options-section h3 {
  font-weight: bold;
  margin-top: 0;
}
.dataimport-module .upload-err {
  margin-top: 10px;
  color: red;
}
.dataimport-module .btn-loading-img,
.dataimport-module .btn-done-img {
  margin-left: 10px;
  font-size: 1.2em;
  vertical-align: middle;
}
.dataimport-module .btn-done-img {
  color: green;
}
.dataimport-module .show_hide_opts {
  display: inline-block;
  margin-bottom: 10px;
}
"

#' Data import module UI
#' @export
#' @import shiny
dataimportUI <- function(id = "dataimport",
                         fileExt = c("csv", "txt", "xlsx", "ods"),
                         sampleDataPackage = NULL,
                         sampleDatasets = NULL) {

  check_file_formats(fileExt)

  # Provide a list of sample datasets (either from a data package or from a
  # user-provided list of datasets, or both)  
  sample_datasets <- NULL
  if (!is.null(sampleDataPackage)) {
    if (!requireNamespace(sampleDataPackage, quietly = TRUE)) {
      stop(sprintf("`%s` package is not available, please install it", sampleDataPackage))
    }
    sample_datasets <- data(package = sampleDataPackage)$results
    sample_datasets <- setNames(sample_datasets[, 'Item'],
                                sample_datasets[, 'Title'])
  }
  if (!is.null(sampleDatasets)) {
    if (!is.null(sample_datasets)) {
      warning("sampleDatasets is overriding sampleDataPackage, you should not provide both parameters")
    }
    sample_datasets <- names(sampleDatasets)
  }
  
  ns <- NS(id)
  
  div(
    shinyjs::useShinyjs(),
    shinyjs::inlineCSS(dataimportCSS),
    class = "dataimport-module",
    h3(tags$strong("Import a dataset")),
    div(
      style = if(is.null(sample_datasets)) "display:none;",
      selectInput(ns("upload_vs_sample"), NULL, choices = 
                  c("Upload my own file" = "upload",
                    "Use a sample dataset" = "sample"))
    ),
    
    conditionalPanel(
      paste0("input['", ns("upload_vs_sample"), "'] == 'sample'"),
      selectInput(ns("upload_sample_data"), "Choose a dataset", sample_datasets),
      actionButton(ns("upload_sample_btn"), "Import Data",
                   class = "btn-primary"),
      shinyjs::hidden(
        span(id = ns("upload_sample_loader"),
             icon("spinner", class = "fa-spin btn-loading-img")),
        span(id = ns("upload_sample_check"),
             icon("check", class = "btn-done-img"))
      )
    ),
    
    conditionalPanel(
      paste0("input['", ns("upload_vs_sample"), "'] == 'upload'"),
      fileInput(ns("upload_file"), NULL, multiple = FALSE,
              accept = paste0(".", fileExt)),
      shinyjs::hidden(
        selectInput(ns("upload_type"), "File type", selected = "", c("", fileExt))
      ),
  
      conditionalPanel(
        paste0("input['", ns("upload_type"), "'] == 'csv'"),
        actionLink(ns("show_hide_opts_csv"), "", class = "show_hide_opts"),
        div(id = ns("upload_options_csv"),
            class = "upload-options-section",
            div(h3("Import CSV Options")),
            checkboxInput(ns("upload_options_csv_header"),
                          "First row is column names", TRUE),
            checkboxInput(ns("upload_options_csv_stringsAsFactors"),
                          "Strings as factors", FALSE),
            selectInput(ns("upload_options_csv_sep"),
                        "Delimeter",
                        c("Comma" = ",", "Tab" = "\t", "Whitespace" = " ")),
            selectInput(ns("upload_options_csv_quote"),
                        "Quotes around strings",
                        c("Double (\")" = "\"", "Single (')" = "'",
                          "None"))
        )
      ),
      
      conditionalPanel(
        paste0("input['", ns("upload_type"),"'] == 'txt'"),
        actionLink(ns("show_hide_opts_txt"), ""),
        div(id = ns("upload_options_txt"),
            class = "upload-options-section",
            div(h3("Import Text Options")),
            checkboxInput(ns("upload_options_txt_header"),
                          "First row is column names", TRUE),
            checkboxInput(ns("upload_options_txt_stringsAsFactors"),
                          "Strings as factors", FALSE),
            selectInput(ns("upload_options_txt_sep"),
                        "Delimeter",
                        c("Comma" = ",", "Tab" = "\t", "Whitespace" = " ")),
            selectInput(ns("upload_options_txt_quote"),
                        "Quotes around strings",
                        c("Double (\")" = "\"", "Single (')" = "'",
                          "None"))
        )
      ),
      
      conditionalPanel(
        paste0("input['", ns("upload_type"),"'] == 'xlsx'"),
        actionLink(ns("show_hide_opts_xlsx"), ""),
        div(id = ns("upload_options_xlsx"),
            class = "upload-options-section",
            div(h3("Import Excel Options")),
            checkboxInput(ns("upload_options_xlsx_col_names"),
                          "First row is column names", TRUE),
            numericInput(ns("upload_options_xlsx_sheet"), "Sheet to read",
                         min = 1, value = 1)
        )
      ),
      
      conditionalPanel(
        paste0("input['", ns("upload_type"),"'] == 'ods'"),
        actionLink(ns("show_hide_opts_ods"), ""),
        div(id = ns("upload_options_ods"),
            class = "upload-options-section",
            div(h3("Import ODS Options")),
            checkboxInput(ns("upload_options_ods_col_names"),
                          "First row is column names", TRUE),
            numericInput(ns("upload_options_ods_sheet"), "Sheet to read",
                         min = 1, value = 1)
        )
      ),
      shinyjs::hidden(
        actionButton(ns("upload_import_btn"), "Import Data",
                     class = "btn-primary"),
        span(id = ns("upload_import_loader"),
             icon("spinner", class = "fa-spin btn-loading-img")),
        span(id = ns("upload_import_check"),
             icon("check", class = "btn-done-img"))
      ),
      shinyjs::hidden(
        div(id = ns("upload_err"),
            class = "upload-err",
            icon("exclamation-circle"),
            tags$b("Error:"),
            textOutput(ns("upload_err_msg"), inline = TRUE)
        )
      )
    )
  )
}

#' @export
#' @import shiny
dataimportServer <- function(id,
                             fileExt = c("csv", "txt", "xlsx", "ods"),
                             sampleDataPackage = NULL,
                             sampleDatasets = NULL) {
  check_file_formats(fileExt)
  callModule(dataimportServerHelper, id, fileExt = fileExt,
             sampleDataPackage = sampleDataPackage,
             sampleDatasets = sampleDatasets)
}

dataimportServerHelper <- function(input, output, session, id, fileExt,
                                   sampleDataPackage,
                                   sampleDatasets) {

  # map between a file extension to the function that can be used to import it
  FILETYPE_READ_FXN <- c(
    "csv"  = "utils::read.csv",
    "txt"  = "utils::read.delim",
    "xlsx" = "readxl::read_excel",
    "ods"  = "readODS::read_ods"
  )

  values <- reactiveValues(
    data = NULL,
    show_submit = FALSE,
    error = NULL
  )
  
  observe({
    shinyjs::toggle("upload_import_btn", condition = values$show_submit)
  })
  
  observe({
    shinyjs::toggle("upload_err", condition = !is.null(values$error))
  })
  
  output$upload_err_msg <- renderText({
    values$error
  })
  
  # Show/hide options
  lapply(SUPPORTED_FILES, function(x) {
    btn_id <- paste0("show_hide_opts_", x)
    section_id <- paste0("upload_options_", x)
    observe({
      if (input[[btn_id]] %% 2 == 0) {
        shinyjs::html(btn_id, "Show options")
        shinyjs::hide(section_id)
      } else {
        shinyjs::html(btn_id, "Hide options")
        shinyjs::show(section_id)
      }
    })
  })

  # get the local path of the uploaded file (after fixing its filename)
  upload_file_path <- eventReactive(input$upload_file, {
    values$error <- NULL
    file <- input$upload_file
    new_file <- file.path(dirname(file$datapath), file$name)
    file.rename(from = file$datapath, to = new_file)
    new_file
  })
  
  # after uploading a file 
  observeEvent(upload_file_path(), {
    file <- upload_file_path()
    file_ext <- tools::file_ext(file)
    
    # If the uploaded file is not an acceptable file type
    if (!file_ext %in% fileExt) {
      values$show_submit <- FALSE
      values$error <- "Unsupported file format"
      updateSelectInput(session, "upload_type", selected = "")
      return()
    }
    
    values$show_submit <- TRUE
    values$error <- NULL
    shinyjs::hide("upload_type")
    updateSelectInput(session, "upload_type", selected = file_ext)
  })

  # do.call() can't handle a namespaced function, so split it up  
  get_read_fxn <- function(fxn) {
    fxn_split <- strsplit(fxn, "::")[[1]]
    if (length(fxn_split) == 1) {
      return(fxn_split[[1]])
    } else {
      return(get(fxn_split[[2]], asNamespace(fxn_split[[1]])))
    }
  }
  
  # The user clicks on the upload file button
  observeEvent(input$upload_import_btn, {
    values$error <- NULL
    shinyjs::disable("upload_import_btn")
    shinyjs::show("upload_import_loader")
    shinyjs::hide("upload_import_check")
    on.exit({
      shinyjs::enable("upload_import_btn")
      shinyjs::hide("upload_import_loader")
    })
    
    tryCatch({
      # Figure out what function to call
      file <- upload_file_path()
      file_ext <- input$upload_type
      read_fxn <- FILETYPE_READ_FXN[file_ext]
      fxn <- get_read_fxn(read_fxn)
  
      # Gather all the parameters of the read function
      params_regex <- paste0("^upload_options_", file_ext, "_(.*)$")
      params_inputs <- grep(params_regex, names(input), value = TRUE)
      read_params <- list(file)
      lapply(params_inputs, function(x) {
        param <- gsub(params_regex, "\\1", x)
        value <- input[[x]]
  
        if (param == "quote") {
          if (value == "None") {
            value <- ""
          }
        }
        read_params[[param]] <<- value
      })

      values$data <- do.call(fxn, read_params)

      shinyjs::show("upload_import_check")
      shinyjs::delay(2000,
                     shinyjs::hide("upload_import_check", anim = TRUE,
                                   animType = "fade", time = 0.5))
    }, error = function(err) {
      values$error <- err$message
    })
  })
  
  # The user clicks on the upload sample dataset button
  observeEvent(input$upload_sample_btn, {
    shinyjs::disable("upload_sample_btn")
    shinyjs::show("upload_sample_loader")
    shinyjs::hide("upload_sample_check")
    on.exit({
      shinyjs::enable("upload_sample_btn")
      shinyjs::hide("upload_sample_loader")
    })
    
    if (!is.null(sampleDatasets)) {
      values$data <- sampleDatasets[[input$upload_sample_data]]
    } else {
      data(list = input$upload_sample_data, package = sampleDataPackage)
      values$data <- get(input$upload_sample_data)
    }

    shinyjs::show("upload_sample_check")
    shinyjs::delay(2000,
                   shinyjs::hide("upload_sample_check", anim = TRUE,
                                 animType = "fade", time = 0.5))
  })
  
  return(reactive(values$data))
}