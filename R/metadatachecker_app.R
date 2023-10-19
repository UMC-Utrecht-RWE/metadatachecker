#' metadatachecker shiny app
#'
#' In the console, type: metadatachecker_app()
#' @param ... no argument is supplied
#'
#' @return Opens the shiny app in a new window or in browser
#' @export
#'

metadatachecker_app <- function(...) {
  # File upload size is increased
  options(shiny.maxRequestSize = 100 * 1024^2)
  # temp directory for results
  output_directory <- tempdir()
  ui <- shiny::fluidPage(
    theme = shinythemes::shinytheme("cerulean"),
    shiny::h1("RWE-metadatachecker"),
    shiny::h3("Checking the consistency of metadata files"),
    shiny::navlistPanel(
      shiny::tabPanel("Check consistency of study variables",
                      shiny::fluidRow(
                        shiny::column(width = 4,
                                      shiny::fileInput("study_variables_file",
                                                       "Upload the study variables",
                                                       accept = ".csv")),
                        shiny::column(width = 4,
                                      shiny::fileInput("event_codelist",
                                                       "Upload the codelist for events",
                                                       accept = ".csv"),
                                      shiny::fileInput("drug_codelist",
                                                       "Upload the codelist for drugs",
                                                       accept = ".csv"),
                                      shiny::fileInput("vaccines_file",
                                                       "Upload the codelist for vaccines",
                                                       accept = ".csv")),
                        shiny::column(width = 4,
                                      shiny::fileInput("algorithms_file",
                                                       "Upload the algorithms data",
                                                       accept = ".csv"),
                                      shiny::fileInput("additional_concepts_file",
                                                       "Upload the additional_concepts",
                                                       accept = ".csv")),
                        ),
                      shiny::actionButton("Go","Check consistency"),
                      shiny::h4(""),
                      shiny::textOutput("feedback_text"),
                      shiny::tableOutput("base_table"),
                      ),
      shiny::tabPanel("Check consistency of other meta data files",
                      shiny::h3("Select a meta data file to compare with study variables"),
                      shiny::fluidRow(
                        shiny::column(width = 12,
                                      shiny::fileInput("study_variables_file_meta",
                                                       "Upload the study variables",
                                                       accept = ".csv"))
                        ),
                      shiny::fluidRow(
                        shiny::column(width = 6,
                                      shiny::fileInput("metadata",
                                                       "Upload the meta data file",
                                                       accept = ".csv")),
                        shiny::column(width = 6,
                                      shiny::selectizeInput("metafile_column",
                                                            "Select column name",
                                                            choices = "No selection available"))
                        ),
                      shiny::fluidRow(
                        shiny::h4(""),
                        shiny::textOutput("feedback_text_meta"),
                        shiny::tableOutput("meta_study_table")
                        )
                      ),
      shiny::tabPanel("Generate report",
                      shiny::h4("Download html report for the checked file(s)"),
                      shiny::downloadButton(
                        outputId = "report",
                        label = "Generate report")
                      )
      )
    )

  server <- function(input, output, session) {
    # Study variables vs events, drugs, vaccines, additional concepts, algorithms codelists
    shiny::observeEvent(input$Go, {
      events <- utils::read.csv(input$event_codelist$datapath, stringsAsFactors = F,
                         na.strings = c(""," ","NA"))
      drugs <- utils::read.csv(input$drug_codelist$datapath, stringsAsFactors = F,
                        na.strings = c(""," ","NA"))
      vaccines <- utils::read.csv(input$vaccines_file$datapath, stringsAsFactors = F,
                           na.strings = c(""," ","NA"), sep = ";")
      algorithms <- utils::read.csv(input$algorithms_file$datapath, stringsAsFactors = F,
                             na.strings = c(""," ","NA"), sep = ";")
      additional_concepts <- utils::read.csv(input$additional_concepts_file$datapath, stringsAsFactors = F,
                                      na.strings = c(""," ","NA"), sep = ";")
      # Combine all sources of study variables
      initial_reference_set <- rbind(data.frame(codenames = paste(events$system,
                                                                  events$event_abbreviation,
                                                                  events$type, sep = "_"),
                                                source = input$event_codelist$datapath),
                                     data.frame(codenames = drugs$drug_abbreviation,
                                                source = input$drug_codelist$datapath),
                                     data.frame(codenames = vaccines$StudyVar,
                                                source = input$vaccines_file$datapath),
                                     data.frame(codenames = algorithms$NEW_CONCEPT,
                                                source = input$algorithms_file$datapath),
                                     data.frame(codenames = additional_concepts$StudyVar,
                                                source = input$additional_concepts_file$datapath)
      )
      # Read study variables
      study_variables <- utils::read.csv(input$study_variables_file$datapath,
                                  stringsAsFactors = F,
                                  na.strings = c(""," ","NA","NULL"),
                                  strip.white = T,
                                  sep = ";")

      # Save the initial_reference_set
      utils::write.table(initial_reference_set, paste(output_directory,
                                               "combined_codelist_drugs_vaccines_algo_additional.csv",
                                               sep = "/"), row.names = F,
                  quote = F, sep = ";")

      #Study variables
      study_vars_reference = input$study_variables_file$datapath
      reference_colname = "VarName"

      # Study variables
      pfizer_study_var_check <- check_study_variables(paste(output_directory,
                                                            "combined_codelist_drugs_vaccines_algo_additional.csv",
                                                            sep = "/"),
                                                      "codenames",
                                                      study_vars_reference,
                                                      reference_colname)

      # Results table to display
      results_table <- data.frame(metadata_file = input$study_variables_file$name,
                                  column_to_check = reference_colname,
                                  variable_name = paste(pfizer_study_var_check,
                                                        collapse = "\t"))
      if (length(pfizer_study_var_check)>0){
        output$feedback_text <- shiny::renderText("The variables in variable_name column
      are not present in the events/drugs/vaccines/additional
      concepts/algorithms codelists")
      } else {
        output$feedback_text <- shiny::renderText("All the variable names in study
                                         variables is consistent with
                                         the events/drugs/vaccines/additional
                                         concepts/algorithms codelists")
      }
      output$base_table <- shiny::renderTable({
        results_table
      })
      # Save results table
      utils::write.table(results_table,
                  file = paste(output_directory,"study_variables_results.csv", sep = "/"),
                  row.names = F,
                  sep = ",")
    }
    )

    # Display the meta data file and column names after upload
    shiny::observeEvent(input$metadata, {
      metadata_df <- utils::read.csv(input$metadata$datapath,
                              stringsAsFactors = F,
                              strip.white = T,
                              sep = ";")

      shiny::updateSelectInput(session, "metafile_column",
                        label = "Select column name",
                        choices = colnames(metadata_df))

    })

    # Check consistency after column selection
    to_listen <- shiny::reactive({
      list(input$metafile_column)
    })
    shiny::observeEvent(to_listen(), {
      if(input$metafile_column == "No selection available"){
        return()
      }
      study_variables <- utils::read.csv(input$study_variables_file_meta$datapath,
                                  stringsAsFactors = F,
                                  na.strings = c(""," ","NA","NULL"),
                                  strip.white = T,
                                  sep = ";")

      resultsfile_name <- gsub("\\.csv","",input$metadata$name)
      # Log file for Pfizer study variables
      pfizer_var_check <- check_study_variables(input$study_variables_file_meta$datapath,
                                                "VarName",
                                                input$metadata$datapath,
                                                input$metafile_column)

      # Results table to display
      results_table_meta <- data.frame(metadata_file = input$metadata$name,
                                       column_to_check = input$metafile_column,
                                       variable_name = paste(pfizer_var_check,
                                                             collapse = "\t"))
      if (length(pfizer_var_check)>0){
        output$feedback_text_meta <- shiny::renderText("The variables in variable_name column
      are not present in the study variables VarName")
      } else {
        output$feedback_text_meta <- shiny::renderText("All the variable names in meta data
      file is consistent with study variables VarName")
      }
      output$meta_study_table <- shiny::renderTable({
        results_table_meta
      })
      utils::write.table(results_table_meta,
                  file = paste(output_directory,paste0(resultsfile_name,"_results.csv"), sep = "/"),
                  row.names = F,
                  sep = ",")
    }
    )
    output$report <- shiny::downloadHandler(
      filename <-  "metadatachecker_report.html",
      content = function(file) {
        tempReport <- file.path(output_directory, 
                                "metadatachecker_report.Rmd")
        file.copy(system.file("rmd",
                              "metadatachecker_report.Rmd",
                              package = "metadatachecker"),
                  tempReport, 
                  overwrite = TRUE)
        params <- list(output_dir = output_directory)
        rmarkdown::render(tempReport, output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
      }
    )

  }
  shiny::shinyApp(ui, server)

}
