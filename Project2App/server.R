library(shiny)
library(httr)
library(tidyverse)
library(jsonlite)
library(DT)

function(input, output, session) {
  cancer_data <- reactive({
    firebrowse_query <- function(endpoint, cancer) {
      base <- "http://firebrowse.org/api/v1/Samples/"
      end <- "&page=1&page_size=3000&sort_by=cohort"
      
      if (endpoint == "mRNASeq" & cancer == "LUAD") {
        url <- paste0(base, endpoint, "?format=json&gene=met%2Cerbb2&cohort=",
                      cancer, "&protocol=RSEM", end)
      } else if (endpoint == "mRNASeq" & cancer == "BLCA") {
        url <- paste0(base, endpoint, "?format=json&gene=FGFR3%2Cerbb2&cohort=",
                      cancer, "&protocol=RSEM", end)
      } else if (endpoint == "mRNASeq" & cancer == "BRCA") {
        url <- paste0(base, endpoint, "?format=json&gene=brca2%2Cerbb2&cohort=",
                      cancer, "&protocol=RSEM", end)
      } else if (endpoint == "Clinical") {
        url <- paste0(base, endpoint, "?format=json&cohort=",
                      cancer, end)
      } else {
        url <- "Error"
      }
      
      response <- GET(url)
      parsed <- fromJSON(rawToChar(response$content))
      as_tibble(parsed[[endpoint]])
    }
    
    firebrowse_query(input$endpoint, input$cancer)
  })
  observeEvent ({
    input$endpoint
    input$cancer
  }, {
    updateCheckboxInput(session, "subsetColumns", value = FALSE)
    updateCheckboxInput(session, "subsetRows", value = FALSE)
  })
  
  observe({
    data <- cancer_data()
    if (input$endpoint == "mRNASeq" & (input$cancer == "LUAD" | 
                                       input$cancer == "BRCA" | 
                                       input$cancer == "BLCA")) {
      updateSelectInput(session, "columns", 
                        choices = 
                          c("cohort", "gene", "expression_log2",
                            "tcga_participant_barcode", "z-score"))
      updateSelectInput(session, "filterColumn", choices = c("gene"))
    } else if (input$endpoint == "Clinical" & input$cancer == "BRCA") {
      updateSelectInput(session, "columns", 
                        choices = c("patient_id", 
                                    "age_at_initial_pathologic_diagnosis",
                                    "cohort", "gender", "race", "pathologic_m",
                                    "pathologic_n", "pathologic_t", 
                                    "pathologic_stage",
                                    "radiation_therapy", 
                                    "targeted_molecular_therapy",
                                    "breast_carcinoma_estrogen_receptor_status",
                                    "breast_carcinoma_progesterone_receptor_status",
                                    "lab_proc_her2_neu_immunohistochemistry_receptor_status",
                                    "histological_type"
                        )
      )
      updateSelectInput(session, "filterColumn",
                        choices =
                          c("gender", "pathologic_stage", "radiation_therapy",
                            "race", "targeted_molecular_therapy",
                            "breast_carcinoma_estrogen_receptor_status",
                            "breast_carcinoma_progesterone_receptor_status",
                            "lab_proc_her2_neu_immunohistochemistry_receptor_status"))
    } else if (input$endpoint == "Clinical" & input$cancer == "BLCA") {
      updateSelectInput(session, "columns", 
                        choices = c("patient_id", 
                                    "age_at_initial_pathologic_diagnosis",
                                    "cohort", "gender", "race", "pathologic_m",
                                    "pathologic_n", "pathologic_t", 
                                    "pathologic_stage",
                                    "diagnosis_subtype", 
                                    "primary_therapy_outcome_success",
                                    "additional_treatment_completion_success_outcome",
                                    "lymphovascular_invasion_present",
                                    "histological_type"
                        )
      )
      updateSelectInput(session, "filterColumn",
                        choices =
                          c("gender", "pathologic_stage", "radiation_therapy",
                            "race", "primary_therapy_outcome_success",
                            "additional_treatment_completion_success_outcome")
      )
    } else if (input$endpoint == "Clinical" & input$cancer == "LUAD") {
      updateSelectInput(session, "columns", 
                        choices = c("patient_id", 
                                    "age_at_initial_pathologic_diagnosis",
                                    "cohort", "gender", "race", "pathologic_m",
                                    "pathologic_n", "pathologic_t", 
                                    "pathologic_stage",
                                    "radiation_therapy", 
                                    "targeted_molecular_therapy",
                                    "primary_therapy_outcome_success",
                                    "histological_type"
                        )
      )
      updateSelectInput(session, "filterColumn",
                        choices =
                          c("gender", "pathologic_stage", "radiation_therapy",
                            "race", "targeted_molecular_therapy",
                            "primary_therapy_outcome_success"))
    } else {
      updateSelectInput(session, "columns", choices = colnames(data))
      updateSelectInput(session, "filterColumn", choices = NULL)
    }
  })
  
  observe({
    data <- cancer_data()
    if (!is.null(input$filterColumn) & input$filterColumn %in% colnames(data)) {
      updateSelectInput(session, "filterLevel", choices = 
                          unique(data[[input$filterColumn]]))
    } else {
      updateSelectInput(session, "filterLevel", choices = NULL)
    }
  })
  
  subset_data <- reactive({
    data <- cancer_data()
    if (input$subsetColumns) {
      columns <- input$columns
      if (!is.null(columns)) {
        data <- data[, columns, drop = FALSE]
      }
    }
    if (input$subsetRows) {
      if (!is.null(input$filterColumn) & !is.null(input$filterLevel)) {
        data <- data[data[[input$filterColumn]] %in% input$filterLevel, ]
      }
    }
    data
  })
  
  output$cancerTable <- renderDT({
    subset_data()
  }, plugins = "ellipsis", 
  options = list(scrollX = TRUE,
                 autoWidth = TRUE,
                 lengthMenu = c(10, 20, 40, 90),
                 columnDefs = list(
                   list(
                     width = '100px',
                     targets = "_all",
                     render = 
                       JS("$.fn.dataTable.render.ellipsis( 15, false)")
                   ))
  )
  )
  
  output$download <- downloadHandler(
    filename = function(){"CancerData.csv"}, 
    content = function(fname){
      write.csv(subset_data(), fname)
    }
  )
}