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
    updateCheckboxInput(session, "tableOption", value = FALSE)
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
  
  #PLOTS
  output$plotExplore <- renderPlot({
    dat <- cancer_data()
    str(dat)
    if (input$plotmRNASeq == "Density" & input$cancer == "BRCA") {
      g <- ggplot(dat, aes(x = expression_log2))
      g + geom_density(alpha = 0.5, aes(fill = gene)) +
        labs(x = "Gene Expression", y = "Density", 
             title = "Gene Expression of BRCA2 and HER2 in Breast Cancer")
    } else if (input$plotmRNASeq == "Density" & input$cancer == "BLCA") {
      g <- ggplot(dat, aes(x = expression_log2))
      g + geom_density(alpha = 0.5, aes(fill = gene)) +
        labs(x = "Gene Expression", y = "Density", 
             title = "Gene Expression of FGFR3 and HER2 in Bladder Cancer")
    } else if (input$plotmRNASeq == "Density" & input$cancer == "LUAD") {
      g <- ggplot(dat, aes(x = expression_log2))
      g + geom_density(alpha = 0.5, aes(fill = gene)) +
        labs(x = "Gene Expression", y = "Density", 
             title = "Gene Expression of MET and HER2 in Lung Cancer")
    } else if (input$plotmRNASeq == "Box Plot" & input$cancer == "BRCA") {
      g <- ggplot(dat)
      g + geom_boxplot(aes(x = gene, y = expression_log2, fill = gene)) +
        labs(x = "Gene", y = "Gene Expression", 
             title = "Gene Expression of BRCA2 and HER2 in Breast Cancer")
    } else if (input$plotmRNASeq == "Box Plot" & input$cancer == "BLCA") {
      g <- ggplot(dat)
      g + geom_boxplot(aes(x = gene, y = expression_log2, fill = gene)) +
        labs(x = "Gene", y = "Gene Expression", 
             title = "Gene Expression of FGFR3 and HER2 in BLadder Cancer")
    } else {
      g <- ggplot(dat)
      g + geom_boxplot(aes(x = gene, y = expression_log2, fill = gene)) +
        labs(x = "Gene", y = "Gene Expression", 
             title = "Gene Expression of MET and HER2 in Lung Cancer")
    }
  })
  
  #TABLES
  output$tableExplore <- renderDT ({
    dat <- cancer_data()
    
    if (input$tableClinical == "Gender") {
      dat_gender <- dat |>
        group_by(gender) |>
        summarize(count = n())
      dat_gender
    } else if (input$tableClinical == "Stage") {
      dat_stage <- dat |>
        rename("Stage" = "pathologic_stage") |>
        mutate(across(everything(), function(x) na_if(x, "NA"))) |>
        drop_na(Stage) |>
        group_by(Stage) |>
        summarize(count = n())
      dat_stage
    } else if (input$tableClinical == "Race") {
      dat_race <-dat |>
        mutate(across(everything(), function(x) na_if(x, "NA"))) |>
        drop_na(race) |>
        group_by(race) |>
        summarize(count = n())
      
      dat_race
    } else if (input$tablemRNASeq == "Summary Statistics"){
      dat <- cancer_data()
      dat_summary <- dat |>
        group_by(gene) |>
        summarize(across(expression_log2, .fns = list("Mean" = mean,
                                                      "Median" = median,
                                                      "Standard_Deviation" = sd,
                                                      "Variance" = var), 
                         .names = "{.fn}_{.col}"))
      dat_summary
      
    } else {
      NULL
    }
    
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