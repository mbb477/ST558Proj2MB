library(shiny)
library(httr)
library(tidyverse)
library(jsonlite)
library(DT)

function(input, output, session) {
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
  
  cancer_data <- reactive({
    firebrowse_query(input$endpoint, input$cancer)
  })
  
  firebrowse_queryE <- function(endpointE, cancerE) {
    base <- "http://firebrowse.org/api/v1/Samples/"
    end <- "&page=1&page_size=3000&sort_by=cohort"
    
    if (endpointE == "mRNASeq" & cancerE == "LUAD") {
      url <- paste0(base, endpointE, "?format=json&gene=met%2Cerbb2&cohort=",
                    cancerE, "&protocol=RSEM", end)
    } else if (endpointE == "mRNASeq" & cancerE == "BLCA") {
      url <- paste0(base, endpointE, "?format=json&gene=FGFR3%2Cerbb2&cohort=",
                    cancerE, "&protocol=RSEM", end)
    } else if (endpointE == "mRNASeq" & cancerE == "BRCA") {
      url <- paste0(base, endpointE, "?format=json&gene=brca2%2Cerbb2&cohort=",
                    cancerE, "&protocol=RSEM", end)
    } else if (endpointE == "Clinical") {
      url <- paste0(base, endpointE, "?format=json&cohort=",
                    cancerE, end)
    } else {
      url <- "Error"
    }
    
    response <- GET(url)
    parsed <- fromJSON(rawToChar(response$content))
    as_tibble(parsed[[endpointE]])
  }
  
  cancer_dataE <- reactive({
    firebrowse_queryE(input$endpointE, input$cancerE)
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
    dat <- cancer_dataE()
    print(dat)
    str(dat)
    print(input$endpointE)
    print(input$cancerE)
    print(input$outcomeL)
    if (input$plotmRNASeq == "Density" & input$cancerE == "BRCA") {
      g <- ggplot(dat, aes(x = expression_log2))
      g + geom_density(alpha = 0.5, aes(fill = gene)) +
        labs(x = "Gene Expression", y = "Density", 
             title = "Gene Expression of BRCA2 and HER2 in Breast Cancer")
    } else if (input$plotmRNASeq == "Density" & input$cancerE == "BLCA") {
      g <- ggplot(dat, aes(x = expression_log2))
      g + geom_density(alpha = 0.5, aes(fill = gene)) +
        labs(x = "Gene Expression", y = "Density", 
             title = "Gene Expression of FGFR3 and HER2 in Bladder Cancer")
    } else if (input$plotmRNASeq == "Density" & input$cancerE == "LUAD") {
      g <- ggplot(dat, aes(x = expression_log2))
      g + geom_density(alpha = 0.5, aes(fill = gene)) +
        labs(x = "Gene Expression", y = "Density", 
             title = "Gene Expression of MET and HER2 in Lung Cancer")
    } else if (input$plotmRNASeq == "Box Plot" & input$cancerE == "BRCA") {
      g <- ggplot(dat)
      g + geom_boxplot(aes(x = gene, y = expression_log2, fill = gene)) +
        labs(x = "Gene", y = "Gene Expression", 
             title = "Gene Expression of BRCA2 and HER2 in Breast Cancer")
    } else if (input$plotmRNASeq == "Box Plot" & input$cancerE == "BLCA") {
      g <- ggplot(dat)
      g + geom_boxplot(aes(x = gene, y = expression_log2, fill = gene)) +
        labs(x = "Gene", y = "Gene Expression", 
             title = "Gene Expression of FGFR3 and HER2 in BLadder Cancer")
    } else if (input$plotmRNASeq == "Box Plot" & input$cancerE == "LUAD") {
      g <- ggplot(dat)
      g + geom_boxplot(aes(x = gene, y = expression_log2, fill = gene)) +
        labs(x = "Gene", y = "Gene Expression", 
             title = "Gene Expression of MET and HER2 in Lung Cancer")
    } else if (input$varClinicalL == 
               "Radiation Therapy and Targeted Molecular Therapy" &
               input$outcomeL == "Treatments Bar Chart") {
      g <- ggplot(dat |> 
                    mutate(across(everything(), function(x) na_if(x, "NA"))) |>
                    select(radiation_therapy, targeted_molecular_therapy) |>
                    drop_na(radiation_therapy, targeted_molecular_therapy) |>
                    pivot_longer(cols = everything(), 
                                 names_to = "Therapy", values_to = "Received"),
                  aes(x = Therapy, fill = Received))
      g + geom_bar() +
        labs(x = "Treatment Therapy", y = "Count", 
             title = "Stacked Bar Chart of Administered Treatments for Lung Cancer") +
        scale_x_discrete(labels = c("Radiation_Therapy" = "Radiation Therapy", 
                                    "Targeted_Molecular_Therapy" = "Targeted Molecular Therapy"))
      
    } else if (input$varClinicalL == "Primary Therapy Outcome" &
               input$outcomeL == "Outcome Bar Chart") {
      g <- ggplot(dat |> 
                    filter(primary_therapy_outcome_success != "NA"),
                  aes(x = primary_therapy_outcome_success, 
                      fill = primary_therapy_outcome_success))
      g + geom_bar() +
        labs(x = "Outcome", y = "Count", 
             title = "Bar Chart of Primary Therapy Outcome for Lung Cancer") +
        labs(fill = "Outcome") +
        coord_flip()
    } else {
      dat
    }
  })
  
  #TABLES
  output$tableExplore <- renderDT ({
    dat <- cancer_dataE()
    
    if (input$tableClinical == "Gender" & input$endpointE == "Clinical") {
      dat <- dat |>
        group_by(gender) |>
        summarize(count = n())
    
    } else if (input$tableClinical == "Stage") {
      dat <- dat |>
        rename("Stage" = "pathologic_stage") |>
        mutate(across(everything(), function(x) na_if(x, "NA"))) |>
        drop_na(Stage) |>
        group_by(Stage) |>
        summarize(count = n())
      
    } else if (input$tableClinical == "Race") {
      dat <-dat |>
        mutate(across(everything(), function(x) na_if(x, "NA"))) |>
        drop_na(race) |>
        group_by(race) |>
        summarize(count = n())
      
    } else {
      dat
    }
    dat
  })
  
  output$geneSelection <- renderUI({
    if (input$endpointE == "mRNASeq") {
      gene_choices <- unique(cancer_dataE()$gene)
      selectInput("genesel", "Select Gene", choices = gene_choices, selected = NULL)
    }
  })
  
  output$statSelection <- renderUI({
    req(input$genesel)
    
    if (input$endpointE == "mRNASeq") {
      selectInput("statSelection", "Select Statistic",
                  choices = c("mean", "median", "standard deviation", "variance"))
    }
  })
  
  output$statisticOutput <- renderText({
    req(input$genesel, input$statSelection)
    
    if (input$endpointE == "mRNASeq") {
      dat <- cancer_dataE() |>
        filter(gene == input$genesel)
      
      summary_stats <- summarise(dat,
                                 expression_log2_Mean = mean(expression_log2, na.rm = TRUE),
                                 expression_log2_Median = median(expression_log2, na.rm = TRUE),
                                 expression_log2_Standard_Deviation = sd(expression_log2, na.rm = TRUE),
                                 expression_log2_Variance = var(expression_log2, na.rm = TRUE))
      
      selected_stat <- switch(input$statSelection,
                              "mean" = summary_stats$expression_log2_Mean,
                              "median" = summary_stats$expression_log2_Median,
                              "standard deviation" = summary_stats$expression_log2_Standard_Deviation,
                              "variance" = summary_stats$expression_log2_Variance)
      
      paste("The", input$statSelection, "of the gene expression of",
            input$genesel, "is", round(selected_stat, 2))
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