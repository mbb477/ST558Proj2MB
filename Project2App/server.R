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
  
  observeEvent ({

    input$EndpointE
  }, {
    updateSelectInput(session, "plotmRNASeq", selected = character(0))
   
  })
  
  observeEvent ({
    input$varClinicalL
    input$cancerE
  }, {
    updateCheckboxInput(session, "therL", value = FALSE)
    updateCheckboxInput(session, "outcomeL", value = FALSE)
    
  })
  
  observeEvent ({
    input$cancerE
    input$varClinicalBL
  }, {
    updateCheckboxInput(session, "outcomeBL", value = FALSE)
    updateCheckboxInput(session, "stageBL", value = FALSE)
    
  })
  
  observeEvent ({
    input$cancerE
  }, {
    updateRadioButtons(session, "varClinicalL", selected = character(0))
    updateRadioButtons(session, "varClinicalBL", selected = character(0))
    
  })
  
  
  #PLOTS
  output$plotExplore <- renderPlot({
    dat <- cancer_dataE()
    print(dat)
    str(dat)
    print(input$endpointE)
    print(input$cancerE)
    print(input$outcomeL)
    print(input$therL)
    print(input$varClinicalL)
    print(input$varClinicalBL)
    print(input$outcomeBL)
    print(input$stageBL)
    print(input$plotmRNASeq)
  
    if (input$plotmRNASeq == "Density" & input$cancerE == "BRCA") {
      g <- ggplot(dat, aes(x = expression_log2))
      g + geom_density(alpha = 0.5, aes(fill = gene)) +
        labs(x = "Gene Expression", y = "Density", fill = "Gene", 
             title = "Gene Expression of BRCA2 and ERBB2 in Breast Cancer")
    } else if (input$plotmRNASeq == "Density" & input$cancerE == "BLCA") {
      g <- ggplot(dat, aes(x = expression_log2))
      g + geom_density(alpha = 0.5, aes(fill = gene)) +
        labs(x = "Gene Expression", y = "Density", "Gene",
             title = "Gene Expression of FGFR3 and ERBB2 in Bladder Cancer")
    } else if (input$plotmRNASeq == "Density" & input$cancerE == "LUAD") {
      g <- ggplot(dat, aes(x = expression_log2))
      g + geom_density(alpha = 0.5, aes(fill = gene)) +
        labs(x = "Gene Expression", y = "Density", "Gene",
             title = "Gene Expression of MET and ERBB2 in Lung Cancer")
    } else if (input$plotmRNASeq == "Box Plot" & input$cancerE == "BRCA") {
      g <- ggplot(dat)
      g + geom_boxplot(aes(x = gene, y = expression_log2, fill = gene)) +
        labs(x = "Gene", y = "Gene Expression", "Gene",
             title = "Gene Expression of BRCA2 and ERBB2 in Breast Cancer")
    } else if (input$plotmRNASeq == "Box Plot" & input$cancerE == "BLCA") {
      g <- ggplot(dat)
      g + geom_boxplot(aes(x = gene, y = expression_log2, fill = gene)) +
        labs(x = "Gene", y = "Gene Expression", "Gene",
             title = "Gene Expression of FGFR3 and ERBB2 in BLadder Cancer")
    } else if (input$plotmRNASeq == "Box Plot" & input$cancerE == "LUAD") {
      g <- ggplot(dat)
      g + geom_boxplot(aes(x = gene, y = expression_log2, fill = gene)) +
        labs(x = "Gene", y = "Gene Expression", "Gene",
             title = "Gene Expression of MET and ERBB2 in Lung Cancer")
    } else if (input$varClinicalL == 
               "Radiation Therapy and Targeted Molecular Therapy" &
               input$therL == "Treatments Bar Chart") {
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
                                    "Targeted_Molecular_Therapy" = 
                                      "Targeted Molecular Therapy"))
      
    } else if (input$varClinicalL == "Primary Therapy Outcome" &
               input$outcomeL == "Outcome Bar Chart") {
      g <- ggplot(dat |> 
                    mutate(across(everything(), function(x) na_if(x, "NA"))) |>
                    drop_na(primary_therapy_outcome_success) |>
                    mutate(factor(primary_therapy_outcome_success, 
                                  levels = c("complete remission/response",
                                             "partial remission/response",
                                             "stable disease", "progressive disease"), 
                                  labels = c("Complete disease Remission/Response",
                                             "Partial Remission/Response",
                                             "Stable Disease", "Progressive Disease"))),
                  aes(x = primary_therapy_outcome_success, 
                      fill = primary_therapy_outcome_success))
      g + geom_bar() +
        labs(x = "Outcome", y = "Count", 
             title = "Bar Chart of Primary Therapy Outcome for Lung Cancer") +
        labs(fill = "Outcome") +
        coord_flip()
    } else if (input$varClinicalBL == "Primary Therapy Outcome, 
               Additional Therapy Outcoome" &
               input$outcomeBL == "Outcome by Therapy Bar Chart") {
      g <- ggplot(dat |>
                    mutate(across(everything(), function(x) na_if(x, "NA"))) |>
                    select(Primary_Therapy_Outcome = primary_therapy_outcome_success,
                           Additional_Treatment_Outcome = 
                             additional_treatment_completion_success_outcome,) |>
                    mutate(Additional_Treatment_Outcome =
                             ifelse(Additional_Treatment_Outcome == 
                                      "complete response", "complete remission/response",
                                    ifelse(Additional_Treatment_Outcome == 
                                             "partial response", "partial remission/response",
                                           Additional_Treatment_Outcome))) |>
                    pivot_longer(cols = everything(), 
                                 names_to = "Therapy", values_to = "Outcome") |>
                    drop_na(Therapy, Outcome) |>
                    mutate(Therapy = factor(Therapy, 
                                            levels = 
                                              c("Primary_Therapy_Outcome",
                                                "Additional_Treatment_Outcome"))),
                  aes(x = Therapy, fill = Outcome))
      g + geom_bar() +
        labs(x = "Therapy Type", y = "Count", 
             title = "Stacked Bar Chart of Outcome of Treatment Types for Bladder Cancer") +
        scale_x_discrete(labels = c("Primary_Therapy_Outcome" = "Primary",
                                    "Additional_Treatment_Outcome" = "Additional"))
    } else  if (input$varClinicalBL == "Age, Stage" &
                 input$outcomeBL == "Age vs. Stage Bar Chart") {
      g <- ggplot(dat |> 
                    mutate(across(everything(), function(x) na_if(x, "NA"))) |>
                    mutate(Age_Grouping = 
                             ifelse(age_at_initial_pathologic_diagnosis <= 39, "Young", 
                                    ifelse(age_at_initial_pathologic_diagnosis >= 40 & 
                                             age_at_initial_pathologic_diagnosis <= 64, "Middle Age",
                                           ifelse(age_at_initial_pathologic_diagnosis > 64, "Old", NA)))) |>
                    mutate(Age_Grouping = factor(Age_Grouping, levels = c("Young", "Middle Age",
                                                                          "Old"))) |>
                    group_by(Age_Grouping, pathologic_stage) |>
                    drop_na(pathologic_stage), aes(x = Age_Grouping, fill = pathologic_stage))
      g + geom_bar() +
        labs(x = "Age Group", y = "Count",
             title = 
               "Stacked Bar Chart of Cancer Stage by Age Group in Bladder Cancer") + 
        scale_fill_manual(values = c("stage i" = "pink",
                                     "stage ii" = "skyblue",
                                     "stage iii" = "cornflowerblue",
                                     "stage iv" = "purple"
        ),
        labels = c("stage i" = "I" ,
                   "stage ii" = "II",
                   "stage iii" = "III",
                   "stage iv" = "V")) +
        labs(fill = "Stage")
    } else {
      NULL
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
  
  observeEvent ({
    input$endpointE
    input$cancerE
  }, {
    updateCheckboxInput(session, "summary", value = FALSE)
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
  } else {
      ""
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
