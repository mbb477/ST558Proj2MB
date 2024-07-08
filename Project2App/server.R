library(shiny)
library(httr)
library(tidyverse)
library(jsonlite)
library(DT)

function(input, output, session) {
  
  #API Query using endpoint and cancer for Data Download Page
  
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
  
  #API Query using endpointE and cancerE for Data Explortion Tab
  
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
  
  #update functions for Data Download Page
  
  observeEvent ({
    input$endpoint
    input$cancer
  }, {
    updateCheckboxInput(session, "subsetColumns", value = FALSE)
    updateCheckboxInput(session, "subsetRows", value = FALSE)
    updateCheckboxInput(session, "tableOption", value = FALSE)
  })
  
  # Defining columns and row filtering for data download tab
  
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
  
  # produce subset data from user input on data download page
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
  
  # update functions for selecting plots in data exploration tab
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
    input$varClinicalBL
    input$cancerE
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
  
  
  observeEvent ({
    input$cancerE
    input$varClinicalBR
  }, {
    updateCheckboxInput(session, "positiveBR", value = FALSE)
    
  })
  
  observeEvent ({
    input$cancerE
    input$varClinicalBR
  }, {
    updateRadioButtons(session, "statusPlotsBR", selected = "")
    
  })
  
  # PLOTS for data exploration
  
  output$plotExplore <- renderPlot({
    dat <- cancer_dataE()
    
    #Plots for mRNASeq endpoint
    if (input$endpointE == "mRNASeq") {
      if (input$plotmRNASeq == "Density" && input$cancerE == "BRCA") {
        ggplot(dat, aes(x = expression_log2)) +
          geom_density(alpha = 0.5, aes(fill = gene)) +
          labs(x = "Gene Expression", y = "Density", fill = "Gene", 
               title = "Gene Expression of BRCA2 and ERBB2 in Breast Cancer") +
          theme(text=element_text(size=16))
        
      } else if (input$plotmRNASeq == "Density" && input$cancerE == "BLCA") {
        ggplot(dat, aes(x = expression_log2)) +
          geom_density(alpha = 0.5, aes(fill = gene)) +
          labs(x = "Gene Expression", y = "Density", fill = "Gene", 
               title = "Gene Expression of FGFR3 and ERBB2 in Bladder Cancer") +
          theme(text=element_text(size=16))
        
      } else if (input$plotmRNASeq == "Density" && input$cancerE == "LUAD") {
        ggplot(dat, aes(x = expression_log2)) +
          geom_density(alpha = 0.5, aes(fill = gene)) +
          labs(x = "Gene Expression", y = "Density", fill = "Gene", 
               title = "Gene Expression of MET and ERBB2 in Lung Adenocarcinoma") +
          theme(text=element_text(size=16))
        
      } else if (input$plotmRNASeq == "Box Plot" && input$cancerE == "BRCA") {
        ggplot(dat, aes(x = gene, y = expression_log2, fill = gene)) +
          geom_boxplot() +
          labs(x = "Gene", y = "Gene Expression", fill = "Gene", 
               title = "Gene Expression of BRCA2 and ERBB2 in Breast Cancer") +
          theme(text=element_text(size=16))
        
      } else if (input$plotmRNASeq == "Box Plot" && input$cancerE == "BLCA") {
        ggplot(dat, aes(x = gene, y = expression_log2, fill = gene)) +
          geom_boxplot() +
          labs(x = "Gene", y = "Gene Expression", fill = "Gene", 
               title = "Gene Expression of FGFR3 and ERBB2 in Bladder Cancer") +
          theme(text=element_text(size=16))
        
      } else if (input$plotmRNASeq == "Box Plot" && input$cancerE == "LUAD") {
        ggplot(dat, aes(x = gene, y = expression_log2, fill = gene)) +
          geom_boxplot() +
          labs(x = "Gene", y = "Gene Expression", fill = "Gene", 
               title = "Gene Expression of MET and ERBB2 in Lung Adenocarcinoma") +
          theme(text=element_text(size=16))
        
      } else {
        return(NULL)
      }
      #plots for Clinical endpoint
    } else if (input$endpointE == "Clinical") {
      if (input$varClinicalL == "Radiation Therapy, Targeted Molecular Therapy" && input$therL == TRUE) {
        ggplot(dat |> 
                 mutate(across(everything(), ~na_if(.x, "NA"))) |> 
                 select(radiation_therapy, targeted_molecular_therapy) |> 
                 drop_na(radiation_therapy, targeted_molecular_therapy) |> 
                 pivot_longer(cols = everything(), names_to = "Therapy", 
                              values_to = "Received"),
               aes(x = Therapy, fill = Received)) +
          geom_bar() +
          labs(x = "Treatment Therapy", y = "Count", 
               title = "Administered Treatments for Lung Adenocarcinoma") +
          scale_x_discrete(labels = c("Radiation_Therapy" = "Radiation Therapy", 
                                      "Targeted_Molecular_Therapy" = "Targeted Molecular Therapy")) +
          theme(text=element_text(size=16))
        
      } else if (input$varClinicalL == "Primary Therapy Outcome" && input$outcomeL == TRUE) {
        ggplot(dat |> 
                 mutate(across(everything(), ~na_if(.x, "NA"))) |> 
                 drop_na(primary_therapy_outcome_success) |> 
                 mutate(primary_therapy_outcome_success = 
                          factor(primary_therapy_outcome_success, 
                                 levels = c("complete remission/response",
                                            "partial remission/response",
                                            "stable disease", "progressive disease"),
                                 labels = c("Complete Response",
                                            "Partial Response",
                                            "Stable Disease", "Progressive Disease"))),
               aes(x = primary_therapy_outcome_success, 
                   fill = primary_therapy_outcome_success)) +
          geom_bar() +
          labs(x = "Outcome", y = "Count", 
               title = "Primary Therapy Outcome for Lung Adenocarcinoma") +
          labs(fill = "Outcome") +
          theme(text=element_text(size=14), 
                axis.text.y = element_text(angle = -45, hjust = 1, vjust = 1)) +
          coord_flip()
        
      } else if (input$varClinicalBL == "Primary Therapy Outcome, Additional Therapy Outcome" && input$outcomeBL == TRUE) {
        ggplot(dat |> 
                 mutate(across(everything(), ~na_if(.x, "NA"))) |> 
                 select(Primary_Therapy_Outcome = primary_therapy_outcome_success,
                        Additional_Treatment_Outcome = additional_treatment_completion_success_outcome) |> 
                 mutate(Additional_Treatment_Outcome = 
                          ifelse(Additional_Treatment_Outcome == "complete response",
                                 "complete remission/response",
                                 ifelse(Additional_Treatment_Outcome == 
                                          "partial response", "partial remission/response",
                                        Additional_Treatment_Outcome))) |> 
                 pivot_longer(cols = everything(), 
                              names_to = "Therapy", values_to = "Outcome") |> 
                 drop_na(Therapy, Outcome) |> 
                 mutate(Therapy = factor(Therapy, 
                                         levels = c("Primary_Therapy_Outcome", 
                                                    "Additional_Treatment_Outcome"))),
               aes(x = Therapy, fill = Outcome)) +
          geom_bar() +
          labs(x = "Therapy Type", y = "Count", 
               title = "Outcome of Treatment Types for Bladder Cancer") +
          scale_x_discrete(labels = 
                             c("Primary_Therapy_Outcome" = "Primary", 
                               "Additional_Treatment_Outcome" = "Additional")) +
          scale_fill_discrete(labels = 
                                c("complete remission/response" = "Complete Response", 
                                  "partial remission/response" = "Partial Response",
                                  "progressive disease" = " Progressive Disease",
                                  "stable disease" = "Stable Disease")) +
          theme(text=element_text(size=15))
        
      } else if (input$varClinicalBL == "Age, Stage" && input$stageBL == TRUE) {
        ggplot(dat |> 
                 mutate(across(everything(), ~na_if(.x, "NA"))) |> 
                 mutate(Age_Grouping = ifelse(age_at_initial_pathologic_diagnosis <= 
                                                39, "Young", 
                                              ifelse(age_at_initial_pathologic_diagnosis >= 
                                                       40 & age_at_initial_pathologic_diagnosis <= 
                                                       64, "Middle Age",
                                                     ifelse(age_at_initial_pathologic_diagnosis > 
                                                              64, "Old", NA)))) |> 
                 mutate(Age_Grouping = factor(Age_Grouping, 
                                              levels = c("Young", "Middle Age", "Old"))) |> 
                 group_by(Age_Grouping, pathologic_stage) |> 
                 drop_na(pathologic_stage),
               aes(x = Age_Grouping, fill = pathologic_stage)) +
          geom_bar() +
          labs(x = "Age Group", y = "Count", 
               title = "Cancer Stage by Age Group in Bladder Cancer") + 
          scale_fill_manual(values = 
                              c("stage i" = "pink", "stage ii" = "skyblue", 
                                "stage iii" = "cornflowerblue", "stage iv" = "purple"),
                            labels = 
                              c("stage i" = "I", "stage ii" = "II", 
                                "stage iii" = "III", "stage iv" = "IV")) +
          labs(fill = "Stage") + theme(text=element_text(size=18))
      } else if (input$varClinicalBR == 
                 "Age, HER2 Receptor Status, Estrogen Receptor Status, Progesterone Receptor Status" &&
                 input$positiveBR == TRUE) {
        g <- ggplot(dat |>
                      select(Estrogen_Receptor_Status = breast_carcinoma_estrogen_receptor_status, 
                             Progesterone_Receptor_Status = breast_carcinoma_progesterone_receptor_status, 
                             HER2_Receptor_Status = lab_proc_her2_neu_immunohistochemistry_receptor_status, 
                             Diagnosis_Age = age_at_initial_pathologic_diagnosis)|>
                      mutate(across(everything(), ~na_if(.x, "NA"))) |>
                      drop_na(Estrogen_Receptor_Status, 
                              Progesterone_Receptor_Status, 
                              HER2_Receptor_Status, Diagnosis_Age) |>
                      mutate(Age_Grouping = 
                               ifelse(Diagnosis_Age <= 39, "Young", 
                                      ifelse(Diagnosis_Age >= 40 & Diagnosis_Age <= 64, "Middle Age",
                                             ifelse(Diagnosis_Age > 64, "Old", NA))))|>
                      mutate(Age_Grouping = factor(Age_Grouping, levels = c("Young", "Middle Age",
                                                                            "Old", NA)))|>
                      pivot_longer(cols = 
                                     c(Estrogen_Receptor_Status, 
                                       Progesterone_Receptor_Status, 
                                       HER2_Receptor_Status),
                                   names_to = "Receptor_Type", values_to = "Receptor_Status") |>
                      filter(Receptor_Status == "positive") |>
                      group_by(Age_Grouping) |>
                      mutate(total_count = n()) |>
                      group_by(Age_Grouping, Receptor_Type) |>
                      summarize(count = n(), total_count = first(total_count), .groups = "drop") |>
                      mutate(Proportion = count /total_count),
                    aes(x = Age_Grouping, y = Proportion, fill = Receptor_Type))
        g + geom_bar(stat = "identity") +
          labs(x = "Age Group", y = "Proportion",
               title = "Positive Receptor Status by Age") + 
          scale_fill_manual(values = c("Estrogen_Receptor_Status" = "pink",
                                       "Progesterone_Receptor_Status" = "skyblue",
                                       "HER2_Receptor_Status" = "cornflowerblue"
          ),
          labels = c("Estrogen_Receptor_Status" = "Estrogen Positive" ,
                     "Progesterone_Receptor_Status" = "Progesterone Positive",
                     "HER2_Receptor_Status" = "Her2 Positive")) +
          labs(fill = "Receptor") +
          theme(text=element_text(size=18))
      } else if (input$varClinicalBR == 
                 "HER2 Receptor Status, Estrogen Receptor Status, Progesterone Receptor Status" &&
               input$statusBR == "Heatmap") {
        dat2 <- dat |>
          select(Estrogen_Receptor_Status = breast_carcinoma_estrogen_receptor_status, 
                 Progesterone_Receptor_Status = breast_carcinoma_progesterone_receptor_status, 
                 HER2_Receptor_Status = lab_proc_her2_neu_immunohistochemistry_receptor_status) |>
          mutate(across(everything(), ~na_if(.x, "NA"))) |>
          pivot_longer(cols = everything(), 
                       names_to = "Receptor", values_to = "Status") |>
          drop_na(Receptor, Status)
        status_table <- table(dat2$Receptor, dat2$Status)
        status_table <- as.data.frame(status_table)
        
        g <- ggplot(status_table, aes(Var1, Var2, fill = Freq))
        g + geom_tile() +
          labs(x = "Receptor", y = "Status", title = 
                 "Heatmap of Receptor Status in Breast Cancer",
               fill = "Frequency") +
          scale_x_discrete(labels = 
                             c("Estrogen_Receptor_Status" = "Estrogen Receptor",
                               "HER2_Receptor_Status" = "HER2 Receptor",
                               "Progesterone_Receptor_Status" = "Progesterone Receptor")) +
          theme(text=element_text(size=16))
      } else if (input$varClinicalBR == 
                 "HER2 Receptor Status, Estrogen Receptor Status, Progesterone Receptor Status" &&
                 input$statusBR == "Bar Chart" && input$statusPlotsBR == "Stacked") {
        g <- ggplot(dat |>
                      select(Estrogen_Receptor_Status = breast_carcinoma_estrogen_receptor_status, 
                             Progesterone_Receptor_Status = breast_carcinoma_progesterone_receptor_status, 
                             HER2_Receptor_Status = lab_proc_her2_neu_immunohistochemistry_receptor_status) |>
                      mutate(across(everything(), ~na_if(.x, "NA"))) |>
                      pivot_longer(cols = everything(),
                                   names_to = "Receptor", values_to = "Status") |>
                      drop_na(Status), 
                    aes(x = Receptor, fill = Status))
        g + geom_bar() +
          labs(x = "Receptor", y = "Count", title = "Stacked Bar Chart of Receptor Status") +
          scale_x_discrete(labels = c("Estrogen_Receptor_Status" = "Estrogen Receptor", 
                                      "Progesterone_Receptor_Status" = "Progesterone Receptor", 
                                      "HER2_Receptor_Status" = "HER2 Receptor")) +
          scale_fill_manual(values = c("positive" = "pink",
                                       "negative" = "skyblue",
                                       "indeterminate" = "purple",
                                       "equivocal" = "cornflowerblue")) +
          theme(text=element_text(size=16))
      } else if (input$varClinicalBR == 
                 "HER2 Receptor Status, Estrogen Receptor Status, Progesterone Receptor Status" &&
                 input$statusBR == "Bar Chart" && input$statusPlotsBR == "Faceted") {
        g <- ggplot(dat |>
                      select(Estrogen_Receptor_Status = breast_carcinoma_estrogen_receptor_status, 
                             Progesterone_Receptor_Status = breast_carcinoma_progesterone_receptor_status, 
                             HER2_Receptor_Status = lab_proc_her2_neu_immunohistochemistry_receptor_status) |>
                      mutate(across(everything(), ~na_if(.x, "NA"))) |>
                      pivot_longer(cols = everything(),
                                   names_to = "Receptor", values_to = "Status") |>
                      drop_na(Status), 
                    aes(x = Receptor, fill = Status))
        g + geom_bar(position = "dodge") +
          labs(x = "Receptor", y = "Count", title = "Side by Side Bar Chart of Receptor Status") +
          scale_x_discrete(labels = c("Estrogen_Receptor_Status" = "Estrogen Receptor", 
                                      "Progesterone_Receptor_Status" = "Progesterone Receptor", 
                                      "HER2_Receptor_Status" = "HER2 Receptor")) +
          scale_fill_manual(values = c("positive" = "pink",
                                       "negative" = "skyblue",
                                       "indeterminate" = "purple",
                                       "equivocal" = "cornflowerblue")) +
          theme(text=element_text(size=16))
        
        
      } else {
        return(NULL)
      }
      
    } else {
      return(NULL)
    }
  })
  
  
  
  
  #Contingencty TABLES for data exploration
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
  
  #Code for Statistics Calculations in Data Exploration
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
  # Data output and download handling for data download tab
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
