library(shiny)
library(DT)

fluidPage(
  navbarPage(
    "App for Clinical Survey and Analysis of Key Genes in Breast, Lung and Bladder Cancer",
    tabPanel(
      "About",
      titlePanel("About this App"),
      br(),
      tags$p("This app allows the user to query the FireBrowse API. The 
              FireBrowse API contains clinical cancer data from the Broad
              Institute of MIT and Harvard's Firehose analysis 
              infrastructure. Data obtained from FireBrowse has varying
              levels of completeness and the data overlaps with API data 
              available from the Genomic Data Commons (GDC) and cBioPortal.
              Complete data sets can be downloaded at cBioPortal,",
             tags$a(href="https://www.cbioportal.org/", "cBioPortal"), ".
              Other links of interest are ", 
             tags$a(href= "https://gdc.cancer.gov/", "GDC"), " and",
             tags$a(href="https://gdac.broadinstitute.org/", "Firehose"),
             "."),
      br(),
      tags$p("This app consists of two additional tabs, the Data 
              Download tab and the Data Exploration tab. The Data Download 
              tab allows the user to select 6 different combinations of 
              endpoints and cancer types. The two endpoints are mRNASeq, 
              which provides gene expression data, and Clinical, which 
              provides various types of information, mostly categorical, 
              depending on the cancer type selected. The cancer type 
              options for the query include breast, lung and bladder 
              cancer. Selecting an endpoint and cancer type will produce 
              a data table which can subsequently be subsetted and saved as 
              a CSV file."),
      tags$img(src = "mRNA.webp")
    ),
    tabPanel(
      "Data Download",
      sidebarLayout(
        sidebarPanel(
          selectInput("endpoint", "Select Endpoint", 
                      choices = c("mRNASeq", "Clinical")),
          br(),
          selectInput("cancer", "Select Cancer Type",
                      choices = c("BRCA", "BLCA", "LUAD")),
          checkboxInput("subsetColumns", "Do you want to subset columns?",
                        value = FALSE),
          conditionalPanel(
            condition = "input.subsetColumns == true",
            selectInput("columns", "Select Columns",
                        choices = NULL, multiple = TRUE)
          ),
          checkboxInput("subsetRows", "Do you want to subset rows?",
                        value = FALSE),
          conditionalPanel(
            condition = "input.subsetRows == true",
            selectInput("filterColumn", "Select filter variable", 
                        choices = NULL),
            selectInput("filterLevel", "Filter Options", choices = NULL,
                        multiple = TRUE)
          ),
          downloadButton("download", "Download Results in CSV Format")
        ),
        mainPanel(
          DTOutput("cancerTable")
        )
      )
    ),
    tabPanel(
      "Data Exploration",
      sidebarLayout(
        sidebarPanel(
          selectInput("endpointE", "Select Endpoint", 
                      choices = c("mRNASeq", "Clinical"), selected = "mRNASeq"),
          selectInput("cancerE", "Select Cancer Type",
                      choices = c("BRCA", "BLCA", "LUAD"), selected = "BRCA"),
          br(),
          conditionalPanel(
            condition = "input.endpointE == 'mRNASeq'",
            selectInput("plotmRNASeq", "Select a Plot Type",
                        choices = c("Density", "Box Plot"))
          ),
          br(),
          conditionalPanel(
            condition = "input.endpointE == 'Clinical' & input.cancerE == 'BRCA'",
            radioButtons(
              "varClinicalBR", 
              "Select a variable(s) for Plotting",
              choices = c(
                "HER2 Receptor Status, Estrogen Receptor Status, Progesterone Receptor Status", 
                "Age, Positive Receptor Status"), selected = ""),
            conditionalPanel(
              condition = "input.varClinicalBR == 'Age, Positive Receptor Status'",
              checkboxInput("positiveBR", "Positive Receptor Status by Age Chart", 
                value = FALSE)),
            conditionalPanel(
              condition = 
                "input.varClinicalBR == 'HER2 Receptor Status, Estrogen Receptor Status, Progesterone Receptor Status'",
              selectInput(
                "statusBR", 
                "Select a plot option",
                choices = c("Heatmap", "Bar Chart")),
              conditionalPanel(
                condition = "input.statusBR == 'Bar Chart'",
                radioButtons("statusPlotsBR", "Select the type of Bar Chart",
                             choices = c("Stacked", "Faceted"), selected = "")))
            ),
          br(),
          conditionalPanel(
            condition = "input.endpointE == 'Clinical' & input.cancerE == 'LUAD'",
            radioButtons("varClinicalL", "Select a variable(s) for Plotting",
                         choices = c("Radiation Therapy and Targeted Molecular Therapy", 
                                     "Primary Therapy Outcome"), selected = ""),
            conditionalPanel(
              condition = "input.varClinicalL == 
              'Radiation Therapy and Targeted Molecular Therapy'",
              checkboxInput("therL", "Treatments Bar Chart", value = FALSE)),
            
            conditionalPanel(
              condition = "input.varClinicalL == 'Primary Therapy Outcome'",
              checkboxInput("outcomeL", "Outcome Bar Chart", value = FALSE))),
          br(),
          conditionalPanel(
            condition = "input.endpointE == 'Clinical' & input.cancerE == 'BLCA'",
            radioButtons("varClinicalBL", "Select a variable(s) for Plotting",
                         choices = c("Age, Stage", 
                                     "Primary Therapy Outcome, Additional Therapy Outcome"),
                         selected = ""),
            conditionalPanel(
              condition = "input.varClinicalBL == 
              'Age, Stage'",
              checkboxInput("stageBL", "Age vs. Stage Bar Chart", value = FALSE)),
            
            conditionalPanel(
              condition = "input.varClinicalBL == 
              'Primary Therapy Outcome, Additional Therapy Outcome'",
              checkboxInput("outcomeBL", "Outcome by Therapy Bar Chart", value = FALSE))),
          br(),
          conditionalPanel(
            condition = "input.endpointE == 'Clinical'",
            selectInput("tableClinical", "Select a Contingency Table",
                        choices = c("Gender", "Race", "Stage"))),
          br(),
          conditionalPanel(
            condition = "input.endpointE == 'mRNASeq'",
            checkboxInput("summary", "Select Gene and Summary Statistics?")
          ),
          conditionalPanel(
            condition = "input.summary == true",
            uiOutput("geneSelection")
          ),
          conditionalPanel(
            condition = "input.endpointE == 'mRNASeq' & input.genesel != null & 
            input.summary",
            uiOutput("statSelection")
          ),
          
          textOutput("statisticOutput")
        ),
        mainPanel(
          
          plotOutput("plotExplore"),
          textOutput("statisticOutput"),
          DTOutput("tableExplore")
        )
      )
    )
  )
)

