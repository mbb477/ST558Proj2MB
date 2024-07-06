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
              a CSV file.")
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
          selectInput("endpoint", "Select Endpoint", 
                      choices = c("mRNASeq", "Clinical")),
          selectInput("cancer", "Select Cancer Type",
                      choices = c("BRCA", "BLCA", "LUAD")),
          br(),
          conditionalPanel(
            condition = "input.endpoint == 'mRNASeq'",
            selectInput("plotmRNASeq", "Select a Plot Type",
                        choices = c("Density", "Box Plot"))
          ),
          conditionalPanel(
            condition = "input.endpoint == 'Clinical' & 
            input.cancer == 'BRCA'",
            selectInput("plotClinicalBR", "Select a Plot",
                        choices = c("Treatment Bar Chart", 
                                    "Receptor Status Bar Chart", 
                                    "Receptor Status Heat Map",
                                    "Status by Age Bar Chart"))
          ),
          conditionalPanel(
            condition = "input.endpoint == 'Clinical' & 
            input.cancer == 'BRCA' && input.plotClinicalBR == 
            'Status by Age Bar Chart'",
            radioButtons("plotType", "Select a Plot Type", choices = 
                           c("Stacked Bar Chart", "Faceted Bar Chart"),
                         selected = "")
          ),
          conditionalPanel(
            condition = "input.endpoint == 'Clinical' & input.cancer == 'BLCA'",
            selectInput("plotClinicalBLCA", "Select a Plot",
                        choices = c("Outcome Bar Chart", 
                                    "Stage by Age Bar Chart"))
          ),
          conditionalPanel(
            condition = "input.endpoint == 'Clinical' & input.cancer == 'LUAD'",
            selectInput("plotClinicalLUAD", "Select a Plot",
                        choices = c("Treatments Bar Chart", 
                                    "Outcome Horizontal Bar Chart"))
          ),
          br(),
          checkboxInput("tableOption", "Would you like to select a table?",
                        value = FALSE),
          conditionalPanel(
            condition = "input.tableOption & input.endpoint == 'mRNASeq'",
            selectInput("tableMRNASeq", "Select a Table",
                        choices = c("Summary Statistics"))
          ),
          conditionalPanel(
            condition = "input.tableOption & input.endpoint == 'Clinical'",
            selectInput("tableClinical", "Select a Contingency Table",
                        choices = c("Gender", "Race", "Stage"))
          )
        ),
        mainPanel(
          plotOutput("plotExplore"),
          DTOutput("tableExplore")
        )
      )
    )
  )
)
