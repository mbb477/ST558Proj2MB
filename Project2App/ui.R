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
              a data table which can subsequently subsetted and saved as 
              a CSV file.")
    ),
    tabPanel(
      "Data Download",
      sidebarLayout(
        sidebarPanel(
          selectInput("endpoint", "Select Endpoint", 
                      choices = list("mRNASeq", "Clinical")),
          br(),
          selectInput("cancer", "Select Cancer Type",
                      choices = list("BRCA", "BLCA", "LUAD")),
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
          selectInput("endpoint_explore", "Select Endpoint", 
                      choices = list("mRNASeq", "Clinical")),
          selectInput("cancer_explore", "Select Cancer Type",
                      choices = list("BRCA", "BLCA", "LUAD")),
          br(),
          selectInput("type", "Select data output type", 
                      choices = list("Plot", "Table")),
          conditionalPanel(
            condition = "input.type == 'Plot'",
            selectInput("plots", "Select a Plot Type",
                        choices = c("Density Plot",
                                    "Box Plot", 
                                    "Violin Plot",
                                    "Bar Chart",
                                    "Pie Chart"))
          )
        ),
        mainPanel(
          DTOutput("distPlot")
        )
      )
    )
  )
)
