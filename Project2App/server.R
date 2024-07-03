

library(shiny)
library(httr)
library(tidyverse)
library(jsonlite)
library(DT)


# Define server logic required to draw a histogram
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
        url <- "Error: Endpoint or cancer type not recognized."
      }
      
      response <- GET(url)
      parsed <- fromJSON(rawToChar(response$content))
      as_tibble(parsed[[endpoint]])
    }
    
    # Call the function with reactive inputs
    firebrowse_query(input$endpoint, input$cancer)
  })
    
  
    output$distPlot <- renderPlot({

        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')

    })
    output$cancerTable <- renderDT({
      cancer_data()
    }, plugins = "ellipsis", 
    options = list(scrollX = TRUE,
                   autoWidth = TRUE,
                   columnDefs = list(
                     list(
                       targets = "_all",
                       render = 
                         JS("$.fn.dataTable.render.ellipsis( 10, false)")
                     ))
    )
    )
    
    output$download <- downloadHandler(
      filename = function(){"CancerData.csv"}, 
      content = function(fname){
        write.csv(cancer_data(), fname)
      }
    )
}


