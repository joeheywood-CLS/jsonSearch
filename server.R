
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
source("R/jsonTest.R")
source("R/jsonTest_shiny.R")
fls <- getRecFiles()
library(shiny)

shinyServer(function(input, output, session) {

  output$recName <- renderText({
      jsn <- getRecJSON(input$select)
      output$vrbLabel <- renderText({jsn$dimension[[input$vrb]]$displaylabel})
      output$labels <- renderDataTable(
          datatable(getCategoryInfo(jsn$dimension[[input$vrb]]$category), 
                    options = list(searching = FALSE, paging = FALSE), escape = FALSE)
          
      )
      updateSelectizeInput(session, 'vrb', choices = names(jsn$dimension), server = TRUE) 
      output$recDoc <- renderText({jsn$doc})
      output$numVars <- renderText({paste0("Number of variables: ", length(jsn$dimension))})
      paste0(jsn$recNum, ": ", jsn$recName)
  })
  

  output$schUpd <- renderText({
      if(nchar(input$sch) > 1) {
          load("N:/jsonSearch//rda/labels.Rda")
          lbs <- labels[which(grepl(input$sch, labels$label)), ]
          rcs <- unique(lbs$rec )
          updateSelectizeInput(session, 'schIn', choices = rcs, server = TRUE)
          save(lbs, file = "N:/jsonSearch/tmp/lbs.Rda")
          paste0("Found ", nrow(lbs), " items in ", length(rcs), " records" )
      }


  })
  
  output$schT <- renderText({
      if(nchar(input$schIn) > 0) {
          load("N:/jsonSearch/tmp/lbs.Rda")
          jsn <- getRecJSON(input$schIn, kyw = input$sch)
          output$schRecName <- renderText({paste0(jsn$recNum, ": ", jsn$recName)})
          output$schRecDoc <- renderText({jsn$doc})
          lbs <- lbs[which(lbs$rec == jsn$recNum),]
          save(jsn, file = "N:/jsonSearch/tmp/jsn.Rda")
          updateSelectizeInput(session, 'schVrb', 
                               choices = unique(lbs$var), server = TRUE) 
      }
      "blah"
  })
  
  output$schLabels <- renderDataTable({
      if(nchar(input$schVrb) > 0) {
          load("N:/jsonSearch/tmp/jsn.Rda")
          
          output$srchVrbLabel <- renderUI ({HTML(jsn$dimension[[input$schVrb]]$displaylabel)})
          return(datatable(getCategoryInfo(jsn$dimension[[input$schVrb]]$category), 
                    options = list(searching = FALSE, paging = FALSE), escape = FALSE))
      }
      datatable(data.frame())
      
  })
  

})
