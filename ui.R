
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
source("R/jsonTest.R")
library(DT)
fls <- getRecFiles()
library(shiny)
library(shinythemes)

shinyUI(navbarPage("App Title", shinytheme("journal"),
            tabPanel("Rec Info",
                     selectInput("select", label = h3("Record"), 
                                 choices = fls$recNum, 
                                 selected = 100),
                     h2(textOutput("recName")),
                     h3(textOutput("recDoc")),
                     h4(textOutput("numVars")),
                     selectizeInput("vrb", "Select Variable", choices = NULL),
                     h4(textOutput("vrbLabel")),
                     dataTableOutput("labels")
            ),
            tabPanel("Search",
                     textInput("sch", label = "Search text"),
                     textOutput("schT"),
                     textOutput("schUpd"),
                     selectizeInput("schIn", "Found in recs:", choices = NULL),
                     h2(textOutput("schRecName")),
                     h3(textOutput("schRecDoc")),
                     h4(textOutput("schNumVars")),
                     selectizeInput("schVrb", "Select Variable", choices = NULL),
                     p(uiOutput("srchVrbLabel")),
                     dataTableOutput("schLabels")
            )
        ))