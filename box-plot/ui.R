library(tidyverse)
library(shiny)
library(readxl)
library(plotly)


shinyUI(fluidPage(

    titlePanel("One-way ANOVA"),

    sidebarLayout(
        sidebarPanel(width = 3 ,
            fileInput("archivo" ,
                      "select file csv or xlsx" ,
                      accept = c(".xlsx" , ".csv")) ,
            fluidRow(
                column(
                    width = 5 ,
                    numericInput(
                        "n_hoja" ,
                        "sheet" ,
                        width = '80px' ,
                        value = 1 ,
                        min = 1 ,
                        max = 2
                    )
                ) ,
                column(
                    width = 5 ,
                    numericInput(
                        "n_col" ,
                        "col" ,
                        width = '80px' ,
                        value = 2 ,
                        min = 2 ,
                        max = 7
                    )
                )
            ) ,
            tabsetPanel(id = "tabset" ,
                        tabPanel("Plot" ,
                                 br() ,
                                 radioButtons(
                                     "id_fun_order" ,
                                     "ordered by" ,
                                     choices = c("NULL" , "mean" , "median" , "sd") ,
                                     inline = T
                                 ) ,
                                 radioButtons(
                                     "id_order" ,
                                     "order" ,
                                     choices = c("ascending" , "descending") ,
                                     inline = T
                                 ) ,
                                 br() ,
                                 textInput("nombre" ,
                                           label = "graphic title ") ,
                                 textInput("eje_y" ,
                                           "Y axis title")
                        ) ,
                        tabPanel("Inference" ,
                                 br() ,
                                 radioButtons(
                                     inputId = "selection" ,
                                     "select" ,
                                     inline = T ,
                                     choices = c("Normality" , "Homoscedasticity" , "ANOVA" , "all")
                                 )
                        )
            )
        ) ,
        mainPanel(
            actionButton(
                inputId = "go" ,
                "Go"
            ),
            verbatimTextOutput("t_anova") ,
            plotlyOutput("plot")
        )
    )
))





