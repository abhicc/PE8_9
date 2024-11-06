library(tidyverse)   
library(shiny)
library(Lock5Data)
library(DT)

data("USStates2e")

ui9 <- navbarPage("US States Summaries from 2014",
                 tabPanel("Income",
                          sidebarLayout(
                            sidebarPanel(
                              sliderInput(inputId = "number_bins", 
                                          label = "Choose number of bins",
                                          min = 20, 
                                          max = 40,
                                          value = 30
                              )
                            ),
                            mainPanel(
                              plotOutput("histplot")
                            )
                          )
                 ),
                 tabPanel("Politics",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput(inputId = "region", 
                                          label = "Select a US region",
                                          choices = c("Mid West" = "MW", "North East" = "NE", "South" = "S", "West" = "W"), 
                                          selected = "MW"
                              )
                            ),
                            mainPanel(
                              plotOutput("barplot")
                            )
                          )
                 ),
                 navbarMenu("Other Demographics",
                            tabPanel("Health",
                                     sidebarLayout(
                                       sidebarPanel(
                                         varSelectInput(inputId = "x_var", 
                                                        label = "Select x-axis variable",
                                                        data = USStates2e %>% dplyr::select(PhysicalActivity, HeavyDrinkers, Smokers),
                                                        selected = "PhysicalActivity"
                                         )
                                       ),
                                       mainPanel(
                                         plotOutput("scatterplot")
                                       )
                                     )
                            ),
                            tabPanel("Education",
                                     DT::dataTableOutput("table")
                            )
                 )
)


server9 <- function(input, output, session) {
  
  output$histplot <- renderPlot({
    
    USStates2e %>% 
      ggplot() +
      geom_histogram(aes(x = HouseholdIncome), bins = input$number_bins, color = "white", fill = "blue")
    
  })
  
  output$barplot <- renderPlot({
    
    USStates2e %>% filter(Region == input$region) %>% 
      ggplot() +
      geom_bar(aes(y = ObamaRomney), fill = "purple") +
      labs(x = "States won by Obama and Romney", y = "") +
      scale_y_discrete(labels = c("Obama", "Romney"))
    
    
  })
  
  output$scatterplot <- renderPlot({
    
    USStates2e %>% 
      ggplot +
      geom_point(aes(x = !!input$x_var, y = Obese))
    
  })
  
  
  output$table <- DT::renderDataTable({
    
    DT::datatable(USStates2e %>% 
                    group_by(Region) %>% 
                    dplyr::summarize(Averegae_HSGrad_Percentage = round(mean(HighSchool, na.rm = TRUE), 2),
                              Average_CollegeGrad_Percentage = round(mean(College, na.rm = TRUE), 2)))
    
  })
  
}

# Run the app
shinyApp(ui = ui9, server = server9)
