# loads the required library
library(dplyr)
library(ggplot2)
library(tidyr)
library(maps)
library(stringr)
library(plotly)
library(scales)

# loads the file
source("analysis.R")
countries.index <- All.Country.List() #The list of all the countries and thir ISO3 code
# Creates a server function 
server <- function(input, output, clientData, session) {
  
  # This is a reactive varaible that finds a new set of pie info when input parameters are changed
  pies <- reactive({
    select <- select()
    year <- year()
    Attack.Info.Pies(country.iso(), year, select)
  })
  
  YN.to.01 <- function(selection){
    if(selection == 'YES') return('1')
    if(selection == 'NO') return('0')
    return('-1')
  }
  
  # It is a reactive variable that helps in selecting data for pie chart.
  select <- reactive({
    type <- input$type.select
    target <- input$target.select
    weap <- input$weap.select
    multi <- input$multi.select
    success <- input$success.select
    suicide <- input$suicide.select
    select = list()
    if(type != 'ALL') select <- c(select, 'attacktype1_txt' = type)
    if(target != 'ALL') select <- c(select, 'targtype1_txt' = target)
    if(weap != 'ALL') select <- c(select, 'weaptype1_txt' = weap)
    if(multi != 'BOTH') select <- c(select, 'multiple' = YN.to.01(multi))
    if(success != 'BOTH') select <- c(select,'success' = YN.to.01(success))
    if(suicide != 'BOTH') select <- c(select, 'suicide' = YN.to.01(suicide))
    return(select)
  })
  
  # It is a reactive variable that returns the range for year.
  year <- reactive({
    year <- input$year
    return(c(year))
    year
  })
  
  country.iso <- reactive({
    country.iso <- countries.index[input$country]
    return(country.iso)
  })
  
  lists <- reactive({
    select <- select()
    year <- year()
    Attack.Info.List(country.iso, year, select)
  })
  
  # Passes the input recieved from the ui to a function to get the plot.
  output$graph <- renderPlotly({
    return(Global.Terrorism.Attacks(year()[1],year()[2]))
    
  })
  
  # The three pie charts to disply attack information
  output$type.pie <- renderPlotly({pies()[['type']]})
  output$target.pie <- renderPlotly(pies()[['targets']])
  output$weap.pie <- renderPlotly(pies()[['weap']])
  output$plot.multiple <- renderPlotly(pies()[['multi']])
  output$plot.success <- renderPlotly(pies()[['success']])
  output$plot.suicide <- renderPlotly(pies()[['suicide']])
  
  
  observe({ # Listen to when the to-be-included attributes are changed
    lists <- Attack.Info.List(country.iso(), input$year, c())
    updateSelectInput(session, 'type.select', choices = lists[['type']]) # Change the attribute choices for plot's x-axis
    updateSelectInput(session, 'target.select', choices = lists[['target']]) # Change the attribute choices for plot's y-axis
    updateSelectInput(session, 'weap.select', choices = lists[['weap']]) # Change the attribute choices for table's sorting method
  })
}
shinyServer(server)