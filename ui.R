# Loads the required library
library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)

# Loads the file
source("analysis.R")
bool.choices <- c('BOTH', 'YES', 'NO')
# Creats a ui containing navbar, fluid page layout.
ui <- tagList(
  navbarPage(theme  =  shinythemes::shinytheme("united"), "Global Terrorism Database", # adds a shiny theme and a crates a navbar page
             tabPanel("Introduction",
                fluidPage(
                  h1("Introduction"),
                  p("Welcome to the Global Terrorism Dataset Exploration Tool (GTDET). Using this tool, you will be able to explore the data of more than 150,000 confirmed terrorists attacks occurred worldwide between the year 1970 to 2015. The data is taken from Global Terrorism Database which is maintained by Study of Terrorism and Responses to Terrorism (START) at the University of Maryland."),
                  h1("Objective"),
                  p("Terrorism is one of the biggest challenge of the 21st century which is affecting almost all countries in the world, one way or another. In order to stay safe from terrorism attacks, it is important to analyze the nature of terrorist attacks occurred around the location of concern so that security measures can be taken accordingly. This tool offers user an interactive way to explore historical terrorism data and get the useful insights from it.")
                )
              ), # Creates a Tab
             tabPanel("Data Visualization", # Creates a Tab
                      fluidPage( # creates a fluid page layout
                        h1("World Map"),
                        p("The following graph illustrates the number of terrorist attacks in each country between the selected year range.  Countries with greater overall terrorism attacks are filled with darker color, whereas those with less terrorism attacks are marked with a lighter color.  "),
                        div(plotlyOutput("graph")), # craetes a division and displays a map
                        fluidRow( # Creates a fluid row with columns
                          column(width =  4, selectInput("country", "country", names(All.Country.List()))), # displays a select input box to select country
                          column(width = 4, sliderInput("year", "Year range:", min = 1970, max = 2015, value = c(1970,2015))) # displays a slider to select year range
                        ),
                        tags$hr(style="border-color: black;"), # adds a horizontal line with black colour
                        h1("Attack Composition"),
                        p("This section is dedicated to display the composition of attacks in the selected region.  There are 6 attributes that we are looking into.  From each attribute, users can manually select one of the fields for the other charts to display the composition of attacks solely of the selected attribute."),
                        fluidRow( # creates a fluid row layout with columns
                          column(width = 4, selectInput("type.select", 'Select Attack Type', choices = c()), # displays a drop down list to select attack type;no multiples allowed
                                 plotlyOutput("type.pie")), # displays the pie for attack type
                          column(width = 4, selectInput("target.select",'Select Target Type', choices = c()), # displays a drop down list to select target type;no multiples allowed 
                                 plotlyOutput("target.pie")), # displays the pie for target type
                          column(width = 4, selectInput("weap.select", 'Select Weapon Type',choices = c()), # displays a drop down list to select weapon type;no multiple allowed
                                 plotlyOutput("weap.pie")) # displays they pie for weapon type
                        ),
                        
                        fluidRow( # Creates a fluid row with column layout
                          column(width = 4, selectInput("multi.select", 'Select Yes or No', choices = bool.choices),
                                 plotlyOutput("plot.multiple")), # displays the pie for wheather the attack multiple
                          column(width = 4, selectInput("success.select", 'Select Yes or No', choices = bool.choices),
                                 plotlyOutput("plot.success")), # displays the pie for wheather the attack is successful
                          column(width = 4, selectInput("suicide.select", 'Select Yes or No', choices = bool.choices),
                                 plotlyOutput("plot.suicide")) # displays the pie for wheather the attack is suicidal
                        )
                      )
             )
  )
)

# Loads the shinyUi app
shinyUI(ui)