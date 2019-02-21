# Final Project
# Info 201 AA1
# 2017/03/02
# This is a file contains functions to generate plots to show on a pages
library(dplyr)
library(ggplot2)
library(tidyr)
library(maps)
library(stringr)
library(plotly)
library(scales)
library(reshape2)

DATA <- read.csv('./data/trimmed.csv', stringsAsFactors = FALSE)
#DATA <- read.csv('./data/globalterrorismdb_0616dist.csv', stringsAsFactors = FALSE)
ISO3.CONVERT <- read.csv('./data/country_data.csv', stringsAsFactors = FALSE)
DATA.w.ISO3 <- left_join(DATA, ISO3.CONVERT)
countries <- read.csv('./data/country_data.csv', stringsAsFactors = FALSE)


# pre: should pass as ISO3(current) string(ALL CAPS) or 'WORLD' to country.iso3, a vector of a starting year
#    and ending year(numbers) to year.range, and a list of filters to selected.
#    Format for filter: ['col.name'='attribute to filter']
#    Example for filter: [attacktype1_txt='Assassination', targtype1_txt='Private Citizens & Property']
#
# post: Will return a list of plotly pie charts indicating Attacks Type, Targets,
#   and Used Weapons
Attack.Info.Pies <- function(country.iso3, year.range, selected){
  filtered <- Pie.Data.Filter(country.iso3, year.range, selected)
  return(list(type = Attack.Type.Pie(filtered),
              targets = Attack.Target.Pie(filtered),
              weap = Attack.Weap.Pie(filtered),
              multi = compare.rates(filtered, 'multiple'),
              suicide = compare.rates(filtered,'suicide'),
              success = compare.rates(filtered,'success')))
}

# pre: should give data a filtered data
# post: will return a plotly contains a pie chart showing the ratio of each kind of
#   attack
Attack.Type.Pie <- function(data){
  gathered <- data %>%
    gather(key = num, value = type,
           attacktype1_txt) %>% #, attacktype2_txt, attacktype3_txt) %>%
    group_by(type) %>%
    summarise(time = n()) %>%
    filter(type != '.')
  
  return(plot_ly(gathered, labels = ~type, values = ~time,
                 textposition = 'inside', textinfo = 'label+percent',
                 showlegend = FALSE) %>%
           add_pie(hole = 0.6) %>% 
           layout(title = "Attack Types",
                  xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                  yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)))
}

# pre: should give data a filtered data
# post: will return a plotly pie chart showing the ratio of each kind of attack targets
Attack.Target.Pie <- function(data){
  gathered <- data %>%
    gather(key = num, value = type,
           targtype1_txt) %>% #, targtype2_txt, targtype3_txt) %>%
    group_by(type) %>%
    summarise(time = n()) %>%
    filter(type != '.')
  
  return(plot_ly(gathered, labels = ~type, values = ~time,
                 textposition = 'inside', textinfo = 'label+percent',
                 showlegend = FALSE) %>%
           add_pie(hole = 0.6) %>% 
           layout(title = "Attack Targets",
                  xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                  yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)))
}


# pre: should give data a filtered data
# post: will return a plotly pie chart showing the ratio od each kind of Weapons used
#   in attacks
Attack.Weap.Pie <- function(data){
  gathered <- data %>%
    gather(key = num, value = type,
           weaptype1_txt) %>% #, weaptype2_txt, weaptype3_txt, weaptype4_txt) %>%
    group_by(type) %>%
    summarise(time = n()) %>%
    filter(type != '.')
  
  return(plot_ly(gathered, labels = ~type, values = ~time,
                 textposition = 'inside', textinfo = 'label+percent',
                 showlegend = FALSE) %>%
           add_pie(hole = 0.6) %>% 
           layout(title = "Attack Weapons",
                  xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                  yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)))
}

# pre: should pass as ISO3(current) string(ALL CAPS) or 'WORLD' to country.iso3, a vector of a starting year
#    and ending year(numbers) to year.range, and a list of filters to selected.
#    Format for filter: ['col.name'='attribute to filter']
#    Example for filter: [attacktype1_txt='Assassination', targtype1_txt='Private Citizens & Property']
#
# post: will return a list of information that are not duplicated in Attack type, targets and weapon
Attack.Info.List <- function(country.iso3, year.range, selected){
  filtered <- Pie.Data.Filter(country.iso3, year.range, selected)
  
  return(list('type' = Attack.Type.List(filtered), 'target'=Attack.Target.List(filtered),
              'weap' = Attack.Weap.List(filtered)))
}

# pre: should give data a filtered data
# post: will return a list of non-duplicate attack type under the data
Attack.Type.List <- function(data){
  gathered <- data %>%
    gather(key = num, value = type,
           attacktype1_txt) %>% #, attacktype2_txt, attacktype3_txt) %>%
    group_by(type) %>%
    summarise(time = n()) %>%
    filter(type != '.')
  
  return(c('ALL', gathered[['type']]))
}


# pre: should give data a filtered data
# post: will return a list of non-duplicate attack target under the data
Attack.Target.List <- function(data){
  gathered <- data %>%
    gather(key = num, value = type,
           targtype1_txt) %>% #, targtype2_txt, targtype3_txt) %>%
    group_by(type) %>%
    summarise(time = n()) %>%
    filter(type != '.')
  
  return(c('ALL', gathered[['type']]))
}

# pre: should give data a filtered data
# post: will return a list of non-duplicate attack weapon under the data
Attack.Weap.List <- function(data){
  gathered <- data %>%
    gather(key = num, value = type,
           weaptype1_txt) %>% #, weaptype2_txt, weaptype3_txt, weaptype4_txt) %>%
    group_by(type) %>%
    summarise(time = n()) %>%
    filter(type != '.')
  
  return(c('ALL', gathered[['type']]))
}

# pre: should pass as ISO3(current) string(ALL CAPS) or 'WORLD' to country.iso3, a vector of a starting year
#    and ending year(numbers) to year.range, and a list of filters to selected.
#    Format for filter: ['col.name'='attribute to filter']
#    Example for filter: [attacktype1_txt='Assassination', targtype1_txt='Private Citizens & Property']
#
# post: will return a data frame that was filtered by give filter
Pie.Data.Filter <- function(country.iso3, year.range, selected){
  # Filters out the data within the year.range
  filtered <- DATA.w.ISO3 %>%
    filter(iyear >= year.range[1], iyear <= year.range[2])
  
  # filter out the selected country if needed
  if(country.iso3 != 'WORLD'){
    filtered <- filtered %>%
      filter(New.ISO3 == country.iso3)
  }
  
  # filter out the selected data
  for(key in names(selected)){
    filtered <- filtered %>%
      filter_(paste0(key, '=="' ,selected[key], '"'))
  }
  #write.csv(filtered, 'testing.csv')
  return(filtered)
}

# pre: when need to obtain a list of countries that are on in the attack database
# post: will return a list of countries in the format of "Name" = ISO3
All.Country.List <- function(){
  country <- ISO3.CONVERT %>%
    filter(ISO3 == New.ISO3)
  
  countries <- c('WORLD', country[['country_txt']])
  country.iso3 <- c('WORLD', country[['New.ISO3']])
  names(country.iso3) <- countries
  
  return(country.iso3)
}

# pre:  Insert date range in terms of years (min & max year)
# post: The function will return a ggplotly world map illustrating the number of terrorist attacks
#       during the selected years
Global.Terrorism.Attacks <- function(year.min, year.max) {  
  data <-
    DATA.w.ISO3 %>%
    filter(iyear >= year.min & iyear <= year.max) %>% 
    group_by(country_txt, ISO3) %>%
    summarize(Attacks = n()) %>% 
    right_join(countries, by = c('country_txt' = 'country_txt')) %>%
    select(Country = country_txt, ISO3 = ISO3.y, Attacks) %>% 
    arrange(Attacks)
  
  
  data$Attacks[is.na(data$Attacks)] <- 0
  
  
  l <- list(color = toRGB("grey"), width = 0.5)
  
  g <- list(
    showframe = FALSE,
    showcoastlines = FALSE,
    projection = list(type = 'Mercator')
  )
  
  return(plot_geo(data) %>%
           add_trace(z = ~Attacks, color = ~Attacks, colors = 'Reds', hoverinfo = 'text',
                     text = ~paste(Country, "</br>Attacked", Attacks, "Times"), 
                     marker = list(line = l), locations = ~ISO3) %>%
           colorbar(title = 'Attacks') %>%
           layout(
             title = paste(ifelse(year.min == year.max, year.min, paste(year.min, "to", year.max)),'Global Terrorism Attacks<br>Source:<a href="http://start.umd.edu/gtd/">Global Terrorism Database</a>'),
             geo = g
           )
  )
}



# It creates a function that takes the name of attack and type of attack as parameter
# and returns a donut pie chart representing percentage of whether the type of attack and 
# the attack hapened or not.
compare.rates <- function(filtered.data, data.type){
  
  multiple.data <- filtered.data  %>%
    select_(paste0("`", data.type, "`")) %>% 
    group_by_(data.type) %>%
    summarise(count = n()) %>%
    mutate_("yes.or.no" = paste0('ifelse(', data.type, ' == 0,"No", "Yes")'))
  
  validate(need(nrow(multiple.data) != 0, "No plot to display, Please choose different option."))
  p <- plot_ly(multiple.data, labels = ~yes.or.no, values = ~count,
               textposition = 'inside', textinfo = 'label+percent',
               showlegend = FALSE) %>%
    add_pie(hole = 0.6) %>%
    layout(title = paste0('Is it a "', data.type, '" attack?'),
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  return(p)
  
}