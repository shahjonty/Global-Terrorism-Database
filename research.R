library(dplyr)
library(plotly)
library(ggplot2)
library(maps)
library(countrycode)
library(tidyr)

source('./API.R')
DATA <- read.csv('./data/globalterrorismdb_0616dist.csv', stringsAsFactors = FALSE)
ISO3.CODE <- read.csv('./data/country_data.csv', stringsAsFactors = FALSE)


# This block will try to find out the terrorist attact per year

attack.per.year <- DATA %>%
				   group_by(iyear) %>%
				   summarise(times = n()) %>%
				   mutate(`Collected Institution` = ifelse(iyear <= 1997, 'PGIS',
				   										ifelse(iyear <= 2008, 'CETIS',
				   											ifelse(iyear <= 2011, "ISVG", 'START'))))

write.csv(attack.per.year, './data/attack.per.year.csv')

# This block will show graph of attacks each year
ggplot(data = attack.per.year) +
 	   geom_point(mapping = aes(x = iyear, y = times, 
		 						color = `Collected Institution`)) +
	   labs(title = "Attack time in time",
			x = 'Year', y = "Times")

# this block will contain table of attacks each country from 1970 to 2015
attack.per.country <- DATA %>%
					  group_by(country_txt) %>%
					  summarise(`Attacked Times` = n())
write.csv(attack.per.country, './data/attack_per_country.csv')

# this block will show map distribution about world titol terrorist distribution
world.map <- map_data('world2') %>%
			 mutate(ISO3 = iso.alpha(region, n = 3))

attack.per.country <- left_join(attack.per.country, ISO3.CODE)

ggplot(data = left_join(world.map, attack.per.country, by = c('ISO3' = 'New.ISO3'))) +
	geom_polygon(mapping = aes(x = long, y = lat, group = group,
							   fill = `Attacked Times`)) + 
	labs(title = 'Total Attacked Times From 1970 to 2015') +
	theme(axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank())

# this will show attack total every 10 year

attack.per.10 <- DATA %>%
				 mutate(year.group = floor((iyear - 1970) / 10)) %>%
				 mutate(year.group.label = paste(year.group * 10 + 1970, '~',
                                          ifelse(year.group * 10 + 1980 > 2015, 2015,
                                                 year.group * 10 + 1980))) %>%
				 group_by(country_txt, year.group.label) %>%
				 summarise(attack.times = n()) %>%
				 left_join(ISO3.CODE)

ggplot(data = left_join(world.map, attack.per.10, by = c('ISO3' = 'New.ISO3'))) +
	geom_polygon(mapping = aes(x = long, y = lat, group = group,
							   fill = attack.times)) +
	facet_grid(year.group.label ~ .) + 
	coord_quickmap() + 
	labs(title = "Tatal Attacks in every 10 years from 1970 to 2015") + 
	theme(axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank())


# show the distribution of weapons from 1970 to 2015
weapons <- DATA %>%
		   gather(key = from, value = weap.type,
		   		  weaptype1_txt, weaptype2_txt,
				  weaptype3_txt, weaptype4_txt) %>%
		   filter(weap.type != ".", weap.type != "Unknown")


ggplot(data = weapons) + 
	geom_bar(mapping = aes(x = weap.type), stat = "count")

# this shows the attack group
groups <- DATA %>%
		  group_by(gname) %>%
		  summarise(times = n())

write.csv(groups, './data/groups.csv')


# motive list
motive <- DATA %>%
		  group_by(motive) %>%
		  summarise(times = n())

write.csv(motive, './data/motive.csv')

# kill unnecessary cols
kill <- DATA %>%
		select(iyear, attacktype1_txt, targtype1_txt, weaptype1_txt, country_txt, success, suicide, multiple) %>%
		filter(iyear >= 1970)

write.table(kill, file = "./data/trimmed.csv", row.name = FALSE, sep =',')

# data for every five year

every.five <- DATA %>%
			  select(iyear, attacktype1_txt, targtype1_txt, weaptype1_txt, country_txt, success, suicide, multiple) %>%
			  mutate(year.group = (iyear - 1970) / 5) %>%
			  group_by(country_txt, year.group, success, suicide, multiple, attacktype1_txt, targtype1_txt, weaptype1_txt) %>%
			  summarise(times = n()) %>%
			  mutate(iyear = 1970 + year.group * 5)

write.table(every.five, file = "./data/everyfive.csv", row.name = FALSE, sep =',')