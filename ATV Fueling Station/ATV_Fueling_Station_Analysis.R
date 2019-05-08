## reading in the fueling station by state data
library(readxl)
fueling_stations_state <- read_excel("fueling_stations_state.xlsx")
View(fueling_stations_state)

library(plotly)

##give state boundires a white border
l <- list(color=toRGB("white"),width=2)

##specify some map projection
g <- list( scope='usa', projection=list(type="albers usa"), showlakes= TRUE, lakecolor=toRGB("white"))

## creating a heat map of the number of fueling station in the United States by stat
p3 <- plot_geo(fueling_stations_state, locationmode = 'USA-states') %>%    
  add_trace( z = fueling_stations_state$Num_fueling_stations, locations = fueling_stations_state$state, color = fueling_stations_state$Num_fueling_stations, colors='Reds') %>% colorbar(title = "# of Stations") %>% layout( title="Fueling Stations by State(99-17)", geo=g)
##view heatmap
p3

##reading in the stations by year
library(readxl)
fueling_stations_yearly <- read_excel("fueling_stations_yearly.xlsx")
View(fueling_stations_yearly)

##creating a smooth scatter plot
scatter.smooth(x=fueling_stations_yearly$Year, y=fueling_stations_yearly$Occurences, main="Fueling Stations by Year")

## seeing a correlation between year and number of stations
cor(fueling_stations_yearly$Occurences, fueling_stations_yearly$Year )
##[1] 0.8835146

##bulding linear model based on number of stations per year
linearModel <- lm(fueling_stations_yearly$Occurences~fueling_stations_yearly$Year)
linearModel
summary(linearModel)


##adding total sale vectorfrom ATV Sales with factors data Project_data.csv
totalSales <- c(17, 9350, 20282, 36035, 47600, 84199, 209711, 252636, 352274, 312386,	290271, 274210, 266345, 434813, 495529, 452172, 384404, 346948, 370685)
##correlation of total number of fueling stations in the US and total ATV Sales
cor(fueling_stations_yearly$Occurences, totalSales)
##[1] 0.7175252