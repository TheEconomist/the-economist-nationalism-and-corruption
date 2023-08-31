# Give ISO3C + data column, get ggplot2


plot_data_on_world_map <- function(data){

  library(tidyverse)

  # create data for world coordinates using
  # map_data() function
  world <- map_data("world")
  world <- world[world$region != 'Antarctica', ]
  world$iso3c <- countrycode(world$region, 'country.name', 'iso3c')
  world$order <- 1:nrow(world)
  world <- merge(world, data, by = 'iso3c', all.x= T)
  world <- world[order(world$order), ]
  world$plot <- world[, ncol(world)]

  ggplot()+geom_map(data = world,
                    map = world, aes(long, lat, map_id = region, fill = plot))+theme_minimal()+coord_equal()+scale_fill_continuous(low ='gray95', high ='red')+ylab('')+xlab('')
}
