library(tidyverse)
library(sf)

extrafont::loadfonts(device = "win")

# election results
ge_data <- read_csv("http://researchbriefings.files.parliament.uk/documents/CBP-7979/HoC-GE2017-constituency-results.csv") %>% 
  filter(region_name == "London") %>% 
  select(ons_id, constituency_name, con:green)

# shapefile filtered to London region
# data available here: https://www.dropbox.com/s/4iajcx25grpx5qi/uk_650_wpc_2017_full_res_v1.8.zip?dl=0
uk <- st_read("uk_650_wpc_2017_full_res_v1.8.shp", stringsAsFactors = FALSE) %>% 
  st_transform(4326) %>% 
  filter(REGN == "London") %>% 
  select(ons_id = PCONCODE)

# merge the data
sf_data <- left_join(ge_data, uk) %>% 
  st_as_sf()

# data frame of number of dots to plot for each party (1 for every 100 votes)
num_dots <- ceiling(select(as.data.frame(sf_data), con:green) / 100)

# generates data frame with coordinates for each point + what party it is assiciated with
sf_dots <- map_df(names(num_dots), 
                  ~ st_sample(sf_data, size = num_dots[,.x], type = "random") %>% # generate the points in each polygon
                    st_cast("POINT") %>%                                          # cast the geom set as 'POINT' data
                    st_coordinates() %>%                                          # pull out coordinates into a matrix
                    as_tibble() %>%                                               # convert to tibble
                    setNames(c("lon","lat")) %>%                                  # set column names
                    mutate(Party = factor(.x, levels = names((num_dots))))        # add categorical party variable
) # map_df then binds each party's tibble into one

# colour palette for our party points
pal <- c("con" = "#0087DC", "lab" = "#DC241F", "ld" = "#FCBB30", "ukip" = "#70147A", "green" = "#78B943")

# plot it and save as png big enough to avoid over-plotting of the points
p <- ggplot() +
  geom_sf(data = sf_data, fill = "transparent", colour = "white") +
  geom_point(data = sf_dots, aes(lon, lat, colour = Party)) +
  scale_colour_manual(values = pal) +
  coord_sf(crs = 4326, datum = NA) +
  theme_void(base_family = "Iosevka", base_size = 48) +
  labs(x = NULL, y = NULL,
       title = "UK General Election 2017",
       subtitle = "1 dot = 100 votes",
       caption = "Map by @PaulCampbell91 | Data Sources: House of Commons Library, Alasdair Rae") +
  guides(colour = guide_legend(override.aes = list(size = 18))) +
  theme(legend.position = c(0.8, 1.01), legend.direction = "horizontal",
        plot.background = element_rect(fill = "#212121", color = NA), 
        panel.background = element_rect(fill = "#212121", color = NA),
        legend.background = element_rect(fill = "#212121", color = NA),
        legend.key = element_rect(fill = "#212121", colour = NA),
        plot.margin = margin(1, 1, 1, 1, "cm"),
        text =  element_text(color = "white"),
        title =  element_text(color = "white"),
        plot.caption = element_text(size = 28)
  )

ggsave("party_points.png", plot = p, dpi = 320, width = 85, 
       height = 70, units = "cm")
