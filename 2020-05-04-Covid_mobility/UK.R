library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthhires)
library(tmap)
library(tmaptools)
library(RColorBrewer)

### Data set-up ---------------------------------

#Load the data
global <- read_csv("Data/Global_Mobility_Report_03-05-20.csv", col_types = "ccccDdddddd")
UK_key <- read_csv("Data/uk_sf_map_data_key.csv")
UK_map <- ne_states(country = "united kingdom", returnclass = "sf")

#Create custom palette with plotHelper() and https://learnui.design/tools/data-color-picker.html

color_scheme <- c("aquamarine","palegreen", "darkolivegreen1", "beige", "navajowhite")
color_hex_6 <- rev(c("#e3dd6b", "#92d37b",  "#3ac297",  "#00abae",  "#0091b5",  "#2e74a6"))
color_hex_7 <- rev(c("#e3dd6b",  "#94d673", "#26c88f", "#00b6af",  "#009fc8",   "#0085ce",  "#0066bd"))

UK <- global %>% 
  filter(country_region_code == "GB") %>% 
  filter(sub_region_1 != "NA") %>% 
  select(-country_region_code, -sub_region_2)


UK_indexed <- left_join(UK_key, UK, by = c("google_name" = "sub_region_1"))
  

UK_full <- inner_join(UK_map, UK_indexed, by = "name")

#Just want to know about time period starting week before lockdown

UK_late <- UK_full %>%
  filter(date > "2020-03-04")

### Creating animated maps-----------------------------


anim1 <- UK_late %>%
  tm_shape() +
  tm_fill("retail_and_recreation_percent_change_from_baseline", title = "Retail and Recreation Mobility", 
              palette = color_hex_7, colorNA = NULL) +
  tm_borders(col = "white", lwd = 0.5) +
  tm_layout(legend.outside = TRUE, legend.outside.position = "right", legend.title.size = 4) +
  tm_facets(along = "date")

tmap_animation(anim1, filename = "retail_UK_anim.gif", width = 1000, height = 1000,
               delay = 50, restart.delay = 200)


anim2 <- UK_late %>%
  tm_shape() +
  tm_fill("grocery_and_pharmacy_percent_change_from_baseline", title = "Grocery and Pharmacy Mobility", 
          palette = color_hex_7, colorNA = NULL) +
  tm_borders(col = "white", lwd = 0.5) +
  tm_layout(legend.outside = TRUE, legend.outside.position = "right", legend.title.size = 4) +
  tm_facets(along = "date")

tmap_animation(anim2, filename = "grocery_UK_anim.gif", width = 1000, height = 1000,
               delay = 50, restart.delay = 200)


anim3 <- UK_late %>%
  tm_shape() +
  tm_fill("transit_stations_percent_change_from_baseline", title = "Transit Station Mobility", 
          palette = color_hex_7, colorNA = NULL) +
  tm_borders(col = "white", lwd = 0.5) +
  tm_layout(legend.outside = TRUE, legend.outside.position = "right", legend.title.size = 4) +
  tm_facets(along = "date")

tmap_animation(anim3, filename = "transit_UK_anim.gif", width = 1000, height = 1000,
               delay = 50, restart.delay = 200)


anim4 <- UK_late %>%
  tm_shape() +
  tm_fill("workplaces_percent_change_from_baseline", title = "Workplace Mobility", palette = color_hex_6,
          colorNA = NULL) +
  tm_borders(col = "white", lwd = 0.5) +
  tm_layout(legend.outside = TRUE, legend.outside.position = "right", legend.title.size = 4) +
  tm_facets(along = "date")

tmap_animation(anim4, filename = "workplace_UK_anim.gif", width = 1000, height = 1000,
               delay = 50, restart.delay = 200)


### Look at mobilities overall-----------------------
library(hrbrthemes)
library(lubridate)

UK_overall <- global %>% 
  filter(country_region_code == "GB") %>% 
  filter(is.na(sub_region_1)) %>% 
  select( 5:ncol(.) )

UK_overall_long <- UK_overall %>% pivot_longer(cols = 2:7, names_to = "type", values_to = "change")

#Set up weekend tracking
UK_overall_long$weekend <- weekdays(UK_overall_long$date) %in% c("Saturday", "Sunday")

UK_weekends <- UK_overall_long %>% 
  filter(weekend == "TRUE")


#plot
ggplot() +
  geom_line(data = UK_overall_long, aes(x= date, y=change, color = type), size = 1) +
  geom_point(data = UK_weekends, aes(x= date, y=change, color = type), size = 2) +
  theme_ipsum() +
  labs(title = "Changes in mobility for the UK overall") +
  scale_color_manual(labels = c("Grocery and pharmacy", "Parks", "Residential", 
                                "Retail and recreation", "Transit stations", "Workplaces"), 
                     values = c("#007ab3", "#7279c9", "#c16ec1", "#fa659b", "#ff7664", "#ff9d26")) +
  annotate("rect", fill = "lightsalmon", alpha = 0.1, 
           xmin = as.Date("2020-03-23"), xmax = as.Date("2020-04-26"),
           ymin = -Inf, ymax = Inf) +
  annotate("rect", fill = "lightgoldenrod2", alpha = 0.1, 
           xmin = as.Date("2020-03-16"), xmax = as.Date("2020-03-23"),
           ymin = -Inf, ymax = Inf) +
  annotate("text", x = as.Date("2020-03-29"), y = 35, label = "Lockdown") +
  annotate("text", x = as.Date("2020-03-19"), y = 35, label = "Social
  distancing")



ggsave("timeline.png", plot = last_plot(), device = "png", path = NULL, scale = 1, width = 10, height = 6, units = "in")
