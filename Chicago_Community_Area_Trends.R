library(tidyverse)
library(tidycensus)
library(sf)

### Spatial functions to combine tracts and Community Areas ####
## Get Initial Boundaries
# Get Chicago boundaries
chi_bound <- tigris::places(state = "IL", cb = T, class = "sf") %>%
  filter(NAME == "Chicago") %>%
  select(geometry)

# 2010 Census tracts
cook_tract_bound <- tigris::tracts(state = "IL", county = "Cook", year = 2010, class = "sf")

## Limit tracts in just Chicago
# Get intersecitons
intersections_cook <- st_intersection(chi_bound, cook_tract_bound)
intersections_cook$area <- as.numeric(st_area(intersections_cook$geometry))

# Find Cook County tracts that intersect with Chicago
intersections_cook <- intersections_cook %>%
  filter(area != 0)

# Load neighborhoods shapefile
comm_ares <- rgdal::readOGR("./Comm Areas")
cook_tracts <- rgdal::readOGR("Tract Boundaries")


# Convert to simple files
tracts_sf <- st_as_sf(cook_tracts)[,c("GEOID10", "geometry")]
comm_sf <- st_as_sf(comm_ares)[c("COMMUNITY", "geometry")]

# plot both
plot(tracts_sf)
plot(comm_sf)

# Find which tracts intersect with each neighborhood
intersections <- st_intersection(comm_sf, tracts_sf)

# Get the area of each intersection
intersections$area <- as.numeric(st_area(intersections$geometry))

# Assign each tract to the neighborhood that it has the most area in
inter_comm <- intersections %>%
  group_by(GEOID10) %>%
  summarise(comm = COMMUNITY[which.max(area)])

###

cook_tract_bound_chi <- cook_tract_bound %>%
  filter(GEOID10 %in% inter_comm$GEOID10)

# Use Census tract boundaries instead of Chicago Limited
inter_comm2 <- merge(as.data.frame(inter_comm)[,c("GEOID10", "comm")], 
              cook_tract_bound_chi[,c("GEOID10", "geometry")],
              by = "GEOID10") %>%
  st_as_sf()
  
# Show All tracts
cook_tract_bound_chi$geometry %>%
  ggplot()+
  geom_sf(color = "gray", fill = NA)+
  geom_sf(data = chi_bound, color = "black", fill = NA)+
  theme_void()+
  labs(title = "Chicago City Limits with Census Tracts")

# Plot neighborhood boundaries with tracts
inter_comm2 %>%
  ggplot()+
  geom_sf(color = NA, aes(fill = comm))+
  geom_sf(data = comm_sf$geometry, color = "black", fill = NA)+
  theme_void()+
  theme(legend.position = "none")+
  labs(title = "Chicago Community Areas with Combined Census Tracts")

# Combine the census tract geometries together
comm_tract_boundaries <- inter_comm %>%
  group_by(comm) %>%
  summarise(geo = st_union(geometry))

# Plot the combined boundaries
neigh_tract_boundaries %>%
  ggplot()+
  geom_sf(fill = NA)+
  theme_void()

#### Data Work ####

## Get variable lists
# 2010 Census
variables10 <- load_variables(2010, "sf1")

# Get 2018 5-year ACS variables
variables18 <- load_variables(2018, "acs5")

## Get Cook County Estimates and limit to Chicago
# 2010 
chi_10 <- get_decennial(geography = "tract", 
                        variables = c("P012001"),
                        state = "IL", county = "Cook",
                        geometry = F, year = 2010) %>%
  filter(GEOID %in% inter_comm$GEOID10) 

# per capital income: B19301_001

# 2018
chi_18 <- get_acs(geography = "tract", 
                  variables = c("B02001_001", # Total population (race)
                                "B02001_002", # White
                                "B02001_003", # Black or African American
                                "B03002_012", # Hispanic or latino
                                "B19301_001"), # Per capita income 
                  state = "IL", county = "Cook",
                  geometry = F, year = 2018, survey = "acs5") %>%
  filter(GEOID %in% inter_comm$GEOID10) %>%
  select(-moe) %>%
  spread(key = variable, value = estimate)

# Set NA income to 0 (population is also 0 so it won't affect results)
chi_18$B19301_001[is.na(chi_18$B19301_001)] <- 0

variables18$label[variables18$name %in% colnames(chi_18)]



# Combine population counts, combine into community areas, find the differences 
chi_data <- full_join(chi_10[,c("GEOID", "value")],
                  chi_18, 
                  by = "GEOID") %>%
  left_join(as.data.frame(inter_comm)[,c("GEOID10", "comm")], 
            by = c("GEOID" = "GEOID10")) %>%
  mutate(total.income = (B19301_001*B02001_001)) %>% # get total income from per capita income
  group_by(comm) %>%
  summarise(White = sum(B02001_002), Black = sum(B02001_003), Hisp = sum(B03002_012), # Race/ethnicity stats
            pop10 = sum(value), pop18 = sum(B02001_001), # Summarise population
            per.cap.income = sum(total.income)/pop18) %>% # income
  mutate(white.pct = White/pop18, black.pct = Black/pop18, hisp.pct = Hisp/pop18,
         diff = pop18 - pop10, pct.change = diff/pop10) # Population changes


# Add back geomery
chi_data <- left_join(comm_tract_boundaries[,c("comm", "geo")], 
                      chi_data, 
                         by = "comm")

pop_changes <- cbind(chi_data, st_coordinates(st_centroid(chi_data)))

# Show the population changes
pop_changes %>%
  ggplot(aes(fill = pct.change))+
  geom_sf(color = "black")+
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0,
                       breaks = c(-2., -.1, 0, .1, .2),
                       labels = c("-20%", "-10%", "0%", "10%", "20%"))+
  theme_void()+
  guides(fill = guide_colourbar(title = "Population Change", 
                                ticks.colour = "black"))+
  labs(title = "Chicago Community Areas Population Change\n(2010 to 2018)")+
  theme(plot.title = element_text(hjust = .5))

# Plot community area population changes on a bar chart
pop_changes %>%
  ggplot(aes(x = reorder(comm, pct.change), y = pct.change, fill = pct.change))+
  geom_col(color = "black")+
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0,
                       breaks = c(-2., -.1, 0, .1, .2),
                       labels = c("-20%", "-10%", "0%", "10%", "20%"))+
  scale_y_continuous(name = "Percentage Change",
                     breaks = c(-.2, -.1, 0, .1, .2), 
                     labels = c("-20%", "-10%", "0%", "10%", "20%"))+
  scale_x_discrete(name = "", labels = element_blank())+
  labs(title = "Chicago Community Areas Population Change\n(2010 to 2018)")+
  theme(plot.title = element_text(hjust = .5),
        panel.grid.major.y = element_line(color = "black"),
        panel.background = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "none")

# Replicate the bar chart but only limit it to community areas with population changes of 
# greater than plus/minus 10%
pop_changes %>%
  filter(pct.change < -.1 | pct.change > .1) %>%
  mutate(pct.change = round(pct.change, 2)) %>%
  ggplot(aes(x = reorder(comm, pct.change), y = pct.change, fill = pct.change))+
  geom_col(color = "black")+
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0,
                       breaks = c(-2., -.1, 0, .1, .2),
                       labels = c("-20%", "-10%", "0%", "10%", "20%"))+
  scale_y_continuous(name = "Percentage Change",
                     breaks = c(-.2, -.1, 0, .1, .2), 
                     labels = c("-20%", "-10%", "0%", "10%", "20%"))+
  labs(title = "Chicago Community Areas Population Change\n(2010 to 2018)", x = element_blank())+
  theme(plot.title = element_text(hjust = .5),
        panel.grid.major.y = element_line(color = "black"),
        panel.background = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(angle = 90, 
                                   #vjust = 4,
                                   face = "bold"))+
  geom_text(aes(label = paste(pct.change*100, "%", sep = "")),
                vjust = -.5)

# Get total diff
(sum(pop_changes$pop18)-sum(pop_changes$pop10))/sum(pop_changes$pop10)

# Get labels for notable income distribution areas
pop_changes_labels <- pop_changes %>%
  filter(comm %in% c("NEAR SOUTH SIDE", "NEAR NORTH SIDE", "LOOP", "NORTH CENTER",
                     "RIVERDALE", "ENGLEWOOD", "WEST ENGLEWOOD", "NEW CITY", 
                     "FULLER PARK")) 

# Plot Income per capital by community area
pop_changes %>%
  ggplot(aes(fill = per.cap.income))+
  geom_sf(color = "black")+
  scale_fill_gradient(low = "white", high = "darkgreen",
                      breaks = c(25000, 50000, 75000), 
                      labels = c("$25,000", "$50,000", "$75,000"))+
  theme_void()+
  guides(fill = guide_colourbar(title = "Per Capita Income", 
                                ticks.colour = "black"))+
  labs(title = "Chicago Community Areas Per Capita Income")+
  theme(plot.title = element_text(hjust = .5))+
  ggrepel::geom_text_repel(data = pop_changes_labels, aes(x = X, y = Y,
                                                          label =as.character(comm)),
                           nudge_x = c(-60000, 90000, 10000, 30000, 70000, -50000, 30000, -30000, -50000),
                           nudge_y = c( 10000, 10000, 0, 10000, 1, 10000, 0, 0, -10000),
                           size = 3, color = "black")

# Get city wide per capita income
sum(pop_changes$per.cap.income * pop_changes$pop18)/sum(pop_changes$pop18)

# Plot income per capita vs Population Change
pop_changes %>%
  ggplot(aes(x = pct.change, y = per.cap.income))+
  geom_point()+
  geom_smooth(se = F, method = "lm")+
  theme_minimal()+
  labs(title = "Chicago Community Areas\nPopulation Change and Per Capita Income",
       x = "Population Change", y = "Per Capita Income")+
  scale_x_continuous(breaks = c(-.2, -.1, 0, .1, .2, .3),
                     labels = c("-20%", "-10%", "0%", "10%", "20%", "30%"))+
  scale_y_continuous(breaks = c(0, 25000, 50000, 75000, 100000),
                     labels = c("$0", "$25,000", "$50,000", "$75,000", "$100,000"))+
  ggrepel::geom_text_repel(aes(label=ifelse(pct.change > .1 | pct.change < -.1, 
                             as.character(stringr::str_to_title(comm)), "")),
            hjust = 0, vjust = 0)+
  theme(plot.title = element_text(hjust = .5))+
  geom_hline(yintercept = 34749.52)+
  geom_text(aes(-.1, 34749.52), label = "Chicago Per Capita Income", vjust = -.5, size = 4)

# White Population
pop_changes %>%
  ggplot(aes(fill = white.pct))+
  geom_sf(color = "black")+
  scale_fill_gradient(low = "white", high = "darkblue",
                      breaks = c(.25, .5, .75), 
                      labels = c("25%", "50%", "75%"))+
  theme_void()+
  guides(fill = guide_colourbar(title = "Percentage White", 
                                ticks.colour = "black"))+
  labs(title = "Chicago Community Areas Distribution of\nWhite Population")+
  theme(plot.title = element_text(hjust = .5))


# Get labels for notable Black or African American Population community ares
pop_changes_labels <- pop_changes %>%
  filter(comm %in% c("RIVERDALE", "OAKLAND", "DOUGLAS")) 

# Black or African American Population distribution
pop_changes %>%
  ggplot(aes(fill = black.pct))+
  geom_sf(color = "black")+
  scale_fill_gradient(low = "white", high = "darkblue",
                      breaks = c(.25, .5, .75), 
                      labels = c("25%", "50%", "75%"))+
  theme_void()+
  guides(fill = guide_colourbar(title = "Percentage Black\n or African American", 
                                ticks.colour = "black"))+
  labs(title = "Chicago Community Areas Distribution of\nBlack or African American Population")+
  theme(plot.title = element_text(hjust = .5))+
  ggrepel::geom_text_repel(data = pop_changes_labels, aes(x = X, y = Y,
                                                          label =as.character(comm)),
                           nudge_x = c(300000, 30000, -30000),
                           size = 3, color = "black")

# Get labels for notable Hispanic or Latino Population community ares
pop_changes_labels <- pop_changes %>%
  filter(comm %in% c("CLEARING", "EAST SIDE", "WEST ELSDON")) 

# Plot Hispanic or Latino distribution
pop_changes %>%
  ggplot(aes(fill = hisp.pct))+
  geom_sf(color = "black")+
  scale_fill_gradient(low = "white", high = "darkblue",
                      breaks = c(.25, .5, .75), 
                      labels = c("25%", "50%", "75%"))+
  theme_void()+
  guides(fill = guide_colourbar(title = "Percentage Hispanic\n or Latino", 
                                ticks.colour = "black"))+
  labs(title = "Chicago Community Areas Distribution of\nHispanic or Latino Population")+
  theme(plot.title = element_text(hjust = .5))+
  ggrepel::geom_text_repel(data = pop_changes_labels, aes(x = X, y = Y,
                             label =as.character(comm)),
            nudge_x = c(-20000, 0, -30000), nudge_y = c(-5000, 30000, 10000),
            size = 3, color = "black")


