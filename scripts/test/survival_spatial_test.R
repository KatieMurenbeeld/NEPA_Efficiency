library(ggplot2)
library(ggmap)
library(gganimate)
library(gifski)
library(maps)
library(sf)
library(stringr)
library(dplyr)
library(tidyr)
library(broom)
library(transformr)

#library(tidyverse)
library(forcats)
library(survminer)
library(survival)
library(lubridate)
library(reshape2)
#library(ggplot2)
library(ggpubr)
library(fastDummies)
library(chron)
library(vtable)

## 00 Load Data

# Set working directory
setwd('/Users/kathrynmurenbeeld/Analysis/NEPA_Efficiency/')

# Join the X data to the shape file data for forests and regions
states <- st_read('data/original/cb_2018_us_state_5m/cb_2018_us_state_5m.shp')
counties <- st_read('data/original/S_USA.ALPGeopoliticalUnit/S_USA.ALPGeopoliticalUnit.shp')
forests <- st_read('data/original/S_USA.AdministrativeForest/S_USA.AdministrativeForest.shp')
budget <- read.csv('data/original/FY2013-2018_National_Forest_Allotment.csv')
pals <- read.csv('data/original/pals_ongoing_projects_11-2022.csv', sep = ";")

## 01 Process the Data
# Pals: convert date columns from characters to datetime
# Pals: filter for NEPA initiation >= 2009-01-01 and drop extraneous columns (tbd)
# Pals: filter for Completed (no censoring this time?)

# PALS: select desired columns/variables
pals_df <- select(pals, c(REGION_ID, FOREST_ID, PROJECT.NUMBER, PROJECT.NAME, PROJECT.STATUS,
                          PROJECT.CREATED, INITIATION.DATE, DECISION.SIGNED, DECISION.TYPE,
                          ELAPSED.DAYS, DECISION_LEVEL, ongoing))

# PALS: create a binary column to serve as the Event Occurring for the survival analysis. 
# Status of Completed = 1, every other status = 0. This will make In Progress and Cancelled projects = 0.
pals_df$EVENT <- ifelse(pals_df$PROJECT.STATUS == "Complete", 1, 0)

# PALS: replace NA with 0 for ongoing
pals_df$ongoing[is.na(pals_df$ongoing)] <- 0

# PALS: make FOREST_ID a factor
pals_df$FOREST_ID <- as.factor(pals_df$FOREST_ID)

# PALS: filter by year initiated >2009-01-01 and status = Complete
## First need to convert character strings to datetime or calendar time objects
pals_df$PROJECT.CREATED <- as.Date(pals_df$PROJECT.CREATED, format = "%m/%d/%Y")
pals_df$INITIATION.DATE <- as.Date(pals_df$INITIATION.DATE, format = "%m/%d/%Y")
pals_df$DECISION.SIGNED <- as.Date(pals_df$DECISION.SIGNED, format = "%m/%d/%Y")

## Then filter by year NEPA was initiated and (&) status = complete
## Also remove Test Project TEST and project 35233
pals2009_df <- pals_df %>%
  filter(INITIATION.DATE > "2009-01-01" & PROJECT.STATUS == "Complete") %>%
  filter(PROJECT.NAME != "Test Project TEST") %>%
  filter(PROJECT.NUMBER != 35233) %>%
  filter(ongoing == 0)

# PALS: Survival Analysis, average survival % by forest at t duration = 1 year (365 days) 
# and t duration = 2 years (730 days)

fit_assess_all <- survfit(Surv(ELAPSED.DAYS, EVENT) ~ 1, data = pals2009_df)
fit_assess_summary <- fit_assess_all %>% tidy()

one_year <- fit_assess_summary %>%
  filter(time == 365)
print(one_year$estimate)
two_year <- fit_assess_summary %>%
  filter(time == 730)
print(two_year$estimate)

# Plot for fun...
km_fit_assess_all <- ggsurvplot(fit_assess_all,
                                conf.int = TRUE,
                                risk.table = FALSE,
                                risk.table.col = "strata",
                                surv.median.line = "hv",
                                break.time.by = 365,
                                risk.table.y.text=FALSE,
                                censor = FALSE,
                                ylab = "Probability that NEPA Assessment Incomplete",
                                xlab = "NEPA Initiation to NEPA Signed (days)",
                                palette = c("forestgreen"),
                                surv.plot.height = 1,
                                ggtheme = theme(aspect.ratio = 0.75, 
                                                axis.line = element_line(colour = "black"),
                                                panel.grid.major = element_line(colour = "grey"),
                                                panel.border = element_blank(),
                                                panel.background = element_blank()),
                                tables.theme =  theme(aspect.ratio = 0.06)
)
print(km_fit_assess_all)

# With the exception of the plot, make the above commands into a function or loop (or both? or neither?)
# to get the average "estimate" for each forest, for each decision type (ROD, DN, DM) at time = 365 and at time = 730

fit_assess_forest <- survfit(Surv(ELAPSED.DAYS, EVENT) ~ FOREST_ID, data = pals2009_df)
fit_forest_summary <- fit_assess_forest %>% tidy()

one_year_forest <- fit_forest_summary %>%
  filter(time == 365)

fit_assess_forest_nepa <- survfit(Surv(ELAPSED.DAYS, EVENT) ~ FOREST_ID + DECISION.TYPE, data = pals2009_df)
fit_forest_nepa_summary <- fit_assess_forest_nepa %>% tidy()

one_year_forest_nepa <- fit_forest_nepa_summary %>%
  filter(time == 365)

# Check the number of EIAs completed for each forest
eia_forest <- pals2009_df %>%
  group_by(FOREST_ID, DECISION.TYPE) %>%
  summarise(ave = mean(ELAPSED.DAYS))

eia_count_forest <- pals2009_df %>%
  count(FOREST_ID)

# May need to go by region and nepa
fit_assess_reg <- survfit(Surv(ELAPSED.DAYS, EVENT) ~ REGION_ID, data = pals2009_df)
fit_reg_summary <- fit_assess_reg %>% tidy()

one_year_reg <- fit_reg_summary %>%
  filter(time == 365)

two_year_reg <- fit_reg_summary %>%
  filter(time == 730)

fit_assess_reg_nepa <- survfit(Surv(ELAPSED.DAYS, EVENT) ~ REGION_ID + DECISION.TYPE, data = pals2009_df)
fit_reg_nepa_summary <- fit_assess_reg_nepa %>% tidy()

one_year_reg_nepa <- fit_reg_nepa_summary %>%
  filter(time == 365)

two_year_reg_nepa <- fit_reg_nepa_summary %>%
  filter(time == 730)

# To join I need to strip the leading 0 from FORESTORGC in the shapefile. 
# Change the numeric budget$Unit to a string
# Then I can left the budget data and the GIS data.


budget$Unit <- as.character(budget$Unit)
forests$FORESTORGC
forests$FORESTORGC <- str_remove(forests$FORESTORGC, "^0+")

joined_df <- left_join(forests, budget,
                       by = c("FORESTORGC" = "Unit"))

# Then I want to drop: Unit.Name, ADMINFORES, REGION, FORESTNUMB

joined_df <- select(joined_df, -c(Unit.Name, ADMINFORES, REGION, FORESTNUMB))

region1 <- joined_df %>%
  filter(Region == 1)
region4 <- joined_df %>%
  filter(Region == 4)

idaho <- states %>%
  filter(NAME == "Idaho")
pnw <- states %>%
  filter(NAME == "Idaho" | NAME =="Washington" | NAME =="Oregon" | NAME =="Montana" | NAME =="Nevada" | NAME =="Utah" | NAME =="California" | NAME =="Nevada" | NAME == "Colorado" | NAME == "Wyoming")
idaho_co <- counties %>%
  filter(STATENAME == "Idaho")

#id_basemap <- get_map(location=c(lon = -75.16522, lat = 39.95258), zoom=11, maptype = 'terrain-background', source = 'stamen')

ggplot() +
  geom_sf(data = idaho, size = 0.5) + 
  coord_sf()


test_region4 <- ggplot() + 
  #geom_sf(data = region4, size = 0.5, fill = "forestgreen") + 
  geom_sf(data = pnw, size = 0.5) + 
  geom_sf(data = region4, size = 0.5, aes(fill = X2013)) + 
  ggtitle("Test Forest Plot") + 
  coord_sf()
ggsave("test_pnw_2013.png", test_region4)
dev.off()

test_region4 <- ggplot() + 
  #geom_sf(data = region4, size = 0.5, fill = "forestgreen") + 
  geom_sf(data = pnw, size = 0.5) + 
  geom_sf(data = region4, size = 0.5, aes(fill = X2014)) + 
  ggtitle("Test Forest Plot") + 
  coord_sf()
ggsave("test_pnw_2014.png", test_region4)
dev.off()

# Now need to figure out animation. Probably need to reformat data so that year is a column?
# Need: Forest | Location Info | Budget | Year
# so each Forest should account for 6 rows?

region4_long <- region4 %>%
  pivot_longer(cols = c("X2013", "X2014", "X2015", "X2016", "X2017", "X2018"), names_to = "year", values_to = "budget")

region4_long$year<-gsub("X","",as.character(region4_long$year))
region4_long$year <- as.numeric(region4_long$year)

# Let's try animation!

map_with_data <- 
  ggplot() + 
  geom_sf(data = pnw, size = 0.5) + 
  geom_sf(data = region4_long, size = 0.5, aes(fill = budget)) + 
  ggtitle("Test Forest Plot") +  
  coord_sf()
ggsave("map_with_data.png", map_with_data)


map_with_animation <- map_with_data +
  transition_time(year) +
  ggtitle('Year: {frame_time}',
          subtitle = 'Frame {frame} of {nframes}')
num_years <- max(region4_long$year) - min(region4_long$year) + 1
animate(map_with_animation, nframes = num_years, fps = 2)
anim_save("region4_animated.gif", map_with_animation, fps =2, nframes = num_years)

## Need to figure out how to add a basemap. I want the state outlines. 
#map_with_data <- idaho_map +
#  geom_sf(data = region1, size = 3, fill = "green") + 
#  coord_sf(crs = st_crs(4269)) 

# If I leave coord_df() empty I still get the "Coordinate system already present" error.
# NAD83 has an EPSG code of 4269

# I think I will need to reformat the data so that year is a column like what I did in the jupyter notebook