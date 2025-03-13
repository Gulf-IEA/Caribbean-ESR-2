# Code to plot the commercial fishing engagement and reliance data as maps

# Data were sent by Tarsila Seara 4/10/24

# Last updated 3/13/25 by Carissa Gervasi

rm(list = ls())

plot.new()
dev.off()

############ First download shape files

library(sf)
library(dplyr)
library(ggplot2)
library(tidyr)
library(readr)
library(grid)
library(readxl)
library(stringr)

pr_shapefile <- st_read("../../../Downloads/kx-puerto-rico-municipality-boundaries-SHP/puerto-rico-municipality-boundaries.shp")

vi_shapefile <- st_read("../../../Downloads/tl_2023_78_cousub/tl_2023_78_cousub.shp")

#Rename the name column to Community
pr_shapefile = pr_shapefile %>% 
  rename(Community = NOMBRE)

vi_shapefile = vi_shapefile %>% 
  rename(Community = NAME)


# remove some weird rows in the vi file
vi_shapefile = vi_shapefile %>% 
  filter(Community != "Island subdivision not defined")

# There are three East Ends, need to re-name them
vi_shapefile = vi_shapefile %>% 
  mutate(Community = ifelse(Community == "East End" & COUNTYFP == "010", "East End STX",
                            ifelse(Community == "East End" & COUNTYFP == "020", "East End STJ",
                                   ifelse(Community == "East End" & COUNTYFP == "030", "East End STT",
                                          Community))))


# Separate St. Croix from St. Thomas and St. John

STX_shapefile = vi_shapefile %>% 
  filter(COUNTYFP == "010")

STSJ_shapefile = vi_shapefile %>% 
  filter(COUNTYFP != "010")

###########################################
################## Read in CSVI data

############## Data cleaning 

#Load the data files
PR = read.csv("indicator_data/inputsToBeUpdatedAnnually/Puerto Rico ER scores.csv")
VI = read.csv("indicator_data/inputsToBeUpdatedAnnually/USVI ER scores.csv")

# Need to fix the community names so they are consistent with the shapefiles

vi_shapefile$Community
VI$SUBDIVISION
#VI is good, we just need to modify the names of the East Ends
VI = VI %>% 
  mutate(Community = recode(SUBDIVISION, "East End_T" = "East End STT", "East End_X" = "East End STX"))
VI$Community

pr_shapefile$Community
PR$MUNICIPALITY
PR = PR %>% 
  mutate(Community = str_to_title(MUNICIPALITY, locale = "es"))
PR$Community
PR = PR %>% 
  mutate(Community = recode(Community, "Anasco" = "Añasco", "Catano" = "Cataño", "Guanica" = "Guánica", "Juana Diaz" = "Juana Díaz", "Loiza" = "Loíza", "Manati" = "Manatí", "Mayaguez" = "Mayagüez", "Penuelas" = "Peñuelas", "Rincon" = "Rincón", "Rio Grande" = "Río Grande"))
PR$Community

# Rename the indicator columns
PR <- PR %>%
  rename(Eng_score = ENGSCORE,
         Rel_score = RELSCORE)
VI <- VI %>% 
  rename(Eng_score = Engagement_T,
         Rel_score = Reliance_T)

#Create Region column
PR$Region = rep("Puerto Rico", times = nrow(PR))
VI = VI %>% 
  rename(Region = ISLAND) 
VI = VI %>% 
  mutate(Region = recode(Region, "STJ" = "St. John", "STT" = "St. Thomas", "STX" = "St. Croix"))


# Remove columns that are not needed, only keep community, Region, eng_score, rel_score



### PICK BACK UP HERE

























################## Data manipulation

CSVI_2010$Year <- 2010
CSVI_2020$Year <- 2020

# Combine both dataframes
df_combined <- bind_rows(CSVI_2010, CSVI_2020)


# Convert to long format for ggplot
df_long <- df_combined %>%
  pivot_longer(cols = ends_with("ct"),
               names_to = "Indicator",
               values_to = "Score")

# Remove rows with NA values
df_long <- na.omit(df_long)

df_long$Community = as.character(df_long$Community)
df_long$Region = as.character(df_long$Region)
df_long$Year = as.factor(df_long$Year)
df_long$Indicator = as.character(df_long$Indicator)
df_long$Score = as.integer(df_long$Score)

# Rename and combine regions
df_long <- df_long %>%
  mutate(Region = recode(Region,
                         'St. Thomas' = 'STSJ',
                         'St. John' = 'STSJ',
                         'St. Croix' = 'STX'))

# Verify the changes
unique(df_long$Region)

# Rename indicators
df_long <- df_long %>%
  mutate(Indicator = recode(Indicator,
                            'PerDis_ct' = 'Personal Disruption',
                            'PopCom_ct' = 'Pop Composition',
                            'Poverty_ct' = 'Poverty',
                            'LabFrc_ct' = 'Labor Force',
                            'HsChr_ct' = 'Housing Charac',
                            'RetMig_ct' = 'Retiree Migration'))

unique(df_long$Indicator)

# There are two East Ends, one in STX and one in STT. Need to rename them
df_long = df_long %>% 
  mutate(Community = ifelse(Community == "East End" & Region == "STX", "East End STX",
                            ifelse(Community == "East End" & Region == "STSJ", "East End STSJ",
                                   Community)))


############### merge shapefiles with CSVI data

pr_map_dat = right_join(df_long, pr_shapefile, by = "Community")
STSJ_map_dat = right_join(df_long, STSJ_shapefile, by = "Community")
STX_map_dat = right_join(df_long, STX_shapefile, by = "Community")

head(pr_map_dat)
head(STSJ_map_dat)
head(STX_map_dat)


# Need to fill out rows for communities with no data and add NAs for the score variable

# Define all possible values for Year and Indicator
years <- as.factor(c(2010, 2020))
indicators <- c("Personal Disruption", "Pop Composition", "Poverty", 
                "Labor Force", "Housing Charac", "Retiree Migration")

# Extract geometry column separately
community_geometry_pr <- pr_map_dat %>%
  select(Community, geometry) %>%
  distinct()

community_geometry_STSJ <- STSJ_map_dat %>%
  select(Community, geometry) %>%
  distinct()

community_geometry_STX <- STX_map_dat %>%
  select(Community, geometry) %>%
  distinct()

# Create a grid of all combinations
all_combinations_pr <- expand.grid(
  Community = unique(pr_map_dat$Community), 
  Year = years,
  Indicator = indicators
)

all_combinations_STSJ <- expand.grid(
  Community = unique(STSJ_map_dat$Community), 
  Year = years,
  Indicator = indicators
)

all_combinations_STX <- expand.grid(
  Community = unique(STX_map_dat$Community), 
  Year = years,
  Indicator = indicators
)

# Add rows for all combinations to pr_map_dat
pr_map_dat_complete <- all_combinations_pr %>%
  left_join(community_geometry_pr, by = "Community") %>% # Add geometry
  left_join(pr_map_dat, by = c("Community", "Year", "Indicator", "geometry")) # Merge with original data

STSJ_map_dat_complete <- all_combinations_STSJ %>%
  left_join(community_geometry_STSJ, by = "Community") %>% # Add geometry
  left_join(STSJ_map_dat, by = c("Community", "Year", "Indicator", "geometry")) # Merge with original data

STX_map_dat_complete <- all_combinations_STX %>%
  left_join(community_geometry_STX, by = "Community") %>% # Add geometry
  left_join(STX_map_dat, by = c("Community", "Year", "Indicator", "geometry")) # Merge with original data

# Replace missing `Score` with NA
pr_map_dat_complete <- pr_map_dat_complete %>%
  mutate(Score = ifelse(is.na(Score), NA, Score))

STSJ_map_dat_complete <- STSJ_map_dat_complete %>%
  mutate(Score = ifelse(is.na(Score), NA, Score))

STX_map_dat_complete <- STX_map_dat_complete %>%
  mutate(Score = ifelse(is.na(Score), NA, Score))



######################### Make the plots!

#change dataframe to sf

pr_map_dat_complete <- st_as_sf(pr_map_dat_complete)
st_geometry_type(pr_map_dat_complete)

STSJ_map_dat_complete <- st_as_sf(STSJ_map_dat_complete)
st_geometry_type(STSJ_map_dat_complete)

STX_map_dat_complete <- st_as_sf(STX_map_dat_complete)
st_geometry_type(STX_map_dat_complete)




# Prepare data for facet wrapping
facet_data_pr <- pr_map_dat_complete %>%
  filter(Year %in% c(2010, 2020)) %>%
  mutate(
    Year = as.factor(Year),  # Ensure Year is treated as a factor for faceting
    Indicator = factor(Indicator, levels = c(
      "Personal Disruption", "Pop Composition", "Poverty", "Labor Force", "Housing Charac", 
      "Retiree Migration" 
    ))
  )

# Plot with facets
p1 = ggplot(facet_data_pr) +
  geom_sf(aes(fill = Score), color = "black", size = 0.2) +
  scale_fill_gradientn(colors = c("blue", "yellow", "red"), 
                       limits = c(0, 4), 
                       breaks = 0:4,
                       na.value = "white",
                       name = "Vulnerability Score\n(0=low, 4=high)") +
  theme_minimal() +
  labs(title = "Social Vulnerability across Indicators and Years",
       subtitle = "Puerto Rico",
       caption = "White fill denotes no data") +
  facet_grid(Indicator ~ Year, switch = "y") +
  theme(
    strip.text.y = element_text(size = 10, angle = 0),
    strip.text.x = element_text(size = 10),
    panel.spacing = unit(0.5, "lines"),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),  # White panel background
    plot.background = element_rect(fill = "white", color = NA)    # White plot background
  )

ggsave("indicator_plots/CSVI_plots/PR_vulnerability3.png", plot = p1, width = 12, height = 10)




# Prepare data for facet wrapping
facet_data_STSJ <- STSJ_map_dat_complete %>%
  filter(Year %in% c(2010, 2020)) %>%
  mutate(
    Year = as.factor(Year),  # Ensure Year is treated as a factor for faceting
    Indicator = factor(Indicator, levels = c(
      "Personal Disruption", "Pop Composition", "Poverty", "Labor Force", "Housing Charac", 
      "Retiree Migration" 
    ))
  )

# Plot with facets
p2 = ggplot(facet_data_STSJ) +
  geom_sf(aes(fill = Score), color = "black", size = 0.2) +
  scale_fill_gradientn(colors = c("blue", "yellow", "red"), 
                       limits = c(0, 4), 
                       breaks = 0:4,
                       na.value = "white",
                       name = "Vulnerability Score\n(0=low, 4=high)") +
  theme_minimal() +
  labs(title = "Social Vulnerability across Indicators and Years",
       subtitle = "St. Thomas and St. John",
       caption = "White fill denotes no data") +
  facet_grid(Indicator ~ Year, switch = "y") +
  theme(
    strip.text.y = element_text(size = 10, angle = 0),
    strip.text.x = element_text(size = 10),
    panel.spacing = unit(0.5, "lines"),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),  # White panel background
    plot.background = element_rect(fill = "white", color = NA)    # White plot background
  )

ggsave("indicator_plots/CSVI_plots/STSJ_vulnerability3.png", plot = p2, width = 12, height = 10)




# Prepare data for facet wrapping
facet_data_STX <- STX_map_dat_complete %>%
  filter(Year %in% c(2010, 2020)) %>%
  mutate(
    Year = as.factor(Year),  # Ensure Year is treated as a factor for faceting
    Indicator = factor(Indicator, levels = c(
      "Personal Disruption", "Pop Composition", "Poverty", "Labor Force", "Housing Charac", 
      "Retiree Migration" 
    ))
  )

# Plot with facets
p3 = ggplot(facet_data_STX) +
  geom_sf(aes(fill = Score), color = "black", size = 0.2) +
  scale_fill_gradientn(colors = c("blue", "yellow", "red"), 
                       limits = c(0, 4), 
                       breaks = 0:4,
                       na.value = "white",
                       name = "Vulnerability Score\n(0=low, 4=high)") +
  theme_minimal() +
  labs(title = "Social Vulnerability across Indicators and Years",
       subtitle = "St. Croix",
       caption = "White fill denotes no data") +
  facet_grid(Indicator ~ Year, switch = "y") +
  theme(
    strip.text.y = element_text(size = 10, angle = 0),
    strip.text.x = element_text(size = 10),
    panel.spacing = unit(0.5, "lines"),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),  # White panel background
    plot.background = element_rect(fill = "white", color = NA)    # White plot background
  )

ggsave("indicator_plots/CSVI_plots/STX_vulnerability3.png", plot = p3, width = 12, height = 10)


