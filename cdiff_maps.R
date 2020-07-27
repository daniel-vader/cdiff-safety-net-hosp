# Map cases, controls, and social deprivation index for the city of philadelphia
# Add census tract level identifier and census tract level social deprivation
  # index to primary cdiff dataset and output (saved as cdiff-sdi_geoid.rds)

# Author: Daniel Vader

################################################################################
### Place cases and controls on choropleth maps
################################################################################

### Load packages ###
library(tidyverse)

# mapping packages
library(tidycensus)
library(tigris)
library(sf)
options(tigris_class ="sf") # set shapefile class to work with sf package
library(ggsn) # lets us add a distance scale


### Merge data and geometry ###

# Get geometry
phl.tracts <- tracts(state="PA", county="Philadelphia", cb=T, year=2018)

# Get cdiff data
# These data were geocoded using DeGAUSS geocoder verstion 2.3
cdiff <- read.csv("cdiff_r3_geocoded.csv") %>% dplyr::filter(city == "Philadelphia")
cdiff.geo <- cdiff %>% st_as_sf(coords = c("lon", "lat"), agr="constant", crs = 4269)

# Set points and geometry to same coordinate system
proj <- st_crs(4269)
phl.tracts <- phl.tracts %>% st_transform(proj)
cdiff.geo <- cdiff.geo %>% st_transform(proj)

# Add geometry data to points
cdiff.geo2 <- st_join(cdiff.geo, phl.tracts, join=st_nearest_feature)
saveRDS(cdiff.geo2, "cdiff_geoid.rds") # save this so that we can add social deprivation later

# Summarize by count of controls in each tract
cdiff.con <- cdiff.geo2 %>% dplyr::filter(cdiff==2)

cdiff.con.tract <- cdiff.con %>%
  dplyr::group_by(GEOID) %>%
  dplyr::summarise(n_controls=dplyr::n()) %>%
  as.data.frame()

# Summarize by count of cases in each tract
cdiff.case <- cdiff.geo2 %>% dplyr::filter(cdiff==1)

cdiff.case.tract <- cdiff.case %>%
  dplyr::group_by(GEOID) %>%
  dplyr::summarise(n_cases=dplyr::n()) %>%
  as.data.frame()

# Add summary data to geometry
cdiff.tract <- geo_join(phl.tracts, cdiff.con.tract, by_sp = "GEOID", by_df = "GEOID")
cdiff.tract <- geo_join(cdiff.tract, cdiff.case.tract, by_sp = "GEOID", by_df = "GEOID")
cdiff.tract$n_controls <- ifelse(is.na(cdiff.tract$n_controls), 0, cdiff.tract$n_controls)
cdiff.tract$n_cases <- ifelse(is.na(cdiff.tract$n_cases), 0, cdiff.tract$n_cases)

# Add hospital location
hosp <- data.frame(place=" ", long=-75.163638, lat=39.956927)
hosp <- hosp %>% st_as_sf(coords = c("long", "lat"), crs=4269, agr="constant")
hosp_col <- c(" " = "black")


### Plot total N ###
cdiff.tract$n_total <- cdiff.tract$n_cases + cdiff.tract$n_controls

map.total <- ggplot() + 
  geom_sf(data=cdiff.tract, aes(fill=n_total), color=NA) +
  labs(fill="Count") +
  theme_minimal() +
  theme(axis.text = element_blank(),
        legend.position = "right") + 
  scale_fill_viridis_c(breaks=c(0,2,5,8,11), option="plasma") + 
  # Add hospital to map
  geom_sf(data=hosp, aes(color=place), shape=23, size=3, fill="cyan", stroke=2) +
  ggtitle("B) Number of subjects by census tract") +
  labs(color = "Hospital") +
  scale_color_manual(values=hosp_col) +
  xlab(NULL) + ylab(NULL)
map.total


### Plot cases ###
map.cases <- ggplot() + 
  geom_sf(data=cdiff.tract, aes(fill=n_cases), color=NA) +
  labs(fill="Count") +
  theme_minimal() +
  theme(axis.text = element_blank(),
        legend.position = "right") + 
  scale_fill_viridis_c(breaks=c(0,1,3,5,6), option="plasma") + 
  # Add hospital to map
  geom_sf(data=hosp, aes(color=place), shape=23, size=3, fill="cyan", stroke=2) +
  ggtitle("C) Number of cases by census tract") +
  labs(color = "Hospital") +
  scale_color_manual(values=hosp_col) +
  # Remove x and y labels
  xlab(NULL) + ylab(NULL)
map.cases


### Plot controls ###
map.controls <- ggplot() + 
  geom_sf(data=cdiff.tract, aes(fill=n_controls), color=NA) +
  labs(fill="Count") +
  theme_minimal() +
  theme(axis.text = element_blank(),
        legend.position = "right") + 
  scale_fill_viridis_c(na.value = "gray90", breaks=c(0,1,3,5,7,9), option="plasma") + 
  # Add hospital to map
  geom_sf(data=hosp, aes(color=place), shape=23, size=3, fill="cyan", stroke=2) +
  ggtitle("D) Number of controls by census tract") +
  labs(color = "Hospital") +
  scale_color_manual(values=hosp_col) +
  xlab(NULL) + ylab(NULL)
map.controls



################################################################################
### Social deprivation index and other demographics ###
################################################################################
# social deprivation packages
library(psych)
source("ndi.R") # load social depriviation index code by Ivan Castro: https://github.com/iecastro/deprivation-index

# Generate social deprevation index
phl.sdi <- ndi("PA", "Philadelphia")

# Join sdi with geometry
phl.geo.sdi <- geo_join(phl.tracts, phl.sdi, by_sp = "GEOID", by_df = "GEOID")

# Set projection
proj <- st_crs(4269)

# Apply projection
phl.geo.sdi <- phl.geo.sdi %>% st_transform(proj)

# Build map
map.sdi <- ggplot() +
  geom_sf(data=phl.geo.sdi, aes(fill=NDI), color = NA) +
  labs(fill="SDI") +
  theme_minimal() +
  theme(axis.text = element_blank(),
        legend.position = "right") +  
  scale_fill_viridis_c(option = "plasma") + 
  geom_sf(data=hosp, aes(color=place), shape=23, size=3, fill="cyan", stroke=2) +
  ggtitle("A) Deprivation index by census tract") +
  labs(color = "") +
  scale_color_manual(values=hosp_col) +
  labs(color = "Hospital") +
  xlab(NULL) + ylab(NULL)
map.sdi


### Throw all these maps onto a single grid.
library(cowplot)

tiff("map_grid.tiff", units="in", width=8, height=8, res=1200)
plot_grid(map.sdi, map.total, map.cases, map.controls)
dev.off()



################################################################################
### Tabulate population characteristics of all tracts containing at least one
# subject for comparison with national C diff data. 
################################################################################

# Set ACS variables we want to access.
demo_vars <- c("B01001_001", # Total pop
               "B01001_002", # Total male
               "B01001_026", # Total female
               "B02001_002", # Total White
               "B02001_003", # Total Black
               "B01001_003", # Male < 5
               "B01001_004", # Male 5-9
               "B01001_005", # Male 10-14
               "B01001_006", # Male 15-17
               "B01001_007", # Male 18-19
               "B01001_008", # Male 20
               "B01001_009", # Male 21
               "B01001_010", # Male 22-24
               "B01001_011", # Male 25-29
               "B01001_012", # Male 30-34
               "B01001_013", # Male 35-39
               "B01001_014", # Male 40-44
               "B01001_015", # Male 45-49
               "B01001_016", # Male 50-54
               "B01001_017", # Male 55-59
               "B01001_018", # Male 60-61
               "B01001_019", # Male 62-64
               "B01001_027", # Female < 5
               "B01001_028", # Female 5-9
               "B01001_029", # Female 10-14
               "B01001_030", # Female 15-17
               "B01001_031", # Female 18-19
               "B01001_032", # Female 20
               "B01001_033", # Female 21
               "B01001_034", # Female 22-24
               "B01001_035", # Female 25-29
               "B01001_036", # Female 30-34
               "B01001_037", # Female 35-39
               "B01001_038", # Female 40-44
               "B01001_039", # Female 45-49
               "B01001_040", # Female 50-54
               "B01001_041", # Female 55-59
               "B01001_042", # Female 60-61
               "B01001_043") # Female 62-64

# Pull ACS data
acs_data <- tidycensus::get_acs(geography = "tract", 
                                variables = demo_vars,
                                state = "PA",  
                                county = "Philadelphia",
                                output = "wide",
                                year = 2018)
acs_data <- acs_data %>% select(-ends_with("M"))

# Re-label sex and race variables
acs_data$tmale <- acs_data$B01001_002E
acs_data$tfemale <- acs_data$B01001_026E
acs_data$tblack <- acs_data$B02001_003E
acs_data$twhite <- acs_data$B02001_002E
acs_data$ttotal <- acs_data$B01001_001E

# Create age categories to match CDiff surveillance data (1-17, 18-44, 45-64, 64+)
acs_data$tage1_17 <- rowSums(acs_data[,8:11]) + rowSums(acs_data[,25:28])
acs_data$tage18_44 <- rowSums(acs_data[,12:19]) + rowSums(acs_data[,29:36])
acs_data$tage45_64 <- rowSums(acs_data[,20:24]) + rowSums(acs_data[,37:41])


# Subset to variables of interest
acs_data2 <- acs_data %>% select(ttotal, tmale, tfemale, tblack, twhite, tage1_17,
                                 tage18_44, tage45_64, GEOID)

# Join other demo stats with geometry
phl.geo.sdi.demo <- geo_join(phl.geo.sdi, acs_data2, by_sp = "GEOID", by_df = "GEOID") %>% as.data.frame()

# Attach hospital data, this join drops counties with no subjects
cdiff.sdi.geoid <- geo_join(cdiff.geo2, phl.geo.sdi.demo, by_sp = "GEOID", by_df = "GEOID")

# Tabulate results
t <- cdiff.sdi.geoid %>% as.data.frame()
demo.tract <- t %>%
  dplyr::group_by(GEOID) %>%
  dplyr::summarise(ntotal=mean(ttotal),
                   nmale=mean(tmale),
                   nfemale=mean(tfemale),
                   nblack=mean(tblack),
                   nwhite=mean(twhite),
                   nage1_17=mean(tage1_17),
                   nage18_44=mean(tage18_44),
                   nage45_64=mean(tage45_64)) %>%
  as.data.frame() %>% select(-GEOID)
