library(remotes)

library(tidyverse)
library(janitor)
library(lubridate)
library(plotly)
library(reshape2)
library(DT)
library(civis)


# read in Carbon data
vax <-  read_civis("db_name", database = "city_redshift_name") 

# read in Demographics data
demo <- read_csv("data/demographics.csv")

total_vax <- vax %>% filter(appt_status == "Discharged") %>% 
  filter(shot_number == "Second Shot"| brand == "Janssen") %>% 
  nrow()


# Weekly stuff
customweek <- function(dateweek){
  return(
    as.integer(as.integer(((ymd(dateweek)-ymd(min(vax$appointment_date)))))/7)
  )
}

vax %<>%
  mutate(week = customweek(appointment_date)) %>%
  arrange(desc(week)) 


###### Zip code analysis
la <- read_csv("data/la_city_zips.csv") %>%
  clean_names() %>% 
  select(zipcode) %>% 
  mutate(city = "City")  
  #rename(patient_zipcode = zipcode)


county <- read_csv("data/county_zips.csv") %>%
  clean_names()  %>%
  select(geoid10) %>%
  mutate(county = "County") %>%
  rename(zipcode = geoid10)





#####################################[NON REACTIVE MAPPING]############################################
##library(sf)
library(esri2sf)
library(leaflet)
library(tmap)

# Token is neccessary
token = ""
test_pos_url <- "https://services5.arcgis.com/7nsPwEMP38bSkCjy/arcgis/rest/services/Join_Test_Positivity_County_Zip_VIEW/FeatureServer/0"
case_rate <- "https://services5.arcgis.com/7nsPwEMP38bSkCjy/arcgis/rest/services/Two_Week_Case_Rates/FeatureServer/0"
vax_rate_url <- "https://services5.arcgis.com/7nsPwEMP38bSkCjy/arcgis/rest/services/la_city_vaccinations/FeatureServer/0"

# Download shps
test_pos <- esri2sf(test_pos_url,token=token)
case_rate <- esri2sf(case_rate,token=token)
vax_rate<- esri2sf(vax_rate_url,token=token)

zcta <- read_sf("data/Zip_Codes__LA_County/Zip_Codes__LA_County_.shp")
la_outline <- read_sf("data/city_outline/City_of_LA_land2.shp")
features <- read_csv("data/zcta_features.csv") %>% clean_names()
zcta <- merge(zcta, features, by.x="ZIPCODE", by.y="ZIPCODE", all.x=TRUE)

# Mapping
tmap_mode("view")

vax_popup = c("Neighborhood" = "Display_Name", "% of Population Vaccinated" = "vax_rate")

vax_rate_map <- tm_basemap(leaflet::providers$CartoDB.Positron) +
  tm_shape(vax_rate)+
  tm_view(view.legend.position=c("top","right"))+
  tm_polygons(col='vax_rate', palette = "GnBu",n = 4, alpha=.5, border.col="#174A85",border.alpha=.5, lwd=1,
              style = "pretty",
              textNA = "No Available Data",title="Vaccine Coverage",
              id='Display Name', popup.vars=vax_popup, group = "la city vaccinations")


test_popup = c("ZIPCODE"="GEOID10", "Test Positivity Rate" = "test_positivity_perc")
test_pos_map <- tm_basemap(leaflet::providers$CartoDB.Positron) +
  tm_shape(test_pos)+
  tm_view(view.legend.position=c("top","right"))+
  tm_polygons(col='test_positivity_perc', palette = "Oranges",alpha=.5, border.col="#174A85",border.alpha=.5, lwd=1,
              style = "pretty",
              textNA = "No Available Data",title="Test Positivity Percentage",
              id='GEOID10',popup.vars=test_popup,group="Test Positivity Percentage")
#test_pos_map

case_popup = c("Community Name"="Display_Name", "Adjusted Case Rate" = "adj_case_rate")
case_map <- tm_basemap(leaflet::providers$CartoDB.Positron) +
  tm_shape(case_rate)+
  tm_view(view.legend.position=c("top","right"))+
  tm_polygons(col='adj_case_rate', palette = "Purples",alpha=.5, border.col="#174A85",border.alpha=.5, lwd=1,
              style = "pretty",
              textNA = "No Available Data",title="Adjusted Case Rate",
              id='Display_Name',popup.vars=case_popup,group="Adjusted Case Rates")
#case_map

layerd_map <- vax_rate_map + case_map
city outline
utline <- tm_shape(la_outline) + 
 tm_view(view.legend.position=c("top","right"))+
 tm_polygons(col='CITY', palette = "Blues",alpha=0, border.col="#000000", lwd=2,
             style = "pretty",legend.show = FALSE, popup.vars = FALSE, title = "LA City Outline"
 ) 


#### Weekly tables

week_dates <- vax %>% group_by(week) %>%
  summarize(date = max(appointment_date))

weekly_race <- vax %>%
  filter(!is.na(race_ethnicity)) %>%
  filter(shot_number== "First Shot") %>%
  dcast(week ~ race_ethnicity, value.var="week") %>%
  mutate(across(c(AIAN, Asian, Black, Hispanic_Latino, White, Other, NHOPI), ~cumsum(.x))) %>%
  left_join(week_dates, by= "week") %>%
  arrange(date)


weekly_hpi <- vax %>%
  mutate(zip = as.integer(patient_zipcode)) %>%
  merge(features, by.x="patient_zipcode", by.y = "zipcode") %>%
  mutate(quartile = as.character(zip_hpi_quartile)) %>%
  mutate(quartile= str_replace_all(quartile, "1", "HPI_1")) %>%
  mutate(quartile= str_replace_all(quartile, "2", "HPI_2")) %>%
  mutate(quartile= str_replace_all(quartile, "3", "HPI_3")) %>%
  mutate(quartile= str_replace_all(quartile, "4", "HPI_4")) %>%
  filter(!is.na(zip)) %>%
  filter(shot_number== "First Shot") %>%
  dcast(week ~ quartile, value.var="week") %>%
  mutate(across(c(HPI_1, HPI_2, HPI_3, HPI_4), ~cumsum(.x))) %>%
  left_join(week_dates, by= "week") %>%
  arrange(date) 



weekly_brands <- vax %>%
  filter(!is.na(brand)) %>%
  filter(shot_number== "First Shot") %>%
  dcast(week ~ brand, value.var="week") %>%
  mutate(across(c(Janssen, Moderna, Pfizer), ~cumsum(.x))) %>%
  left_join(week_dates, by= "week") %>%
  arrange(date)





############### bivarmap
tracts <- read_sf("data/hpi_tracts/hpi_tracts.shp")#
gc_mrn <- read_csv("data/geocoded_mrn.csv") %>% clean_names()#
gc_vax <- merge(vax, gc_mrn, by="patient_mrn")#
tract_vaccine <-gc_vax %>%
  group_by(tractce10) %>%
  summarise(total = n())#

hpi_vax_tracts <- merge(tracts,tract_vaccine,by.x="TRACTCE10",by.y='tractce10',all.x=TRUE)#
hpi_vax_tracts <- hpi_vax_tracts %>% mutate(pop_vax = round((total/POP)*100,2),
                                            vax_percentage = round((total/total_vax)*100,2))#
summary(hpi_vax_tracts)#
source("BivariateTMap/bivariate_tmap.R")
hpi_vax_tracts_spdef <- as(hpi_vax_tracts, "Spatial")#
risk_vax_map <- bivariate_choropleth(hpi_vax_tracts_spdef, c("vax_percentage", "hpi2score", "TRACT"),save_png=TRUE)
