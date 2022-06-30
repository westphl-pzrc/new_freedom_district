#title: new freedom district demographic calculation
#name: nissim lebovits
#last edit: 6/6/2022

#summary: the goal of this script is to calculate certain demographics variables requested by
#mr. muhammed, who runs the new africa center and is a key promise zone partner.
#i have approximated his geography by a bounding box, and then either aggregated point data
#to the bounding box or used spatial interpolation (not population weights) to estimate
#demographic figures.


#steps
#1: create bounding box to approximate nfd
#2: import shootings + gun crimes; aggregate to nfd
#3: import desired acs variables; group into spatially intensive vs. extensive; interpolate
#4: combine into final dataframe
#5: repeat for whole city to provide point of comparison
#6: write .csv files

library(tigris, quietly = T)
library(tidyverse, quietly = T)
library(sf, quietly = T)
library(mapview, quietly = T)
library(acs, quietly = T)
library(tidycensus, quietly = T)
library(gtools, quietly = T) #for smartbind

############BOUNDING BOX TO APPROXIMATE NEW FREEDOM DIST#################
lon = c(-75.217728, -75.201881)
lat = c(39.962477, 39.974157)

nfd_df = data.frame(lon, lat)

nfd = nfd_df %>% 
  st_as_sf(coords = c("lon", "lat"), 
           crs = st_crs("EPSG:4326")) %>% 
  st_bbox() %>% 
  st_as_sfc()

#mapview(nfd)

vars20 = load_variables(2020, 
                        "acs5")


######################IMPORT GUN CRIMES, USE BOUNDING BOX TO DEFINE AS IN OR OUT##########################
shootings_import = read.csv("C:/Users/Nissim.Lebovits/OneDrive - City of Philadelphia/Desktop/Data/R Scripts and Datasets/Public Safety/Crime/Shootings/PhilaShootings2016to2021_Jan_2022.csv")

shootings = shootings_import |>
  filter(year == 2021)|>
  drop_na(lat) |>
  drop_na(lng) |>
  filter(lat > 38 &
           lng > -78) |>
  mutate(text_general_code = "Shooting") |>
  rename(date = date_)|>
  select(text_general_code,
         lat,
         lng,
         year,
         date)

shootings = st_as_sf(shootings, 
                     coords = c("lng", "lat"), crs = "EPSG:4326") |>
  distinct()


crimes_import_twentyone = read.csv("C:/Users/Nissim.Lebovits/OneDrive - City of Philadelphia/Desktop/Data/R Scripts and Datasets/Public Safety/Crime/PhilaCrime2021.csv")

gun_crimes_twentyone = crimes_import_twentyone |>
  filter(text_general_code == "Robbery Firearm" |
           text_general_code == "Aggravated Assault Firearm")|>
  mutate(year = 2021)|>
  select(text_general_code,
         lat,
         lng,
         year,
         dispatch_date) |>
  rename(date = dispatch_date)

all_phl_gun_crimes = smartbind(shootings, gun_crimes_twentyone)|>
  filter(!(duplicated(date) &
             duplicated(lat) &
             duplicated(lng))) |>
  drop_na(lat) |>
  drop_na(lng) |>
  filter(lat > 38 &
           lng < 78)

gun_crimes_spatial = st_as_sf(all_phl_gun_crimes, 
                              coords = c("lng", "lat"), crs = "EPSG:4326") |>
  distinct()

#identify those that are or are not in NFD
#see Lovelace 4.2.1 for explanation of subsetting: https://geocompr.robinlovelace.net/spatial-operations.html#spatial-subsetting
gun_crimes_spatial$in_nfd = st_intersects(gun_crimes_spatial, nfd, sparse = FALSE)[, 1]

gun_crimes_for_tally = gun_crimes_spatial |>
                          as.data.frame() |>
                          dplyr::select(-geometry)
######################PULL DEMOS####################################

phl_demos <- get_acs(geography = "tract", # What is the lowest level of geography are we interested in?
                             year = 2020, # What year do we want - this can also be used for 2000 census data
                             variables = c(
                               #Population
                               "B01003_001E", #Total population
                               "B11001_001E", #Total number of households
                               "B09001_001E", #Total population under 18
                               
                               #Race
                               "B02001_002E", #Total white population
                               "B02001_003E", #Total Black population
                               "B02001_004E", #American Indian and Alaska Native alone
                               "B02001_005E", #Total Asian population
                               "B02001_006E", #Native Hawaiian and Other Pacific Islander alone
                               "B02001_007E", #Some other race alone
                               "B02001_008E", #Two or more races
                               
                               #Ethnicitiy
                               "B01001I_001E", #Total: Hispanic or Latino (distinct from race)
                               
                               #Housing
                               "B25064_001E", #Median gross rent
                               "B25070_007E", #Rent 30.0 to 34.9 percent
                               "B25070_008E", #Rent 35.0 to 39.9 percent
                               "B25070_009E", #Rent 40.0 to 49.9 percent
                               "B25070_010E", #Rent 50.0 percent or more
                               "B25003_002E", #Owner occupied
                               "B25003_003E", #Renter occupied
                               "B25001_001E", #Number of residential units
                               "B25002_002E", #Occupied
                               "B25002_003E", #Vacant
                               "B25077_001E", #Median house value (dollars)
                               
                               #Income & Work
                               "B19013_001E", #Median household income
                               "B17001_002E", #Income in the past 12 months below poverty level:
                               "B23025_003E", #In labor force:!!Civilian labor force:
                               "B23025_004E", #In labor force:!!Civilian labor force:!!Employed
                               "B23025_005E", #In labor force:!!Civilian labor force:!!Unemployed
                               
                               #Education
                               "B15003_001E", #Total Pop 25+
                               "B15003_017E", #Regular high school diploma
                               "B15003_018E", #GED or alternative credential
                               "B15003_019E", #Some college, less than 1 year
                               "B15003_020E", #Some college, 1 or more years, no degree
                               "B15003_021E", #Associate's degree
                               "B15003_022E", #Bachelor's degree
                               "B15003_023E", #Master's degree
                               "B15003_024E", #Professional school degree
                               "B15003_025E"),  #Doctorate degree 
                             geometry = T, # Do we want this as a shapefile? No, not now.
                             state = "PA", # What state?
                             county = "Philadelphia", # What County?
                             output = "wide") |>
                      rename(#Population
                        tot_pop = B01003_001E,
                        tot_hh = B11001_001E,
                        tot_under_eighteen = B09001_001E,
                        
                        #Race
                        tot_wht_pop = B02001_002E,
                        tot_blk_pop = B02001_003E,
                        tot_native_pop = B02001_004E,
                        tot_other_race = B02001_007E,
                        tot_two_plus_race = B02001_008E,
                        
                        #Ethnicitiy
                        tot_hisp_pop = B01001I_001E,
                        
                        #Housing
                        med_gross_rent = B25064_001E, #Median gross rent
                        owner_occ = B25003_002E, #Owner occupied
                        renter_occ = B25003_003E, #Renter occupied
                        num_resid_units = B25001_001E, #Number of residential units
                        occ_units = B25002_002E, #Occupied
                        vac_units = B25002_003E, #Vacant
                        med_house_value = B25077_001E, #Median house value (dollars)
                        
                        #Income & Work
                        med_hh_inc = B19013_001E, #Median household income
                        inc_below_pov = B17001_002E, #Income in the past 12 months below poverty level:
                        tot_lab_force = B23025_003E, #In labor force:!!Civilian labor force:
                        tot_unempl = B23025_005E, #In labor force:!!Civilian labor force:!!Unemployed
                        
                        #Education
                        tot_pop_25plus = B15003_001E) |> #Total Pop 25+
              mutate(non_wht_pop = tot_pop - tot_wht_pop,
                     tot_aapi_pop = (B02001_005E + #Tot Asian pop
                                       B02001_006E), #Tot Hawaiian and Pacific Islander pop
                     tot_rent_burden = (B25070_007E + #Rent 30.0 to 34.9 percent
                                          B25070_008E + #Rent 35.0 to 39.9 percent
                                          B25070_009E + #Rent 40.0 to 49.9 percent
                                          B25070_010E), #Rent 50.0 percent or more
                     tot_hs_dip_or_alt = (B15003_017E+ #Regular high school diploma
                                         B15003_018E), #GED or alternative credential
                     tot_some_college = (B15003_019E + #Some college, less than one year
                                           B15003_020E), #Some college, 1 or more years, no degree
                     tot_bach_plus = (B15003_021E +  #Associate's degree
                                        B15003_022E + #Bachelor's degree
                                        B15003_023E + #Master's degree
                                        B15003_024E + #Professional school degree
                                        B15003_025E))  #Doctorate degree 

phl_demos = st_transform(phl_demos, crs = st_crs("EPSG:4326"))

#########################SEPARATE VARIABLES BY TYPE#######################

#Extensive variables
phl_demos_ext = phl_demos |>
                  dplyr::select(tot_pop,
                                tot_hh,
                                tot_under_eighteen,
                                tot_wht_pop,
                                tot_blk_pop,
                                tot_native_pop,
                                tot_other_race,
                                tot_two_plus_race,
                                tot_hisp_pop,
                                owner_occ,
                                renter_occ,
                                num_resid_units,
                                occ_units,
                                vac_units,
                                inc_below_pov,
                                tot_lab_force,
                                tot_unempl,
                                tot_pop_25plus, 
                                non_wht_pop,
                                tot_aapi_pop,
                                tot_rent_burden,
                                tot_hs_dip_or_alt,
                                tot_some_college,
                                tot_bach_plus)

#Intensive variables
phl_demos_int = phl_demos |>
  dplyr::select(med_gross_rent,
                med_house_value,
                med_hh_inc)

######################RUN SPATIAL INTERPOLATION#########################

nfd_demos_ext = st_interpolate_aw(phl_demos_ext[, 1:24], nfd, ext = TRUE)

nfd_demos_int = st_interpolate_aw(na.omit(phl_demos_int[, 1:3]), nfd, ext = FALSE)
  
nfd_demos_tot = st_join(nfd_demos_ext, nfd_demos_int)

nfd_demos_tot = nfd_demos_tot |>
                  mutate(pct_und_18 = (tot_under_eighteen / tot_pop),
                         pct_non_wht_pop = (non_wht_pop / tot_pop),
                         pct_blk = (tot_blk_pop / tot_pop),
                         pct_aapi = (tot_aapi_pop / tot_pop),
                         pct_hisp = (tot_hisp_pop / tot_pop),
                         pct_own_occ = (owner_occ / num_resid_units),
                         pct_rent_occ = (renter_occ / num_resid_units),
                         resid_vac_rate = (vac_units / num_resid_units),
                         pov_rt = (inc_below_pov / tot_pop),
                         unempl_rt = (tot_unempl / tot_lab_force),
                         pct_hs_or_equiv = (tot_hs_dip_or_alt / tot_pop_25plus),
                         pct_some_college = (tot_some_college / tot_pop_25plus),
                         pct_bach_plus = (tot_bach_plus / tot_pop_25plus)
                  )

nfd_demos_tot$tot_2021_gun_crimes = tally(gun_crimes_for_tally[gun_crimes_spatial$in_nfd == "TRUE", ])
#Failing to convert to non sf and remove geometry column makes it write weirdly as a .csv
nfd_demos_tot = as.data.frame(nfd_demos_tot) |>
                  dplyr::select(-geometry) |>
                  mutate(gun_crimes_per_capita = tot_2021_gun_crimes/tot_pop)


################REPEAT FOR PHL

phl_county_demos <- get_acs(geography = "county", # What is the lowest level of geography are we interested in?
                     year = 2020, # What year do we want - this can also be used for 2000 census data
                     variables = c(
                       #Population
                       "B01003_001E", #Total population
                       "B11001_001E", #Total number of households
                       "B09001_001E", #Total population under 18
                       
                       #Race
                       "B02001_002E", #Total white population
                       "B02001_003E", #Total Black population
                       "B02001_004E", #American Indian and Alaska Native alone
                       "B02001_005E", #Total Asian population
                       "B02001_006E", #Native Hawaiian and Other Pacific Islander alone
                       "B02001_007E", #Some other race alone
                       "B02001_008E", #Two or more races
                       
                       #Ethnicitiy
                       "B01001I_001E", #Total: Hispanic or Latino (distinct from race)
                       
                       #Housing
                       "B25064_001E", #Median gross rent
                       "B25070_007E", #Rent 30.0 to 34.9 percent
                       "B25070_008E", #Rent 35.0 to 39.9 percent
                       "B25070_009E", #Rent 40.0 to 49.9 percent
                       "B25070_010E", #Rent 50.0 percent or more
                       "B25003_002E", #Owner occupied
                       "B25003_003E", #Renter occupied
                       "B25001_001E", #Number of residential units
                       "B25002_002E", #Occupied
                       "B25002_003E", #Vacant
                       "B25077_001E", #Median house value (dollars)
                       
                       #Income & Work
                       "B19013_001E", #Median household income
                       "B17001_002E", #Income in the past 12 months below poverty level:
                       "B23025_003E", #In labor force:!!Civilian labor force:
                       "B23025_004E", #In labor force:!!Civilian labor force:!!Employed
                       "B23025_005E", #In labor force:!!Civilian labor force:!!Unemployed
                       
                       #Education
                       "B15003_001E", #Total Pop 25+
                       "B15003_017E", #Regular high school diploma
                       "B15003_018E", #GED or alternative credential
                       "B15003_019E", #Some college, less than 1 year
                       "B15003_020E", #Some college, 1 or more years, no degree
                       "B15003_021E", #Associate's degree
                       "B15003_022E", #Bachelor's degree
                       "B15003_023E", #Master's degree
                       "B15003_024E", #Professional school degree
                       "B15003_025E"),  #Doctorate degree 
                     geometry = F, 
                     state = "PA", # What state?
                     county = "Philadelphia", # What County?
                     output = "wide") |>
  rename(#Population
    tot_pop = B01003_001E,
    tot_hh = B11001_001E,
    tot_under_eighteen = B09001_001E,
    
    #Race
    tot_wht_pop = B02001_002E,
    tot_blk_pop = B02001_003E,
    tot_native_pop = B02001_004E,
    tot_other_race = B02001_007E,
    tot_two_plus_race = B02001_008E,
    
    #Ethnicitiy
    tot_hisp_pop = B01001I_001E,
    
    #Housing
    med_gross_rent = B25064_001E, #Median gross rent
    owner_occ = B25003_002E, #Owner occupied
    renter_occ = B25003_003E, #Renter occupied
    num_resid_units = B25001_001E, #Number of residential units
    occ_units = B25002_002E, #Occupied
    vac_units = B25002_003E, #Vacant
    med_house_value = B25077_001E, #Median house value (dollars)
    
    #Income & Work
    med_hh_inc = B19013_001E, #Median household income
    inc_below_pov = B17001_002E, #Income in the past 12 months below poverty level:
    tot_lab_force = B23025_003E, #In labor force:!!Civilian labor force:
    tot_unempl = B23025_005E, #In labor force:!!Civilian labor force:!!Unemployed
    
    #Education
    tot_pop_25plus = B15003_001E) |> #Total Pop 25+
  mutate(non_wht_pop = tot_pop - tot_wht_pop,
         tot_aapi_pop = (B02001_005E + #Tot Asian pop
                           B02001_006E), #Tot Hawaiian and Pacific Islander pop
         tot_rent_burden = (B25070_007E + #Rent 30.0 to 34.9 percent
                              B25070_008E + #Rent 35.0 to 39.9 percent
                              B25070_009E + #Rent 40.0 to 49.9 percent
                              B25070_010E), #Rent 50.0 percent or more
         tot_hs_dip_or_alt = (B15003_017E+ #Regular high school diploma
                                B15003_018E), #GED or alternative credential
         tot_some_college = (B15003_019E + #Some college, less than one year
                               B15003_020E), #Some college, 1 or more years, no degree
         tot_bach_plus = (B15003_021E +  #Associate's degree
                            B15003_022E + #Bachelor's degree
                            B15003_023E + #Master's degree
                            B15003_024E + #Professional school degree
                            B15003_025E))  #Doctorate degree 


phl_county_demos$citywide_annual_gun_crimes_2021 = tally(gun_crimes_for_tally)

phl_county_demos = phl_county_demos |>
                    mutate(gun_crimes_per_capita = citywide_annual_gun_crimes_2021/tot_pop)
#######WRITE CSV FILES

#nfd
write.csv(nfd_demos_tot, file = "NewFreedomDistrictDemographics_Apr27_2022.csv")

#phl county
write.csv(phl_county_demos, file = "PHLCountyDemographics_Apr27_2022.csv")





