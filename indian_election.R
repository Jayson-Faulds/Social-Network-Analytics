library(data.table)
library(dplyr)
library(plm)
library(pglm)
library(panelAR)

setwd("~/Emory Official Stuff/Social Networks/Datasets")

# reads in the data
district_info <- fread('district_information.csv')
rain_info <- fread('rain_information.csv')
border_info <- fread('border_information.csv')
new_parties <- fread('new_parties_in_each_district_by_candidate.csv')


###################################################### Question 1 ################################################

### Part a

# Creates a vector for each election year
years <- c(1945, unique(district_info$year))
years <- years[-length(years)]


# Here, the cut function will make a new column. For rows with year below 1951, election_year will be 1951
# for rows with year between 1951 and 1956, election_year will be 1957. And so on
rain_info$election_year <- cut(rain_info$year, breaks=years, labels=c(1951, 1957, 1962, 1967, 1971, 1977, 1980, 1984,
                                                                        1989, 1991, 1996, 1998, 1999))

# removes the original year column and renames election_year for merging purposes
rain_info <- rain_info[, -2]
colnames(rain_info)[colnames(rain_info)=="election_year"] <- "year"

# removes NAs
rain_info <- as.data.frame(rain_info)[!is.na(rain_info$year), ]

# performs aggregation by district and election year to find sum of rain and average SPI for that election period
rain_2 <- as.data.table(rain_info)[, .(rain_sum = sum(rain), spi_avg = mean(spi)), by = .(district, year)]

# joins the district and rain data
rain_2$year <- as.numeric(as.character(rain_2$year))
merged <- inner_join(district_info, rain_2, by = c('district', 'year'))

plot(merged$rain_sum, merged$new_parties, main = 'Raw Rain Levels Against New Parties')
plot(merged$spi_avg, merged$new_parties, main = 'SPI levels Against New Parties')

### part b

# The number of unique districts in each column should be equal, but there is an imbalance
length(unique(border_info$focal_district))
length(unique(border_info$district))

# To fix this, we make a new dataframe that swaps the columns, and then combine with our original dataframe
border_2 <- border_info[,c(2,1)]
border_combined <- rbind(border_info, border_2, use.names=FALSE) # stack the dataframes
border_final <- unique(border_combined) # this will eliminate all of the duplicate instances to make our data balanced
border_final <- arrange(border_final, district) # now we have sort by district rather than focal_district
length(unique(border_final$focal_district))
length(unique(border_final$district)) # these are now equal

# Creates lag variable for rainfall from previous election period for that district
rain_3 <- rain_2
rain_3$lag_rain <- 0 # initiates the column values as zeros manually
rain_3$lag_spi <- 0
rain_3$lag_rain[-1] <- rain_3$rain_sum[-4833] # basically this makes the lag columns equal to the previous row's value
rain_3$lag_spi[-1] <- rain_3$spi_avg[-4833]

# Right now, we have rain_3 which is a list of each district, sorted alphabetically, and then sorted chronologically
# Right now, we are pulling the 1999 rain values from the previous district to fill the 1951 value for our next district
# this is a problem
# to do this, each instance the year is 1951, we turn it into an NA (We do not have values for this year anyway)
for (i in 1:nrow(rain_3)) {
  if (rain_3$year[i] == 1951) {
    rain_3$lag_rain[i] <- NA 
    rain_3$lag_spi[i] <- NA
  }
}

# This basically repeats our border_info dataframe 13 times, and then sorts alphabetically
# We are going to join this dataframe with rain_3, with this dataframe providing the information about neighbors
# We repeat this data 13 times, one for each election period, to ensure each district matches up with its neighbors
# correctly for each year
border_lags <- border_final[rep(1:nrow(border_final), 13), ]
border_lags <- arrange(border_lags, focal_district, district)

# this removes the years 1945 (Not an election year) and 1985 (No elections took place this year)
election_years <- years[-c(1, 15)]
border_lags$year <- election_years # this attaches an election year to each 13 repetitions of the district,
border_lags <- left_join(border_lags, rain_3, by=c("district","year")) # this will join the border and rain dataframes

# Here we group by the original district and year
# Each row contains a district, and the neighbor associated with that district
# So when we aggregate by district, we are calculating the mean for each of that district's neighbors
# For each calculation, we are finding the mean of lag_rain and lag_spi and imputing that into a new column
# These means are the averages of the lagged neighbors for each district
border_lags <- border_lags %>%
  group_by(focal_district, year) %>%
  mutate(neighbor_rain_lag = mean(lag_rain, na.rm=TRUE),
         neighbor_spi_lag = mean(lag_spi, na.rm=TRUE)
  )

# We still have a ton of duplicate rows from the rep() statement we applied earlier
# We use distinct_at() to keep the instances of distinct pairs and remove everything else
# In this case, order matters (Adilibad-Chandrapur and Chandrapur-Adilibad are two different pairs)
border_lags_final <- distinct_at(border_lags, .vars=c("focal_district","year"), .keep_all = TRUE)
border_lags_final <- border_lags_final[,c("focal_district","year","neighbor_rain_lag","neighbor_spi_lag")] # removes some columns

# Here we rename the focal_district column to district, and merge with the rain dataframe that contains all of our lags now
colnames(border_lags_final)[colnames(border_lags_final) == "focal_district"] <- "district"
rain_3 <- left_join(rain_3, border_lags_final, by=c("district","year"))

# runs the model for rain totals based on lagged rain
model_rain <- plm(rain_sum ~ lag_rain + neighbor_rain_lag, data = rain_3, effect = "twoways", model = "within", index = "district")
summary(model_rain)

# runs the model for SPI based on lagged spi
model_spi <- plm(spi_avg ~ lag_spi + neighbor_spi_lag, data = rain_3, effect = "twoways", model = "within", index = "district")
summary(model_spi)

### part c

# rereads the original rain data and creates a dummy if the rain spi is above or below 1 and -1 respectively
rain_new <- fread('rain_information.csv')
rain_new$FloodDrought <- ifelse(rain_new$spi > 1 | rain_new$spi < -1, 1, 0)

# The next few lines of code repeat the process for part a, but now calculates the dummy for the drought/floodings
years <- c(1945, unique(district_info$year))
years <- years[-15] # removes 1985 as an election year

# Assigns each year to its upcoming election year
rain_new$election_year <- cut(rain_new$year, breaks=years, labels=c(1951, 1957, 1962, 1967, 1971, 1977, 1980, 1984,
                                                                      1989, 1991, 1996, 1998, 1999))

# throws away the original year column, renames the new column for merging purposes
rain_new <- rain_new[, -2]
colnames(rain_new)[colnames(rain_new)=="election_year"] <- "year"

# remove NAs, generates the drought/flood count by summing the dummy variable for each election year
rain_new <- as.data.frame(rain_new)[!is.na(rain_new$year), ]
rain_2new <- as.data.table(rain_new)[, .(extreme_conditions = sum(FloodDrought)), by = .(district, year)]

# the following code reproduces what we did in part b, to calculate the lagged drought/flood values for a district
rain_2new$lag_droughts <- 0 
rain_2new$lag_droughts[-1] <- rain_2new$extreme_conditions[-4501]
for (i in 1:nrow(rain_2new)) {
  if (rain_2new$year[i] == 1951) {
    rain_2new$lag_droughts[i] <- NA 
  }
}

# the following group of code recreates what we did in part b, to calculate lagged drought/flood values for neighbors
rain_2new$year <- as.numeric(as.character(rain_2new$year))
border_lags_new <- left_join(border_lags, rain_2new, by=c("district","year")) # this will join the border and rain dataframes
border_lags_new <- border_lags_new %>%
  group_by(focal_district, year) %>%
  mutate(neighbor_droughts_lag = mean(lag_droughts, na.rm=TRUE)
  )

border_lags_final_new <- distinct_at(border_lags_new, .vars=c("focal_district","year"), .keep_all = TRUE)
border_lags_final_new <- border_lags_final_new[,c("focal_district","year",
                                                  "extreme_conditions","lag_droughts", 'neighbor_droughts_lag')] 

# Here we rename the focal_district column to district, and merge with the rain dataframe that contains all of our lags now
colnames(border_lags_final_new)[colnames(border_lags_final_new) == "focal_district"] <- "district"

# finally this merges the datasets together, allowing us to use the newly created variable
rain_3 <- left_join(rain_3, border_lags_final_new, by=c("district","year"))

model_FloodDroughts <- pglm(extreme_conditions ~ lag_droughts + neighbor_droughts_lag, data = rain_3,
                            effect = 'twoways', model = 'within', index = 'district', family = 'poisson')
summary(model_FloodDroughts)

################################################## Question 2 ####################################################

# performs a join to get the new party data into the rain_3 dataframe
# we are also keeping column 23 (Concentration) to be used later on in question 4
rain_3 <- left_join(rain_3, district_info[, c(2:20, 23)], by=c("district","year"))

# This for loop will calculate the number of years elapsed during each election period
rain_3$time_between <- 0
for(i in 1:nrow(rain_3)){
  if(rain_3$year[i] == 1951){
    rain_3$time_between[i] <- 5 # we hard code the first election period to have 5 years
  }else{
    rain_3$time_between[i] <- rain_3$year[i] - rain_3$year[i-1] # every other duration is the difference between election periods in years
  }
}

# builds the regression for new parties on the drought/flooding conditions, with controls for election year and duration
model_new_parties <- panelAR(new_parties ~ extreme_conditions + time_between, data = rain_3, panelVar = 'district',
                             timeVar = 'year', autoCorr = 'psar1', panelCorrMethod = 'phet', rho.na.rm = TRUE)
summary(model_new_parties)

# this apply function will build the regression for each party type and store the output
partyCols <- colnames(rain_3)[14: length(colnames(rain_3))-1]
my_lms <- lapply(14:length(colnames(rain_3))-1, function(x) panelAR(rain_3[, x] ~ extreme_conditions + time_between, 
                                                                    data = rain_3, panelVar = 'district',timeVar = 'year', 
                                                                    autoCorr = 'psar1', panelCorrMethod = 'phet', 
                                                                    rho.na.rm = TRUE)$coefficients[2])

# Above, we only kept the coefficient for the flooding/drought indicator
# Now, we convert the list to a matrix, transpose it, and edit the rownames to our party type
# We can look at these values to compare how party types are affected by the drought/flooding conditions
my_lms_df <- t(as.matrix(as.data.frame(my_lms)))
rownames(my_lms_df) <- partyCols

####################################################### Question 3 ####################################################

# builds the regresssion for new parties on the drought/flood conditions, the lagged conditions for neighbors,
# and including the number of years between election periods as a control variable
model_parties_neighbors <- panelAR(new_parties ~ extreme_conditions + time_between + neighbor_droughts_lag,
                                   data = rain_3, panelVar = 'district', timeVar = 'year', autoCorr = 'psar1',
                                   panelCorrMethod = 'phet', rho.na.rm = TRUE)
summary(model_parties_neighbors)

###################################################### Question 4 #####################################################

### part a

# builds the regression model for new national scope parties based on drought/flood conditions, 
# lagged conditions, and control for time between election periods
model_nationalParties <- panelAR(new_parties_national_scope ~ extreme_conditions + time_between + neighbor_droughts_lag,
                                   data = rain_3, panelVar = 'district', timeVar = 'year', autoCorr = 'psar1',
                                   panelCorrMethod = 'phet', rho.na.rm = TRUE)
summary(model_nationalParties)

# builds the regression model for new state scope parties based on drought/flood conditions, 
# lagged conditions, and control for time between election periods
model_stateParties <- panelAR(new_parties_state_scope ~ extreme_conditions + time_between + neighbor_droughts_lag,
                                 data = rain_3, panelVar = 'district', timeVar = 'year', autoCorr = 'psar1',
                                 panelCorrMethod = 'phet', rho.na.rm = TRUE)
summary(model_stateParties)

# builds the regression model for new regional scope parties based on drought/flood conditions, 
# lagged conditions, and control for time between election periods
model_regionalParties <- panelAR(new_parties_regional_scope ~ extreme_conditions + time_between + neighbor_droughts_lag,
                                 data = rain_3, panelVar = 'district', timeVar = 'year', autoCorr = 'psar1',
                                 panelCorrMethod = 'phet', rho.na.rm = TRUE)
summary(model_regionalParties)

### part b

# builds the regression model for Herfindhal index based on drought/flood conditions, lagged conditions,
# and control for time between election periods
model_concentration <- panelAR(political_concentration ~ extreme_conditions + time_between + neighbor_droughts_lag,
                                 data = rain_3, panelVar = 'district', timeVar = 'year', autoCorr = 'psar1',
                                 panelCorrMethod = 'phet', rho.na.rm = TRUE)
summary(model_concentration)

#################################################### Question 5 #################################################

# removes the columns we do not need
new_parties <- new_parties[, c(1, 3, 5)]

# groups by district and year, stores a list of all new parties for that year and district in a new column
new_parties <- as.data.table(new_parties)[, .(party_vector = list(party_name)), by = .(district, year)]

# the next few lines of code will repeat what we did in question 1 part b
# the goal is to get a dataframe that is indexed by focal district and year
# the values of the dataframe will include neighboring districts, along with new parties that started during that election period
border_lags2 <- border_final[rep(1:nrow(border_final), 13), ]
border_lags2 <- arrange(border_lags2, focal_district, district)
election_years <- years[-c(1, 15)]
border_lags2$year <- election_years 
border_lags2 <- left_join(border_lags2, new_parties, by=c("district","year")) # this performs the actual merge

# This is a nested for loop that iterates over the focal district and then over year
# Starting with the first district, we find from the df the list of new parties for the first year
# We then check to see if any of these new parties can be found in the neighbors' new party list from the previous election
# If there are any matches, we get a TRUE for that element. We then sum the element and append it to one of our empty vectors
# We repeat this process for each election year for that same focal district
# We repeat this entire process for each focal district
NewPartiesFromBorder <- c()
FocalDistrict <- c()
Year <- c()
for (i in unique(border_lags2$focal_district)){
  for (j in unique(border_lags2$year)){
    x <- border_lags2[border_lags2$district == i & border_lags2$year == j, 4][[1]] %in% border_lags2[border_lags2$focal_district == i & border_lags2$year < j, 4]
    NewPartiesFromBorder <- append(NewPartiesFromBorder, sum(x))
    FocalDistrict <- append(FocalDistrict, i)
    Year <- append(Year, j)
  }
}

# We take the objects from the nested for loop and construct a dataframe
party_df <- data.frame(FocalDistrict, Year, NewPartiesFromBorder)

# Merges with our grand dataframe
names(party_df) <- c('district', 'year', 'Border_parties')
rain_3 <- left_join(rain_3, party_df, by = c('district', 'year'))

# builds the model for new parties from neighboring districts
model_border_parties <- panelAR(Border_parties ~ extreme_conditions + time_between + neighbor_droughts_lag,
                               data = rain_3, panelVar = 'district', timeVar = 'year', autoCorr = 'psar1',
                               panelCorrMethod = 'phet', rho.na.rm = TRUE)
summary(model_border_parties)

# makes a new variable for number of parties NOT from neighboring districts
rain_3$non_border_parties <- rain_3$new_parties - rain_3$Border_parties

# builds the model for new parties not from neighboring districts
model_not_border_parties <- panelAR(non_border_parties ~ extreme_conditions + time_between + neighbor_droughts_lag,
                                data = rain_3, panelVar = 'district', timeVar = 'year', autoCorr = 'psar1',
                                panelCorrMethod = 'phet', rho.na.rm = TRUE)
summary(model_not_border_parties)






