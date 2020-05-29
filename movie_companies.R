library(data.table)
library(dplyr)
library(qdapTools)
library(ggplot2)
library(proxy)
library(MASS)
library(igraph)
library(zoo)

setwd("~/Emory Official Stuff/Social Networks/Datasets")

# read in the data
producers_and_films <- fread('producers_and_films.csv')
box_office_revenues <- fread('box_office_revenues.csv')
film_keywords <- fread('film_keywords.csv')
production_subsidiaries <- fread('production_subsidiaries.csv')
film_cast_members <- fread('film_cast_members.csv')

# keeps the producers from the US, removes duplicate entries
producers_and_films <- producers_and_films[producers_and_films$country == 'us', ]
producers_and_films <- unique(producers_and_films)

################################################# Question 1 ##################################################

### Part A

# we need the year to determine whether a movie's keywords are new. So we merge keyword data with producer and films data
keywords_films <- left_join(film_keywords, producers_and_films)
keywords_films <- keywords_films[!is.na(keywords_films$year),] # removes the instances where matches failed and produced NAs
keywords_films <- keywords_films[,1:3] # we only need these first three columns at the moment
keywords_films <- unique(keywords_films) # removes duplicates

# this will sort the dataframe by keyword and then by year (ascending)
keywords_films <- keywords_films %>%
  group_by(keyword) %>%
  arrange(keyword, year)

# We want to create a dummy for a word being new or not (1==yes, 0==no)
# We identify if the keyword is appearing for the first time (Gets a 1 for the dummy)
# we also check if the keyword is appearing within 3 years since the first time (Also gets a 1 for the dummy)
keywords_films <- keywords_films %>%
  group_by(keyword) %>%
  mutate(new_word = ifelse(year %in% c(min(year), min(year)+1, min(year)+2, min(year)+3),1,0))

# Now we group by movie, and count the total number of new words for each movie
keywords_films <- keywords_films %>%
  group_by(pindex) %>%
  mutate(new_word_sum = sum(new_word))

# We merge the new word count variable back into our producer and films data
producers_and_films <- left_join(producers_and_films, keywords_films[, c(1, 5)])
producers_and_films <- unique(producers_and_films)
producers_films_2 <- producers_and_films[!is.na(producers_and_films$new_word_sum),] # Eliminates movies that don't have key words

# We group by movie, and combine each key word used by the movie into a list
keywords_films_2 <- keywords_films %>% 
  group_by(pindex) %>% 
  mutate(words = paste(list(keyword), sep=","))

# Here we repeat the same process as above, where we construct a dummy that equals 1 if the combination of key words has been used before
# We also check to see if the same key word combo was used within 3 years of it first being used, which would also get a 1
keywords_films_2 <- keywords_films_2 %>%
  group_by(words) %>%
  mutate(new_word_combo = ifelse(year %in% c(min(year), min(year)+1, min(year)+2, min(year)+3),1,0))

# Here we group by movie, and then find the average number of new word combinations for each movie
keywords_films_2 <- keywords_films_2 %>% 
  group_by(pindex) %>% 
  summarise(new_combo=mean(new_word_combo))

# merges this newly-created dataframe with our producer and film data
producers_films_2 <- left_join(producers_films_2,keywords_films_2)

# Keeps the movie index, year, and production company index
producers_films_3 <- producers_and_films[,c(1,2,4)]

# Here we group by year and film company to count the number of movies each company makes per year
producers_films_3 <- producers_films_3 %>%
  group_by(year, pcindex) %>%
  mutate(films_count = n())

# removes the movie index column and removes duplicates
producers_films_4 <- producers_films_3[,c(2,3,4)]
producers_films_4 <- unique(producers_films_4)

# Here, we group by year. For each year, we calculate the upper quartile value
producers_films_4 <- producers_films_4 %>%
  group_by(year) %>%
  mutate(quartile = quantile(films_count, prob=0.75))

# If a production company made more films than the upper quartile value, they are coded as generalists
producers_films_4$generalist_count <- ifelse(producers_films_4$films_count > producers_films_4$quartile,1,0)

# merges our generalist dummy into our producer and film data
producers_films_2 <- left_join(producers_films_2, producers_films_4, by=c("year","pcindex"))

# Here we group by movie. If more than one company worked on a movie, the movie is classified as Co-production
producers_films_2 <- producers_films_2 %>%
  group_by(pindex) %>%
  mutate(co_prod_vs_solo = ifelse(n()>1,"Co-production","Solo"))

# Here we group by movie again. We create five_types, which is the classification variable. If a movie is classified as
# solo, and it has a generalist, it is Central Solo. If it doesn't have a generalist, it is Peripheral Solo.
# If it is classified as Co-production, and contains a mix of generalists and specialists, it is Hybrid.
# If it is Co-production and only contains generalists, then it is Central Co-prod. Otherwise it is Peripheral Co-prod
producers_films_2 <- producers_films_2 %>%
  group_by(pindex) %>%
  mutate(five_types = ifelse(co_prod_vs_solo=="Solo",
                             ifelse(generalist_count==1,"Central Solo","Peripheral Solo"),
                             ifelse(length(unique(generalist_count))>1,"Hybrid Co-prod",
                                    ifelse(unique(generalist_count)==1,"Central Co-prod","Peripheral Co-prod"))))

# Takes the sum of new key words and new key word combinations for each movie
producers_films_2$total_keywords_or_combos <- producers_films_2$new_word_sum + producers_films_2$new_combo

# gets rid of duplicates for each movie; we don't care about the actual movie at this point, only the "innovation" values
# that we have built up to this point. We only want one singular entry for each movie in order to not overcount while aggregating
film_levels <- distinct_at(producers_films_2,.vars="pindex",.keep_all = TRUE)

# Here we group by classification type and year. Then we sum up the "innovation" category for each grouping.
# This will give us a value for each combination of classification type and year
film_category_level <- film_levels %>%
  group_by(five_types, year) %>%
  mutate(keywords_by_type = sum(total_keywords_or_combos))

# Gets rid of duplicate year/type entries. We only want these values once, so we can graph them
film_category_level_2 <- distinct_at(film_category_level,.vars=c("year","five_types"),.keep_all = TRUE)

# builds the multi-faceted line graph
ggplot(film_category_level_2,aes(x=year,y=keywords_by_type)) +
  geom_line(aes(color=five_types)) +
  labs(title="1A: Keywords vs. Year by Film Type",x="Year",y="Keywords",color="Film Type")

# Here we build the edgelist to construct the second generalist measure
# In the for loop, we iterate over each unique movie. If a movie is a solo movie, we skip that movie.
# The t(combn(t())) function basically finds all of the unique combinations of column 4, which is pcindex (production companies)
# So we are finding the unique combinations of production companies for each movie
# we store all of these combinations in an object called edgelist
edgelist <- data.frame()
for (i in unique(producers_films_2$pindex)){
  if (nrow(producers_films_2[producers_films_2$pindex==i, ]) < 2){ # this is how we skip the solo movies
    next
  }
  temp <- data.frame(t(combn(t(producers_films_2[producers_films_2$pindex==i, 4]), 2))) # new row for each tie of film company
  temp$year <- unique(producers_films_2[producers_films_2$pindex==i, 2][[1]]) # adds the year of the movie/tie to each row
  edgelist <- rbind(edgelist, temp) # combines everything together
}

# Gets rid of duplicate ties and sorts by year
edgelist <- unique(edgelist)
edgelist <- arrange(edgelist, year)

# We make a list where each year has its own element in the list
# Each year contains all of the ties for that year, and the years before (year 2010 has all ties from 1985-2010)
# And then we build a network for each year from that list
year_vector <- seq(1985, 2018)
edges_yearly = lapply(seq_along(year_vector), function(i) edgelist[edgelist$year <= year_vector[i], ])
vcnets = lapply(seq_along(edges_yearly), function(i) graph.data.frame(edges_yearly[[i]], directed = FALSE))

# This will calculate the eigen centrality for each network and store into a list
eigen_yearly = lapply(seq_along(vcnets), function(i) data.table(pcindex = V(vcnets[[i]])$name, 
                                                                eigcent = eigen_centrality(vcnets[[i]])$vector, 
                                                                year = E(vcnets[[i]])$year))
# Combines the list into a long dataframe
eigen_yearly = rbindlist(eigen_yearly)

# uses rollapplyr() to get average eigen centrality for each company for each year, lagged by 10 years
eigen_yearly <- eigen_yearly %>%
  group_by(pcindex) %>%
  mutate(lag_avg = rollapplyr(eigcent, list(-10:1), mean, partial = TRUE, fill = NA))

# calculates the upper quartile threshold for eigen centrality for each year
eigen_yearly <- eigen_yearly %>%
  group_by(year) %>%
  mutate(quartile = quantile(lag_avg, prob=0.75))

# ifelse statement to make a dummy that equals 1 if eigen centrality is greater than the quartile threshold
eigen_yearly$generalist2 <- ifelse(eigen_yearly$eigcent > eigen_yearly$quartile,1,0)

# merges the dummy variable into our big dataframe
producers_films_2 <- left_join(producers_films_2, eigen_yearly, by = c('year', 'pcindex'))

# The following code will reclassify our movies based on the new generalist dummy
producers_films_2 <- producers_films_2 %>%
  group_by(pindex) %>%
  mutate(five_types2 = ifelse(co_prod_vs_solo=="Solo",
                             ifelse(generalist2==1,"Central Solo","Peripheral Solo"),
                             ifelse(length(unique(generalist2))>1,"Hybrid Co-prod",
                                    ifelse(unique(generalist2)==1,"Central Co-prod","Peripheral Co-prod"))))

# this again is a repitition of the code above but with the new info being used instead
film_levels <- distinct_at(producers_films_2,.vars="pindex",.keep_all = TRUE)
film_category_level <- film_levels %>%
  group_by(five_types2, year) %>%
  mutate(keywords_by_type = sum(total_keywords_or_combos))
film_category_level_2 <- distinct_at(film_category_level,.vars=c("year","five_types2"),.keep_all = TRUE)

# finally, this will make the plot of new key words against year, colored by film type
ggplot(film_category_level_2[!is.na(film_category_level_2$five_types2), ],aes(x=year,y=keywords_by_type)) +
  geom_line(aes(color=five_types2)) +
  labs(title="1A: Second Generalist Type",x="Year",y="Keywords",color="Film Type")

### Part b

# After grouping by production company and year, we create three new variables. Central_co is equal to the total number
# of Central Co-production movies made by that company for that year. The other two variables are similar
producers_films_2 <- producers_films_2 %>%
  group_by(pcindex, year) %>%
  mutate(Central_co = sum(five_types=='Central Co-prod'),
         Peripheral_co = sum(five_types=='Peripheral Co-prod'),
         Hybrid_co = sum(five_types=='Hybrid Co-prod'))

# merges in the box office revenue data for each movie, removes rows that did not appear in the revenue data
producers_films_2 <- left_join(producers_films_2, box_office_revenues)
producers_films_2 <-producers_films_2[!is.na(producers_films_2$budget), ]

# Calculates the amount of box office revenue by summing the revenues, for each year, for each movie made by a firm
producers_films_2 <- producers_films_2 %>%
  group_by(pcindex, year) %>%
  mutate(yearly_rev = sum(total_box))

# Takes current year minus the company's first year to calculate how old the company is when they made each movie
producers_films_2 <- producers_films_2 %>%
  group_by(pcindex) %>%
  mutate(Company_age = year-min(year))

# merges our subsidiary data into our production company/movie data, initiates the subsidiary dummy as a column of 0s
producers_films_2 <- left_join(producers_films_2, production_subsidiaries, by = 'pcindex')

# We make a dummy: if the current year is less than the last year a firm was a subsidiary, the dummy = 1
# If the current year is after a company stopped being a subsidiary, the dummy is coded as a 0
# There are lots of NAs when we make this dummy, because most firms were never subsidiaries to begin with, and don't have
# a value for "last year". We convert all of the NAs to 0 as well
producers_films_2$is_subsidiary <- ifelse(producers_films_2$year <= producers_films_2$last_year, 1, 0)
producers_films_2[is.na(producers_films_2$is_subsidiary), 25] <- 0

# Creates the first regression for new words based on the three production variables and the required controls
glm.nb(new_word_sum ~ Central_co + Peripheral_co + Hybrid_co + yearly_rev + Company_age + is_subsidiary +
         factor(year), data = producers_films_2, offset(films_count))

# Same as above but for new combinations of key words
glm.nb(new_combo ~ Central_co + Peripheral_co + Hybrid_co + yearly_rev + Company_age + is_subsidiary +
         factor(year), data = producers_films_2, offset(films_count))

# Here we create the co-production variables using the second generalism type
producers_films_2 <- producers_films_2 %>%
  group_by(pcindex, year) %>%
  mutate(Central_co2 = sum(five_types2=='Central Co-prod'),
         Peripheral_co2 = sum(five_types2=='Peripheral Co-prod'),
         Hybrid_co2 = sum(five_types2=='Hybrid Co-prod'))

# Creats the new word regression based on the second generalism type
glm.nb(new_word_sum ~ Central_co2 + Peripheral_co2 + Hybrid_co2 + yearly_rev + Company_age + is_subsidiary +
         factor(year), data = producers_films_2, offset(films_count))

# Creates the new combo regression based on the second generalism type
glm.nb(new_combo ~ Central_co2 + Peripheral_co2 + Hybrid_co2 + yearly_rev + Company_age + is_subsidiary +
         factor(year), data = producers_films_2, offset(films_count))

############################################## Question 3 #######################################################

# creates the return variable by grouping by company and year, and then dividing box office returns by release coverage
producers_films_2 <- producers_films_2 %>%
  group_by(pcindex, year) %>%
  mutate(return = total_box/release_coverage)

# Movies with a coverage of 0.0 return Inf after this calculation, we turn those into NAs here
producers_films_2[is.infinite(producers_films_2$return), 34] <- NA

# Standardizes the return variable, grouping by year
producers_films_2 <- producers_films_2 %>%
  group_by(year) %>%
  mutate(standard_return = (return - mean(return, na.rm = TRUE))/sd(return, na.rm = TRUE))

distinct_companies <- distinct_at(producers_films_2, .vars=c("year", "pcindex"), .keep_all = TRUE)

# builds the regression model using the core-periphery generalist type
lm(standard_return ~ Central_co2 + Peripheral_co2 + Hybrid_co2 + yearly_rev + Company_age + is_subsidiary +
         factor(year), data = producers_films_2)

################################################ Question 4 #####################################################

### part A

# Creates a new dataframe, filtered by solo-produced movies. We find the sum of new words for each company for each year
solo_producers <- producers_films_2 %>%
  filter(co_prod_vs_solo == 'Solo') %>%
  group_by(pcindex, year) %>%
  mutate(solo_words = sum(new_word_sum))

# keeps the year, company index, and solo_words columns, and then left joins with the large dataset
solo_producers <- solo_producers[, c(2, 4, 36)]
producers_films_2 <- left_join(producers_films_2, solo_producers, by = c('year', 'pcindex'))
producers_films_2 <- unique(producers_films_2)

# repeats the same process but for new keywords from hybrid collaborations
hybrid_producers <- producers_films_2 %>%
  filter(five_types2 == 'Hybrid Co-prod') %>%
  group_by(pcindex, year) %>%
  mutate(hybrid_words = sum(new_word_sum))

# merges the data back to producers_films_2
hybrid_producers <- hybrid_producers[, c(2, 4, 37)]
producers_films_2 <- left_join(producers_films_2, hybrid_producers, by = c('year', 'pcindex'))
producers_films_2 <- unique(producers_films_2)

# For instances when a company has a value for solo_words but an NA for hybrid_words, we change those NAs to 0
# That way, these rows are not left out of the regression
producers_films_2[!is.na(producers_films_2$solo_words) & is.na(producers_films_2$hybrid_words), 37] <- 0

# gets rid of all duplicate entries for company-year combinations, allowing us to run our regression without duplicates
distinct_companies <- distinct_at(producers_films_2, .vars=c("year", "pcindex"), .keep_all = TRUE)

# runs the glm model for solo words on hybrid words, using the same control variables as earlier  
glm.nb(solo_words ~ hybrid_words + yearly_rev + Company_age + is_subsidiary +
         factor(year), data = distinct_companies, offset(films_count))

### part B

# builds the same regression function in question 3 but adds number of new keywords as a predictor
lm(standard_return ~ new_word_sum + Central_co2 + Peripheral_co2 + Hybrid_co2 + yearly_rev + Company_age + is_subsidiary +
     factor(year), data = distinct_companies)

#################################################### Extra Credit #######################################################

# only keeps the movie index, year, and talent id columns
film_cast_members <- film_cast_members[, 2:4]

# We want to create a column for number of new keywords for each movie; we do this by merging in a column from keywords_films
film_cast_members <- inner_join(film_cast_members, keywords_films[, c(1, 5)], by = 'pindex')
film_cast_members <- unique(film_cast_members)

# this just sorts by year and cast member
temp <- film_cast_members %>%
  group_by(nconst) %>%
  arrange(nconst, year)

# Calculates the total number of new words attributed to a cast member for each year
temp <- temp %>%
  group_by(nconst, year) %>%
  mutate(yearly_words = sum(new_word_sum))

temp2 <- distinct_at(temp, .vars=c("year", "nconst"), .keep_all = TRUE)

# Cumsum() will cumulatively sum up the yearly_words value as you go down the rows
# So for example, year 2012 for a cast member would have total new words attributed to that actor from 1985-2011
temp2 <- temp2 %>%
  group_by(nconst) %>%
  arrange(nconst, year) %>%
  mutate(cast_innovation = cumsum(yearly_words))

# merges the cumulative number column
temp <- left_join(temp, temp2[, c(2, 3, 6)], by = c('year', 'nconst'))
temp <- unique(temp)

# Calculates the total number of new words for all cast members by movie
temp <- temp %>%
  group_by(pindex) %>%
  mutate(innovation_per_movie = sum(cast_innovation))

# merges this total into the large dataset by matching by movie
producers_films_2 <- inner_join(producers_films_2, temp, by = 'pindex')
producers_films_2 <- unique(producers_films_2)

last_df_hopefully <- distinct_at(producers_films_2, .vars = c('pcindex', 'year.x', 'pindex'), .keep_all = TRUE)

# Now we group by production company and year, and sum up the total innovativeness for the cast(s) for the year
last_df_hopefully <- last_df_hopefully %>%
  group_by(pcindex, year.x) %>%
  mutate(total_cast_innovation = sum(innovation_per_movie))

nope_one_more_df <- distinct_at(last_df_hopefully, .vars = c('pcindex', 'year.x'), .keep_all = TRUE)

# And finally we build the regression (I used linear because glm.nb was giving me problems)
lm(total_cast_innovation ~ Central_co + Peripheral_co + Hybrid_co + yearly_rev + Company_age + is_subsidiary +
         factor(year.x), data = nope_one_more_df)  
  
  
  
  
  
  
  
  







