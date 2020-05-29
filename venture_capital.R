library(readxl)
library(tidyr)
library(igraph)
library(ggplot2)
library(data.table)

setwd("~/Emory Official Stuff/Social Networks/Datasets")

# read in the data
funding1 <- read.csv('Funding_events_7.14.csv')
funding2 <- read_excel("Funding_events_7.14_page2.xlsx")
outcomes <- read.csv('Venture_capital_firm_outcomes.csv')

# the dates have different formats in the two datasets; here we convert both to date variables and then combine the datasets
funding1$Deal.Date <- as.character(funding1$Deal.Date)
funding1$Deal.Date <- as.Date(funding1$Deal.Date, format = '%m/%d/%y')
funding2$`Deal Date` <- as.Date(funding2$`Deal Date`)
colnames(funding2) <- colnames(funding1)
funding <- rbind(funding1, funding2)

# keeps the startup, investors, and date
funding <- funding[, c(1, 11, 4)]
funding$Investors <- as.character(funding$Investors)
funding <- funding[funding$Investors != '', ] # removes rows that just have a blank instead of an investor

# the for loop creates 27 strings that look like this: Investor1, Investor2, ...
names <- c()
for (i in 1:27){
  names <- append(names, paste('Investor', i, sep = ''))
}

# splits the Investors column into multiple columns, we use the names vector for the new column names
funding <- separate(funding, col = Investors, into = names, sep = ',')
funding <- funding[!is.na(funding$Investor2), ] # gets rid of all rows that don't have a second investor

# investors dataframe only contains investor info
investors <- funding[, 2:28]

# identifies each combination of investors per row, pivots the data to make it long, removes duplicate rows
edgelist <- apply(t(investors), 2, function(x) t(combn(x[!is.na(x)], 2)))
edgelist <- setNames(data.frame(do.call(rbind, edgelist)), c("Investor1", "Investor2"))
edgelist <- unique(edgelist)

# because separate function used ',' as a separator, we eliminate all cases where things like ', inc.' became a separate row
edgelist <- edgelist[edgelist$Investor1 != ' Inc.',]
edgelist <- edgelist[edgelist$Investor2 != ' Inc.',]
edgelist <- edgelist[edgelist$Investor1 != ' Ltd.',]
edgelist <- edgelist[edgelist$Investor2 != ' Ltd.',]
edgelist <- edgelist[edgelist$Investor1 != " Inc",]
edgelist <- edgelist[edgelist$Investor2 != " Inc",]
edgelist <- edgelist[edgelist$Investor1 != " LLC",]
edgelist <- edgelist[edgelist$Investor2 != " LLC",]
edgelist <- edgelist[edgelist$Investor1 != " LLC.",]
edgelist <- edgelist[edgelist$Investor2 != " LLC.",]
edgelist <- edgelist[edgelist$Investor1 != " L.L.C",]
edgelist <- edgelist[edgelist$Investor2 != " L.L.C",]
edgelist <- edgelist[edgelist$Investor1 != " Lung",]
edgelist <- edgelist[edgelist$Investor2 != " Lung",]
edgelist <- edgelist[edgelist$Investor1 != " and Blood",]
edgelist <- edgelist[edgelist$Investor2 != " and Blood",]

# makes the igraph object
net1 <- graph_from_data_frame(edgelist, directed = FALSE)

####################################################### Question 1 ########################################

### part a

# calculates closeness for each vertex, sorts it, and selects the highest closeness
sort(closeness(net1), decreasing = TRUE)[1]

### part b

# returns the number of nodes
vcount(net1)

# takes the inverse of closeness for each node, divides by n-1, sorts, and then grabs the lowest instance (This is the average shortest path distance)
sort(1/(closeness(net1)*(vcount(net1) - 1)))[1]

### part c

# takes the average of the average path lengths, which is equal to the average shortest path length for the entire network
mean(1/(closeness(net1)*(vcount(net1) - 1))) # this is pretty high because there is one broker and because of the way we dealt with isolates

####################################################### Question 2 #########################################

# makes a new dataframe with date as the first row
Investors2 <- funding[, c(29,2:28)]

# creates another edgelist that maintains the corresponding dates, does not remove duplicates this time
# here we also get rid of the erroneous extra rows made, like we did above
edgelist2 <- apply(t(Investors2), 2, function(x) cbind(x[1], t(combn(x[!is.na(x)], 2))))
edgelist2 <- setNames(data.frame(do.call(rbind, edgelist2)), c("Date", "Investor1", "Investor2"))
edgelist2 <- unique(edgelist2)
edgelist2 <- edgelist2[!grepl("/", edgelist2$Investor1),]
edgelist2 <- edgelist2[edgelist2$Investor1 != ' Inc.',]
edgelist2 <- edgelist2[edgelist2$Investor2 != ' Inc.',]
edgelist2 <- edgelist2[edgelist2$Investor1 != ' Ltd.',]
edgelist2 <- edgelist2[edgelist2$Investor2 != ' Ltd.',]
edgelist2 <- edgelist2[edgelist$Investor1 != " Inc",]
edgelist2 <- edgelist2[edgelist$Investor2 != " Inc",]
edgelist2 <- edgelist2[edgelist$Investor1 != " LLC",]
edgelist2 <- edgelist2[edgelist$Investor2 != " LLC",]
edgelist2 <- edgelist2[edgelist$Investor1 != " LLC.",]
edgelist2 <- edgelist2[edgelist$Investor2 != " LLC.",]
edgelist2 <- edgelist2[edgelist$Investor1 != " L.L.C",]
edgelist2 <- edgelist2[edgelist$Investor2 != " L.L.C",]
edgelist2 <- edgelist2[edgelist$Investor1 != " Lung",]
edgelist2 <- edgelist2[edgelist$Investor2 != " Lung",]
edgelist2 <- edgelist2[edgelist$Investor1 != " and Blood",]
edgelist2 <- edgelist2[edgelist$Investor2 != " and Blood",]

# Here we ran into a problem where the combn() function was including the date column for the combinations
# for example, in some instances we would have dates in the investor column, which is a problem
# So here we make the investor column a string, and then use grepl() to search for observations that have dates in the investor column
edgelist2$Investor1 <- as.character(edgelist2$Investor1)
edgelist2 <- edgelist2[!grepl('\\d\\d\\d\\d-\\d\\d-\\d\\d', edgelist2$Investor1), ]

# Here we want to make an index column where 1 equals the first month, 2 equals the second month, etc.
# as.POSIXlt() allows us to extract the year and month from each data in the dataframe
# We the first year (1981) from the row years and then multiply by 12; this tells us how many years have passed in months
# then we add the month column to account for how deep into the current year each observation is
edgelist2$Date <- as.POSIXlt(edgelist2$Date)
edgelist2$year <- edgelist2$Date$year
edgelist2$month <- edgelist2$Date$mon
edgelist2$time <- 12*(edgelist2$year - 81) + (edgelist2$month - 5)

### part a

# creates a network for each additional month and calculates coreness for each
cores <- c()
for(i in 0:max(edgelist2$time)){
  net <- graph_from_data_frame(edgelist2[edgelist2$time <= i, 2:3])
  core <- coreness(net)
  cores <- append(cores, mean(core))
}

# creates a numeric vector for each month represented in the data, and then plots average coreness against time
times <- seq(1, 398)
plot(times, cores)
title('Question 2 part a')

### part b

# repeats part a) but removes duplicates inside of the loop using simplify()
cores2 <- c()
for(i in 0:max(edgelist2$time)){
  net <- graph_from_data_frame(edgelist2[edgelist2$time <= i, 2:3])
  net_simple <- simplify(net, remove.multiple = TRUE)
  core <- coreness(net_simple)
  cores2 <- append(cores2, mean(core))
}

plot(times, cores2)
title('Question 2 part b')

### part c

# sorts the dataframe by the time variable (Our index variable), adds 1 so that the first month has index 1 (Before it was 0)
edgelist2 <- edgelist2[order(edgelist2$time), ]
edgelist2$time <- edgelist2$time + 1

# We loop over each month, and check if that particular month has any ties in our dataset
# if it does, then we construct a network for that month and all previous months
# in the second if statement, we are assigning positive weights to all edges that maintain their relationship within 60 months
# those that do not maintain the relationship get a weight of zero and then are removed as ties
graphs3 <- list()
for (i in 1:398) {
  if (i %in% edgelist2$time) {
    graphs3[[i]] <- graph_from_edgelist(as.matrix(edgelist2[edgelist2$time <= i, 2:3]), directed = FALSE)
    if (i > 60) {
      E(graphs3[[i]])$weight <- pmax(edgelist2[edgelist2$time<=i, 6]-(i-60),0)
      graphs3[[i]] <- delete.edges(graphs3[[i]], which(E(graphs3[[i]])$weight==0))
      graphs3[[i]] <- simplify(graphs3[[i]], edge.attr.comb=list(weight="sum"))
    }
  }
}

# makes a new dataframe to store our index and coreness that we will calculate
core.vals.decay <- setNames(data.frame(matrix(ncol = 2, nrow = 253)), c("index", "coreness"))
core.vals.decay[,1] <- seq(1, 253)

# if a month corresponds to a month in our index, we take the network associated with that graph and calculate the mean coreness
# if the month does not correspond to a month in our data, then we assign a zero as a placeholder
for (i in 1:length(graphs3)) {
  core.vals.decay[i,1] <- i
  if (i %in% edgelist2$time) {
    core.vals.decay[i,2] <- mean(coreness(graphs3[[i]]))
  } else {
    core.vals.decay[i,2] <- 0
  }
}

# gets rid of the zeros we manually placed
core.vals.decay <- core.vals.decay[core.vals.decay[,2]!=0,]

# builds the plot of coreness over months
ggplot(core.vals.decay, aes(x = index, y = coreness)) +
  geom_line() + ggtitle("Question 2, Part C")

######################################################### Question 3 ############################

### part a part 1

# checks the eigenvector centrality for the first month to see if they are equal; (The problem tells us to start when this is the case)
eigen_centrality(graphs3[[1]])$vector

# this constructs a list of graphs that we attempted to coerce into being "ideal" graphs.
# in other words, we could not figure out how to get cp* so we tried to make it ourselves
ideal.graphs <- list()
for (i in 1:length(graphs3)) {
  if (i %in% edgelist2$time) {
    eigen <- eigen_centrality(graphs3[[i]])$vector
    eigen1 <- as.matrix(eigen)
    delta <- eigen1%*%t(eigen1)
    delta[delta>0.5] <- 1
    delta[delta != 1] <- 0
    ideal.graphs[[i]] <- graph.adjacency(delta, mode = 'undirected')
  }
}

# calculates the coreness for each graph in our list of "ideals" and stores in core.unique
core.unique <- list()
for(i in 1:length(ideal.graphs)){
  if(i %in% edgelist2$time){
    core.unique[[i]] <- coreness(ideal.graphs[[i]])
  }else {
    core.unique[[i]] <- 0
  }
}

# calculates the coreness for each graph in our list of actual graphs (from graphs3)
core.graphs3 <- list()
for(i in 1:length(graphs3)){
  if(i %in% edgelist2$time){
    core.graphs3[[i]] <- coreness(graphs3[[i]])
  }else {
    core.graphs3[[i]] <- 0
  }
}

# correlates the coreness values for each month to obtain concentration
concentration <- setNames(data.frame(matrix(ncol = 2, nrow = 398)), c("index", "concentrations"))
concentration[,1] <- seq(1, 398)
for(i in 1:length(graphs3)){
  if(i %in% edgelist2$time & (sd(core.unique[[i]])!=0 & sd(core.graphs3[[i]])!=0)){
    concentration[i,2] <- cor(core.unique[[i]], core.graphs3[[i]])
}else{
    concentration[i,2] <- 0
  }
}

# removes the rows that we manually inserted zeros into
concentration <- concentration[concentration[,2]!=0, ]

# plots our correlations for each month
ggplot(concentration, aes(x = index, y = concentrations)) +
  geom_line() + ggtitle("Question 3, Part A I")

### part a part 2

# Here, we want to identify the proportion of people that are inside the core partition vs outside.
# Those nodes that are outside this partition would be considered periphery, while those inside would be the core
# We want to graph this proportion over time to see how much our network resembles a core-periphery structure over time

# we could not obtain the ideal proportion corresponding to maximum concentration scores as we were not able to get 3.A.1 correctly
# what I did instead is take the concentration values for each month and used that as the proportion.
# I then plotted the number of firms that would be inside of this theoretical partition value.
# This is not correct but was my best effort

# here we are multiplying concentration (Which we are using as a proportion as mentioned above) by the number of nodes
# at that specific month index
# this will give us a number that we estimate to be inside of the core
concentration$partitions <- 0
for(i in 1:length(graphs3)){
  if(i %in% concentration$index){
    concentration[i, 3] <- concentration[i, 2]*vcount(graphs3[[i]])
  }
}

# removes the zero values and builds the plot
concentration <- concentration[concentration[,3]!=0, ]
ggplot(concentration, aes(x = index, y = partitions)) +
  geom_line() + ggtitle("Question 3, Part A II") + ylab('Number of Firms')

### part a part 3

# my apologies, but I could not make it to the rest of the homework so I will be detailing how I would conceptualize these
# problems if I had more time

# problem 3.A.1 was completed incorrectly which prevented me from doing the rest of this problem. If 3.A.1 were to go how it
# was supposed to, then I would have generated many concentration scores for each month (Rather than just one)
# This would have allowed me to correctly plot the maximum concentration score for each month for part 1
# This would have allowed me to identify the ideal partition for each month to properly calculate and plot the proportions over time in part 2
# For 3.A.3, I would have arbitrarily selected a month to use for each year (May for my birthday month)
# For each year in the data, I would identify each partition size for the month of May
# For each partition size, I would calculate the range (max-min) of concentration scores
# I would then plot the range against partition size (p)
# I would generate one plot like this for each year in the data, and then shrink/combine the plots into a grid using ggplot

### part b

# One way I would try to assess the merits of a core-periphery structure is by visualizing the most recent iteration
plot.igraph(graphs3[[398]], directed = FALSE, vertex.label = '')

# Another way I would try to assess core-periphery structure is by checking the spread of closeness
# Here, I calculate the closeness for each node, and then calculate the standard deviation
close <- closeness(graphs3[[398]], mode = 'all')
mean(close)
sd(close)

#################################################### Question 4 ##################################################33

# Demetrius' function to get important centrality statistics
getNetStats=function(net)
{
  deg_in = degree(net, mode = "in")
  deg_out = degree(net, mode = "out")
  close= closeness(net, mode = "total")
  betw = betweenness(net)
  prank = page_rank(net)$vector
  id=V(net)$name
  stats= as.data.table(list(id = id, deg_in = deg_in, deg_out = deg_out, close = close, betw = betw, prank = prank))
  return(stats)
}

### part a

# calculates statistics for the original network object from question 1
netstats <- getNetStats(net1)
outcomes2 <- aggregate(outcomes$successful_investments~outcomes$firm_name,outcomes, sum) # sums successful investments by firm
outcomes2 <- data.frame(outcomes2, stringsAsFactors = FALSE)
colnames(outcomes2) <- c("id", "success") # renames column to "id" for merging purposes
netstats <- merge(netstats, outcomes2) # merges data

# creates a correlation matrix between the centrality statistics and number of successes
cor(netstats[, 3:7])

# repeats the same process above but for the most recent network where ties can decay
netstats2 <- getNetStats(graphs3[[398]])
netstats2 <- merge(netstats2, outcomes2)
cor(netstats2[, 3:7])

### part b

# Repeats the process in part a but instead we are aggregating based on out_of_business
outcomes2b <- aggregate(outcomes$out_of_business~outcomes$firm_name,outcomes, sum) # sums successful investments by firm
outcomes2b <- data.frame(outcomes2b, stringsAsFactors = FALSE)
colnames(outcomes2b) <- c("id", "out_of_business") # renames column to "id" for merging purposes
netstats2b <- merge(netstats[, 1:6], outcomes2b)
cor(netstats2b[, 3:7])











