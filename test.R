library("tidyr")
library("dplyr")
library("ggplot2")
library("maps")
library("ggmap")
library("mapdata")
library("Hmisc")
library("sp")
library("maptools")
library("reshape2")
library("shiny")
library("plotly")

#states
states  <-
  c(
    "AL",
    "AK",
    "AS",
    "AZ",
    "AR",
    "CA",
    "CO",
    "CT",   
    "DE",
    "DC",
    "FL",
    "GA",
    "GU",
    "HI",
    "ID",
    "IL",
    "IN",
    "IA",
    "KS",
    "KY",
    "LA",
    "ME",
    "MD",
    "MH",
    "MA",
    "MI",
    "MN",
    "MS",
    "MO",
    "MT",
    "NE",
    "NV",
    "NH",
    "NJ",
    "NM",
    "NY",
    "NC",
    "ND",
    "OH",
    "OK",
    "OR",
    "PA",
    "PR",
    "RI",
    "SC",
    "SD",
    "TN",
    "TX",
    "UT",
    "VT",
    "VA",
    "WA",
    "WV",
    "WI",
    "WY"
  )

only.mean.earning <- c(
  "MN_EARN_WNE_P10",
  "MN_EARN_WNE_P8",
  "MN_EARN_WNE_P6"
)

only.median.earning <- c(
  "MD_EARN_WNE_P10",
  "MD_EARN_WNE_P8",
  "MD_EARN_WNE_P6"
)

only.sd.earning <- c(
  "SD_EARN_WNE_P10",
  "SD_EARN_WNE_P8",
  "SD_EARN_WNE_P6"
)

columns.earning <- c(
  "MN_EARN_WNE_P10",
  "MD_EARN_WNE_P10",
  "SD_EARN_WNE_P10",
  "MN_EARN_WNE_P8",
  "MD_EARN_WNE_P8",
  "SD_EARN_WNE_P8",
  "MN_EARN_WNE_P6",
  "MD_EARN_WNE_P6",
  "SD_EARN_WNE_P6"
) 

column.tuition <- c(
  "NPT4_PUB",
  "NPT4_PRIV"
)

timeAfterGraduation <- c(
  "10 years after graduate",
  "8 years after graduate",
  "6 years after graduate",
  "average of these three term"
)

columns.need.to.convert.to.numberic <- c(
  "MN_EARN_WNE_P10",
  "MN_EARN_WNE_P8",
  "MN_EARN_WNE_P6",
  "realcost"
)

income.breaker <- seq(from=10000, to=50000, by = 2000)

## this method take in a date input
## and get all state summary, which is the mean of each
## time period
get.all.state.summary <- function(data.input) {
  group.state <- data.input
  group.state[columns.earning] <- sapply(group.state[columns.earning], as.numeric)
  group.state <- group_by(group.state, STABBR) %>% summarise(
    avg.10yrs = mean(MN_EARN_WNE_P10),
    avg.8yrs = mean(MN_EARN_WNE_P8),
    avg.6yrs = mean(MN_EARN_WNE_P6)
  )
  return(group.state)
}


## this method take a dataframe input and abb state and typeof data 
## this return the earning ratio of the each states according
## to the type of summary of data the client choose
get.one.state.all.city.earning.ratio <- function(data.input, state.name, type.you.want) {
  if(type.you.want == 1) {
    result.data <- mutate(data.input, "earning_tuition_ratio" = MN_EARN_WNE_P10 / realcost)
  } else if (type.you.want == 2) {
    result.data <- mutate(data.input, "earning_tuition_ratio" = MN_EARN_WNE_P6 / realcost)
  } else {
    result.data <- mutate(data.input, "mean_earning_of_3_terms" = (MN_EARN_WNE_P10 + MN_EARN_WNE_P8 + MN_EARN_WNE_P6)/3)
    result.data <- mutate(result.data, "earning_tuition_ratio" = mean_earning_of_3_terms/realcost)
  }
  prepare.for.city.summary <- get.one.state(result.data, state.name)
  city.summary <- group_by(prepare.for.city.summary, CITY) %>% summarise(avg_earning_tuition_ration=mean(earning_tuition_ratio))
  return(city.summary)
}

## this method take a dataframe input and abb state
## this method return the dataframe which is used to plot the
## box and whisker diagram
get.data.for.box.whisker <- function(data.input, state.name) {
  bwd.data <- get.one.state(data.input, state.name)
  bwd.data.result <- select(bwd.data, only.mean.earning)
  bwd.data.result <- mutate(bwd.data.result, "test" = MN_EARN_WNE_P10)
  bwd.data.result <- melt(bwd.data.result, id.vars = "test" )
  bwd.data.result <- select(bwd.data.result, "variable", "value")
  bwd.data.result <- sapply(bwd.data.result, as.numeric)
  bwd.data.result <- as.data.frame(bwd.data.result)
  return(bwd.data.result)
}


## this method take a dataframe input and abb state 
## this to get the top 10 univerisity in that state
get.the.top10.highest.earning.in.one.state <- function(data.input, state.code) {
  prepare.for.summary <- get.one.state(data.input, state.code)
  prepare.for.summary <- head(prepare.for.summary[order(prepare.for.summary$MN_EARN_WNE_P10, decreasing = TRUE),],10)
  return(prepare.for.summary)
}

## this method take a dataframe input and abb state 
## this method returns the earning which only involve this
## country's data
get.one.state <- function(data.input, state.code) {
  data.input <- filter(data.input, STABBR == state.code)
  return (data.input)
}

## this method take a dataframe input and abb state 
## this return the summary of the data, which summarize the data
## the mean of the earning in each time period
get.one.state.summary <- function(data.input, state.code){
  prepare.for.summary <- get.one.state(data.input, state.code)
  prepare.for.summary <- select(prepare.for.summary, columns.earning)
  prepare.for.summary <- sapply(prepare.for.summary, as.numeric)
  prepare.for.summary <- as.data.frame(prepare.for.summary)
  result.summary <- summarise(prepare.for.summary,
                              avg.10yrs = mean(MN_EARN_WNE_P10),
                              avg.8yrs = mean(MN_EARN_WNE_P8),
                              avg.6yrs = mean(MN_EARN_WNE_P6)
  )
  return(result.summary)
  
}


## this method take in a date input
## and get all state summary, which is the mean of median of each
## time period
get.all.state.median<- function(data.input) {
  group.state <- data.input
  group.state[columns.earning] <- sapply(group.state[columns.earning], as.numeric)
  group.state <- group_by(group.state, STABBR) %>% summarise(
    median.10yrs = mean(MD_EARN_WNE_P10),
    median.8yrs = mean(MD_EARN_WNE_P8),
    median.6yrs = mean(MD_EARN_WNE_P6)
  )
  return(group.state)
}

## this method take in a date input
## and get all state summary, which is the mean of sd of each
## time period
get.all.state.sd <- function(data.input) {
  group.state <- data.input
  group.state[columns.earning] <- sapply(group.state[columns.earning], as.numeric)
  group.state <- group_by(group.state, STABBR) %>% summarise(
    sd.10yrs = mean(SD_EARN_WNE_P10),
    sd.8yrs = mean(SD_EARN_WNE_P8),
    sd.6yrs = mean(SD_EARN_WNE_P6)
  )
  return(group.state)
}

## this method takes in lat and long and return the state
## which is clicked
latlong2state <- function(pointsDF) {
  # Prepare SpatialPolygons object with one SpatialPolygon
  # per state (plus DC, minus HI & AK)
  states <- map('state', fill=TRUE, col="transparent", plot=FALSE)
  IDs <- sapply(strsplit(states$names, ":"), function(x) x[1])
  states_sp <- map2SpatialPolygons(states, IDs=IDs,
                                   proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # Convert pointsDF to a SpatialPoints object 
  pointsSP <- SpatialPoints(pointsDF, 
                            proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # Use 'over' to get _indices_ of the Polygons object containing each point 
  indices <- over(pointsSP, states_sp)
  
  # Return the state names of the Polygons object containing each point
  stateNames <- sapply(states_sp@polygons, function(x) x@ID)
  stateNames[indices]
}







#sb <- read.csv('data/MERGED2015_16_PP.csv', stringsAsFactors = FALSE)
long <- state.name[match(states, state.abb)]
long <- tolower(long)
combine2 <- data.frame(states, long)
university.name.state <- c("INSTNM", "CITY", "STABBR")

## this is about to load the raw data from the csv file
## and filter the NULL and PrivacySuppressed out of picture
univeristy.raw <- read.csv("CollegeScorecard_Raw_Data/MERGED2013_14_PP.csv", stringsAsFactors=FALSE)
univeristy.raw.only.earning <- select(univeristy.raw, INSTNM,	CITY,	STABBR, columns.earning)
univeristy.raw.only.earning[univeristy.raw.only.earning == "NULL"] <- NA
univeristy.raw.only.earning[univeristy.raw.only.earning == "PrivacySuppressed"] <- NA
processed.university.only.earning <- na.omit(univeristy.raw.only.earning)

## this is about to get the raw data from the csv file
## and combine the public and private tuition into one and
## and filter out the NULL and PrivacySuprpressed out of picure
univeristy.raw.only.tuition <- select(univeristy.raw, INSTNM,	CITY,	STABBR, column.tuition)
univeristy.raw.only.tuition[univeristy.raw.only.tuition == "NULL"] <- 0
univeristy.raw.only.tuition[univeristy.raw.only.tuition == "PrivacySuppressed"] <- 0
univeristy.raw.only.tuition$NPT4_PUB <- as.numeric(as.character(univeristy.raw.only.tuition$NPT4_PUB))
univeristy.raw.only.tuition$NPT4_PRIV <- as.numeric(as.character(univeristy.raw.only.tuition$NPT4_PRIV))
univeristy.raw.only.tuition <- mutate(univeristy.raw.only.tuition, "realcost" = NPT4_PUB +NPT4_PRIV)
univeristy.raw.only.tuition <- select(univeristy.raw.only.tuition, INSTNM, CITY, STABBR, realcost)
processed.univeristy.raw.only.tuition <- filter(univeristy.raw.only.tuition, realcost != 0)

## get the map data ready for combine of data 
usa.map <- map_data("state")
usa.map <- mutate(usa.map, State.Code = state.abb[match(toupper(usa.map$region),toupper(state.name))])

## combine the earning and tuition together
combine.earning.tuition <- left_join(processed.univeristy.raw.only.tuition, processed.university.only.earning, by = university.name.state ) %>% na.omit()
combine.earning.tuition <- select(combine.earning.tuition, INSTNM, CITY, STABBR, "realcost", only.mean.earning)
combine.earning.tuition[columns.need.to.convert.to.numberic] <- sapply(combine.earning.tuition[columns.need.to.convert.to.numberic], as.numeric)

## combine the map and earning to plot map, and cut the map data into
## several break point and plot
all.country.summary <- get.all.state.summary(processed.university.only.earning)
names(all.country.summary)[1] <- paste("State.Code")
all.country.summary.map <- left_join(usa.map, all.country.summary, by = "State.Code")
col.name <- colnames(all.country.summary)[length(colnames(all.country.summary))]
cut.column <- all.country.summary.map[[col.name]]
cut.x <- as.numeric(unlist(cut.column))
all.country.summary.map$avg.of.three.time <- cut(cut.x, breaks = income.breaker)




