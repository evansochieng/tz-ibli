##### Pricing Tanzania #####

######### Data Pre-processing #########
# Load data
tzData <- readr::read_csv("eVIIRS_Garoowe.csv", show_col_types = FALSE)
head(tzData)

# Handle missing values
# Check for missing values
colSums(is.na(tzData))
# 2002 and 2022 have missing values

# Fill the missing values in 2002 with zeros
tzData$`2002`[is.na(tzData$`2002`)] <- 0
# Fill the two missing values in 2022 column with mean value
tzData$`2022`[is.na(tzData$`2022`)] <- mean(tzData$`2022`, na.rm=TRUE)


########## Data Preparation ##########

# Stack the dekads together
stackedDekads <- as.numeric(rownames(tzData)) |> 
  sprintf(fmt = "%02d")  |>
  rep(2)

# create a new dataframe with data from next year onwards (shift left)
# create a column of zeros for the current year
nextYearRawData <- tzData |>
  dplyr::select(-1) |>
  dplyr::mutate(Lastyear = 0) |>
  `colnames<-`(colnames(tzData)[1]:colnames(tzData)[ncol(tzData)])

# merge the dataframes together (stack to account of overlapping)
stackedTZData <- rbind(tzData, nextYearRawData)

# create a column with dekads
stackedTZData <- stackedTZData |>
  dplyr::mutate(
    Dekad = stackedDekads
  )

# create a function to map dates to respective dekad
# dekads are from 1:36
dekadMap <- function(date) {
  
  # check the month
  if (lubridate::month(lubridate::ymd(date)) == 1) {
    # check the range where the day falls and get dekad
    if (dplyr::between(lubridate::day(lubridate::ymd(date)), 1, 10)) {dekad <- 1}
    if (dplyr::between(lubridate::day(lubridate::ymd(date)), 11, 20)) {dekad <- 2}
    if (dplyr::between(lubridate::day(lubridate::ymd(date)), 21, 31)) {dekad <- 3}
  } else if (lubridate::month(lubridate::ymd(date)) == 2) {
    if (dplyr::between(lubridate::day(lubridate::ymd(date)), 1, 10)) {dekad <- 4}
    if (dplyr::between(lubridate::day(lubridate::ymd(date)), 11, 20)) {dekad <- 5}
    if (dplyr::between(lubridate::day(lubridate::ymd(date)), 21, 31)) {dekad <- 6}
  } else if (lubridate::month(lubridate::ymd(date)) == 3) {
    if (dplyr::between(lubridate::day(lubridate::ymd(date)), 1, 10)) {dekad <- 7}
    if (dplyr::between(lubridate::day(lubridate::ymd(date)), 11, 20)) {dekad <- 8}
    if (dplyr::between(lubridate::day(lubridate::ymd(date)), 21, 31)) {dekad <- 9}
  } else if (lubridate::month(lubridate::ymd(date)) == 4) {
    if (dplyr::between(lubridate::day(lubridate::ymd(date)), 1, 10)) {dekad <- 10}
    if (dplyr::between(lubridate::day(lubridate::ymd(date)), 11, 20)) {dekad <- 11}
    if (dplyr::between(lubridate::day(lubridate::ymd(date)), 21, 31)) {dekad <- 12}
  } else if (lubridate::month(lubridate::ymd(date)) == 5) {
    if (dplyr::between(lubridate::day(lubridate::ymd(date)), 1, 10)) {dekad <- 13}
    if (dplyr::between(lubridate::day(lubridate::ymd(date)), 11, 20)) {dekad <- 14}
    if (dplyr::between(lubridate::day(lubridate::ymd(date)), 21, 31)) {dekad <- 15}
  } else if (lubridate::month(lubridate::ymd(date)) == 6) {
    if (dplyr::between(lubridate::day(lubridate::ymd(date)), 1, 10)) {dekad <- 16}
    if (dplyr::between(lubridate::day(lubridate::ymd(date)), 11, 20)) {dekad <- 17}
    if (dplyr::between(lubridate::day(lubridate::ymd(date)), 21, 31)) {dekad <- 18}
  } else if (lubridate::month(lubridate::ymd(date)) == 7) {
    if (dplyr::between(lubridate::day(lubridate::ymd(date)), 1, 10)) {dekad <- 19}
    if (dplyr::between(lubridate::day(lubridate::ymd(date)), 11, 20)) {dekad <- 20}
    if (dplyr::between(lubridate::day(lubridate::ymd(date)), 21, 31)) {dekad <- 21}
  } else if (lubridate::month(lubridate::ymd(date)) == 8) {
    if (dplyr::between(lubridate::day(lubridate::ymd(date)), 1, 10)) {dekad <- 22}
    if (dplyr::between(lubridate::day(lubridate::ymd(date)), 11, 20)) {dekad <- 23}
    if (dplyr::between(lubridate::day(lubridate::ymd(date)), 21, 31)) {dekad <- 24}
  } else if (lubridate::month(lubridate::ymd(date)) == 9) {
    if (dplyr::between(lubridate::day(lubridate::ymd(date)), 1, 10)) {dekad <- 25}
    if (dplyr::between(lubridate::day(lubridate::ymd(date)), 11, 20)) {dekad <- 26}
    if (dplyr::between(lubridate::day(lubridate::ymd(date)), 21, 31)) {dekad <- 27}
  } else if (lubridate::month(lubridate::ymd(date)) == 10) {
    if (dplyr::between(lubridate::day(lubridate::ymd(date)), 1, 10)) {dekad <- 28}
    if (dplyr::between(lubridate::day(lubridate::ymd(date)), 11, 20)) {dekad <- 29}
    if (dplyr::between(lubridate::day(lubridate::ymd(date)), 21, 31)) {dekad <- 30}
  } else if (lubridate::month(lubridate::ymd(date)) == 11) {
    if (dplyr::between(lubridate::day(lubridate::ymd(date)), 1, 10)) {dekad <- 31}
    if (dplyr::between(lubridate::day(lubridate::ymd(date)), 11, 20)) {dekad <- 32}
    if (dplyr::between(lubridate::day(lubridate::ymd(date)), 21, 31)) {dekad <- 33}
  } else if (lubridate::month(lubridate::ymd(date)) == 12) {
    if (dplyr::between(lubridate::day(lubridate::ymd(date)), 1, 10)) {dekad <- 34}
    if (dplyr::between(lubridate::day(lubridate::ymd(date)), 11, 20)) {dekad <- 35}
    if (dplyr::between(lubridate::day(lubridate::ymd(date)), 21, 31)) {dekad <- 36}
  }
  
  # return the dekad
  return(dekad)
}

### Map the dekads to their respective months ###
# create a map
dekadMonthMap <- rep(x = 1:12, each = 3)
names(dekadMonthMap) <- 1:36 |> sprintf(fmt = "%02d")

# define other key parameters
triggerLevel <- 0.25
exitLevel <- "5th Percentile"
maxPayout <- 1
LRSeasonPerc <- 0.58
SRSeasonPerc <- 0.42

#######################################################################
######## Unimodal Pricing ###########

# Define rainy season width
RSstartDate <- as.Date("01-11-2023", format = "%d-%m-%Y")
RSendDate <- as.Date("31-05-2024", format = "%d-%m-%Y")

# Get the dekads for the start and end dates of the season
RSstartDekad <- dekadMap(RSstartDate)
RSendDekad <- dekadMap(RSendDate)

# subset data
# if the season falls within the same year, subset normally
if (lubridate::year(lubridate::ymd(RSstartDate)) == lubridate::year(lubridate::ymd(RSendDate))) {
  RSNDVIdata <- stackedTZData[RSstartDekad:RSendDekad, ]
} else if (lubridate::year(lubridate::ymd(RSendDate)) > lubridate::year(lubridate::ymd(RSstartDate))) {
  # if season overlaps to the next year, adjust end dekad by 36
  RSendDekad <- RSendDekad + 36
  RSNDVIdata <- stackedTZData[RSstartDekad:RSendDekad, ]
}

# Map the dekadals column with the dekadMonthMap:
RSNDVIdata <- RSNDVIdata |> 
  dplyr::mutate(Month = dekadMonthMap[Dekad])

# Drop the `Dekad` column
RSNDVIdata <- RSNDVIdata |> 
  dplyr::select(-c("Dekad"))

# DATA WRANGLING
# Average dekadal NDVI to find monthly estimates (rounded off to 4 decimal 
# places)
# group data by month => gives monthly averages:

RSMonthlyDekadAverages <- RSNDVIdata |> 
  dplyr::group_by(Month) |> 
  dplyr::summarise_all("mean") |>
  dplyr::mutate(dplyr::across(everything(), round, 4)) |>
  dplyr::ungroup()

# drop the month column
RSMonthlyDekadAverages <- RSMonthlyDekadAverages |>
  dplyr::select(-c(Month))

# Aggregate RS season rainfall
RSSeasonalAggregate <- RSMonthlyDekadAverages |> 
  dplyr::summarise_all("sum") |>
  dplyr::mutate(dplyr::across(everything(), round, 4))

# data normalization:
RSSeasonalMean <- round(mean(as.numeric(RSSeasonalAggregate[1,])), digits = 4)

RSSeasonalStd <- round(sd(as.numeric(RSSeasonalAggregate[1,])), digits = 4)

# normalize the raw data
RSNormalizedNDVI <- as.data.frame((RSSeasonalAggregate - RSSeasonalMean) / RSSeasonalStd) |>
  dplyr::mutate(dplyr::across(everything(), round, 4))

# calculate exit based on the chosen percentile level
if (exitLevel == 'Minimum'){
  RSExit <- min(RSNormalizedNDVI)
} else if (exitLevel == '1st Percentile'){
  RSExit <- RSNormalizedNDVI |> quantile(probs = 0.01) |>
    round(digits = 4)
} else if (exitLevel == '5th Percentile'){
  RSExit <- RSNormalizedNDVI |> quantile(probs = 0.05) |>
    round(digits = 4)
}

# calculate the trigger based on the percentile passed
RSTrigger <- RSNormalizedNDVI |> quantile(probs = triggerLevel) |> 
  round(digits = 4)

# create a dataframe for the payouts
# make a copy of the RSNormalizedNDVI dataframe:
RSPayouts <- RSNormalizedNDVI

# calculate payouts:
for (year in 1:ncol(RSNormalizedNDVI)) {
  # extract value for calculating payout:
  RSActualValue <- RSNormalizedNDVI[, year]
  
  # calculate payout:
  if (RSActualValue > RSTrigger) {
    RSPayouts[, year] <- 0
  }else if (RSActualValue <= RSExit) {
    RSPayouts[, year] <- maxPayout #make this reactive
  }else if (RSActualValue <= RSTrigger & RSActualValue > RSExit) {
    RSPayouts[, year] <- ((RSTrigger - RSActualValue) / (RSTrigger - RSExit))
  }
}

### Visualize the payouts
RSPayouts <- RSPayouts |>
  t() |>
  as.data.frame() |>
  tibble::rownames_to_column("Year") |>
  setNames(c("Year", "RS Payouts"))


# draw a bar graph for the rainy season payouts using plot_ly
RSFig <- plotly::plot_ly(RSPayouts, x = ~Year,
                         y = ~`RS Payouts`, type = 'bar', name = 'Rainy Season') |>
  plotly::layout(yaxis = list(title = 'Annual Payouts'), 
                 title="Historical Payouts")
RSFig

# Calculate premium rate
unimodalPremiumRate <- round(mean(as.numeric(RSPayouts[, "RS Payouts"])), digits = 2)


##########################################################################
############ Bimodal Pricing ###################

# Define rainy seasons width
LRstartDate <- as.Date("01-03-2023", format = "%d-%m-%Y")
LRendDate <- as.Date("30-06-2023", format = "%d-%m-%Y")

SRstartDate <- as.Date("01-10-2023", format = "%d-%m-%Y")
SRendDate <- as.Date("31-12-2023", format = "%d-%m-%Y")

##### Long Rains (LR) Season #####
# Get the dekads for the start and end dates of the LR season
LRstartDekad <- dekadMap(LRstartDate)
LRendDekad <- dekadMap(LRendDate)

# subset data
# if the season falls within the same year, subset normally
if (lubridate::year(lubridate::ymd(LRstartDate)) == lubridate::year(lubridate::ymd(LRendDate))) {
  LRNDVIdata <- stackedTZData[LRstartDekad:LRendDekad, ]
} else if (lubridate::year(lubridate::ymd(LRendDate)) > lubridate::year(lubridate::ymd(LRstartDate))) {
  # if season overlaps to the next year, adjust end dekad by 36
  LRendDekad <- LRendDekad + 36
  LRNDVIdata <- stackedTZData[LRstartDekad:LRendDekad, ]
}

# Map the dekadals column with the dekadMonthMap:
LRNDVIdata <- LRNDVIdata |> 
  dplyr::mutate(Month = dekadMonthMap[Dekad])

# Drop the `Dekad` column
LRNDVIdata <- LRNDVIdata |> 
  dplyr::select(-c("Dekad"))

# DATA WRANGLING
# Average dekadal NDVI to find monthly estimates (rounded off to 4 decimal 
# places)
# group data by month => gives monthly averages:

LRMonthlyDekadAverages <- LRNDVIdata |> 
  dplyr::group_by(Month) |> 
  dplyr::summarise_all("mean") |>
  dplyr::mutate(dplyr::across(everything(), round, 4)) |>
  dplyr::ungroup()

# drop the month column
LRMonthlyDekadAverages <- LRMonthlyDekadAverages |>
  dplyr::select(-c(Month))

# Aggregate LR season NDVI
LRSeasonalAggregate <- LRMonthlyDekadAverages |> 
  dplyr::summarise_all("sum") |>
  dplyr::mutate(dplyr::across(everything(), round, 4))

# data normalization:
LRSeasonalMean <- round(mean(as.numeric(LRSeasonalAggregate[1,])), digits = 4)

LRSeasonalStd <- round(sd(as.numeric(LRSeasonalAggregate[1,])), digits = 4)

# normalize the raw data
LRNormalizedNDVI <- as.data.frame((LRSeasonalAggregate - LRSeasonalMean) / LRSeasonalStd) |>
  dplyr::mutate(dplyr::across(everything(), round, 4))

# calculate exit based on the chosen percentile level
if (exitLevel == 'Minimum'){
  LRExit <- min(LRNormalizedNDVI)
} else if (exitLevel == '1st Percentile'){
  LRExit <- LRNormalizedNDVI |> quantile(probs = 0.01) |>
    round(digits = 4)
} else if (exitLevel == '5th Percentile'){
  LRExit <- LRNormalizedNDVI |> quantile(probs = 0.05) |>
    round(digits = 4)
}

# calculate the trigger based on the percentile passed
LRTrigger <- LRNormalizedNDVI |> quantile(probs = triggerLevel) |> 
  round(digits = 4)

# create a dataframe for the payouts
# make a copy of the LRNormalizedNDVI dataframe:
LRPayouts <- LRNormalizedNDVI

# calculate payouts:
for (year in 1:ncol(LRNormalizedNDVI)) {
  # extract value for calculating payout:
  LRActualValue <- LRNormalizedNDVI[, year]
  
  # calculate payout:
  if (LRActualValue > LRTrigger) {
    LRPayouts[, year] <- 0
  }else if (LRActualValue <= LRExit) {
    LRPayouts[, year] <- maxPayout * LRSeasonPerc #make this reactive
  }else if (LRActualValue <= LRTrigger & LRActualValue > LRExit) {
    LRPayouts[, year] <- ((LRTrigger - LRActualValue) / (LRTrigger - LRExit)) * LRSeasonPerc
  }
}

##### Short Rains (SR) Season #####
# Get the dekads for the start and end dates of the SR season
SRstartDekad <- dekadMap(SRstartDate)
SRendDekad <- dekadMap(SRendDate)

# subset data
# if the season falls within the same year, subset normally
if (lubridate::year(lubridate::ymd(SRstartDate)) == lubridate::year(lubridate::ymd(SRendDate))) {
  SRNDVIdata <- stackedTZData[SRstartDekad:SRendDekad, ]
} else if (lubridate::year(lubridate::ymd(SRendDate)) > lubridate::year(lubridate::ymd(SRstartDate))) {
  # if season overlaps to the next year, adjust end dekad by 36
  SRendDekad <- SRendDekad + 36
  SRNDVIdata <- stackedTZData[SRstartDekad:SRendDekad, ]
}

# Map the dekadals column with the dekadMonthMap:
SRNDVIdata <- SRNDVIdata |> 
  dplyr::mutate(Month = dekadMonthMap[Dekad])

# Drop the `Dekad` column
SRNDVIdata <- SRNDVIdata |> 
  dplyr::select(-c("Dekad"))

# DATA WRANGLING
# Average dekadal NDVI to find monthly estimates (rounded off to 4 decimal 
# places)
# group data by month => gives monthly averages:

SRMonthlyDekadAverages <- SRNDVIdata |> 
  dplyr::group_by(Month) |> 
  dplyr::summarise_all("mean") |>
  dplyr::mutate(dplyr::across(everything(), round, 2)) |>
  dplyr::ungroup()

# drop the month column
SRMonthlyDekadAverages <- SRMonthlyDekadAverages |>
  dplyr::select(-c(Month))

# Aggregate SR season rainfall
SRSeasonalAggregate <- SRMonthlyDekadAverages |> 
  dplyr::summarise_all("sum") |>
  dplyr::mutate(dplyr::across(everything(), round, 2))

# Data normalization
SRSeasonalMean <- round(mean(as.numeric(SRSeasonalAggregate[1,])), digits = 4)

SRSeasonalStd <- round(sd(as.numeric(SRSeasonalAggregate[1,])), digits = 4)

# normalize the raw data
SRNormalizedNDVI <- as.data.frame((SRSeasonalAggregate - SRSeasonalMean) / SRSeasonalStd) |>
  dplyr::mutate(dplyr::across(everything(), round, 4))

# calculate exit based on the chosen percentile level
if (exitLevel == 'Minimum'){
  SRExit <- min(SRNormalizedNDVI)
} else if (exitLevel == '1st Percentile'){
  SRExit <- SRNormalizedNDVI |> quantile(probs = 0.01) |>
    round(digits = 4)
} else if (exitLevel == '5th Percentile'){
  SRExit <- SRNormalizedNDVI |> quantile(probs = 0.05) |>
    round(digits = 4)
}

# calculate the trigger based on the percentile passed
SRTrigger <- SRNormalizedNDVI |> quantile(probs = triggerLevel) |> 
  round(digits = 4)

# create a dataframe for the payouts
# make a copy of the LRNormalizedNDVI dataframe:
SRPayouts <- SRNormalizedNDVI

# calculate payouts:
for (year in 1:ncol(SRNormalizedNDVI)) {
  # extract value for calculating payout:
  SRActualValue <- SRNormalizedNDVI[, year]
  
  # calculate payout:
  if (SRActualValue > SRTrigger) {
    SRPayouts[, year] <- 0
  }else if (SRActualValue <= SRExit) {
    SRPayouts[, year] <- maxPayout * SRSeasonPerc #make this reactive
  }else if (SRActualValue <= SRTrigger & SRActualValue > SRExit) {
    SRPayouts[, year] <- ((SRTrigger - SRActualValue) / (SRTrigger - SRExit)) * SRSeasonPerc
  }
}

##### Visualize the payouts FOR LR and SR seasons
# merge the two dataframes
payoutsCombo <- rbind(LRPayouts, SRPayouts)
payoutsCombo <- LRPayouts
payoutsCombo[2, ] <- SRPayouts[1, ]
payoutsCombo <- payoutsCombo |>
  t() |>
  as.data.frame() |>
  tibble::rownames_to_column("Year") |>
  setNames(c("Year", "LR Season Payout", "SR Season Payout"))

# draw a stack bar graph for the long rains and short rains seasons payouts using plot_ly
fig <- plotly::plot_ly(payoutsCombo, x = ~Year,
                       y = ~`LR Season Payout`, type = 'bar', name = 'LR Season') |>
  plotly::add_trace(y = ~`SR Season Payout`, name = 'SR Season') |>
  plotly::layout(yaxis = list(title = 'Annual Payouts'),
                 barmode = 'stack',title="Historical Payouts")
fig

### Premium Calculation
# Calculate the total payout
totalPayouts <- LRPayouts + SRPayouts

# Calculate premium rate
bimodalPremiumRate <- round(mean(as.numeric(totalPayouts[1,])), digits = 2)


##################################################
