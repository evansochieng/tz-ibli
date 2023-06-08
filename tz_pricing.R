##### Pricing Tanzania #####

# Load data
tzData <- readr::read_csv("eVIIRS_Garoowe.csv", show_col_types = FALSE)
head(tzData)

# Handle missing values
# Check for missing values
colSums(is.na(tzData))
# 2002 and 2022 have missing values

# # Drop the first column because it has several missing values
# tzData <- tzData |>
#   dplyr::select(-c("2002"))

# Fill the missing values in 2002 with zeros
tzData$`2002`[is.na(tzData$`2002`)] <- 0
# Fill the two missing values in 2022 column with mean value
tzData$`2022`[is.na(tzData$`2022`)] <- mean(tzData$`2022`, na.rm=TRUE)

# create a column with dekads
tzData <- tzData |>
  dplyr::mutate(
    Dekad = as.numeric(rownames(tzData)) |> sprintf(fmt = "%02d")
  )

### Map the dekads to their respective months ###
# create a map
dekadMonthMap <- rep(x = 1:12, each = 3)
names(dekadMonthMap) <- 1:36 |> sprintf(fmt = "%02d")

# Map the dekadals column with the dekadMonthMap:
tzData <- tzData |> 
  dplyr::mutate(Month = dekadMonthMap[Dekad])

# Drop the `Dekad` column
tzData <- tzData |> 
  dplyr::select(-c("Dekad"))

# define other key parameters
triggerLevel <- 0.25
exitLevel <- "5th Percentile"
maxPayout <- 1
LRSeasonPerc <- 0.58
SRSeasonPerc <- 0.42


###### Tanga Pricing ######
# define start month
LRstartMonth <- "March" |> match(month.name)
LRendMonth <- "June" |> match(month.name)
SRstartMonth <- "October" |> match(month.name)
SRendMonth <- "December" |> match(month.name)

# subset long rain season data
LRData <- tzData |>
  dplyr::filter(dplyr::between(Month, LRstartMonth, LRendMonth))

# subset short rain season data
SRData <- tzData |>
  dplyr::filter(dplyr::between(Month, SRstartMonth, SRendMonth))

# DATA WRANGLING
# Average dekadal NDVI to find monthly estimates (rounded off to 2 decimal 
# places)
# group data by month => gives monthly averages:

#### Long Rains Season
LRMonthlyDekadAverages <- LRData |> 
  dplyr::group_by(Month) |> 
  dplyr::summarise_all("mean") |>
  dplyr::mutate(dplyr::across(everything(), round, 2)) |>
  dplyr::ungroup()

# drop the month column
LRMonthlyDekadAverages <- LRMonthlyDekadAverages |>
  dplyr::select(-c(Month))

# Aggregate LR season rainfall
LRSeasonalAggregate <- LRMonthlyDekadAverages |> 
  dplyr::summarise_all("sum") |>
  dplyr::mutate(dplyr::across(everything(), round, 2))

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
####################################################################

#### Short Rains Season
SRMonthlyDekadAverages <- SRData |> 
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
################################################################

##### Visualize the payouts
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
premiumRate <- round(mean(as.numeric(totalPayouts[1,])), digits = 2)


##### Morogoro Pricing ######

# define the rainy season
RSstartMonth <- "November" |> match(month.name)
RSendMonth <- "May" |> match(month.name)

RSendYearMonth <- "December" |> match(month.name)
RSstartYearMonth <- "January" |> match(month.name)

# define an array to store the rainy season NDVI values
RSNdvi <- data.frame(matrix(ncol = 0, nrow = 21))
RSMonths <- c()

for (year in 1:(ncol(tzData)-2)) {
  # get the current year and the next year
  currentYear <- colnames(tzData)[year]
  nextYear <- colnames(tzData)[year + 1]
  
  # extract data for the current year
  curYrNDVI <- tzData[c(currentYear, "Month")] |>
    dplyr::filter(dplyr::between(Month, RSstartMonth, RSendYearMonth))
  # extract data for the next year
  nxtYrNDVI <- tzData[c(nextYear, "Month")] |>
    dplyr::filter(dplyr::between(Month, RSstartYearMonth, RSendMonth)) |>
    `colnames<-`(c(currentYear, "Month"))
  
  # merge the two subsets to have ndvi for whole season at once
  curYearSeasonNDVI <- rbind(curYrNDVI, nxtYrNDVI)
  
  # create a dataframe to have all the historical data for the rainy season
  RSNdvi[, currentYear] <- curYearSeasonNDVI[[currentYear]]
  
  RSMonths <- curYearSeasonNDVI$Month
}

# add the months column to the dataframe
RSNdvi$Month <- RSMonths
