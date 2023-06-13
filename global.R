# # load data
# rawNdvidata <- readr::read_csv("uaiData.csv")
# rawNdvidata <- round(rawNdvidata[, 2:length(colnames(rawNdvidata))], digits = 4)
# 
# # set row names
# rawNdvidata <- as.data.frame(rawNdvidata)
# rownames(rawNdvidata) <- c("Ajaj", "Murhal", "Alaf", "Eena", "Daoul")
# 
# # read in data
# ndviFile <- readr::read_csv(
#   "SDN_IBLI_eMODIS_NDVI_200219_202136_DEKADAL_AGGREGATE.csv"
# )
# 
# # drop the first column
# ndviFile <- ndviFile[, -1]


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
  
  # convert the date to the right format
  date <- as.Date(date, format = "%d-%m-%Y")
  
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

# define other key fixed parameters
LRSeasonPerc <- 0.58
SRSeasonPerc <- 0.42
