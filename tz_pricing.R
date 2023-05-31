##### Pricing Tanzania #####

# Load data
tzData <- readr::read_csv("eVIIRS_Garoowe.csv", show_col_types = FALSE)
head(tzData)

# Handle missing values
# Check for missing values
colSums(is.na(tzData))
# 2002 and 2022 have missing values

# Drop the first column because it has several missing values
tzData <- tzData |>
  dplyr::select(-c("2002"))

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


###### Morogoro Pricing ######
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
