payoutCalculator <- function(ndviFile, uai, pattern, triggerlevel, exitlevel, maxPayout, sumInsured) {
  # convert all uai name to lower case:
  uaiName <- tolower(uai)
  
  # use if else to assign map former uais to the current 5:
  uaiList <- list(
    ajaj = c("North Darfur", "North Kordofan"),
    murhal = c("West Darfur", "West Kordofan"),
    alaf = c("South Darfur", "Central Darfur"),
    eena = c("South Kordofan"),
    daoul = c("East Darfur", "Abyaei PCA")
  )
  
  formerUaiList <- uaiList[[uaiName]]
  
  # subset and find average for the uai:
  uaiData <- ndviFile |> 
    dplyr::filter(ADM1_EN %in% formerUaiList)
  
  uaiNDVI <- uaiData |> 
    dplyr::select(-c("ADM0_PCODE", "ADM1_EN", "ADM2_EN", "PIXELS", "UAI")) |> 
    colMeans()
  
  # get index of the dataframe:
  YYYYDK <- names(uaiNDVI)
  
  # add this to the raw dataframe + year part, dekadal part:
  uaiNDVI <- data.frame(
    YYYYDK = YYYYDK,
    `X0` = uaiNDVI
  ) |> 
    dplyr::mutate(
      Year = substr(x = YYYYDK, start = 1, stop = 4),
      Dekad = substr(x = YYYYDK, start = 5, stop = 6)
    )
  
  dekadMonthMap <- rep(x = 1:12, each = 3)
  names(dekadMonthMap) <- 1:36 |> sprintf(fmt = "%02d")
  
  # Map the dekadals column with the dekadMonthMap:
  uaiNDVI <- uaiNDVI |> 
    dplyr::mutate(Month = dekadMonthMap[Dekad])
  
  # Create a dictionary to map months to the seasons they fall
  # WT: Wet season, DS: Dry season
  monthSeasonMap = c(
    rep(x = "DS", times = 6), rep(x = "WS", times = 4), rep(x = "DS", times = 2)
  )
  names(monthSeasonMap) <- 1:12
  
  
  # map the months column with the monthSeasonMap:
  uaiNDVI <- uaiNDVI |> 
    dplyr::mutate(SEASON = monthSeasonMap[Month])
  
  
  uaiRawData <- uaiNDVI |> 
    # subset raw data values and the months, this will help in finding the 
    # monthly averages:
    dplyr::select(-c("YYYYDK", "Dekad"))
  
  
  # DATA WRANGLING
  # Average decadal UAI NDVI to find monthly estimates (rounded off to 2 decimal 
  # places)
  # group data by month, year and season => gives monthly averages:
  monthlyDekadAverages <- uaiRawData |> 
    dplyr::group_by(Month, Year, SEASON) |> 
    dplyr::summarise(`X0` = mean(`X0`))
  
  # round off the values to 2dp:
  monthlyDekadAverages <- monthlyDekadAverages |> 
    dplyr::mutate(`X0` = round(`X0`, digits = 2))
  
  # cumulate the data by wet season months (specific to Sudan ie. July to Oct)
  # Group the data by year and season
  # I had already mapped the months as either wet season or dry season:
  monthlyDekadAverages <- monthlyDekadAverages |> 
    dplyr::ungroup() |>
    dplyr::select(-c("Month"))
  
  seasonalAggregate <- monthlyDekadAverages |> 
    dplyr::group_by(Year, SEASON) |> 
    dplyr::summarise(`X0` = round(sum(`X0`), digits = 2)) |>
    dplyr::ungroup()
  
  # data normalization:
  seasonalMean <- seasonalAggregate |> 
    dplyr::select(-c("Year")) |> 
    dplyr::group_by(SEASON) |> 
    dplyr::summarise(`X0` = round(mean(`X0`), digits = 2))
  # rename column:
  names(seasonalMean)[2] <- uaiName
  
  seasonalStd <- seasonalAggregate |> 
    dplyr::select(-c("Year")) |> 
    dplyr::group_by(SEASON) |> 
    dplyr::summarise(`X0` = round(sd(`X0`), digits = 2))
  
  # rename column:
  names(seasonalStd)[2] <- uaiName
  
  # create a list of seasons:
  seasons <- c("DS", "WS")
  
  # make a copy of the original seasonal aggregate dataframe:
  normalizedNDVI <- seasonalAggregate
  
  # rownames(normalizedNDVI) <- normalizedNDVI$SEASON
  
  names(normalizedNDVI)[names(normalizedNDVI) == "X0"] <- uaiName
  normalizedNDVI <- as.data.frame(normalizedNDVI)
  
  # normalize data:
  for (season in seasons) {
    # normalizedNDVI |> 
    #   dplyr::filter(SEASON %in% season) |> 
    #   dplyr::select(dplyr::all_of(uaiName)) |> 
    #   
    
    normalizedNDVI[normalizedNDVI$SEASON == season, uaiName] <- 
      round(
        {
          (normalizedNDVI[normalizedNDVI$SEASON == season, uaiName, drop=TRUE] - 
             seasonalMean[seasonalMean$SEASON == season, uaiName, drop=TRUE]
          ) / 
            (
              seasonalStd[seasonalStd$SEASON == season, uaiName, drop=TRUE]
            )
        },
        digits = 2
      )
  }
  
  # clean the dataframe with the normalized NDVI values:
  wsNormalizedNDVI <- normalizedNDVI |> 
    dplyr::filter(SEASON != "DS")
  
  # payout calculation
  # # calculate exit (minimum value across the years):
  # exit <- min(wsNormalizedNDVI[[uaiName]])
  
  # calculate exit based on it
  if (exitlevel == 'Minimum'){
    exit <- min(wsNormalizedNDVI[[uaiName]])
  } else if (exitlevel == '1st Percentile'){
    exit <- wsNormalizedNDVI[[uaiName]] |> quantile(probs = 0.01) |>
      round(digits = 2)
  } else if (exitlevel == '5th Percentile'){
    exit <- wsNormalizedNDVI[[uaiName]] |> quantile(probs = 0.05) |>
      round(digits = 2)
  }
  
  # calculate the trigger based on the percentile passed:
  trigger <- wsNormalizedNDVI[[uaiName]] |> quantile(probs = triggerlevel) |> 
    round(digits = 2)
  
  # Create a dataframe for trigger and exit
  triEx <- data.frame(uai, pattern, trigger, exit, sumInsured, maxPayout)
  colnames(triEx) <- c("UAI", "Rainfall Pattern", "Trigger", "Exit", "Sum Insured", "Maximum Payout")
  
  # create a dataframe to store the payouts.
  # make a copy of the wsNormalizedNDVI dataframe:
  wsPayouts <- wsNormalizedNDVI
  
  # calculate payouts:
  for (year in wsNormalizedNDVI$Year) {
    # extract value for calculating payout:
    actualValue <- wsNormalizedNDVI |> 
      dplyr::filter(Year == year) |> 
      dplyr::pull(var = dplyr::all_of(uaiName))
    
    # calculate payout:
    if (actualValue > trigger) {
      wsPayouts[wsPayouts$Year == year, uaiName] <- 0
    }else if (actualValue <= exit) {
      wsPayouts[wsPayouts$Year == year, uaiName] <- maxPayout #make this reactive
    }else if (actualValue <= trigger & actualValue > exit) {
      wsPayouts[wsPayouts$Year == year, uaiName] <- (trigger - actualValue) / (trigger - exit)
    }
  }
  
  # rename the last column
  names(wsPayouts)[3] = "Payouts"
  
  # Calculate premium rate
  premiumRate <- round(mean(wsPayouts$Payouts), digits = 4)
  
  # Extract the payouts and transpose
  claims <- wsPayouts
  claims <- claims |>
    dplyr::mutate(`Payouts` = round(`Payouts`, digits = 4)) |>
    dplyr::select(-c("SEASON")) |> 
    dplyr::mutate(group = 1) |>
    tidyr::spread(Year, Payouts) |>
    dplyr::select(-c("group")) 
  
  
  # return the payouts dataframe
  return(
    list(
      payouts = wsPayouts,
      premiumRate = premiumRate,
      claims = claims,
      triEx = triEx
    )
  )
}
