claimCalculator <- function(
    stackedTZData, uai, pattern, triggerLevel, exitLevel, maxPayout, sumInsured,
    RSstartDate, RSendDate, LRstartDate, LRendDate,
    SRstartDate, SRendDate
    ) {
  if (uai == "Tanga") {
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
    
    # Aggregate LR season rainfall
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
    
    ### Outputs ###
    # Create a dataframe for summary of the parameters
    sumParameters <- data.frame(uai, pattern, sumInsured, maxPayout)
    colnames(sumParameters) <- c("UAI", "Rainfall Pattern", "Sum Insured", "Maximum Payout")
    
    # draw a stack bar graph for the long rains and short rains seasons payouts using plot_ly
    biFig <- plotly::plot_ly(payoutsCombo, x = ~Year,
                           y = ~`LR Season Payout`, type = 'bar', name = 'LR Season') |>
      plotly::add_trace(y = ~`SR Season Payout`, name = 'SR Season') |>
      plotly::layout(yaxis = list(title = 'Annual Payouts'),
                     barmode = 'stack',title="Historical Payouts")
    
    ### Premium Calculation
    # Calculate the total payout
    totalPayouts <- LRPayouts + SRPayouts
    
    # display the total payouts to users
    biClaims <- totalPayouts
    
    # Calculate premium rate
    bimodalPremiumRate <- round(mean(as.numeric(totalPayouts[1,])), digits = 2)
    
    # return the list of frontend outputs
    return(
      list(
        payoutsPlotData = payoutsCombo,
        premiumRate = bimodalPremiumRate,
        claims = biClaims,
        sumParameters = sumParameters
      )
    )
    
  } else if (uai == "Morogoro") {
    ### Unimodal pricing ###
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
    
    ### Outputs ###
    
    # Create a dataframe for summary of the parameters
    sumParameters <- data.frame(uai, pattern, sumInsured, maxPayout)
    colnames(sumParameters) <- c("UAI", "Rainfall Pattern", "Sum Insured", "Maximum Payout")
    
    
    # draw a bar graph for the rainy season payouts using plot_ly
    RSFig <- plotly::plot_ly(RSPayouts, x = ~Year,
                             y = ~`RS Payouts`, type = 'bar', name = 'Rainy Season') |>
      plotly::layout(yaxis = list(title = 'Annual Payouts'), 
                     title="Historical Payouts")
    
    # transpose actual payouts for display to users
    uniClaims <- RSPayouts
    uniClaims <- uniClaims |>
      dplyr::mutate(`RS Payouts` = round(`RS Payouts`, digits = 4)) |>
      dplyr::mutate(group = 1) |>
      tidyr::spread(Year, `RS Payouts`) |>
      dplyr::select(-c("group"))
    
    
    # Calculate premium rate
    unimodalPremiumRate <- round(mean(as.numeric(RSPayouts[, "RS Payouts"])), digits = 2)
  }
  
  # return the list of frontend outputs
  return(
    list(
      payoutsPlotData = RSPayouts,
      premiumRate = unimodalPremiumRate,
      claims = uniClaims,
      sumParameters = sumParameters
    )
  )
}