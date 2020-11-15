library(data.table)
library(ggplot2)
library(tidyr)

data <- fread(paste0("/Users/user/Desktop/R-ds/t2/QVI_data.csv"))

theme_set(theme_bw())
theme_update(plot.title = element_text(hjust = 0.5))

data[, YEARMONTH := year(DATE)*100 + month(DATE)]

measureOverTime <- data[, .(totalSales = uniqueN(TOT_SALES),
                            nCustomers = uniqueN(LYLTY_CARD_NBR),
                            nTxnPerCust = sum(TOT_SALES)/uniqueN(LYLTY_CARD_NBR),
                            nChipsPerTxn = sum(PROD_QTY)/uniqueN(LYLTY_CARD_NBR),
                            avgPricePerUnit = sum(TOT_SALES)/sum(PROD_QTY)), by = c("STORE_NBR","YEARMONTH")][order(YEARMONTH)]
####by = both STORE_NBR & YEARMONTH

####filter stores
storeWithFullObs <- unique(measureOverTime[, .N, STORE_NBR][N == 12, STORE_NBR])
preTrialMeasures <- measureOverTime[YEARMONTH < 201902 & STORE_NBR %in% storeWithFullObs]

####
calculateCorrelation <- function(inputTable, metricCol, storeComparison){
  calcCorrTable = data.table(Store1 = numeric(), Store2 = numeric(), corr_measure = numeric())
  
  storeNumbers <- unique(inputTable[, STORE_NBR])
    for (i in storeNumbers){
      calculateMeasure = data.table("store1" = storeComparison,
                                    "store2" = i, 
                                    "corr_measure" = cor(inputTable[STORE_NBR == storeComparison, eval(metricCol)], 
                                                         inputTable[STORE_NBR == i, eval(metricCol)]))
      calcCorrTable <- rbind(calcCorrTable, calculateMeasure, fill=TRUE)
    }
  return(calcCorrTable)
}

####a function to calculate a standardised magnitude distance for a measure, looping thru each
####store
calculateMagnitudeDistance <- function(inputTable, metricCol, storeComparison)
{
  calcDistTable = data.table(Store1 = numeric(), store2 = numeric(), YEARMONTH = numeric(), measure = numeric())
  
  storeNumbers <- unique(inputTable[, STORE_NBR])
  for (i in storeNumbers){
    calculatedMeasure = data.table("Store1" = storeComparison,
                                   "Store2" = i, "YEARMONTH" = inputTable[STORE_NBR == storeComparison, YEARMONTH],
                                   "measure" = abs(inputTable[STORE_NBR == storeComparison, eval(metricCol)] 
                                                   - inputTable[STORE_NBR == i, eval(metricCol)]))
    calcDistTable <- rbind(calcDistTable, calculatedMeasure, fill=TRUE)
  }
  ####standardise the magnitude distance so that the measure ranges from 0 to 1
    minMaxDist <- calcDistTable[, .(minDist = min(measure), maxDist = max(measure)), by = c("Store1","YEARMONTH")]
    distTable <- merge(calcDistTable, minMaxDist, by = c("Store1", "YEARMONTH"))
    distTable[, magnitudeMeasure := 1 - (measure - minDist)/(maxDist - MinDist)]
    finalDistTable <- distTable[, .(mag_measure = mean(magnitudeMeasure)), by = .(Store1, Store2)]
    return(finalDistTable)
}

trail_store <- 77
corr_nSales <- calculateCorrelation(preTrialMeasures, quote(totalSales),
                                    trial_store)
corr_nCustomers <- calculateCorrelation(preTrialMeasures, quote(nCustomers),
                                    trial_store)

magnitude_nSales <- calculateMagnitudeDistance(preTrialMeasures, quote(totalSales), trail_store)
magnitude_nCustomers <- calculateMagnitudeDistance(preTrialMeasures, quote(nCustomers), trail_store)

####Create a combined score composed of correlation and magnityde
corr_weight <- 0.5
score_nSale <- merge(corr_nSales, magnitude_nSales,
                     by = c("Store1","Store2"))[, scoreNSales := corr_measure *corr_weight + mag_measure * (1 - corr_weight)]

