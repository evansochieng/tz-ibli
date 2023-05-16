# load data
rawNdvidata <- readr::read_csv("uaiData.csv")
rawNdvidata <- round(rawNdvidata[, 2:length(colnames(rawNdvidata))], digits = 4)

# set row names
rawNdvidata <- as.data.frame(rawNdvidata)
rownames(rawNdvidata) <- c("Ajaj", "Murhal", "Alaf", "Eena", "Daoul")

# read in data
ndviFile <- readr::read_csv(
  "SDN_IBLI_eMODIS_NDVI_200219_202136_DEKADAL_AGGREGATE.csv"
)

# drop the first column
ndviFile <- ndviFile[, -1]
