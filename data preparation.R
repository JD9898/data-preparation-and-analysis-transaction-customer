## Load required libraries and datasets
#Note that you will need to install these libraries if you have never used these before. 
#{r 0 Load Libraries, results = 'hide'}


#### Load required libraries 
library(data.table)
library(ggplot2) 
library(ggmosaic) 
library(readr) 


transactionData <- fread("/Users/user/Desktop/R-ds/QVI_transaction_data.csv") 
customerData <- fread("/Users/user/Desktop/R-ds/QVI_purchase_behaviour.csv") 

transactionData$DATE <- as.Date(transactionData$DATE,origin = "1899-12-30")

productWords <- data.table(unlist(strsplit(unique(transactionData[,PROD_NAME])," ")))
setnames(productWords, 'words')

##remove digits
productWords <- productWords[grepl("\\d", words) == FALSE, ]
productWords <- productWords[grepl("[:alpha:]", words), ]#leave only letters
productWords[, .N, words][order(N, decreasing = TRUE)]#.N:counting
#productWords[, .N, words][oder(-N)]

#remoeve salsa products
transactionData[, SALSA:=grepl("salsa",tolower(PROD_NAME))]
transactionData <- transactionData[SALSA == FALSE, ][, SALSA := NULL]

summary(transactionData)

#investigate the 200 packs transaction
transactionData[PROD_QTY == 200, ]
transactionData[LYLTY_CARD_NBR == 226000, ]
transactionData <- transactionData[LYLTY_CARD_NBR != 226000, ]

summary(transactionData)
transactionData[, .N, by = DATE]
transactionData[, .N, transactionData$DATE][order(N)]#counting transactions each day

allDates <- data.table(seq(as.Date("2018/07/01"), as.Date("2019/06/30"),by = "day"))
setnames(allDates,"DATE")
transactions_by_day <- merge(allDates, transactionData[, .N, by = DATE], all.x = TRUE)

theme_set(theme_bw())
theme_update(plot.title = element_text(hjust = 0.5))

ggplot(transactions_by_day, aes(x = DATE, y = N)) + geom_line() +
  labs(x = "Day", y = "Number of transactions", title = "Transactions over
↪ time") +
  scale_x_date(breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

##zoom in to december 
ggplot(transactions_by_day[month(DATE) == 12, ], aes(x = DATE, y = N)) + geom_line() +
  labs(x = "Day", y = "Number of transactions", title = "Transactions over
↪ time") +
  scale_x_date(breaks = "1 day") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

#packsize
transactionData[, PACK_SIZE:= parse_number(PROD_NAME)]
transactionData[, .N, PACK_SIZE][order(PACK_SIZE)]

hist(transactionData[, PACK_SIZE])
##Brands

transactionData[, BRAND := toupper(substr(PROD_NAME, 1, 
                                          regexpr(pattern = ' ', 
                                                  PROD_NAME) -1))]
transactionData[, .N, by=BRAND][order(-N)]

#combine brand names, clean data

transactionData[BRAND == 'RED', BRAND:='RED']
transactionData[BRAND == "SNBTS", BRAND := "SUNBITES"]
transactionData[BRAND == "INFZNS", BRAND := "INFUZIONS"]
transactionData[BRAND == "WW", BRAND := "WOOLWORTHS"]
transactionData[BRAND == "SMITH", BRAND := "SMITHS"]
transactionData[BRAND == "NCC", BRAND := "NATURAL"]
transactionData[BRAND == "DORITO", BRAND := "DORITOS"]
transactionData[BRAND == "GRAIN", BRAND := "GRNWVES"]
transactionData[, .N, by=BRAND][order(-N)]

#customer data
str(customerData)
summary(customerData)
customerData[, .N, LIFESTAGE][order(-N)]
customerData[, .N, PREMIUM_CUSTOMER][order(-N)]

transactions_by_day <- merge(allDates, transactionData[, .N, by = DATE], all.x = TRUE)

##merge transaction data and customer data
data <- merge(customerData, transactionData, all.x = TRUE)
#data <- merge(customerData, transactionData[, .N, by = LYLTY_CARD_NBR], all.x=TRUE)

data[is.null(LIFESTAGE), .N]
data[is.null(PREMIUM_CUSTOMER), .N]

fwrite(data, paste0("/Users/user/Desktop/R-ds/QVI_data.csv"))
