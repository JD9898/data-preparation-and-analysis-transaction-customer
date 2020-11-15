
data <- fread(paste0("/Users/user/Desktop/R-ds/QVI_data.csv"))
str(data)

#total sales by different LIFESTAGE and PREMIUM_CUSTOMER categories
####error happens here:
sales <- data[, .(SALES = sum(TOT_SALES,na.rm=TRUE)), 
              .(LIFESTAGE, PREMIUM_CUSTOMER)]

#create plot
p <- ggplot(data = sales) + 
  geom_mosaic(aes(weight = SALES, 
                  x = product(PREMIUM_CUSTOMER, LIFESTAGE), 
                  fill = PREMIUM_CUSTOMER)) + 
                  labs(x = "Lifestage", y = "Premium customer flag", 
                       title = "Proportion of sales") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))  
#plot and label with proportion of sales
p + geom_text(data = ggplot_build(p)$data[[1]],
              aes(x = (xmin + xmax)/2, y = (ymin + ymax)/2), 
              label = as.character((paste(round(.wt/sum(.wt),3)*100,'%'))))

#### Number of customers by LIFESTAGE and PREMIUM_CUSTOMER
customers <- data[, .(CUSTOMERS = uniqueN(LYLTY_CARD_NBR)), 
                  .(LIFESTAGE, PREMIUM_CUSTOMER)][order(-CUSTOMERS)]

#### Create plot
p <- ggplot(data = customers) +
  geom_mosaic(aes(weight = CUSTOMERS, 
                  x = product(PREMIUM_CUSTOMER, LIFESTAGE), 
                  fill = PREMIUM_CUSTOMER)) + 
                  labs(x = "Lifestage", y = "Premium customer flag", 
                  title = "Proportion of customers") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
#### Plot and label with proportion of customers
p + geom_text(data = ggplot_build(p)$data[[1]],
              aes(x = (xmin + xmax)/2 , y = (ymin + ymax)/2, 
                  label = as.character(paste(round(.wt/sum(.wt),3)*100,'%')))) 


#### Average number of units per customer by LIFESTAGE and PREMIUM_CUSTOMER
avg_units <- data[, .(AVG = sum(PROD_QTY,na.rm=TRUE)/uniqueN(LYLTY_CARD_NBR)),
                  .(LIFESTAGE, PREMIUM_CUSTOMER)][order(-AVG)]

#### Create plot
ggplot(data = avg_units, aes(weight = AVG, x = LIFESTAGE, fill = 
                               PREMIUM_CUSTOMER)) + geom_bar(position = position_dodge()) + 
                               labs(x = "Lifestage", y = "Avg units per transaction", 
                               title = "Units per customer") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

#### Average price per unit by LIFESTAGE and PREMIUM_CUSTOMER
avg_price <- data[, .(AVG = sum(TOT_SALES,na.rm=TRUE)/sum(PROD_QTY,na.rm=TRUE)),
                  .(LIFESTAGE, PREMIUM_CUSTOMER)][order(-AVG)]
#### Create plot
ggplot(data = avg_price, aes(weight = AVG, x = LIFESTAGE, 
                             fill = PREMIUM_CUSTOMER)) + 
  geom_bar(position = position_dodge()) + 
  labs(x = "Lifestage", y = "Avg price per unit", title = "Price per unit") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

#### Perform an independent t‐test between mainstream vs premium and budget midage and
#### young singles and couples
pricePerUnit <- data[, price := TOT_SALES/PROD_QTY]
t.test(data[LIFESTAGE %in% c("YOUNG SINGLES/COUPLES", "MIDAGE SINGLES/COUPLES")
            & PREMIUM_CUSTOMER == "Mainstream", price], 
       data[LIFESTAGE %in% c("YOUNG SINGLES/COUPLES", "MIDAGE SINGLES/COUPLES")& 
              PREMIUM_CUSTOMER != "Mainstream", price], alternative = "greater")

#mainstream young couples/singles: favorite brands??
#### Deep dive into Mainstream, young singles/couples
segment1 <- data[LIFESTAGE == "YOUNG SINGLES/COUPLES" & PREMIUM_CUSTOMER ==
                   "Mainstream",]
other <- data[!(LIFESTAGE == "YOUNG SINGLES/COUPLES" & PREMIUM_CUSTOMER ==
                  "Mainstream"),]
#### Brand affinity compared to the rest of the population
quantity_segment1 <- segment1[, sum(PROD_QTY,na.rm=TRUE)]
quantity_other <- other[, sum(PROD_QTY,na.rm=TRUE)]
quantity_segment1_by_brand <- 
  segment1[, .(targetSegment = sum(PROD_QTY,na.rm=TRUE)/quantity_segment1), by = BRAND]
quantity_other_by_brand <- 
  other[, .(other = sum(PROD_QTY,na.rm=TRUE)/quantity_other), by = BRAND]
brand_proportions <- merge(quantity_segment1_by_brand, quantity_other_by_brand)[, affinityToBrand := targetSegment/other]
brand_proportions[order(-affinityToBrand)]

####preferred size of chips compared to the rest
quantity_segment1_by_pack <- 
  segment1[, .(targetSegment = sum(PROD_QTY,na.rm=TRUE)/quantity_segment1), by = PACK_SIZE]
quantity_other_by_pack <- 
  other[, .(other = sum(PROD_QTY,na.rm=TRUE)/quantity_other), by = PACK_SIZE]
brand_proportions <- merge(quantity_segment1_by_pack, quantity_other_by_pack)[, affinityToPack := targetSegment/other]
brand_proportions[order(-affinityToPack)]

####brands that sell this size
data[PACK_SIZE == 270, unique(PROD_NAME)]
