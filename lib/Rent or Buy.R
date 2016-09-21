library("dplyr")
library("data.table")
library("ggplot2")
library("choroplethr")

source('lib/utilities.R')
#########################
# rent vs own ratio map #
#########################
#reRead <- 1
#pCols <- c("PINCP", "ST","SERIALNO","AGEP")
#hCols <- c("TEN","SERIALNO")
p14 <- readSubData('pPincpStSerialnoAgep14')
h14 <- readSubData('hTenSerialno14')

p14 <- tbl_df(p14) 
h14 <- tbl_df(h14) 
popHouse <- left_join(p14 , h14, by.x=c("SERIALNO"))
rm(p14,h14)

#Which states have highest rent/buy ratio
#first take a look at how many missing value of house type in NY(36) and CA(06)
popHouse %>%
  filter(ST %in% c(6,36)) %>%
  group_by(ST) %>%
  dplyr::summarise(missingProp = sum(is.na(TEN))/length(TEN))
# I think these props are acceptable and comparable

stateRent <- popHouse %>%
    group_by(ST) %>%
    filter(TEN==3) %>%
    dplyr::summarise(rent=n())
stateOwn <- popHouse %>%
    group_by(ST) %>%
    filter(TEN %in% c(1,2)) %>%
    dplyr::summarise(own=n())
stCodes <- stateCodes()

RentVSOwn <- stateRent %>%
  right_join(stateOwn, by.x=c("ST"))
rentRatio<- RentVSOwn %>%
  mutate(value = rent/(own+rent)) %>%
  select(ST,value) %>%
  left_join(stCodes,by.x=c("ST"))

state_choropleth(rentRatio, title = "Percentage of People who Rent", num_colors=9)

#top10 states people rent most
topRent <- rentRatio %>%
  arrange(-value) %>%
  slice(1:10)
#topRent

###################
# house price map #
###################
#reRead <- 1
#hCols <- c("ST","VALP","RNTP","ADJHSG")

#h14 <- readSubData('ss14hus','hStValpRntpAdjhsg14',hCols,reRead)
h14 <- readSubData('hStValpRntpAdjhsg14')
HouseRentValue <- tbl_df(h14)
rm(h14)

medHouseValue <- HouseRentValue %>%
  group_by(ST) %>%
  select(ST,VALP) %>%
  na.omit() %>%
  dplyr::summarise(medPrice=median(VALP)) %>%
  left_join(stCodes,by.x=c("ST")) %>%
  arrange(-medPrice)
colnames(medHouseValue)[2] <- 'value'
state_choropleth(medHouseValue, title = "Median House Price by State", num_colors=9)
#I haven't expect it...NJ's house price is higher than NY and CA has higher rent ratio than NY...maybe it's due to new york state is too large? I wish I could restrict on manhattan!

medRent <- HouseRentValue %>%
  group_by(ST) %>%
  select(ST,RNTP) %>%
  na.omit() %>%
  dplyr::summarise(medRent=median(RNTP)) %>%
  left_join(stCodes,by.x=c("ST")) %>%
  arrange(-medRent)
colnames(medRent)[2] <- 'value'
state_choropleth(medRent, title = "Median Monthly Rent by State", num_colors=9)
# Are people working in NYC living in NJ and who working in DC living in MD contribute to such results?


avgPIncome <- popHouse %>%
  group_by(ST) %>%
  select(ST,PINCP,AGEP) %>%
  filter(AGEP>21) %>%
  na.omit() %>%
  dplyr::summarise(avgPersonIncome=mean(PINCP)) %>%
  left_join(stCodes,by.x=c("ST")) %>%
  arrange(-avgPersonIncome)
colnames(avgPIncome)[2] <- 'value'
state_choropleth(avgPIncome, title = "Average Personal Income by State", num_colors=9)


HouseToRent <- medRent %>%
  select(ST,value,region) %>%
  left_join(medHouseValue,by.x=c("ST")) %>%
  mutate(value=medHouseValue$value/value)
state_choropleth(HouseToRent, title = "House Price to Rent Cost Ratio", num_colors=9)
topShouldRent <- HouseToRent %>% # we assume that if house price is relatively much higher than rent than people tend to rent
  arrange(-value) %>%
  slice(1:10)
#why do people in NV not want to buy a house? it's cheap relative to rent! maybe it's due to the very fluctuation in house price among those years so that people don't trust in house investment anymore?
# I will draw house/rent vs time plots for every states to see if it is correct.
#maybe it's a latent response? let's plot rent ratio in the same picture to contrast, too.

########################################################################
# Education, Culture, Age                                              #
# what influence one's willingness to rent other than housePrice/rent? #
########################################################################
medAge <- popHouse %>%
  group_by(ST) %>%
  select(ST,AGEP) %>%
  na.omit() %>%
  dplyr::summarise(medAge = median(AGEP)) %>%
  left_join(stCodes,by.x=c("ST")) %>%
  arrange(medAge)
colnames(medAge)[2] <- 'value'
state_choropleth(medAge, title = "Median Age by State", num_colors=9)

ageRent <- popHouse %>%
  group_by(AGEP) %>%
  filter(TEN==3) %>%
  dplyr::summarise(rent=n())

ageOwn <- popHouse %>%
  group_by(AGEP) %>%
  filter(TEN %in% c(1,2)) %>%
  dplyr::summarise(own=n())


ageRentVSOwn <- ageRent %>%
  right_join(ageOwn, by.x=c("AGEP"))
ageRentRatio<- ageRentVSOwn %>%
  mutate(rentRatio = rent/(own+rent)) %>%
  select(AGEP,rentRatio) 

ggplot(ageRentRatio,aes(x = AGEP, y = rentRatio)) + geom_point() +
  ggtitle("Rent Ratio against Age") + labs(x = "Age",y="Rent Ratio") 

##################################
# rent ratio and marriage status #
##################################
#reRead <- 1
#pCols <- c("MAR","SERIALNO")
#marHouse <- readSubData('ss14pus','pMarSerialno14',pCols,reRead)
marHouse <- readSubData('pMarSerialno14')
marHouse <- tbl_df(marHouse)
h14 <- readSubData('hTenSerialno14')
marHouse <- marHouse %>%
  left_join(h14,by.x="SERIALNO")
#rm(h14)

marRent <- marHouse %>%
  group_by(MAR) %>%
  filter(TEN==3) %>%
  dplyr::summarise(rent=n())

marOwn <- marHouse %>%
  group_by(MAR) %>%
  filter(TEN %in% c(1,2)) %>%
  dplyr::summarise(own=n())

marRentVSOwn <- marRent %>%
  right_join(marOwn, by.x=c("MAR"))
marRentRatio<- marRentVSOwn %>%
  mutate(rentRatio = rent/(own+rent)) %>%
  select(MAR,rentRatio) 
marRentRatio$MAR <- as.factor(marRentRatio$MAR)
levels(marRentRatio$MAR) <- c("Marriaged","Windowed","Divorced","Separated","Never Marriaged or too Young")
ggplot(marRentRatio,aes(x = MAR, y = rentRatio)) + geom_bar(aes(fill = MAR),stat="identity")+ggtitle("Rent Ratio against Marriage Status") + labs(x = "Marriage Status",y="Rent Ratio") 

#################################
# living place vs working place #
#################################
#reRead <- 1
#pCols <- c("POWSP","ST","SERIALNO")
#wrkPlace <- readSubData('ss14pus','pPowspStSerialno14',pCols,reRead)
wrkPlace <- readSubData('pPowspStSerialno14')
wrkPlace <- tbl_df(wrkPlace)
#h14 <- readSubData('hTenSerialno14')
wrkPlace <- wrkPlace %>%
  left_join(h14,by.x="SERIALNO")
#rm(h14)

###################
# temporal trends #
###################
# read related data from 2000 to 2013
# year <- '06'
# reRead <- 1
# pCols <- c("PINCP", "ST","SERIALNO","AGEP")
# hCols <- c("TEN","SERIALNO")
# pOutputFile <- paste('pPincpStSerialnoAgep',year,sep='')
# hOutputFile <- paste('hTenSerialno',year,sep='')
# pInputFile <- paste('ss',year,'pus',sep='')
# hInputFile <- paste('ss',year,'hus',sep='')
# p <- readSubData(pInputFile,pOutputFile,pCols,reRead)
# h <- readSubData(hInputFile,hOutputFile,hCols,reRead)
# #reRead <- 1
# hCols <- c("ST","VALP","RNTP","ADJINC")
# hOutputFile <- paste('hStValpRntpAdjinc',year,sep='')
# 

# year <- '00'
# reRead <- 1
# pCols <- c("PINCP", "ST","SERIALNO","AGEP")
# hCols <- c("TEN","SERIALNO")
# pOutputFile <- paste('pPincpStSerialnoAgep',year,sep='')
# hOutputFile <- paste('hTenSerialno',year,sep='')
# pInputFile <- paste('ss',year,'pus',sep='')
# hInputFile <- paste('ss',year,'hus',sep='')
# p <- readSubDataBefore05(pInputFile,pOutputFile,pCols,reRead)
# h <- readSubDataBefore05(hInputFile,hOutputFile,hCols,reRead)
# #reRead <- 1
# hCols <- c("ST","VALP","RNTP","ADJINC")
# hOutputFile <- paste('hStValpRntpAdjinc',year,sep='')
# hInputFile <- paste('ss',year,'hus',sep='')
# h <- readSubDataBefore05(hInputFile,hOutputFile,hCols,reRead)




#state rent ratio
year <- '07'
Year <- 2007
pOutputFile <- paste('pPincpStSerialnoAgep',year,sep='')
hOutputFile <- paste('hTenSerialno',year,sep='')
p <- readSubData(pOutputFile)
h <- readSubData(hOutputFile)

p <- tbl_df(p) 
h <- tbl_df(h) 
popHouse <- left_join(p , h, by.x=c("SERIALNO"))
rm(p,h)

#compute actual rent ratio
stateRent <- popHouse %>%
  group_by(ST) %>%
  filter(TEN==3) %>%
  dplyr::summarise(rent=n())

stateOwn <- popHouse %>%
  group_by(ST) %>%
  filter(TEN %in% c(1,2)) %>%
  dplyr::summarise(own=n())

stCodes <- stateCodes()

RentVSOwn <- stateRent %>%
  right_join(stateOwn, by.x=c("ST"))
rentRatio<- RentVSOwn %>%
  mutate(rentRatio = rent/(own+rent)) %>%
  select(ST,rentRatio) 

hOutputFile <- paste('hStValpRntpAdjinc',year,sep='')
h <- readSubData(hOutputFile)
HouseRentValue <- tbl_df(h)
rm(h)

medHouseValue <- HouseRentValue %>%
  group_by(ST) %>%
  select(ST,VALP) %>%
  na.omit() %>%
  dplyr::summarise(medPrice=median(VALP)) %>%
  left_join(stCodes,by.x=c("ST")) %>%
  arrange(-medPrice)


medRent <- HouseRentValue %>%
  group_by(ST) %>%
  select(ST,RNTP) %>%
  na.omit() %>%
  dplyr::summarise(medRent=median(RNTP)) %>%
  left_join(stCodes,by.x=c("ST")) %>%
  arrange(-medRent)


HouseToRent <- medRent %>%
  select(ST,medRent) %>%
  left_join(medHouseValue,by.x=c("ST")) %>%
  mutate(bestRent=medHouseValue$medPrice/medRent)
HouseToRent$bestRent <- HouseToRent$bestRent*10/sum(HouseToRent$bestRent)
ActualRentShouldNew <- rentRatio %>%
  mutate(bestRent=HouseToRent$bestRent,year=Year)
ActualRentShould <- rbind(ActualRentShould,ActualRentShouldNew)
ActualRentShould <- ActualRentShould %>%
  left_join(stCodes,by.x=c('ST'))
write.csv(ActualRentShould,file='output/rentShould08T14.csv')
