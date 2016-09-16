library("dplyr")
library("data.table")
library("ggplot2")
library("choroplethr")

reRead <- 1
##read data and save it as RData to save time nect time:
if(reRead==1){
  pColsToKeep <- c("PINCP", "ST","SERIALNO","AGEP")
  popDataA <- fread("data/ss14pusa.csv",select=pColsToKeep ) 
  popDataB <- fread("data/ss14pusb.csv", select=pColsToKeep )
  populData14 <- rbind(popDataA, popDataB)
  rm(popDataA, popDataB)
  hColsToKeep <- c("TEN","SERIALNO")
  housDataA <- fread("data/ss14husa.csv",select=hColsToKeep ) 
  housDataB <- fread("data/ss14husb.csv", select=hColsToKeep )
  houseData14 <- rbind(housDataA, housDataB)
  rm(housDataA, housDataB)
  save(populData14, file="output/populData14.RData")
  save(houseData14, file="output/houseData14.RData")
}else{
  load("populData14.RData")
  load("houseData14.RData")
} 

populData14 <- tbl_df(populData14) 
houseData14 <- tbl_df(houseData14) 
popHouse <- left_join(populData14 , houseData14, by.x=c("SERIALNO"))
rm('populData','houseData')

#Which states have highest rent/buy ratio
#first take a look at how many missing value of house type in NY(36) and CA(06)
popHouse %>%
  filter(ST %in% c(6,36)) %>%
  group_by(ST) %>%
  dplyr::summarise(missingProp = sum(is.na(TEN))/length(TEN))
# I think these props are acceptable and comparable

stateRent <- popHouse %>%
  group_by(ST) %>%
  filter(TEN==3,AGEP<10) %>%
  dplyr::summarise(rent=n())

stateOwn <- popHouse %>%
  group_by(ST) %>%
  filter(TEN %in% c(1,2),AGEP<10) %>%
  dplyr::summarise(own=n())

stateCodeCSV = "ST,region
001,alabama
002,alaska
004,arizona
005,arkansas
006,california
008,colorado
009,connecticut
010,delaware
011,district of columbia
012,florida
013,georgia
015,hawaii
016,idaho
017,illinois
018,indiana
019,iowa
020,kansas
021,kentucky
022,louisiana
023,maine
024,maryland
025,massachusetts
026,michigan
027,minnesota
028,mississippi
029,missouri
030,montana
031,nebraska
032,nevada
033,new hampshire
034,new jersey
035,new mexico
036,new york
037,north carolina
038,north dakota
039,ohio
040,oklahoma
041,oregon
042,pennsylvania
044,rhode island
045,south carolina
046,south dakota
047,tennessee
048,texas
049,utah
050,vermont
051,virginia
053,washington
054,west virginia
055,wisconsin
056,wyoming"
stateCodes <- fread(stateCodeCSV)
  
RentVSOwn <- stateRent %>%
  right_join(stateOwn, by.x=c("ST"))
rentRatio<- RentVSOwn %>%
  mutate(value = rent/(own+rent)) %>%
  select(ST,value) %>%
  left_join(stateCodes,by.x=c("ST"))

state_choropleth(rentRatio, title = "Percentage of People who Rent", num_colors=9)

#top10 states people rent most
topRent <- rentRatio %>%
  arrange(-value) %>%
  slice(1:10)


houseValueA <- fread("data/ss14husa.csv",select=c("ST","SERIALNO","VALP") ) 
houseValueB <- fread("data/ss14husb.csv",select=c("ST","SERIALNO","VALP") ) 
houseValue <- rbind(houseValueA,houseValueB)
houseValue <- tbl_df(houseValue)
rm(houseValueA,houseValueB)

avgHouseValue <- houseValue %>%
  group_by(ST) %>%
  na.omit() %>%
  dplyr::summarise(avgPrice=mean(VALP)) %>%
  left_join(stateCodes,by.x=c("ST")) %>%
  arrange(-avgPrice)
#I haven't expect it...NJ's house price is higher than NY and CA has higher rent ratio than NY...maybe it's due to new york state is too large? I wish I could restrict on manhattan!


