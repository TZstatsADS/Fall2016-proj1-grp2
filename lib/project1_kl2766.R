install.packages("dplyr")
install.packages("data.table")
install.packages("ggplot2")
install.packages("choroplethr")
install.packages("choroplethrMaps")
install.packages("readr")
library("dplyr")
library("data.table")
library("ggplot2")
library("choroplethr")
library("choroplethrMaps")
library("readr")

reRead<-1

if(reRead==1){
pcols<-c("PINCP","ST","SERIALNO","FINCP","HINCP","SCHL")
p1<-fread("ss14pusa.csv", select=pcols)
p2<-fread("ss14pusb.csv", select=pcols)
p2014<-rbind(p1, p2)
rm(p1, p2)
hcols<-c("VEH","SERIALNO","TEN")
h1<-fread("ss14husa.csv", select=hcols)
h2<-fread("ss14husb.csv", select=hcols)
h2014<-rbind(h1, h2)
rm(h1,h2)
save(p2014, file="p2014.Rdata")
save(h2014, file="h2014.Rdata")
}else{
  load("p2014.Rdata")
  load("h2014.Rdata")
}
p2014<-tbl_df(p2014)
p2014
as.data.frame(p2014)
h2014<-tbl_df(h2014)
h2014
as.data.frame(h2014)
ph2014<-right_join(p2014, h2014, by="SERIALNO", all=TRUE)
ph2014
rm("p2014","h2014")

nocar<- ph2014%>%
  group_by(ST)%>%
  filter(VEH==0)%>%
  dplyr::summarise(nocar=n())
plot(nocar)
#I can tell that there are notable many people who don't have car in California and New york

Carowner<- ph2014%>%
  group_by(ST) %>%
  filter(VEH>0)%>%
  dplyr::summarise(owncar=n())
plot(Carowner) 

Rent<- ph2014%>%
  group_by(ST)%>%
  filter(TEN==3)%>%
  dplyr::summarise(rent=n())
plot(Rent)

Buy<- ph2014%>%
  group_by(ST)%>%
  filter(TEN==c(1,2))%>%
  dplyr::summarise(buy=n())
plot(Buy)

carRent <- ph2014%>%
  group_by(VEH) %>%
  filter(TEN==3)%>%
  na.omit()%>%
  dplyr::summarise(rent=n())
carOwn <-ph2014%>%
  group_by(VEH)%>%
  filter(TEN %in% c(1,2)) %>%
  na.omit()%>%
  dplyr::summarise(own=n())

carRentRatio <- carRent%>%
  left_join(carOwn,by.x=c("VEH"))%>%
  mutate(rentRatio=rent/carOwn$own)
carRentRatio

#It is clear that people who have no vehicle are more likely to renn the house,
#and as more vehicles the people have, more possibiliy of having their own home.


