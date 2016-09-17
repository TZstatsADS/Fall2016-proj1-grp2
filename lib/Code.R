library(readr)
library(dplyr)
library(ggplot2)
A<-read_csv("ss14husa.csv")
B<-read_csv("ss14husb.csv")
FData = rbind(A,B)
FData = as.data.frame(FData)
C<-read_csv("ss14pusa.csv")
D<-read_csv("ss14pusb.csv")
PData = rbind(C,D)
PData = as.data.frame(PData)
gc()
rm(A)
rm(B)
rm(C)
rm(D)


D = FData%>%select(RMSP,TEN,ACR,SERIALNO,NP,FPARC,HUGCL,ST,NOC,RNTP)%>%filter(!is.na(TEN))
Rent = D%>%filter(TEN == 3)%>%distinct(SERIALNO,.keep_all =TRUE)
Buy = D%>%filter(TEN == c(1,2))%>%distinct(SERIALNO,.keep_all =TRUE)

#How many people in a house
N_Rent = Rent%>%filter(!is.na(NP))%>%select(NP,TEN)%>%mutate(NP=as.numeric(NP))%>%group_by(NP)%>%summarise(Percentage = n())%>%mutate(Percentage = Percentage/sum(Percentage))%>%mutate(TEN = "RENT")
N_Buy = Buy%>%filter(!is.na(NP))%>%select(NP,TEN)%>%mutate(NP = as.numeric(NP))%>%group_by(NP)%>%summarise(Percentage = n())%>%mutate(Percentage = Percentage/sum(Percentage))%>%mutate(TEN = "BUY")
N_Rent.Buy = rbind(N_Rent,N_Buy)
ggplot(N_Rent.Buy,aes(x = NP, y = Percentage, fill = TEN , width = 0.7)) + geom_bar(stat = "identity",position = "dodge") +
  ggtitle("Number of People in the House in United States") + labs(x = "Number of people",y="Frequency") 

#New York State Np
N_NY.Rent = Rent%>%filter(!is.na(NP))%>%select(NP,TEN,ST)%>%mutate(NP=as.numeric(NP))%>%
  filter(ST == "34")%>%select(NP,TEN)%>%group_by(NP)%>%summarise(Percentage = n())%>%mutate(Percentage = Percentage/sum(Percentage))%>%mutate(TEN = "RENT")
N_NY.Buy = Buy%>%filter(!is.na(NP))%>%select(NP,TEN,ST)%>%mutate(NP=as.numeric(NP))%>%
  filter(ST == "34")%>%select(NP,TEN)%>%group_by(NP)%>%summarise(Percentage = n())%>%mutate(Percentage = Percentage/sum(Percentage))%>%mutate(TEN = "BUY")
N_NY.Rent.Buy = rbind(N_NY.Rent,N_NY.Buy)
ggplot(N_NY.Rent.Buy,aes(x = NP, y = Percentage, width = 0.7, fill = TEN)) + geom_bar(stat = "identity",  position = "dodge")+
  ggtitle("Number of People in the House in New York") + labs(x = "Number of people",y="Percentage") 


#Children
Children_Rent  = Rent %>% filter(!is.na(NOC)) %>% select(TEN,NOC)%>%mutate(NOC = as.numeric(NOC))%>%group_by(NOC)%>%summarise(Percentage = n())%>%mutate(Percentage = Percentage/sum(Percentage),TEN = "RENT") 
Children_Buy = Buy %>% filter(!is.na(NOC))%>%select(TEN,NOC)%>%mutate(NOC = as.numeric(NOC))%>%group_by(NOC)%>%summarise(Percentage = n())%>%mutate(Percentage = Percentage/sum(Percentage),TEN = "BUY")
Children_Rent.Buy = rbind(Children_Buy,Children_Rent)

ggplot(Children_Rent.Buy,aes(x = NOC, y = Percentage, width = 0.7, fill = TEN)) + geom_bar(stat = "identity",position = "dodge")+ 
  ggtitle("Children in the house in United States") + labs(x = "Number of Children",y="Percentage") 

#Children in New York
Children_Rent.NY  = Rent %>% filter(!is.na(NOC),ST == "34") %>% select(TEN,NOC)%>%mutate(NOC = as.numeric(NOC))%>%group_by(NOC)%>%summarise(Percentage = n())%>%mutate(Percentage = Percentage/sum(Percentage),TEN = "RENT") 
Children_Buy.NY = Buy %>% filter(!is.na(NOC),ST=="34")%>%select(TEN,NOC)%>%mutate(NOC = as.numeric(NOC))%>%group_by(NOC)%>%summarise(Percentage = n())%>%mutate(Percentage = Percentage/sum(Percentage),TEN = "BUY")
Children_Rent.Buy.NY = rbind(Children_Buy,Children_Rent)
ggplot(Children_Rent.Buy.NY,aes(x = NOC, y = Percentage, width = 0.7, fill = TEN)) + geom_bar(stat = "identity",position = "dodge")+ 
  ggtitle("Children in the house in New York") + labs(x = "Number of Children",y="Percentage") 



#ACR
ACR_Rent = Rent%>%filter(!is.na(ACR))%>%select(ACR,TEN)%>%mutate(ACR=as.numeric(ACR))%>%group_by(ACR)%>%summarise(Percentage = n())%>%mutate(Percentage = Percentage/sum(Percentage))%>%mutate(TEN = "RENT")
ACR_Buy = Buy%>%filter(!is.na(ACR))%>%select(ACR,TEN)%>%mutate(ACR = as.numeric(ACR))%>%group_by(ACR)%>%summarise(Percentage = n())%>%mutate(Percentage = Percentage/sum(Percentage))%>%mutate(TEN = "BUY")
ACR_Rent.Buy = rbind(ACR_Rent,ACR_Buy)
ACR_Rent.Buy$ACR = as.factor(ACR_Rent.Buy$ACR)
levels(ACR_Rent.Buy$ACR) = c("less than one acre","one to less ten acres","house on ten or more acres")
ggplot(ACR_Rent.Buy,aes(x = ACR, y = Percentage, fill = TEN , width = 0.7)) + geom_bar(stat = "identity",position = "dodge") +
  ggtitle("Lot size vs Rent/Buy in United States") + labs(x = "Lot Size of Houses",y="Percentage") 

#ACR New York
ACR_Rent.NY = Rent%>%filter(!is.na(ACR),ST == "34")%>%select(ACR,TEN)%>%mutate(ACR=as.numeric(ACR))%>%group_by(ACR)%>%summarise(Percentage = n())%>%mutate(Percentage = Percentage/sum(Percentage))%>%mutate(TEN = "RENT")
ACR_Buy.NY = Buy%>%filter(!is.na(ACR),ST == "34")%>%select(ACR,TEN)%>%mutate(ACR = as.numeric(ACR))%>%group_by(ACR)%>%summarise(Percentage = n())%>%mutate(Percentage = Percentage/sum(Percentage))%>%mutate(TEN = "BUY")
ACR_Rent.Buy.NY = rbind(ACR_Rent.NY,ACR_Buy.NY)
ACR_Rent.Buy.NY$ACR = as.factor(ACR_Rent.Buy.NY$ACR)
levels(ACR_Rent.Buy$ACR) = c("less than one acre","one to less ten acres","house on ten or more acres")
ggplot(ACR_Rent.Buy.NY,aes(x = ACR, y = Percentage, fill = TEN , width = 0.7)) + geom_bar(stat = "identity",position = "dodge") +
  ggtitle("Lot size vs Rent/Buy in New York") + labs(x = "Lot Size of Houses",y="Percentage") 

#RNTP vs Rent
RNTP_Rent = Rent%>%filter(!is.na(RNTP))%>%mutate(RNTP = as.numeric(RNTP),Region = "US")%>%select(RNTP,Region)
RNTP_Rent_NY = Rent%>%filter(!is.na(RNTP),ST == "34")%>%mutate(RNTP = as.numeric(RNTP),Region = "New York")%>%select(RNTP,Region)
RNTP_RENT_US.NY = rbind(RNTP_Rent,RNTP_Rent_NY)
ggplot(RNTP_RENT_US.NY,aes(x = Region ,y = RNTP)) + geom_boxplot(aes(fill = Region),alpha = 0.5)+
  stat_summary(fun.y = mean, geom ="point",shape = 23, size = 4) + ggtitle("Monthly Rent in United States")


#Race
P.Rent = PData%>%select(ST,SEX,COW,RAC2P,INDP,SERIALNO)%>%filter(SERIALNO%in%Rent$SERIALNO)%>%mutate(TEN = "RENT")
P.Buy = PData%>%select(ST,SEX,COW,RAC2P,INDP,SERIALNO)%>%filter(SERIALNO%in%Buy$SERIALNO)%>%mutate(TEN = "BUY")

Race.R.Rent = P.Rent %>% filter(!is.na(RAC2P))%>%group_by(RAC2P)%>%summarise(Count = n())
Race.R.Buy = P.Buy %>% filter(!is.na(RAC2P))%>%group_by(RAC2P)%>%summarise(Count = n())
Race.R.Rent.Buy = left_join(Race.R.Buy,Race.R.Rent,by = "RAC2P")
colnames(Race.R.Rent.Buy) = c("Race","Buy","Rent")
Race.Rent.Ratio = Race.R.Rent.Buy%>%mutate(Ratio = Rent/(Buy+Rent))%>%select(Race,Ratio)

ggplot(Race.Rent.Ratio,aes(x = Race, y = Ratio)) + geom_bar(aes(fill = Race),stat = "identity") + ggtitle("Race vs. Rent Ratio")+
  labs(x = "Race",y = "Rent Ratio")


