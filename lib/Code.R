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
save(PData,file = "PData.RData")
save(FData,file = "FData.RData")

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
  filter(ST == "36")%>%select(NP,TEN)%>%group_by(NP)%>%summarise(Percentage = n())%>%mutate(Percentage = Percentage/sum(Percentage))%>%mutate(TEN = "RENT")
N_NY.Buy = Buy%>%filter(!is.na(NP))%>%select(NP,TEN,ST)%>%mutate(NP=as.numeric(NP))%>%
  filter(ST == "36")%>%select(NP,TEN)%>%group_by(NP)%>%summarise(Percentage = n())%>%mutate(Percentage = Percentage/sum(Percentage))%>%mutate(TEN = "BUY")
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
Children_Rent.NY  = Rent %>% filter(!is.na(NOC),ST == "36") %>% select(TEN,NOC)%>%mutate(NOC = as.numeric(NOC))%>%group_by(NOC)%>%summarise(Percentage = n())%>%mutate(Percentage = Percentage/sum(Percentage),TEN = "RENT") 
Children_Buy.NY = Buy %>% filter(!is.na(NOC),ST=="36")%>%select(TEN,NOC)%>%mutate(NOC = as.numeric(NOC))%>%group_by(NOC)%>%summarise(Percentage = n())%>%mutate(Percentage = Percentage/sum(Percentage),TEN = "BUY")
Children_Rent.Buy.NY = rbind(Children_Buy,Children_Rent)
ggplot(Children_Rent.Buy.NY,aes(x = NOC, y = Percentage, width = 0.7, fill = TEN)) + geom_bar(stat = "identity",position = "dodge")+ 
  ggtitle("Children in the house in New York") + labs(x = "Number of Children",y="Percentage") 



#ACR
ACR_Rent = Rent%>%filter(!is.na(ACR))%>%select(ACR,TEN)%>%mutate(ACR=as.numeric(ACR))%>%group_by(ACR)%>%summarise(Percentage = n())%>%mutate(Percentage = Percentage/sum(Percentage))%>%mutate(TEN = "RENT")
ACR_Buy = Buy%>%filter(!is.na(ACR))%>%select(ACR,TEN)%>%mutate(ACR = as.numeric(ACR))%>%group_by(ACR)%>%summarise(Percentage = n())%>%mutate(Percentage = Percentage/sum(Percentage))%>%mutate(TEN = "BUY")
ACR_Rent.Buy = rbind(ACR_Rent,ACR_Buy)
ACR_Rent.Buy$ACR = as.factor(ACR_Rent.Buy$ACR)
levels(ACR_Rent.Buy$ACR) = c("less than one acre","one to less ten acres","house on ten or more acres")
ggplot(ACR_Rent.Buy,aes(x = ACR, y = Percentage, fill = TEN , width = 0.5)) + geom_bar(stat = "identity",position = "dodge") +
  ggtitle("Lot size vs Rent/Buy in United States") + labs(x = "Lot Size of Houses",y="Percentage") 

#ACR New York
ACR_Rent.NY = Rent%>%filter(!is.na(ACR),ST == "36")%>%select(ACR,TEN)%>%mutate(ACR=as.numeric(ACR))%>%group_by(ACR)%>%summarise(Percentage = n())%>%mutate(Percentage = Percentage/sum(Percentage))%>%mutate(TEN = "RENT")
ACR_Buy.NY = Buy%>%filter(!is.na(ACR),ST == "36")%>%select(ACR,TEN)%>%mutate(ACR = as.numeric(ACR))%>%group_by(ACR)%>%summarise(Percentage = n())%>%mutate(Percentage = Percentage/sum(Percentage))%>%mutate(TEN = "BUY")
ACR_Rent.Buy.NY = rbind(ACR_Rent.NY,ACR_Buy.NY)
ACR_Rent.Buy.NY$ACR = as.factor(ACR_Rent.Buy.NY$ACR)
levels(ACR_Rent.Buy.NY$ACR) = c("less than one acre","one to less ten acres","house on ten or more acres")
ggplot(ACR_Rent.Buy.NY,aes(x = ACR, y = Percentage, fill = TEN , width = 0.5)) + geom_bar(stat = "identity",position = "dodge") +
  ggtitle("Lot size vs Rent/Buy in New York") + labs(x = "Lot Size of Houses",y="Percentage") 

#RNTP vs Rent
RNTP_Rent = Rent%>%filter(!is.na(RNTP))%>%mutate(RNTP = as.numeric(RNTP),Region = "US")%>%select(RNTP,Region)
RNTP_Rent_NY = Rent%>%filter(!is.na(RNTP),ST == "36")%>%mutate(RNTP = as.numeric(RNTP),Region = "New York")%>%select(RNTP,Region)
RNTP_RENT_US.NY = rbind(RNTP_Rent,RNTP_Rent_NY)
ggplot(RNTP_RENT_US.NY,aes(x = Region ,y = RNTP)) + geom_boxplot(aes(fill = Region),alpha = 0.5)+
  stat_summary(fun.y = mean, geom ="point",shape = 23, size = 4) + ggtitle("Monthly Rent in United States") + labs(x = "Region", y = "Monthly Rent")


#Race
P.Rent = PData%>%select(ST,SEX,COW,RAC2P,INDP,SERIALNO)%>%filter(SERIALNO%in%Rent$SERIALNO)%>%mutate(TEN = "RENT")
P.Buy = PData%>%select(ST,SEX,COW,RAC2P,INDP,SERIALNO)%>%filter(SERIALNO%in%Buy$SERIALNO)%>%mutate(TEN = "BUY")

Race.P.Rent = P.Rent %>% filter(!is.na(RAC2P))%>%group_by(RAC2P)%>%summarise(Count = n())
Race.P.Buy = P.Buy %>% filter(!is.na(RAC2P))%>%group_by(RAC2P)%>%summarise(Count = n())
Race.P.Rent.Buy = left_join(Race.P.Buy,Race.P.Rent,by = "RAC2P")
colnames(Race.P.Rent.Buy) = c("Race","Buy","Rent")
Race.Rent.Ratio = Race.P.Rent.Buy%>%mutate(Ratio = Rent/(Buy+Rent))%>%select(Race,Ratio)

ggplot(Race.Rent.Ratio,aes(x = Race, y = Ratio)) + geom_bar(aes(fill = Race),stat = "identity") + ggtitle("Race vs. Rent Ratio")+
  labs(x = "Race",y = "Rent Ratio")

#Industry
Ind.P.Rent = P.Rent %>% filter(!is.na(INDP))%>%group_by(INDP)%>%summarise(Count = n())
Ind.P.Buy = P.Buy %>% filter(!is.na(INDP))%>%group_by(INDP)%>%summarise(Count = n())
Ind.P.Rent.Buy = left_join(Ind.P.Buy,Ind.P.Rent,by = "INDP")
colnames(Ind.P.Rent.Buy) = c("Ind","Buy","Rent")
Ind.Rent.Ratio1 = Ind.P.Rent.Buy%>%mutate(Ratio = Rent/(Buy+Rent))%>%select(Ind,Ratio)%>%arrange(Ratio)
Ind.Rent.Ratio2 = Ind.P.Rent.Buy%>%mutate(Ratio = Rent/(Buy+Rent))%>%select(Ind,Ratio)%>%arrange(-Ratio)
Ind.Rent.Ratio = rbind(Ind.Rent.Ratio1[1:5,],Ind.Rent.Ratio2[1:5,])

ggplot(Ind.Rent.Ratio,aes(x = Ind, y = Ratio)) + geom_bar(aes(fill = Ind,width = 0.6),stat = "identity") + ggtitle("Industry vs. Rent Ratio")+
  labs(x = "Industry",y = "Rent Ratio")


#Industry.Mortgage
#Kind of Buy in US
Buy.Kind = Buy%>%group_by(TEN)%>%summarise(Count = n())
Buy.Kind = cbind(INDP = "US",Buy.Kind)
Ratio.US = Buy.Kind$Count[1]/sum(Buy.Kind$Count)

Ind.P.Buy.L5 = P.Buy%>%filter(INDP%in%Ind.Rent.Ratio1$Ind[1:5])%>%select(SERIALNO,INDP)
Temp = Buy%>%filter(SERIALNO%in%Ind.P.Buy.L5$SERIALNO)%>%select(SERIALNO,TEN)
Ind.P.Buy.L5.M = merge(Ind.P.Buy.L5,Temp,by="SERIALNO")
Ind.P.Buy.Mortgage = Ind.P.Buy.L5.M%>%group_by(INDP,TEN)%>%summarise(Count = n())
library(tidyr)
Ind.P.Buy.Kind = spread(Ind.P.Buy.Mortgage,key = TEN,value = Count)
colnames(Ind.P.Buy.Kind) = c("INDP","Mortgage","Clear")
Ind.P.Buy.Mortgage.Ratio = Ind.P.Buy.Kind %>% mutate(Ratio = Mortgage/(Mortgage+Clear))%>% select(INDP,Ratio)
Ind.P.Buy.Kind.Ratio = as.data.frame(Ind.P.Buy.Mortgage.Ratio)                                                                                              
Ind.P.Buy.Kind.Ratio = rbind(Ind.P.Buy.Kind.Ratio,c("US",Ratio.US))
Ind.P.Buy.Kind.Ratio$Ratio = as.numeric(Ind.P.Buy.Kind.Ratio$Ratio)
ggplot(Ind.P.Buy.Kind.Ratio,aes(x = INDP, y = Ratio)) + geom_bar(aes(fill = INDP,width = 0.6),stat = "identity",position = "dodge") + ggtitle("Industry vs. Mortgage Ratio")+
  labs(x = "Industry",y = "Mortgage Ratio") 


#Work place
People.ST = PData%>%select(POWSP,ST,SERIALNO)%>%filter(!is.na(POWSP),!is.na(ST)) %>% mutate(ST=as.numeric(ST))%>%group_by(ST) %>% summarise(Working.People = n())
WP = PData%>%select(POWSP,ST,SERIALNO)%>%filter(!is.na(POWSP),!is.na(ST))%>%mutate(POWSP = as.numeric(POWSP),ST = as.numeric(ST)) %>% filter(POWSP!=ST)
WP.People = WP%>%mutate(ST = as.numeric(ST))%>%group_by(ST)%>%summarise(Count=n())
WP.Working.Ratio = merge(People.ST,WP.People,by = "ST")
WP.Working.Ratio = WP.Working.Ratio%>%mutate(Working.Out.Ratio = Count/Working.People)%>%select(ST,Working.Out.Ratio)
WP.RENT = WP%>%filter(SERIALNO %in% Rent$SERIALNO)%>%group_by(ST)%>%summarise(RENT = n())
WP.BUY =  WP%>%filter(SERIALNO %in% Buy$SERIALNO)%>%group_by(ST)%>%summarise(BUY = n())
WP.Rent.Buy = merge(WP.RENT,WP.BUY,by = "ST")
WP.Rent.Ratio = WP.Rent.Buy%>%mutate(Ratio = RENT/(RENT+BUY))%>%select(ST,Ratio)
WP.Ratio.People = merge(WP.Rent.Ratio,WP.Working.Ratio,by="ST")

ggplot(WP.Ratio.People,aes(x = Ratio, y = Working.Out.Ratio, color = factor(ST),label=ST)) +geom_point() +labs(x = "Rent Ratio", y = "Migrant Worker Proportion")+
  ggtitle("Migrant Worker Proportion and Rent Ratio in Each State") +geom_text(hjust = 0, vjust =-1, alpha = 0.8)


#Education
Education_P = PData%>%select(SCHL,ST,SERIALNO,AGEP,PINCP)%>%filter(!is.na(SCHL),!is.na(ST),!is.na(SERIALNO),AGEP>18) %>%mutate(ST = as.numeric(ST),SCHL = as.numeric(SCHL))
Education_P$SCHL = as.factor(Education_P$SCHL)
levels(Education_P$SCHL) = c(rep("Below Bachelor",20),"Bachelor","Master","Professional","Doctorate")
Education_Rent = Education_P%>%filter(SERIALNO%in%Rent$SERIALNO)%>%group_by(SCHL)%>%summarise(RENT = n())
Education_Buy = Education_P%>%filter(SERIALNO%in%Buy$SERIALNO)%>%group_by(SCHL)%>%summarise(BUY = n())
Education_Rent.Buy = merge(Education_Buy,Education_Rent,by="SCHL")
Education_Rent.Ratio = Education_Rent.Buy%>%mutate(Ratio = RENT/(RENT+BUY),Education = SCHL)%>%select(Education,Ratio)
ggplot(Education_Rent.Ratio,aes(x = Education, y = Ratio, fill = Education))+geom_bar(aes(width = 0.6),stat = "identity") + ggtitle("Education vs Rent/Buy")

#Income - Rent in Doctorate
ID = Education_P%>%filter(SCHL == "Doctorate",!is.na(PINCP))
IDR = ID%>%filter(ID$SERIALNO%in%Rent$SERIALNO)%>%mutate(PINCP = as.numeric(PINCP),TEN = "RENT")%>%select(PINCP,TEN)
IDB = ID%>%filter(ID$SERIALNO%in%Buy$SERIALNO)%>%mutate(PINCP = as.numeric(PINCP),TEN = "BUY")%>%select(PINCP,TEN)
IDR.B = rbind(IDR,IDB)
ggplot() + geom_density(data = IDR.B,aes(x = PINCP , group = TEN, fill= TEN),alpha = 0.5, adjust = 2) + 
  xlab("Personal Income") + ylab("Density") + ggtitle("Doctorate Income vs Rent/Buy")


