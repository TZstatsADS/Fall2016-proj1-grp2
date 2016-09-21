library("dplyr")
library("data.table")

##read data and save it as RData to save time next time:
readSubData <- function(fileName,OutputName=NULL,ColsToKeep=NULL,reRead=0){
  if(reRead==1){
    #if you are reading from data, filename should be ss14pus or ss14hus such kind
    fileA <- paste('data/',fileName,'a.csv',sep = '')
    fileB <- paste('data/',fileName,'b.csv',sep = '')
    dataA <- fread(fileA,select=ColsToKeep) 
    dataB <- fread(fileB, select=ColsToKeep )
    dataAll <- rbind(dataA, dataB)
    rm(dataA, dataB)
    OutputPath <- paste('output/',OutputName,'.rds',sep='')
    saveRDS(dataAll, file=OutputPath)
    return(dataAll)
  }else{
    #if you are loading existing data, read from output folder
    filePath <- paste('output/',fileName,'.rds',sep = '')
    return(readRDS(filePath))
  } 
}


readSubDataBefore05 <- function(fileName,OutputName=NULL,ColsToKeep=NULL,reRead=0){
  if(reRead==1){
    #if you are reading from data, filename should be ss14pus or ss14hus such kind
    file <- paste('data/',fileName,'.csv',sep = '')
    dataAll <- fread(file,select=ColsToKeep) 
    OutputPath <- paste('output/',OutputName,'.rds',sep='')
    saveRDS(dataAll, file=OutputPath)
    return(dataAll)
  }else{
    #if you are loading existing data, read from output folder
    filePath <- paste('output/',fileName,'.rds',sep = '')
    return(readRDS(filePath))
  } 
}


stateCodes <- function(abbr=T){
  stateCodeCSV = "ST,region,abbr
  001,alabama,AL
  002,alaska,AK
  004,arizona,AZ
  005,arkansas,AR
  006,california,CA
  008,colorado,CO
  009,connecticut,CT
  010,delaware,DE
  011,district of columbia,DC
  012,florida,FL
  013,georgia,GA
  015,hawaii,GU
  016,idaho,ID
  017,illinois,IL
  018,indiana,IN
  019,iowa,IA
  020,kansas,KS
  021,kentucky,KY
  022,louisiana,LA
  023,maine,ME
  024,maryland,MD
  025,massachusetts,MA
  026,michigan,MI
  027,minnesota,MN
  028,mississippi,MS
  029,missouri,MO
  030,montana,MT
  031,nebraska,NE
  032,nevada,NV
  033,new hampshire,NH
  034,new jersey,NJ
  035,new mexico,NM
  036,new york,NY
  037,north carolina,NC
  038,north dakota,ND
  039,ohio,OH
  040,oklahoma,OK
  041,oregon,OR
  042,pennsylvania,PA
  044,rhode island,RI
  045,south carolina,SC
  046,south dakota,SD
  047,tennessee,TN
  048,texas,TX
  049,utah,UT
  050,vermont,VT
  051,virginia,VA
  053,washington,WA
  054,west virginia,WV
  055,wisconsin,WI
  056,wyoming,WY"
  stateCodes <- fread(stateCodeCSV)
  if(abbr){
    return(stateCodes)
    }else{
    return(select(stateCodes,ST,region))
    }
}


prcsData <- function(){
  #read and save related data from 2008 to 2014
  reRead <- 1
  pCols <- c("PINCP","RAC2P","RAC1P","INDP","SCHL","AGEP","MAR","COW","ADJINC","SERIALNO")
  hCols <- c("ST","NP","ADJHSG","VEH","RNTP","VALP","TEN","SERIALNO")
  
  saveData <- function(year){
    pInputFile <- paste('ss',year,'pus',sep='')
    hInputFile <- paste('ss',year,'hus',sep='')
    pOutputFile <- paste('p',year,sep='')
    hOutputFile <- paste('h',year,sep='')
    p <- readSubData(pInputFile,pOutputFile,pCols,reRead)
    h <- readSubData(hInputFile,hOutputFile,hCols,reRead)
  } 
  
  for (y in c('14','13','12','11','10','09','08')){
    saveData(y)
  } 
}
