########SEC Shiny App###########

setwd('C:\\Users\\htxdz\\Desktop\\project')
options(scipen = 140)
options(stringsAsFactor=FALSE)


#Installs necessary packages
plist<- c('edgar','xts','timeDate','httr','stringr','tidyr','dplyr','XML','RCurl','tm','xlsx')

#Enables packages
err<- try(sapply(plist,function(x) library(x,character.only = T)))

#Tests to see if new packages need installing
if(inherits(err,'try-error')){install.packages(plist,dep=T)}

sapply(plist,function(x) library(x,character.only = T))
####################################2007-2017##################################################
#Start and End Date
#1994-2013
sdate1<- 2002
edate1<- 2003
dateSeq1<- seq(sdate1,edate1,1)

#Pulls in all fillings

#Reads in yearly filing indexes
edgarIDX1<- try({

  #Reads in yearly filing indexes
  fileNames<- paste(getwd(),'/Master Index/',dateSeq1,'master.rda',sep='')
  yIDX1<- lapply(fileNames,function(x){
    
    load(x)
    y<- year.master
    rm(year.master)
    return(y)
    
  })
  names(yIDX1)<- dateSeq1
  
})
if(inherits(edgarIDX1,'try-error')){
  
  edgar::getMasterIndex(dateSeq1)
  
  #Reads in yearly filing indexes
  fileNames1<- paste(getwd(),'/Master Index/',dateSeq1,'master.rda',sep='')
  
  yIDX1<- lapply(fileNames1,function(x){
    
    load(x)
    y<- year.master
    rm(year.master)
    return(y)
    
  })
  
  names(yIDX1)<- dateSeq1
  
}

#Input Date of Year for Filings and Row binds lists into matrix
edgarIDX1<- do.call(rbind,lapply(as.character(dateSeq1),function(x) yIDX1[[x]]))

#Sorts into factors
cik1<- unique(edgarIDX1$CIK)
firm1<- unique(edgarIDX1$COMPANY_NAME)
formTypes1<- unique(edgarIDX1$FORM_TYPE)

#Separates types of filings into lists
formList1<- lapply(formTypes1,function(x) edgarIDX1[edgarIDX1$FORM_TYPE %in% x,])
names(formList1)<- formTypes1

#Constructs URLS
handle1<- 'https://www.sec.gov/Archives/'
urls1<- lapply(formList1,function(x) paste(handle1,as.character(x$EDGAR_LINK),sep=''))
indexURLS1<- lapply(formList1,function(x) paste(handle1,as.character(gsub(".txt",'-index.htm',x$EDGAR_LINK)),sep=''))

###################################13D Functions#################################################

#Pulls in text files
idxForm1<- formList1$`SC 13G`

#Inserts HTM Index URL
idxForm1$INDEX_URL<- indexURLS1$`SC 13G`
idxForm1$EDGAR_LINK<- urls1$`SC 13G`

#Cleans up duplicates that result from filer and company both posting 13Ds
cleanLink1<-gsub("\\w+/\\w+/\\d+/","",idxForm1$EDGAR_LINK)
uniqueURL1<- unique(cleanLink1)

rForm1<- list()
for(i in 1:length(uniqueURL1)){
  
  #Finds all urls that match current iteration and extracts
  temp1<- idxForm1[which(uniqueURL1[i]==cleanLink1),]
  
  #Adds the second party's name to the unique row
  targ1<- as.character(as.matrix(cbind(temp1[1,],temp1[2,]$COMPANY_NAME)))
  
  #Replaces NAs caused by only one party files
  targ1[is.na(targ1)]<- targ1[2]
  
  #Stores in list
  rForm1[[i]]<- targ1
  print(i)
}

#Merges list into data frame
cmpForm1<- as.data.frame(do.call(rbind,rForm1))

#Adds column names 
colnames(cmpForm1)<- c(colnames(idxForm1),"FILER")

#Sorts by date
cmpForm1<- cmpForm1[order(cmpForm1$DATE_FILED),]

#Identifies which filer and company
textPage1<- list()
for(i in 1:length(cmpForm1$INDEX_URL)){
  temp<- httr::GET(url=as.character(cmpForm1$INDEX_URL[i]))
  #Sys.sleep(1)
  textPage1[[i]]<-temp
  print(i)
} 

#Scrapes SEC html index to ascertain who is filer and company
finalForms1<- list()
for(i in 1: nrow(cmpForm1)){

  y<-as.character(cmpForm1$COMPANY_NAME[i])
  z<-as.character(cmpForm1$FILER[i])
  x<-textPage1[[i]]

  x<- httr::content(x,'text')# content(textPage[[1]],'text')#
  
  #Gets rid of special characters and excess spaces
  x<- gsub(",|/|\\.|-|\\(|\\)|&amp;|\\\\" ," ",x)
  x<- gsub("&#39;",'',x)
  x<- gsub("\\s+",' ',x)
  
  #Trims excess spaces and lead/trailing spaces
  x1<- gsub('\\s+',' ',y)#gsub(" ","",as.character(cmpForm$COMPANY_NAME[1]))#
  x2<- gsub('\\s+',' ',z) #gsub(" ",'',as.character(cmpForm$FILER[1]))# 
  x1<- gsub('\\s$|\\^s','',x1)
  x2<- gsub('\\s$|\\^s','',x2)

  #regmatches(x,gregexpr(paste(x2,"(Filed by)"),x,fixed=T))
  #cat(paste(x))

  #Matches company/filer and extracts
  n1<- ifelse( regmatches(x,gregexpr(paste(x1,"Subject",sep=' '),x,fixed=T))!= "character(0)",regmatches(x,gregexpr(paste(x1,"Subject",sep=' '),x,fixed=T)),
                 regmatches(x,gregexpr(paste(x1,"Filed by",sep=' '),x,fixed=T)))

  n2<- ifelse( regmatches(x,gregexpr(paste(x2,"Filed by",sep=' '),x,fixed=T))!= "character(0)",regmatches(x,gregexpr(paste(x2,"Filed by",sep=' '),x,fixed=T)),
                  regmatches(x,gregexpr(paste(x2,"Subject",sep=' '),x,fixed=T)))

  print(i)
  nlist1<- unlist(ifelse(grepl('Subject',n1),list(c(n1,n2)),list(c(n2,n1))))
  
  nlist1<- gsub("Subject|Filed by","",nlist1)
  
  #nlist<-gsub("([a-z])([A-Z])","\\1 \\2",nlist)
  
  err1<- try(names(nlist1)<- c("Company","Filer"))
  
  #Tests to see if 
  if(inherits(err1,'Error in names(nlist1) <- c("Company", "Filer")')){cat(i)}
  
  
   
  #Extracts CIKs
  ciks1<- regmatches(x,gregexpr('\\CIK=\\d+',x,fixed=F))
  
  #Removes CIK= portion of string
  ciksC1<- sapply(ciks1,function(z) gsub('CIK=','',z))
 
  #No need to verify, because company CIK always before Filer CIK
  cikCompany1<- ciksC1[1]
  cikFiler1<- ciksC1[2]
     
  temp1<- cmpForm1[i,]
  temp1$COMPANY_NAME<- nlist1[1]
  temp1$FILER<- nlist1[2]
  temp1$CIK<- cikCompany1
  temp1$CIK_FILER<- cikFiler1
  
  finalForms1[[i]]<- temp1
}

#Merges lists into data frame
final13G1<- do.call(rbind,finalForms1)


#Organizes columns
final13G1<- final13G1[,c('FORM_TYPE','DATE_FILED','CIK',"COMPANY_NAME",'CIK_FILER','FILER','INDEX_URL','EDGAR_LINK')]


#Extracts urls for target form and scrapes text file off edgar
####textForms1<- list()
####for(i in 1:length(final13G1$EDGAR_LINK)){
####    print(i)
####    temp1<-httr::GET(url=as.character(final13G1$EDGAR_LINK[i]))
    #Sys.sleep(1)
####    textForms1[[i]]<-temp1
    
####} 

#Cleans and Parses HTML 
####parsedTXT1<- list()
####for(i in 1:length(textForms1)){

####  y<- httr::content(textForms1[[i]],'text')
####  yC<- gsub('<[^>]+>|&#\\d+\\;|&\\w+\\;|\\,', "",y)
####  yC<- gsub("*\\(.*?\\)","",yC)
####  yC<- gsub('\\s+'," ",yC)
  #cat(yC)
####  parsedTXT1[[i]]<- yC
####  print(i)
####}
####aggShares1<- list()
####aggPct1<- list()
####for(i in 1:length(parsedTXT1)){

####  y<- tolower(parsedTXT1[[i]])
  ####  pattern1<- 'aggregate amount beneficially owned by reporting person\\s\\d+|
#### aggregate amount beneficially owned by each reporting person\\s\\d+|
#### aggregate amount beneficially owned by each person\\s\\d+'
  
  ####  aggS1<- unlist(regmatches(y,gregexpr(pattern,y,fixed=F)))[1]
  
  ####  agg1<- gsub('aggregate amount beneficially owned by each reporting person\\s|
    ####           aggregate amount beneficially owned by each person\\s','',aggS1)
  
  #### aggShares1[[i]]<- agg1
  
  #### pctS1<- unlist(regmatches(y,gregexpr('\\d+\\.\\d+\\%|\\d+\\%',y,fixed=F)))
  ####  pct1<- gsub('%','',pctS1)
  ####  pct1<- max(as.numeric(pct1))
####  aggPct1[[i]]<- pct1
####  print(i)
####}

####xx1<- do.call(rbind,aggShares)



write.csv(final13G1,"13G Index 2017(2002-2003).csv")



###################################13G Functions################################
###############rearrange xBook1
xBook1<- read.csv('C:\\Users\\htxdz\\Desktop\\project\\13G Index 1994-2013.csv')
xBook1$X.1 <- NULL
xBook1$X <- NULL
xBook1$Switch.Date <- 0
xBook1$Event__ <- 0
xBook1$FORM_TYPE <- NULL

xBook1 <- xBook1[c(2,3,8,9,4,1,5,6,7)]

colnames(xBook1) <- c("Company.CIK", "Target.Company", "Switch.Date", "Event__", "Filer.CIK", "Date.of.13g", "Switcher", "INDEX_URL", "EDGAR_LINK__")
xBook1 = na.omit(xBook1)
xBook1[,5] <- gsub("(?<![0-9])0+", "", xBook1[,5], perl = TRUE)



##############sort xbook
xBook<- read.csv('C:\\Users\\htxdz\\Desktop\\project\\Switchers_13d.csv')
xBook[which(xBook$Date.of.13g==0),]<-NA
xBook <- subset(xBook, select=-12:-8)
cBook<- na.omit(xBook)
colnames(cBook)[1] <- c('Company.CIK')
cBook$INDEX_URL <- 0
cBook$EDGAR_LINK__ <- 0

#Formats dates
idx<- cBook$Date.of.13g
didx<- as.Date(as.Date(idx,format = '%m/%d/%Y'),'%yyyy/%mm/%dd')

cBook$Date.of.13g<-didx

#Cleans entries that have not valid dates
cBook<- na.omit(cBook)

cBook<- cBook[order(cBook$Date.of.13g),]

#Pulls Nohel's rolling +/- 1 year window for 13Gs
filerCIK<- as.character(cBook$Filer.CIK)

resList<- list()
for(i in 1:length(filerCIK)){
  
  fCIK<- filerCIK[i]
  t<- as.numeric(cBook$Filer.CIK[i])
  a <- xBook1$Filer.CIK == t
  tempRes <- xBook1[a,]
  dateF <- as.Date(cBook$Date.of.13g[i])
  sD<- dateF-365*1
  eD<- dateF+365*1 
  
  targRes<- tempRes[( as.Date(tempRes$Date.of.13g) >= sD & as.Date(tempRes$Date.of.13g) <= eD)&as.Date(tempRes$Date.of.13g)!=dateF,]
  q <- rbind(targRes, cBook[i,])

  q$Switch.Date <- cBook[i,]$Switch.Date
  q$Event__ <- cBook[i,]$Event__
  resList[[i]]<- q

  print(i) 
}
resdata<- do.call(rbind,resList)

write.csv(resdata,"two years period 13G.csv")



