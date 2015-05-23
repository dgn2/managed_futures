# load the rvest package
library(rvest)
# load the lubridate package
library(lubridate)

extractCtaName<-function (programHtmlSession){  
  # extract the CTA name
  ctaName <- programHtmlSession %>% html() %>% 
    html_nodes("#spnHeadingPP1") %>% html_text()
  # remove foreign language characters
  ctaName
}

extractProgramName<-function (programHtmlSession){  
  # extract the program name
  programName <- programHtmlSession %>% html() %>% 
    html_nodes("#spnHeadingPP2") %>% html_text()  
  # remove foreign language characters  
  programName
}

extractMonthlyReturns<-function (programHtmlSession,programId){  
  # extract CTA Name
  ctaName<-extractCtaName(programHtmlSession)
  # extract the program name
  programName<-extractProgramName(programHtmlSession)
  # extract the monthly returns
  a<-html_table(html_nodes(programHtmlSession, "table")[[1]],fill=TRUE)
  # extract the years
  YYYY<-as.numeric(a$X1[3:(length(a$X1)-1)])
  # find the number of years
  nYears<-length(YYYY)
  # create the empty end of month date matrix
  dates<-matrix(data=NA,nrow=nYears,ncol=12)
  # find the last day of each month
  for (yyyyIndex in seq_along(YYYY)){ 
    yyyy<-YYYY[yyyyIndex]
    # find the last day for the current year
    eomDates<-ceiling_date(ISOdate(yyyy,1,1,0,0,0,tz='EST') + 
                             months(1:12))-days(1)
    # store the end of month dates
    dates[yyyyIndex,1:12]<-format(eomDates, '%Y-%m-%d')
  }
  # flatten the end of month matrix
  eomDates<-matrix(dates,nrow=nYears*12,1)  
  # extract the returns
  jan<-as.numeric(a$X2[!is.na(sub('Jan',NA,a$X2))])
  feb<-as.numeric(a$X3[!is.na(sub('Feb',NA,a$X3))])
  mar<-as.numeric(a$X4[!is.na(sub('Mar',NA,a$X4))])
  apr<-as.numeric(a$X5[!is.na(sub('Apr',NA,a$X5))])
  may<-as.numeric(a$X6[!is.na(sub('May',NA,a$X6))])
  jun<-as.numeric(a$X7[!is.na(sub('Jun',NA,a$X7))])
  jul<-as.numeric(a$X8[!is.na(sub('Jul',NA,a$X8))])
  aug<-as.numeric(a$X9[!is.na(sub('Aug',NA,a$X9))])
  sep<-as.numeric(a$X10[!is.na(sub('Sep',NA,a$X10))])
  oct<-as.numeric(a$X11[!is.na(sub('Oct',NA,a$X11))])
  nov<-as.numeric(a$X12[!is.na(sub('Nov',NA,a$X12))])
  dec<-as.numeric(a$X13[!is.na(sub('Dec',NA,a$X13))])
  # create the monthly return data frame
  monthlyReturns<-cbind(ctaName,programName,yyyy,jan,feb,mar,
    apr,may,jun,jul,aug,sep,oct,nov,dec)
  # create the monthly return matrix by year
  monthlyReturnsMatrix<-as.matrix(cbind(jan,feb,mar,apr,
    may,jun,jul,aug,sep,oct,nov,dec))
  # find the dimension of the monthly returns matrix
  dimension<-dim(monthlyReturnsMatrix)
  # flatten the monthly return matrix
  monthlyReturnsData<-matrix(monthlyReturnsMatrix,dimension[1]*dimension[2],1)
  # create the data frame
  data<-data.frame(dates=eomDates,monthlyReturns=monthlyReturnsData,
    stringsAsFactors=FALSE)
  # get the sort index
  sortIndex<-sort(data[,1],index.return=TRUE)
  # order the data
  data<-data[sortIndex$ix,]
  # add the CTA Name
  data['ctaName']<-ctaName
  # add the program name
  data['programName']<-programName
  # add the program ID
  data['programId']<-programId
  # find the NAs
  naIndex<-is.na(data[,2])
  # return the data
  data[!naIndex,]
}

extractAddress<-function (programHtmlSession,ctaName,programName){  
  # extract the address
  address<-html_table(html_nodes(programHtmlSession, 
    "table")[[10]],fill=TRUE)  
  # extract the address
  addressHeader<-t(address[1])
  # remove the colons
  addressHeader<-sub(':','',addressHeader[2:7])
  addressData<-t(address[2])
  # remove new line characters
  addressData<-sub('\n','-',addressData[2:7])
  # create address data frame
  address<-data.frame(cbind(addressHeader,addressData))
  colnames(address)<-c('column','value')
  address['columnType']<-'address'
  address
}

extractInvestmentMethodology<-function (programHtmlSession,ctaName,programName){ 
  # extract the investment methodlogy
  investmentMethodology<-html_table(html_nodes(programHtmlSession,
    "table")[[14]])
  investmentMethodologyHeader<-t(investmentMethodology[1])
  investmentMethodologyData<-t(investmentMethodology[2])
  colnames(investmentMethodology)<-c('column','value')
  investmentMethodology['columnType']<-'investmentMethodology'
  investmentMethodology
}

extractInstruments<-function (programHtmlSession,ctaName,programName){
  # extract the instruments
  instruments<-html_table(html_nodes(programHtmlSession, 
    "table")[[15]])  
  colnames(instruments)<-c('column','value')
  instruments['columnType']<-'instruments'
  instruments
}

# extract program data
extractSectors<-function (programHtmlSession,ctaName,programName){  
  # extract sector information
  sectors<-html_table(html_nodes(programHtmlSession,
    "table")[[13]])  
  # extract sector information
  colnames(sectors)<-c('column','value')
  sectors['columnType']<-'sectors'
  sectors
}

extractGeographicalFocus<-function (programHtmlSession){
  # extract the geographical focus
  geographicalFocus<-html_table(html_nodes(programHtmlSession, 
    "table")[[16]])
  # extract the geographical focus
  colnames(geographicalFocus)<-c('column','value')
  geographicalFocus['columnType']<-'geographicalFocus'
  geographicalFocus
}

extractHoldingPeriod<-function (programHtmlSession){  
  # extract the holding period
  holdingPeriod<-html_table(html_nodes(programHtmlSession,
    "table")[[17]])
  colnames(holdingPeriod)<-c('column','value')
  holdingPeriod['columnType']<-'holdingPeriod'
  holdingPeriod
}

extractInvestmentTermsAndInfo<-function (programHtmlSession){  
  # extract the investment terms and info
  investmentTermsAndInfo<-html_table(html_nodes(programHtmlSession, 
    "table")[[18]])
  investmentTermsAndInfo[,2]<-NULL
  colnames(investmentTermsAndInfo)<-c('column','value')
  investmentTermsAndInfo['columnType']<-'investmentTermsAndInfo'
  investmentTermsAndInfo
}

# extract program data
extractProgramInfo<-function (programHtmlSession,programId){  
  # create the program session
  # extract the CTA name  
  ctaName<-extractCtaName(programHtmlSession)
  # extract the program name
  programName<-extractProgramName(programHtmlSession)
  # extract the address data into a data frame
  address<-extractAddress(programHtmlSession)
  # extract the investment methodology data into a data frame
  investmentMethodology<-extractInvestmentMethodology(programHtmlSession)
  # extract the instruments data into a data frame  
  instruments<-extractInstruments(programHtmlSession)
  # extract the sector data into a data frame
  sectors<-extractSectors(programHtmlSession)  
  # extract the geographical focus data into a data frame
  geographicalFocus<-extractGeographicalFocus(programHtmlSession)
  # extract the holding period data into a data frame
  holdingPeriod<-extractHoldingPeriod(programHtmlSession)
  # extract the investment terms and info data into a data frame
  investmentTermsAndInfo<-extractInvestmentTermsAndInfo(programHtmlSession)
  # bind all of the data frames together
  programInfo<-data.frame(rbind(address,
    investmentMethodology,instruments,sectors,geographicalFocus,
    holdingPeriod,investmentTermsAndInfo),stringsAsFactors=FALSE)
  # add the program CTA name
  programInfo['ctaName']<-ctaName
  # add the program name
  programInfo['programName']<-programName
  # add the program ID
  programInfo['programId']<-programId
  # return the data
  programInfo
}

cleanProgramInfo<-function (programInfo){
  # clean program info
  
  # remove the \r\n newlines
  programInfo <- as.data.frame(lapply(programInfo,
    function(x) if(is.character(x)|is.factor(x)) gsub("\r\n","",x) else x),
    stringsAsFactors=FALSE)  
  # remove the % signs
  programInfo <- as.data.frame(lapply(programInfo,
    function(x) if(is.character(x)|is.factor(x)) gsub("%","",x) else x),
    stringsAsFactors=FALSE)
  # remove the commas
  programInfo <- as.data.frame(lapply(programInfo,
    function(x) if(is.character(x)|is.factor(x)) gsub(",","",x) else x),
    stringsAsFactors=FALSE)
  # remove the colons
  programInfo <- as.data.frame(lapply(programInfo,
    function(x) if(is.character(x)|is.factor(x)) gsub("[:]","",x) else x),
    stringsAsFactors=FALSE)
  # remove the $ sign
  programInfo <- as.data.frame(lapply(programInfo,
    function(x) if(is.character(x)|is.factor(x)) gsub("[$]","",x) else x),
    stringsAsFactors=FALSE)
  # remove the /
  programInfo <- as.data.frame(lapply(programInfo,
    function(x) if(is.character(x)|is.factor(x)) gsub("/","",x) else x),
    stringsAsFactors=FALSE)
  # remove the foreign characters
  programInfo <- as.data.frame(lapply(programInfo,
    function(x) if(is.character(x)|is.factor(x)) gsub("ö","o",x) else x),
    stringsAsFactors=FALSE)
  # remove the foreign characters
  programInfo <- as.data.frame(lapply(programInfo,
    function(x) if(is.character(x)|is.factor(x)) gsub("Ä","A",x) else x),
    stringsAsFactors=FALSE)
  # remove the foreign characters
  programInfo <- as.data.frame(lapply(programInfo,
    function(x) if(is.character(x)|is.factor(x)) gsub("Ö","O",x) else x),
    stringsAsFactors=FALSE)
  programInfo
}

# extract and write CTA monthly returns
extractAndWriteMonthlyReturns<-function (outputFileHandle1,
  programHtmlSession,programId){
  # extract the monthly returns
  monthlyReturns<-extractMonthlyReturns(programHtmlSession,programId)
  # write the monthly returns output file
  if (dim(monthlyReturns)[2]==5){
    write.table(monthlyReturns,file=outputFileHandle1,
      row.names=FALSE,col.names=FALSE,sep='|',
      quote = FALSE)
  }
}

# extract and write CTA program info
extractAndWriteProgramInfo<-function (outputFileHandle2,
  programHtmlSession,programId){
  # extract the program info
  programInfo<-extractProgramInfo(programHtmlSession,programId)
  # clean the program info
  programInfo<-cleanProgramInfo(programInfo)
  # write the program info output file
  if (dim(programInfo)[2]==6){
    write.table(programInfo,file=outputFileHandle2,
      row.names=FALSE,col.names=FALSE,sep='|',
      quote = FALSE)
  }
}

# define the function to extract the Altegris data
extractAltegrisData<-function (outputDirectory){
  # set the URL string 
  urlString<-'http://managedfutures.com/program_profiles.aspx'
  # parse the URL
  htmlSession <- html(urlString)
  # open the output connection
  outputFileHandle1<-file(paste0(outputDirectory,'ctaMonthlyReturns'), "w")
  # open the output connection
  outputFileHandle2<-file(paste0(outputDirectory,'programInfo'), "w")
  # set the URL
  baseURL='http://managedfutures.com/'
  # find the program URLs
  programURLs <- htmlSession %>% html() %>% html_nodes("a") %>% 
    html_attr("href")
  # extract the program URLs
  programURLs <- htmlSession %>% html() %>% html_nodes("div a") %>% 
    html_attr("href")
  # extract the program IDs
  programIDs <- htmlSession %>% html() %>% html_nodes("a") %>% 
    html_attr("rel")
  # find the index
  naIndex<-(is.na(programIDs)==FALSE)
  # extract the program URLs
  programURLs<-programURLs[naIndex]
  # extract the program IDs
  programIDs<-programIDs[naIndex]
  # iterate over the programs
  for (programIndex in seq_along(programURLs)){
    # extract program ID
    programId<-programIDs[programIndex]
    # extract program URL
    programURL<-programURLs[programIndex]
    # create the full program URL
    programPerformanceURL<-paste0(baseURL,programURL)
    # create the program session
    programHtmlSession <- html(programPerformanceURL)
    # try to get hte monthly returns
    try(extractAndWriteMonthlyReturns(outputFileHandle1,
      programHtmlSession,programId),silent=TRUE)
    # try to get the program info
    try(extractAndWriteProgramInfo(outputFileHandle2,
      programHtmlSession,programId),silent=TRUE)
  }
  # close the connection
  close(outputFileHandle1)
  # close the connection
  close(outputFileHandle2)
}

# define the function to create the altegris database
createAltegrisDatabase <- function(dbHandle){
  # create the 'altegris' database
  
  # create the query
  query<-paste0("CREATE DATABASE ",dbName)
  # execute the query
  dbGetQuery(dbHandle,query)
}

# define the function to drop the altegris database
dropAltegrisDatabase <- function(dbHandle){
  # drop the 'altegris' database
  
  # create the query
  query<-paste0("DROP DATABASE ",dbName)
  # execute the query
  dbGetQuery(dbHandle,query)
}

# define the function to create the CTA program info table
createCtaProgramInfoTable <- function (dbHandle){
  # create the SQL statement to create the table
  query<-paste0("CREATE TABLE cta_program_info(",
                "dbUpdateTimestamp TIMESTAMP, ",
                "column_name VARCHAR(250) NOT NULL, ",
                "column_value VARCHAR(250) NOT NULL, ",
                "column_type VARCHAR(50) NOT NULL, ",
                "cta_name VARCHAR(100) NOT NULL, ",
                "program_name VARCHAR(120) NOT NULL, ",
                "program_id INT NOT NULL, ",
                "PRIMARY KEY(program_id,column_name));")
  # create the table
  dbGetQuery(dbHandle,query)
}

# define the function to create the CTA monthly return table
createCtaMonthlyReturnTable <- function (dbHandle){
  # create the SQL statement to create the table
  query<-paste0("CREATE TABLE cta_monthly_returns(",
                "dbUpdateTimestamp TIMESTAMP, ",
                "eom_date DATE NOT NULL, ",
                "monthly_return DECIMAL(20,10) NOT NULL, ",
                "cta_name VARCHAR(100) NOT NULL, ",
                "program_name VARCHAR(120) NOT NULL, ",
                "program_id INT NOT NULL, ",
                "PRIMARY KEY(program_id,eom_date));")
  # create the table
  dbGetQuery(dbHandle,query)
}

# define the function to create the CTA program info table
loadCtaProgramInfoTable <- function (dbHandle,outputDirectory,
                                     fileName){
  #
  query<-paste0("LOAD DATA LOCAL INFILE '",outputDirectory,
                fileName,"' IGNORE INTO TABLE cta_program_info FIELDS ",
                "TERMINATED BY '|' LINES TERMINATED BY '\n' IGNORE 0 LINES ",
                "(column_name,column_value,column_type,cta_name,program_name,",
                "program_id);")
  #
  dbGetQuery(dbHandle,query)
}

# define the function to load data to the CTA monthly return table
loadCtaMonthlyReturnTable <- function(dbHandle,outputDirectory,
                                      fileName){
  #
  query<-paste0("LOAD DATA LOCAL INFILE '",outputDirectory,
                fileName,"' IGNORE INTO TABLE cta_monthly_returns FIELDS ",
                "TERMINATED BY '|' LINES TERMINATED BY '\n' IGNORE 0 LINES ",
                "(eom_date,monthly_return,cta_name,program_name,program_id);")
  #
  dbGetQuery(dbHandle,query)
}

library(RMySQL)

# connection parameters
dbDriver<-dbDriver("MySQL")
dbHost<-'localhost'
dbPort<-3306
dbUser<-'root'
dbPassword<-''
dbName<-'altegris'

# output file path
outputDirectory<-'C:/Users/DerekG/Documents/github/managed_futures/'

# extract the CTA manager, program and monthly return data
extractFromWeb<-FALSE

if (extractFromWeb){
  # extract the Altegris data
  extractAltegrisData(outputDirectory)
}

# connect to MySQL
dbHandle<-dbConnect(dbDriver,
                    host=dbHost,port=dbPort,user=dbUser, 
                    password=dbPassword)


# drop the 'altegris' database
try(dropAltegrisDatabase(dbHandle),silent=TRUE)

# create the 'altegris' database
try(createAltegrisDatabase(dbHandle),silent=TRUE)

# disconnect from the database
dbDisconnect(dbHandle)

# connect to the 'altegris' database
dbHandle<-dbConnect(dbDriver,dbname = dbName,
                    host=dbHost,port=dbPort,user=dbUser, 
                    password=dbPassword)

# create the database tables
# --monthly returns table
try(createCtaMonthlyReturnTable(dbHandle),silent=TRUE)
# --program info table
try(createCtaProgramInfoTable(dbHandle),silent=TRUE)

# load the data to the database
# --monthly returns table
loadCtaProgramInfoTable(dbHandle,outputDirectory,
                        'programInfo')
# --program info table
loadCtaMonthlyReturnTable(dbHandle,outputDirectory,
                          'ctaMonthlyReturns')

# extract the systematic programs
query<-paste0("SELECT * FROM altegris.cta_program_info ",
  "WHERE column_type = 'investmentMethodology' AND ",
  "column_name = 'Systematic' ",
  "ORDER BY cta_name,program_name,column_type;")
# fetch the systematic programs
ctaSystematic<-dbGetQuery(dbHandle,query)

# extract the systematic programs
query<-paste0("SELECT * FROM altegris.cta_program_info ",
  "WHERE column_type = 'investmentTermsAndInfo' AND ",
  "column_name='Margin  Equity Ratio' ",
  "ORDER BY cta_name,program_name,column_type;")
# fetch the margin to equity
ctaMarginToEquity<-dbGetQuery(dbHandle,query)

query<-paste0("SELECT * FROM altegris.cta_program_info ",
  "WHERE column_type='sectors' ",
  "ORDER BY cta_name,program_name,column_type;")

ctaSectors<-dbGetQuery(dbHandle,query)

# disconnect from the 'altegris' database
dbDisconnect(dbHandle)
