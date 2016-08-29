#The Burke-Lauenroth Laboratory 
#STEPWAT R Wrapper
#Note:Make sure to have the database in the same folder, from where this script is run.
tick_on<-proc.time()

source.dir<-getwd()

yr<-30
TYPE2<-"markov"

#Load Required Packages
library(DBI)
library(RSQLite)
library(mail)
library(sendmailR)

#Load required libraries
library(plyr)

#Database location
database<-paste(source.dir,"/","dbWeatherData_Sagebrush_KP.sqlite", sep="")

#Setup parameters for the weather aquisition (years, scenarios, timeperiod, GCMs, etc.) 
simstartyr <- 1979
endyr <- 2010
climate.conditions <- c("Current")

# Difference between start and end year(if you want 2030-2060 use 50; if you want 2070-2100 use 90 below)
deltaFutureToSimStart_yr <- c(50,90)
# Downscaling method
downscaling.method <- c("hybrid-delta")

#Store climate conditons
#list of all future and current scenarios putting "Current" first	

temp<-c("Current")

#Vector of sites, the code needs to be run on

sites<-c(1:898)

#Connecting to the database
db<-dbDriver("SQLite")
db<-dbConnect(db,database)
con.env <- NULL
con.env$con<-db

#Functions to access respective data

#Function to query data from the database
dbW_getWeatherData <- function(Site_id=NULL,lat=NULL,long=NULL,Label=NULL,startYear=NULL,endYear=NULL, Scenario="Current") {
  if(is.null(Site_id) && is.null(Label) && is.null(lat) && is.null(long)) {
    stop("No way to locate weather data from input")
  }
  
  useYears<-FALSE
  useStart<-FALSE
  useEnd  <-FALSE
  if(!is.null(startYear) | !is.null(endYear)) {#See if we should narrow the start end year range
    startYear <- as.integer(startYear)
    if(!is.na(startYear)) useStart<-TRUE
    endYear <- as.integer(endYear)
    if(!is.na(endYear)) useEnd<-TRUE
    if(useStart | useEnd) useYears<-TRUE
    if(useStart & useEnd) {
      if(startYear >= endYear | startYear<0 | endYear<0) {
        stop("Wrong start or end year")
      }
    }
  }
  Site_id<-as.integer(Site_id)
  if(length(Site_id) == 0) {
    Site_id <- dbW_getSiteId(lat,long,Label)
  } else {
    if(!dbGetQuery(con.env$con, paste("SELECT COUNT(*) FROM WeatherData WHERE Site_id=",Site_id,";",sep=""))[1,1]) {
      stop("Site_id does not exist.")
    }
  }
  if(!is.null(Site_id) && is.integer(Site_id) && Site_id >= 0) {
    Scenario <- dbGetQuery(con.env$con, paste("SELECT id FROM Scenarios WHERE Scenario='",Scenario,"';",sep=""))[1,1]
    result <- dbGetQuery(con.env$con, paste("SELECT StartYear,EndYear,data FROM WeatherData WHERE Site_id=",Site_id, " AND Scenario=",Scenario,";",sep=""));
    data <- dbW_blob_to_weatherData(result$StartYear, result$EndYear, result$data)
    if(inherits(data, "try-error")) stop(paste("Weather data for Site_id", Site_id, "is corrupted"))
  } else {
    stop(paste("Site_id for", Label, "not obtained."))
  }
  
  if(useYears) {
    if(useStart && useEnd) {
      startYear_idx <- match(startYear, 
                             as.integer(unlist(lapply(data, FUN=slot, "year"))))
      endYear_idx <- match(endYear, 
                           as.integer(unlist(lapply(data, FUN=slot, "year"))))
      data <- data[startYear_idx:endYear_idx]
    } else if(useStart) {
      startYear_idx <- match(startYear, 
                             as.integer(unlist(lapply(data, FUN=slot, "year"))))
      data <- data[startYear_idx:length(as.integer(unlist(lapply(data, FUN=slot, "year"))))]
    } else if(useEnd) {
      endYear_idx <- match(endYear,
                           as.integer(unlist(lapply(data, FUN=slot, "year"))))
      data <- data[1:endYear_idx]
    }
  }
  return(data)
}

#Function to extract data from blobs
dbW_blob_to_weatherData <- function(StartYear, EndYear, data_blob) {
  if(typeof(data_blob) == "list")
    data_blob <- data_blob[[1]]
  data <- strsplit(rawToChar(memDecompress(data_blob, type="gzip")), ";")[[1]]
  years <- seq(from=StartYear, to=EndYear)
  weatherData <- data.frame()
  for(i in 1:length(years)) {
    ydata <- read.csv(textConnection(data[i]),header=FALSE,sep=",",stringsAsFactors=FALSE)
    ydata <- as.data.frame(cbind(seq(from=1,to=dim(ydata)[1]),ydata))
    colnames(ydata) <- c("DOY","Tmax_C","Tmin_C","PPT_cm")
    ydata$year<-years[i]
    weatherData<-rbind.fill(weatherData,ydata)
  }
  return(weatherData)
}

#Function to extract data for a specific site
.local <- function(sid){
  i_sw_weatherList <- list()
  for(k in seq_along(climate.conditions))
    i_sw_weatherList[[k]] <- dbW_getWeatherData(Site_id=sid, Scenario=climate.conditions[k])
  return(i_sw_weatherList)
  
}


#Function to extract respective data for all sites and save it as a list


extract_data<-function(site_to_extract=NULL)
{
  sw_weatherList <- NULL
  for(i in seq_along(site_to_extract)){
    sw_weatherList[[i]] <- try(.local(sid=site_to_extract[i]), silent=TRUE)
  }
  return (sw_weatherList)
}

sw_weatherList<-extract_data(site_to_extract = sites)


#Extract average ppt for all sites, across all years
avg_ppt_all=c()

for(i in sites) 
{
  sw_weatherList_temp<-data.frame(sw_weatherList[[i]][[1]])
  avg_ppt=mean(sw_weatherList_temp$PPT_cm)
  avg_ppt_all<-c(avg_ppt_all,avg_ppt)
}

#Extract wettest and driest site id
wettest<-which.max(avg_ppt_all)
driest<-which.min(avg_ppt_all)

#Start Year
AssemblyStartYear<-1980

#Get required files for wettest year
setwd(source.dir)
dir.create(paste0("wettest.",wettest), showWarnings = FALSE)
setwd(paste0("wettest.",wettest))
    #Assemble data for every year
    for(year in AssemblyStartYear: (AssemblyStartYear+30))
    {
      sw_weatherList_temp<-data.frame(sw_weatherList[[wettest]][[1]])
      x<-sw_weatherList_temp[sw_weatherList_temp$year==year,1:4];
      colnames(x)<-c("#DOY","Tmax_C", "Tmin_C","PPT_cm")# relabel the columns names 
      rownames(x)<- NULL #make rownames null (need this or else will have an extra column)
      write.table(x, file=paste("weath.",year,sep=""), sep="\t", row.names=F,quote=F) #write your year file in your directory
      year<-year+1
      
    }

#Get required files for driest year
setwd(source.dir)
dir.create(paste0("driest.",driest), showWarnings = FALSE)
setwd(paste0("driest.",driest))
#Assemble data for every year
for(year in AssemblyStartYear: (AssemblyStartYear+30))
{
  sw_weatherList_temp<-data.frame(sw_weatherList[[driest]][[1]])
  x<-sw_weatherList_temp[sw_weatherList_temp$year==year,1:4];
  colnames(x)<-c("#DOY","Tmax_C", "Tmin_C","PPT_cm")# relabel the columns names 
  rownames(x)<- NULL #make rownames null (need this or else will have an extra column)
  write.table(x, file=paste("weath.",year,sep=""), sep="\t", row.names=F,quote=F) #write your year file in your directory
  year<-year+1
  
}

setwd(source.dir)

dbDisconnect(db)

#####################################

  setwd(paste("wettest.",wettest,sep=""))
  scen<-"Current" #load a particular scenario
  
  ############ make mkv_prob.in file #############   
  #input data for particular site and scenario
  DGF<-data.frame(sw_weatherList[[wettest]][[1]])
  #add WET column 
  DGF<-within(DGF, WET<-FALSE)
  DGF[(DGF$PPT_cm>0),"WET"]<-TRUE
  
  #add WET given WET or WET given DRY column
  
  for (i in 1:nrow(DGF))
  {
    if(i==1)
    {
      if((DGF$WET[i]==TRUE)&(DGF$WET[i+364]==TRUE))
      {
        DGF$WW[i]=1
        DGF$WD[i]=0
        
      }
      else if((DGF$WET[i]==TRUE)&(DGF$WET[i+364]==FALSE))
      {
        DGF$WW[i]=0
        DGF$WD[i]=1
      }
      else{
        DGF$WW[i]=0
        DGF$WD[i]=0          
      }
    }
    
    else{
      
      if((DGF$WET[i]==TRUE)&(DGF$WET[i-1]==TRUE))
      {
        DGF$WW[i]=1
        DGF$WD[i]=0      }
      else if((DGF$WET[i]==TRUE)&(DGF$WET[i-1]==FALSE))
      {
        DGF$WW[i]=0
        DGF$WD[i]=1      }
      else{
        DGF$WW[i]=0
        DGF$WD[i]=0          
      }
    }
  }
  
  #create vectors to store data
  DOY<-vector();p_W_W<-vector();p_W_D<-vector();PPT_avg<-vector();PPT_sd<-vector();CF.max.w<-vector();
  CF.max.d<-vector();CF.min.w<-vector();CF.min.d<-vector()
  
  #make a dataframe for storing mkv_prob.in data
  DF<-data.frame(DOY,p_W_W,p_W_D,PPT_avg,PPT_sd,CF.max.w,CF.max.d,CF.min.w,CF.min.d)
  
  #celcius to kelvin conversion
  DGF$Tmax_C<-DGF$Tmax_C+273.15
  DGF$Tmin_C<-DGF$Tmin_C+273.15
  
  
  
  for ( i in 1:366) #loop through all possible days in all years
  {
    #probability of wet|wet is the number of wet given wet years for that day divided by the number
    #of total wet days from the previous day
    
    #prbability of wet|dry is the number of wet given dry years for that day divdied by the number of
    #total years (yrs identified by user) minus the total number of wet days from the previous day
    #or the number of dry days
    
    if(i==1)
    {
      p_W_W<-sum(DGF[(DGF$WW==1)&(DGF$DOY==i),7])
      p_W_D<-sum(DGF[(DGF$WD==1)&(DGF$DOY==i),8])
      
      p_W_W<-p_W_W/(sum(DGF[(DGF$WW==1)&(DGF$DOY==i+364),7])+sum(DGF[(DGF$WD==1)&(DGF$DOY==i+364),8]))
      p_W_D<-p_W_D/(yr-(sum(DGF[(DGF$WW==1)&(DGF$DOY==i+364),7])+sum(DGF[(DGF$WD==1)&(DGF$DOY==i+364),8])))
    }else
    {
      p_W_W<-sum(DGF[(DGF$WW==1)&(DGF$DOY==i),7])
      p_W_D<-sum(DGF[(DGF$WD==1)&(DGF$DOY==i),8])
      
      #p_W_W<-p_W_W/(DF$p_W_W[i-1]+DF$p_W_D[i-1])
      #p_W_D<-p_W_D/(yr-(DF$p_W_W[i-1]+DF$p_W_D[i-1]))
      
      p_W_W<-sum(DGF[(DGF$WW==1)&(DGF$DOY==i),7])
      p_W_D<-sum(DGF[(DGF$WD==1)&(DGF$DOY==i),8])
      
      p_W_W<-p_W_W/(sum(DGF[(DGF$WW==1)&(DGF$DOY==i-1),7])+sum(DGF[(DGF$WD==1)&(DGF$DOY==i-1),8]))
      p_W_D<-p_W_D/(yr-(sum(DGF[(DGF$WW==1)&(DGF$DOY==i-1),7])+sum(DGF[(DGF$WD==1)&(DGF$DOY==i-1),8])))
    }
    
    
    CF.max.w<-(abs(mean(DGF[(DGF$WET=="TRUE"),2])/mean(DGF[(DGF$DOY==i),2]))) + (mean(DGF[(DGF$WET=="TRUE"),2])-mean(DGF[(DGF$DOY==i),2]))/mean(DGF[(DGF$DOY==i),2])
    if (CF.max.w > 1.0) {CF.max.w<-1}
    CF.max.d<-(abs(mean(DGF[(DGF$WET=="FALSE"),2])/mean(DGF[(DGF$DOY==i),2]))) + (mean(DGF[(DGF$WET=="FALSE"),2])-mean(DGF[(DGF$DOY==i),2]))/mean(DGF[(DGF$DOY==i),2])
    if (CF.max.d < 1.0) {CF.max.d<-1}
    CF.min.w<-(abs(mean(DGF[(DGF$WET=="TRUE"),3])/mean(DGF[(DGF$DOY==i),3]))) + (mean(DGF[(DGF$WET=="TRUE"),3])-mean(DGF[(DGF$DOY==i),3]))/mean(DGF[(DGF$DOY==i),3])
    if (CF.min.w > 1.0) {CF.min.w<-1}
    CF.min.d<-(abs(mean(DGF[(DGF$WET=="FALSE"),3])/mean(DGF[(DGF$DOY==i),3]))) + (mean(DGF[(DGF$WET=="FALSE"),3])-mean(DGF[(DGF$DOY==i),3]))/mean(DGF[(DGF$DOY==i),3])
    if (CF.min.d < 1.0) {CF.min.d<-1}
    
    #DF.DAY$W_W[i]<-sum(prob.wet_wet) #sum all of the wet given wet days for the day across all the years
    #DF.DAY$W_D[i]<-sum(prob.wet_dry) #sum all of the wet given dry days for the day across all the years
    
    PPT_avg<-mean(DGF[(DGF$DOY==i),4]) #average the ppt across all the years for that day
    PPT_sd<-(sd((DGF[(DGF$DOY==i),4])))*2 #standard deviation the ppt across all the years for that day
    CF.max.w<-CF.max.w
    CF.max.d<-CF.max.d
    CF.min.w<-CF.min.w
    CF.min.d<-CF.min.d
    
    newrow<-data.frame(DOY=i,p_W_W=p_W_W,p_W_D=p_W_D,PPT_avg=PPT_avg,PPT_sd=PPT_sd,CF.max.w=CF.max.w,CF.max.d=CF.max.d,CF.min.w=CF.min.w,CF.min.d=CF.min.d)
    DF<-rbind(DF,newrow)
    
  } 
  
  # print out the probability file
  colnames(DF)<-c("#DOY","p[W|W]","p[W|D]","PPT_avg","PPT_sd","CF.max.w","CF.max.d","CF.min.w","CF.min.d")# relabel the columns names 
  #DF<-DF[,c("#DOY","p[W|W]","p[W|D]","PPT_avg","PPT_sd","CF.max.w","CF.max.d","CF.min.w","CF.min.d")] #put columns in correct order for output
  rownames(DF)<- NULL      
  write.table(format(DF, digits=5), file=paste("mkv_prob.in"), sep="\t", row.names=F,quote=F) #write your year file
  
  ###########################################################################
  
  ################## Write mkv_covar.in FILE  ##############################
  
  DGF_covar<-data.frame(sw_weatherList[[wettest]][[1]])
  for (k in 1:nrow(DGF_covar))
  {
    tempdate<-strptime(paste(DGF_covar$year[k],DGF_covar$DOY[k]),format="%Y %j")
    DGF_covar$WEEK[k]<-as.numeric(strftime(tempdate,format="%W")) # if PPT >0 the day is labeled wet
    DGF_covar$WEEK[k]<-DGF_covar$WEEK[k]+1
  }
  
  WEEK<-vector();T.MAX.C<-vector();T.MIN.C<-vector();cov_MINMIN<-vector();cov_MAXMIN<-vector();cov_MINMAX<-vector();
  cov_MAXMAX<-vector();
  
  #make a dataframe for storing mkv_prob.in data
  DF_covar<-data.frame(WEEK,T.MAX.C,T.MIN.C,cov_MINMIN,cov_MAXMIN,cov_MINMAX,cov_MAXMAX)
  
  for (w in 1:53) { 
    WEEK<-w
    min<-(DGF_covar[(DGF_covar$WEEK==w),3])
    max<-(DGF_covar[(DGF_covar$WEEK==w),2])
    MIN.MAX<-cov(min,max) #covariance between min and max temp over all days in week for all years
    MIN.MIN<-cov(min,min) #covariance between min temps over all days in week for all years
    MAX.MAX<-cov(max,max) #covariance between max temps over all days in week for all years
    MAX.MIN<-MIN.MAX
    T.MAX.C<-mean(max) #mean max temp for the week 
    T.MIN.C<-mean(min) #mean min temp for the week
    
    newrow<-data.frame(WEEK=WEEK,T.MAX.C=T.MAX.C,T.MIN.C=T.MIN.C,cov_MINMIN=MIN.MIN,cov_MAXMIN=MAX.MIN,cov_MINMAX=MIN.MAX,cov_MAXMAX=MAX.MAX)
    DF_covar<-rbind(DF_covar,newrow)
    
  }
  
  #Then write the files
  #rename columns
  colnames(DF_covar)<-c("#WEEK","T.MAX.C", "T.MIN.C","cov[MIN.MIN]","cov[MAX.MIN]","cov[MIN.MAX]","cov[MAX.MAX]")# relabel the columns names 
  rownames(DF_covar)<- NULL #make rownames null (need this or else will have an extra column)
  write.table(format(DF_covar, digits=5), file=paste("mkv_covar.in"), sep="\t", row.names=F,quote=F) #write your year file in your directory
  
  #reset directory to site level
  setwd(source.dir)
  
####################################################################

  setwd(paste("driest.",driest,sep=""))
  scen<-"Current" #load a particular scenario
  
  ############ make mkv_prob.in file #############   
  #input data for particular site and scenario
  DGF<-data.frame(sw_weatherList[[driest]][[1]])
  #add WET column 
  DGF<-within(DGF, WET<-FALSE)
  DGF[(DGF$PPT_cm>0),"WET"]<-TRUE
  
  #add WET given WET or WET given DRY column
  
  for (i in 1:nrow(DGF))
  {
    if(i==1)
    {
      if((DGF$WET[i]==TRUE)&(DGF$WET[i+364]==TRUE))
      {
        DGF$WW[i]=1
        DGF$WD[i]=0
        
      }
      else if((DGF$WET[i]==TRUE)&(DGF$WET[i+364]==FALSE))
      {
        DGF$WW[i]=0
        DGF$WD[i]=1
      }
      else{
        DGF$WW[i]=0
        DGF$WD[i]=0          
      }
    }
    
    else{
      
      if((DGF$WET[i]==TRUE)&(DGF$WET[i-1]==TRUE))
      {
        DGF$WW[i]=1
        DGF$WD[i]=0      }
      else if((DGF$WET[i]==TRUE)&(DGF$WET[i-1]==FALSE))
      {
        DGF$WW[i]=0
        DGF$WD[i]=1      }
      else{
        DGF$WW[i]=0
        DGF$WD[i]=0          
      }
    }
  }
  
  #create vectors to store data
  DOY<-vector();p_W_W<-vector();p_W_D<-vector();PPT_avg<-vector();PPT_sd<-vector();CF.max.w<-vector();
  CF.max.d<-vector();CF.min.w<-vector();CF.min.d<-vector()
  
  #make a dataframe for storing mkv_prob.in data
  DF<-data.frame(DOY,p_W_W,p_W_D,PPT_avg,PPT_sd,CF.max.w,CF.max.d,CF.min.w,CF.min.d)
  
  #celcius to kelvin conversion
  DGF$Tmax_C<-DGF$Tmax_C+273.15
  DGF$Tmin_C<-DGF$Tmin_C+273.15
  
  
  
  for ( i in 1:366) #loop through all possible days in all years
  {
    #probability of wet|wet is the number of wet given wet years for that day divided by the number
    #of total wet days from the previous day
    
    #prbability of wet|dry is the number of wet given dry years for that day divdied by the number of
    #total years (yrs identified by user) minus the total number of wet days from the previous day
    #or the number of dry days
    
    if(i==1)
    {
      p_W_W<-sum(DGF[(DGF$WW==1)&(DGF$DOY==i),7])
      p_W_D<-sum(DGF[(DGF$WD==1)&(DGF$DOY==i),8])
      
      p_W_W<-p_W_W/(sum(DGF[(DGF$WW==1)&(DGF$DOY==i+364),7])+sum(DGF[(DGF$WD==1)&(DGF$DOY==i+364),8]))
      p_W_D<-p_W_D/(yr-(sum(DGF[(DGF$WW==1)&(DGF$DOY==i+364),7])+sum(DGF[(DGF$WD==1)&(DGF$DOY==i+364),8])))
    }else
    {
      p_W_W<-sum(DGF[(DGF$WW==1)&(DGF$DOY==i),7])
      p_W_D<-sum(DGF[(DGF$WD==1)&(DGF$DOY==i),8])
      
      #p_W_W<-p_W_W/(DF$p_W_W[i-1]+DF$p_W_D[i-1])
      #p_W_D<-p_W_D/(yr-(DF$p_W_W[i-1]+DF$p_W_D[i-1]))
      
      p_W_W<-sum(DGF[(DGF$WW==1)&(DGF$DOY==i),7])
      p_W_D<-sum(DGF[(DGF$WD==1)&(DGF$DOY==i),8])
      
      p_W_W<-p_W_W/(sum(DGF[(DGF$WW==1)&(DGF$DOY==i-1),7])+sum(DGF[(DGF$WD==1)&(DGF$DOY==i-1),8]))
      p_W_D<-p_W_D/(yr-(sum(DGF[(DGF$WW==1)&(DGF$DOY==i-1),7])+sum(DGF[(DGF$WD==1)&(DGF$DOY==i-1),8])))
    }
    
    
    CF.max.w<-(abs(mean(DGF[(DGF$WET=="TRUE"),2])/mean(DGF[(DGF$DOY==i),2]))) + (mean(DGF[(DGF$WET=="TRUE"),2])-mean(DGF[(DGF$DOY==i),2]))/mean(DGF[(DGF$DOY==i),2])
    if (CF.max.w > 1.0) {CF.max.w<-1}
    CF.max.d<-(abs(mean(DGF[(DGF$WET=="FALSE"),2])/mean(DGF[(DGF$DOY==i),2]))) + (mean(DGF[(DGF$WET=="FALSE"),2])-mean(DGF[(DGF$DOY==i),2]))/mean(DGF[(DGF$DOY==i),2])
    if (CF.max.d < 1.0) {CF.max.d<-1}
    CF.min.w<-(abs(mean(DGF[(DGF$WET=="TRUE"),3])/mean(DGF[(DGF$DOY==i),3]))) + (mean(DGF[(DGF$WET=="TRUE"),3])-mean(DGF[(DGF$DOY==i),3]))/mean(DGF[(DGF$DOY==i),3])
    if (CF.min.w > 1.0) {CF.min.w<-1}
    CF.min.d<-(abs(mean(DGF[(DGF$WET=="FALSE"),3])/mean(DGF[(DGF$DOY==i),3]))) + (mean(DGF[(DGF$WET=="FALSE"),3])-mean(DGF[(DGF$DOY==i),3]))/mean(DGF[(DGF$DOY==i),3])
    if (CF.min.d < 1.0) {CF.min.d<-1}
    
    #DF.DAY$W_W[i]<-sum(prob.wet_wet) #sum all of the wet given wet days for the day across all the years
    #DF.DAY$W_D[i]<-sum(prob.wet_dry) #sum all of the wet given dry days for the day across all the years
    
    PPT_avg<-mean(DGF[(DGF$DOY==i),4]) #average the ppt across all the years for that day
    PPT_sd<-(sd((DGF[(DGF$DOY==i),4])))*2 #standard deviation the ppt across all the years for that day
    CF.max.w<-CF.max.w
    CF.max.d<-CF.max.d
    CF.min.w<-CF.min.w
    CF.min.d<-CF.min.d
    
    newrow<-data.frame(DOY=i,p_W_W=p_W_W,p_W_D=p_W_D,PPT_avg=PPT_avg,PPT_sd=PPT_sd,CF.max.w=CF.max.w,CF.max.d=CF.max.d,CF.min.w=CF.min.w,CF.min.d=CF.min.d)
    DF<-rbind(DF,newrow)
    
  } 
  
  # print out the probability file
  colnames(DF)<-c("#DOY","p[W|W]","p[W|D]","PPT_avg","PPT_sd","CF.max.w","CF.max.d","CF.min.w","CF.min.d")# relabel the columns names 
  #DF<-DF[,c("#DOY","p[W|W]","p[W|D]","PPT_avg","PPT_sd","CF.max.w","CF.max.d","CF.min.w","CF.min.d")] #put columns in correct order for output
  rownames(DF)<- NULL      
  write.table(format(DF, digits=5), file=paste("mkv_prob.in"), sep="\t", row.names=F,quote=F) #write your year file
  
  ###########################################################################
  
  ################## Write mkv_covar.in FILE  ##############################
  
  DGF_covar<-data.frame(sw_weatherList[[driest]][[1]])
  for (k in 1:nrow(DGF_covar))
  {
    tempdate<-strptime(paste(DGF_covar$year[k],DGF_covar$DOY[k]),format="%Y %j")
    DGF_covar$WEEK[k]<-as.numeric(strftime(tempdate,format="%W")) # if PPT >0 the day is labeled wet
    DGF_covar$WEEK[k]<-DGF_covar$WEEK[k]+1
  }
  
  WEEK<-vector();T.MAX.C<-vector();T.MIN.C<-vector();cov_MINMIN<-vector();cov_MAXMIN<-vector();cov_MINMAX<-vector();
  cov_MAXMAX<-vector();
  
  #make a dataframe for storing mkv_prob.in data
  DF_covar<-data.frame(WEEK,T.MAX.C,T.MIN.C,cov_MINMIN,cov_MAXMIN,cov_MINMAX,cov_MAXMAX)
  
  for (w in 1:53) { 
    WEEK<-w
    min<-(DGF_covar[(DGF_covar$WEEK==w),3])
    max<-(DGF_covar[(DGF_covar$WEEK==w),2])
    MIN.MAX<-cov(min,max) #covariance between min and max temp over all days in week for all years
    MIN.MIN<-cov(min,min) #covariance between min temps over all days in week for all years
    MAX.MAX<-cov(max,max) #covariance between max temps over all days in week for all years
    MAX.MIN<-MIN.MAX
    T.MAX.C<-mean(max) #mean max temp for the week 
    T.MIN.C<-mean(min) #mean min temp for the week
    
    newrow<-data.frame(WEEK=WEEK,T.MAX.C=T.MAX.C,T.MIN.C=T.MIN.C,cov_MINMIN=MIN.MIN,cov_MAXMIN=MAX.MIN,cov_MINMAX=MIN.MAX,cov_MAXMAX=MAX.MAX)
    DF_covar<-rbind(DF_covar,newrow)
    
  }
  
  #Then write the files
  #rename columns
  colnames(DF_covar)<-c("#WEEK","T.MAX.C", "T.MIN.C","cov[MIN.MIN]","cov[MAX.MIN]","cov[MIN.MAX]","cov[MAX.MAX]")# relabel the columns names 
  rownames(DF_covar)<- NULL #make rownames null (need this or else will have an extra column)
  write.table(format(DF_covar, digits=5), file=paste("mkv_covar.in"), sep="\t", row.names=F,quote=F) #write your year file in your directory
  
  #reset directory to site level
  setwd(source.dir)
  
  

tick_off<-proc.time()-tick_on

