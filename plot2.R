read.data<- function() {
      ## lubridate package will be used to deal with the date and time info
      library(lubridate)
      
      find.data<-function() {
            ## Checks for the existance of file and decompresses if necessary
            
            filesFound<-FALSE
            if(file.exists("household_power_consumption.txt")) {
                  filesFound<-TRUE
            }
            if(!filesFound &
                     file.exists("exdata-data-household_power_consumption.zip")){
                  message("Extracting compressed data to ",wd)
                  unzip("exdata-data-household_power_consumption.zip")
                  filesFound<-TRUE
            }
            return(filesFound)
      }
      
      ## Data might be saved to the working directory or to a folder called
      ## "ExData_Plotting1" (if that folder is not the working directory itself)
      originalWD<-getwd()
      find.data()
      if(!find.data() & file.exists("./ExData_Plotting1")) {
            setwd("./ExData_Plotting1")
            find.data()
      }
      if(!find.data()) {
            setwd(originalWD)
            message("Data not found in ",originalWD)
            stop("Dataset must be saved to your current working directory.")
      }
      ## Since the data table is very large, the following lines of command
      ## have the objectives of:
      ## 1: read just a few lines to gather the column classes in order to pass
      ## them as arguments later, which makes R read the table faster;
      ## 2: calculate the time interval of the data and then estimate which
      ## rows contain the data we are interested in;
      
      Data<-read.table("household_power_consumption.txt", header=TRUE,
                       comment.char="",nrows=100,sep=";")
      classes<-sapply(Data,class)
      Data[,1]<-dmy(Data[,1])
      Data[,2]<-hms(Data[,2])
      
      interval<-(Data[2:100,1]+Data[2:100,2])-(Data[1:99,1]+Data[1:99,2])
      interval<-mean(interval)
      dateOfInterest1<-dmy_hms("01/02/2007 00:00:00")
      dateOfInterest2<-dmy_hms("02/02/2007 23:59:59")
      startRow<-(as.duration(dateOfInterest1-(Data[1,1]+Data[1,2])))/interval
      endRow<-(as.duration(dateOfInterest2-(Data[1,1]+Data[1,2])))/interval
      startRow<-as.integer(startRow)
      endRow<-as.integer(endRow+1)
      nrows<-endRow-startRow
      
      colnames<-names(Data)
      Data<-read.table("household_power_consumption.txt", header=FALSE,
                       comment.char="",colClasses=classes,sep=";",
                       skip=startRow+1,nrows=nrows,col.names=colnames)
      Data[,1]<-dmy(Data[,1])
      Data[,2]<-hms(Data[,2])
      setwd(originalWD)
      return(Data)
}

## Function to create plot.
## 1 - Calls read.data function to get the data
## 2 - Sets the device as a png file inside the "figure" folder
## 3 - Generates plot and closes the png device

plot2<-function() {
      Data<-read.data()
      
      path<-NULL
      wd<-getwd()
      if (grepl("ExData_Plotting1/figure$",wd)) path<-NULL
      if (grepl("ExData_Plotting1$",wd)) path<-"figure/"
      if (dir.exists("ExData_Plotting1")) path<-"ExData_Plotting1/figure/"
      filePath<-paste(path,"plot2.png",sep="")
      png(filePath)
      
      with(Data,plot(Date+Time,Global_active_power,type="l",xlab="",
                     ylab="Global Active Power (kilowatts)"))
      dev.off()
}