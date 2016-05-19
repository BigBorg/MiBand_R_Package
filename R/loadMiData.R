#' @title  Load MiBand data
#' @description  Load in MiBand data from databases directory. Extract steps, slight sleep and deep sleep data as a data frame. Fill in missing data with average values of the same day in a week.
#' @param path a character string specifing the "databases" directory which is copied from android system /data/data/com.xiaomi.hm.health. Example: If folder databases is in home directory, then path should be "~/databases".
#' @param user_id a caracter string specifying your id. User id can be accessed in the user info page in MiBand android app. Or you can find it as a part of file name specifying the origin_db file. For example, you may find a file named as "origin_db_123456" in the databases directory, then parse character string "123456" as user_id.
#' @return return a list with element "cleandata" as a data frame with missing value and "data_week" as a data frame with missing data substitued.
#' @export
#' @import DBI RSQLite dplyr jsonlite
#' @author Borg
loadMiData <- function(path,user_id){
        baklocale <- Sys.getlocale("LC_TIME")
        Sys.setlocale("LC_TIME","en_US.UTF-8")
	if( class(path)!="character" | class(user_id) != "character"){
	        stop("Bad input, please parse character class for prameter path and user_id")
	}

        if( !dir.exists(path) ){
                stop("invalid path: path of directory called \"databases\", such as \"~/databases\". " )
        }

        curwd <- getwd()
        setwd(path)
        filename<-paste("origin_db",user_id,sep = "_")
        if( !file.exists(filename) ){
                stop("invalid origin_db file: please put origin_db_<user_id> file inside databases directory")
        }

        sqlite    <- dbDriver("SQLite")
        sourcedb <- dbConnect(sqlite,filename)
        sourceData <- dbGetQuery(sourcedb,"select * from date_data")
        setwd(curwd)

        sourceData$DATE<-as.Date(sourceData$DATE,"%Y-%m-%d")
        jsondata<-sapply(sourceData$SUMMARY,fromJSON)
        slp<-jsondata[1,]
        stp<-jsondata[4,]
        lightsleep<-unname( unlist( lapply(slp,"[[","lt") )  )   ##extract elements from list of list
        deepsleep<-unname( unlist( lapply(slp,"[[","dp") )  )
        totalstep<-unname( unlist( lapply(stp,"[[","ttl") ) )
        cleanData <- data.frame(date=sourceData$DATE,sleep.light=lightsleep,sleep.deep=deepsleep,step=totalstep,efficiency=deepsleep/(deepsleep+lightsleep) )
        cleanData<-dplyr::arrange(cleanData,date)

        # Fill by mean of week
        cleanData$weekday<-weekdays(cleanData$date)
        cleanData$weekday <- as.factor(cleanData$weekday)
        levels(cleanData$weekday) <- c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")
        groupeddata<-group_by(cleanData,weekday)
        avgs<-summarize(groupeddata,
                        sleep.light=mean(sleep.light[sleep.light!=0]),
                        sleep.deep=mean(sleep.deep[sleep.deep!=0]),
                        step=mean(step[step!=0]),
                        efficiency=mean(efficiency,na.rm = TRUE))
        replace <- function(rowunit,avgs){
                for( col in 1:ncol(rowunit) ){
                        if(is.nan(rowunit[,col]) | rowunit[,col]==0){
                                rowunit[,col]<-avgs[avgs[,1]==as.character(rowunit$weekday),col]
                        }
                }
                rowunit
        }
        completedata<-by(cleanData,1:nrow(cleanData),function(rowunit) replace(rowunit,avgs))
        completedata<-as.data.frame(do.call("rbind",completedata))

        #TO-DO: fill missing data by mean of month
        #TO-DO: fill missing data by mean of the same week
        #TO-DO: fill missing data by mean of the same month
        # Use NA as missing data indicator

        for(i in seq(ncol(cleanData))){
                cleanData[,i][cleanData[,i]==0] <- NA
        }

        Sys.setlocale("LC_TIME",baklocale)
        return(list(data_clean=cleanData,data_week=completedata,avg_week=avgs))
}
