#' @title weeksummary
#' @description summary groupped by week
#' @param MiData is a list returned by function loadMiData
#' @param show whether to show plots directly or just return plots
#' @return a list of plot objects
#' @export
#' @import ggplot2 reshape2
#' @author Borg

weeksummary <- function(MiData,show=TRUE){
        groupeddata<-group_by(MiData$completedata,weekday)
        avgs<-summarize(groupeddata,
                        sleep.light=mean(sleep.light[sleep.light!=0]),
                        sleep.deep=mean(sleep.deep[sleep.deep!=0]),
                        step=mean(step[step!=0]))
        avgs<-avgs[c(4,2,6,7,5,1,3),] #sort by weekday
        level <- avgs$weekday
        avgs$weekday <- as.factor(avgs$weekday)
        levels(avgs$weekday) <- level
        levels(avgs$weekday)

        q1<-ggplot(data=avgs,aes(x=weekday,y=sleep.light/60))+
                geom_bar(stat="identity")+
                labs(title="Sleep Averages")+
                labs(y="average light sleep(hour)")+
                theme(axis.text.x = element_text(face="bold", color="#993333"))
        if(show==TRUE){
                print(q1)
                userinput <- base::readline(prompt="Enter n to stop or anything to continue:")
                if(userinput=="n" | userinput=="N"){
                        stop("Stop according to user's input")
                }
        }
        q2<-ggplot(data=avgs,aes(x=weekday,y=sleep.deep/60))+
                geom_bar(stat="identity")+
                labs(title="Sleep Averages")+
                labs(y="average deep sleep(hour)")+
                theme(axis.text.x = element_text(face="bold", color="#993333"))
        if(show==TRUE){
                print(q2)
                userinput <- base::readline(prompt="Enter n to stop or anything to continue:")
                if(userinput=="n" | userinput=="N"){
                        stop("Stop according to user's input")
                }
        }
        q3<-ggplot(data=avgs,aes(x=weekday,y=step/60))+
                geom_bar(stat="identity")+
                labs(title="Step Averages")+
                theme(axis.text.x = element_text(face="bold", color="#993333"))
        if(show==TRUE){
                print(q3)
#                 userinput <- base::readline(prompt="Enter n to stop or anything to continue:")
#                 if(userinput=="n" | userinput=="N"){
#                         stop("Stop according to user's input")
#                 }
        }
        return(list(lightsleep=q1,deepsleep=q2,step=q3,data=avgs))
}
