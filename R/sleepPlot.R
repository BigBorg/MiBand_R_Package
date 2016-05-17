#' @title Sleep Plots
#' @description plots of light sleep and deep sleep showing trend and difference between light sleep and deep sleep.
#' @param MiData is a list returned by function loadMiData
#' @param show whether to show plots directly or just return plots
#' @return a list of plot objects
#' @export
#' @author Borg

sleepPlot <- function(MiData,show=TRUE){
        sleepframe <- dplyr::filter(melt(MiData$completedata),variable=="sleep.deep"|variable=="sleep.light")
        p1 <- ggplot(data = sleepframe,aes(x=variable,y=value)) + geom_boxplot(aes(fill=variable))+labs(title="Box Plot")
        if(show==TRUE){
                print(p1)
                userinput <- base::readline(prompt="Enter n to stop or anything to continue:")
                if(userinput=="n" | userinput=="N"){
                        stop("Stop according to user's input")
                }
        }
        p2<-ggplot(data=MiData$completedata,aes(x=date,y=value,col=type))+
                geom_point(aes(y=sleep.light/60,col="light sleep"))+
                geom_point(aes(y=sleep.deep/60,col="deep sleep"))+
                geom_smooth(aes(y=sleep.light/60,col="light sleep"))+
                geom_smooth(aes(y=sleep.deep/60,col="deep sleep"))+
                labs(y="sleep(hour)")+labs(title="Sleep Trend")
        if(show==TRUE){
                print(p2)
        }
                return(list(boxplot=p1,trendplot=p2))
}
