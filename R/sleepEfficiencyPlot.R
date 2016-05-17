#' @title Sleep Efficiency Plot
#' @description Make plots of sleep efficiency against total sleep duration and date. Sleep efficiency is caculated as deep sleep duration divided by total sleep duration.
#' @param MiData is a list returned by function loadMiData
#' @param show whether to show plots directly or just return plots
#' @return a list of plots
#' @export
#' @author Borg

sleepEfficiencyPlot <- function(MiData,show=TRUE){
        q1 <- ggplot2::ggplot(data=MiData$completedata,aes(x=(sleep.light + sleep.deep)/60, y=sleep.deep/(sleep.light+sleep.deep)))+ylab("Sleep Efficiency")+xlab("total sleep(hour)")+labs(title="Sleep Efficiency over Total Sleep")+geom_point()+geom_smooth()
        if(show==TRUE){
                print(q1)
                userinput <- base::readline(prompt="Enter n to stop or anything to continue:")
                if(userinput=="n" | userinput=="N"){
                        stop("Stop according to user's input")
                }
        }
        rawdata <- MiData$rawdata[complete.cases(MiData$rawdata),]
        q2 <- ggplot2::ggplot(data=rawdata,aes(x=date, y=sleep.deep/(sleep.light+sleep.deep)))+ylab("Sleep Efficiency")+xlab("Date")+labs(title="Sleep Efficiency over Date")+geom_line()
        if(show==TRUE){
                print(q2)
                userinput <- base::readline(prompt="Enter n to stop or anything to continue:")
                if(userinput=="n" | userinput=="N"){
                        stop("Stop according to user's input")
                }
        }
        return(list(total=q1,date=q2))
}
