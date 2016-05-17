#' @title Step Plots
#' @description plot of step trend
#' @param MiData is a list returned by function loadMiData
#' @param show whether to show plots directly or just return plots
#' @return plot object
#' @export
#' @import ggplot2
#' @author Borg

stepPlot <- function(MiData,show=TRUE){
        q1 <- ggplot(MiData$completedata,aes(step))+geom_histogram(aes(y=..density..),col="blue")+geom_density(stat = "density")+labs(title="Step Histogram")
        if(show==TRUE){
                print(q1)
                userinput <- base::readline(prompt="Enter n to stop or anything to continue:")
                if(userinput=="n" | userinput=="N"){
                        stop("Stop according to user's input")
                }
        }
        q2 <- ggplot(data=MiData$completedata,aes(x=date,y=step))+
                geom_point()+
                geom_smooth(method = "auto")+
                labs(title="Steps")
        if(show==TRUE){
                print(q2)
                userinput <- base::readline(prompt="Enter n to stop or anything to continue:")
                if(userinput=="n" | userinput=="N"){
                        stop("Stop according to user's input")
                }
        }
        return(list(hist=q1,trend=q2))
}
