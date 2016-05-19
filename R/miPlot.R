#' @title Mi Plot
#' @description Generic plot function for Mi Data. Using rChart library to plot.
#' @param MiData is a list returned by function loadMiData
#' @param type character indicating type of plot. Options are "hist", "ts", "week", "box". "week" plots mean value of the same day in a week, such as mean value of Monday.
#' @param  y variable to be plotted as y value. Options are "light", "deep", "sleep", "step". "Sleep" means to plot both light sleep and deep sleep as y values.
#' @param  col color to be parsed to ggplot function.
#' @param bins only used in hist
#' @return ggplot object
#' @export
#' @author Borg

miPlot <- function(MiData,type,y,col="blue",bins=30,by="week",fill="#575757"){
        # plottype: hist_light, hist_deep, hist_step, hist_efficiency
        #           ts_light, ts_deep, ts_light+deep, ts_step, ts_efficiency
        #           week_light, week_deep, week_step, week_efficiency
        #TO-DO      month_light, month_deep, month_step
        #           box_light_deep
        switch (by,
                'week' = {data_plot<-MiData$data_week}
        )
        switch (type,
                "hist" = {
                        switch (y,
                                'step' = {p <- ggplot(data = data_plot,aes(step))+geom_histogram(aes(y=..density..),col=col,bins=bins,fill=fill)+geom_density(stat = "density")+labs(title="Step Histogram")},
                                'light' = {p <- ggplot(data=data_plot,aes(sleep.light/60)) + geom_histogram(aes(y=..density..),col=col,fill=fill,bins=bins) + geom_density(stat= "density") + labs(title="Light Sleep Histogram") },
                                'sleep' ={
                                        data_plot=melt(data_plot)
                                        p<- ggplot(data_plot,aes(x=value)) + geom_histogram(data=filter(data_plot,variable=="sleep.light"),fill="red",alpha=0.4,bins=bins) + geom_histogram(data=filter(data_plot,variable=="sleep.deep"),fill="blue",alpha=0.4,bins=bins)
                                },
                                'deep' = {p <- ggplot(data=data_plot,aes(sleep.deep/60)) + geom_histogram(aes(y=..density..),col=col,bins=bins,fill=fill) + geom_density(stat= "density") + labs(title="Light Sleep Histogram") },
                                'efficiency' = {p <- ggplot(data=data_plot,aes(sleep.efficiency)) + geom_histogram(aes(y=..density..),col=col,bins=bins,fill=fill) + geom_density(stat= "density") + labs(title="Sleep Efficiency Histogram") }
                        )
                },
                "ts"={
                        switch (y,
                                'step' = {p <- ggplot(data = data_plot,aes(x=date,y=step))+ geom_line(col=col) +labs(title="Step Time Sequence")},
                                'light' = {p <- ggplot(data=data_plot,aes(x=date,y=sleep.light/60)) + geom_line(col=col) +labs(title="Light Sleep Time Sequence")},
                                'deep' = {p <- ggplot(data=data_plot,aes(x=date,y=sleep.deep/60)) + geom_line(col=col) +labs(title="Deep Sleep Time Sequence")},
                                'efficiency' = {p <- ggplot(data=data_plot,aes(x=date,y=efficiency)) + geom_line(col=col) +labs(title="Efficiency Time Sequence")},
                                'sleep' = {p <- ggplot(data=data_plot,aes(x=date,y=variable,col=type)) + geom_line(aes(y=sleep.light/60,col="sleep.light")) + geom_line(aes(y=sleep.deep/60,col="sleep.deep")) +labs(title="Sleep Time Sequence")}
                        )
                },
                "week"={
                        data_plot<-MiData$avg_week
                        switch (y,
                                'step' = {p <- ggplot(data = data_plot,aes(x=weekday,y=step))+ geom_bar(col=col,stat="identity",fill=fill) +labs(title="Step Week Mean")},
                                'light' = {p <- ggplot(data=data_plot,aes(x=weekday,y=sleep.light/60)) + geom_bar(col=col,fill=fill,stat="identity") +labs(title="Light Sleep Week Mean")},
                                'deep' = {p <- ggplot(data=data_plot,aes(x=weekday,y=sleep.deep/60)) + geom_bar(col=col,fill=fill,stat="identity") +labs(title="Deep Sleep Week Mean")},
                                'efficiency' = {p <- ggplot(data=data_plot,aes(x=weekday,y=efficiency)) + geom_bar(col=col,fill=fill,stat="identity") +labs(title="Efficiency Week Mean")},
                                'sleep' = {p <- ggplot(data=data_plot,aes(x=weekday,y=variable,col=type)) + geom_bar(aes(y=sleep.light/60,col="sleep.light"),stat = "identity") + geom_bar(aes(y=sleep.deep/60,col="sleep.deep"),stat="identity") +labs(title="Sleep Week Mean")}
                        )
                },
                'box'={
                        switch (y,
                                'sleep' = {
                                        data_plot <- dplyr::filter(melt(data_plot),variable=="sleep.deep"|variable=="sleep.light")
                                        p <- ggplot(data_plot,aes(x=variable,y=value/60)) + geom_boxplot(aes(fill=variable))+labs(title="Sleep Box Plot")+labs(y="duration(hour)")
                                }
                                ,
                                'light' = {
                                        data_plot <- dplyr::filter(melt(data_plot),variable=="sleep.light")
                                        p <- ggplot(data_plot,aes(x=variable,y=value/60)) + geom_boxplot(aes(fill=fill))+labs(title="Light Sleep Box Plot")+labs(y="light sleep(hour)")
                                },
                                'deep' = {
                                        data_plot <- dplyr::filter(melt(data_plot),variable=="sleep.deep")
                                        p <- ggplot(data_plot,aes(x=variable,y=value/60)) + geom_boxplot(aes(fill=fill))+labs(title="Deep Sleep Box Plot")+labs(y="deep sleep(hour)")
                                },
                                'step' = {
                                        data_plot <- dplyr::filter(melt(data_plot),variable=="step")
                                        p <- ggplot(data_plot,aes(x=variable,y=value/60)) + geom_boxplot(aes(fill=fill))+labs(title="Step Box Plot")+labs(y="step count")
                                },
                                'efficiency' = {
                                        data_plot <- dplyr::filter(melt(data_plot),variable=="efficiency")
                                        p <- ggplot(data_plot,aes(x=variable,y=value/60)) + geom_boxplot(aes(fill=fill))+labs(title="Efficiency Box Plot")+labs(y="Efficiency")
                                }
                        )
                }
        )
        return(p)
}
