library(data.table)
library(ggplot2)

dt <- data.table(read.csv("data/covid19dataexport.csv"))
dt[,Date.reported:=as.Date(Date.reported)]
unique(dt$Age.group)

severe_outcomes <-
  data.table(
    Age.group = c("Under 1 year",
                  "1-4 years",
                  "5-9 years",
                  "10-19 years",
                  "20-29 years",
                  "30-39 years",
                  "40-49 years",
                  "50-59 years",
                  "60-69 years",
                  "70-79 years",
                  "80+ years"),
    Hospitalization.Rate = c(
      54/1116,
      30/6615,
      24/9418,
      120/25726,
      417/35801,
      703/36820,
      897/30721,
      1282/23001,
      1400/13529,
      1347/6025,
      1700/6025
    ),
    ICU = c(
      13/1116,
      5/6615,
      12/9418,
      16/25726,
      43/35801,
      90/36820,
      183/30721,
      305/23001,
      391/13529,
      258/6025,
      79/6025
    ),
    Deaths = c(
      0/1116,
      0/6615,
      0/9418,
      0/25726,
      8/35801,
      11/36820,
      32/30721,
      81/23001,
      230/13529,
      423/6025,
      1300/6025
    )
  )


get_sev_outcomes <- function(day){
  new_daily_cases <- dt[Date.reported == day,list(.N),by=c("Age.group")]
  mc <- merge(new_daily_cases,severe_outcomes,by=c("Age.group"))

  n <- 10000

  mc_result <- data.table()
  for(i in 1:nrow(mc)){
    tmp <-
      data.table(Age.group = mc[i,Age.group],
                 Hospitalization = rbinom(10000,size=mc[i,N],prob=mc[i,Hospitalization.Rate]),
                 ICU = rbinom(10000,size=mc[i,N],prob=mc[i,ICU]),
                 Death = rbinom(10000,size=mc[i,N],prob=mc[i,Deaths])
      )
    mc_result <- rbindlist(list(mc_result,tmp))
  }
  mc_result[,N:=seq(1,.N,by=1),by=c("Age.group")]

  mc_summary <- mc_result[,list(
    Hospitalizations = sum(Hospitalization),
    ICU = sum(ICU),
    Death = sum(Death)),
    by=c("N")]

  mc_melt <- melt(mc_summary,id.vars=c("N"))
  mc_melt$Date.reported <- day
  return(mc_melt)
}


days <- seq(max(dt$Date.reported)-20,max(dt$Date.reported),by=1)

plot_data <- rbindlist(lapply(days,FUN=get_sev_outcomes))

plot_data_means <- plot_data[,list(value=mean(value)),by=c("variable","Date.reported")]

plot_period_avg <- plot_data[,list(value=mean(value,na.rm=TRUE),maxx=max(Date.reported),maxy=max(value)),by=c("variable")]

p1 <- ggplot(plot_data,aes(x=Date.reported,y=value,group=Date.reported))+
  geom_hline(data=plot_period_avg,aes(yintercept=value),lwd=0.8,lty=1,colour="blue")+
  geom_violin(adjust=2,aes(fill=variable))+
  geom_line(data=plot_data_means,aes(group=variable),lwd=0.8)+
  geom_point(data=plot_data_means,aes(group=variable),lwd=0.8)+
  facet_grid(variable~.,scales = "free")+
  theme_bw()+
  labs(
    title="Alberta: Forecast Severe Outcomes from Daily Reported Cases",
    subtitle=paste0("Expected severe outcomes based on cases reported daily\n(past 3-week period)"),
    caption = "Forecast using historical severe outcome rates by age in AB")+
  scale_fill_brewer(palette="YlOrRd",guide=FALSE)+
  xlab("Date")+ylab("Forecast Cases w Severe Outcomes")+
  geom_label(data=plot_period_avg,aes(x=maxx,y=maxy,label=paste0("Avg daily ",variable," outcomes: ",round(value,1)),group=NULL),colour="blue",hjust=1,vjust=1)

dir.create('output',showWarnings=FALSE)
png(file = "output/severe_outcomes.png", width = 1600, height = 800, res = 200)
print(p1)
dev.off()


days <- seq(max(dt$Date.reported)-90,max(dt$Date.reported),by=1)

plot_data <- rbindlist(lapply(days,FUN=get_sev_outcomes))

plot_data_means <- plot_data[,list(value=mean(value)),by=c("variable","Date.reported")]

plot_period_avg <- plot_data[,list(value=mean(value,na.rm=TRUE),maxx=max(Date.reported),maxy=max(value)),by=c("variable")]

p2 <- ggplot(plot_data,aes(x=Date.reported,y=value,group=Date.reported))+
  geom_hline(data=plot_period_avg,aes(yintercept=value),lwd=0.8,lty=1,colour="blue")+
  #geom_violin(adjust=2,aes(fill=variable))+
  geom_line(data=plot_data_means,aes(group=variable),lwd=0.8)+
  geom_point(data=plot_data_means,aes(group=variable),lwd=0.8)+
  facet_grid(variable~.,scales = "free")+
  theme_bw()+
  labs(
    title="Alberta: Forecast Severe Outcomes from Daily Reported Cases",
    subtitle=paste0("Expected severe outcomes based on cases reported daily\n(past 90d period)"),
    caption = "Forecast using historical severe outcome rates by age in AB-nData Source: Alberta Health")+
  scale_fill_brewer(palette="YlOrRd",guide=FALSE)+
  xlab("Date")+ylab("Forecast Cases w Severe Outcomes")+
  geom_label(data=plot_period_avg,aes(x=maxx,y=maxy,label=paste0("Avg daily ",variable," outcomes: ",round(value,1)),group=NULL),colour="blue",hjust=1,vjust=1)+

dir.create('output',showWarnings=FALSE)
png(file = "output/severe_outcomes_3mo.png", width = 1600, height = 800, res = 200)
print(p2)
dev.off()
