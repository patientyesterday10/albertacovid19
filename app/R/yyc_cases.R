source("R/lib/packages.R")
source("R/data_loader.R")
dt <- load_ab_covid_data()

yyc <- dt[Alberta.Health.Services.Zone=="Calgary Zone",list(.N),by=c("Date.reported","Age.group")]
yyc_total<-dt[Alberta.Health.Services.Zone=="Calgary Zone",list(.N),by=c("Date.reported")]

yyc[,Age.group:=factor(Age.group,levels=c("Under 1 year","1-4 years","5-9 years","10-19 years","20-29 years","30-39 years","40-49 years","50-59 years","60-69 years","70-79 years","80+ years","Unknown"),ordered = TRUE)]

yyc <- data.table(merge(expand.grid(Date.reported=unique(yyc$Date.reported),Age.group=unique(yyc$Age.group)),yyc,by=c("Date.reported","Age.group"),all=T))
yyc[is.na(N),N:=0]
yyc[,pct:=N/sum(N),by=c("Date.reported")]

max_pt <- yyc_total[N==max(N),]
last_pt <- yyc_total[Date.reported==max(Date.reported),]

setorderv(yyc,c("Date.reported","Age.group"))

plot1 <- (ggplot(yyc,aes(x=Date.reported,y=N))+geom_bar(stat="identity",aes(fill=Age.group))+
            theme_bw()+
            labs(title="YYC New Daily COVID-19 Cases",subtitle=paste0("Cases through ", max(yyc$Date.reported)))+
            geom_label(data=max_pt,aes(label=N))+
            geom_label(data=last_pt,aes(label=N)))+
  ggplot(yyc,aes(x=Date.reported,y=pct,fill=Age.group))+
  geom_area(stat="identity")+
  theme_bw()+
  plot_layout(nrow=2,guides = "collect",heights = c(1,0.5))

png(file = "output/yyc_case_details.png", width = 1600, height = 1600, res = 200)
print(plot1)
dev.off()

yyc[,daily_avg_7dma:=zoo::rollapply(N,width=7,align="center",partial=TRUE,FUN=mean),by=c("Age.group")]
yyc[,daily_pct_7dma:=zoo::rollapply(pct,width=7,align="center",partial=TRUE,FUN=mean),by=c("Age.group")]


plot2 <- ggplot(yyc,aes(x=Date.reported,y=daily_avg_7dma,colour=Age.group))+
  geom_line()+
  geom_text_repel(data=yyc[Date.reported==max(Date.reported),],aes(label=paste0(Age.group," (",round(daily_pct_7dma*100,0),"%)")),hjust=-0.1,direction="y")+
  scale_x_date(expand=expansion(add = c(NA,129)))+
  theme_bw()+
  guides(colour=FALSE)+
  ggtitle("YYC New Daily Cases by Age Group",subtitle="7-day moving average of daily new cases")

png(file = "output/yyc_cases_by_age.png", width = 1600, height = 1200, res = 200)
print(plot2)
dev.off()

# Fcst --------------------------------------------------------------------

fcst_data <- yyc_total[Date.reported>=as.Date("2021-07-01"),]
fcst_data[,day:=as.numeric(Date.reported-min(Date.reported))]
fcst_data[,weekday:=lubridate::wday(Date.reported)]
setorder(fcst_data,Date.reported)

opt<-optim(par=c(
  st=fcst_data[Date.reported==min(Date.reported),N],
  r=1.05
  ),fn = function(x){
  fcst_data[,fit:=x[1]*x[2]^(day),] #*(ifelse(weekday %in% c(1,2),x[3],1))
  return(sqrt(mean((fcst_data$N-fcst_data$fit)^2)))
})

fcst <- arima(x = fcst_data$N,order = c(0,2,2),seasonal=list(order=c(1,1,0),period=7))
fcst_result <- forecast(fcst,h=28,level = 0.8)

fwd_fcst <- merge(fcst_data,data.table(Date.reported=seq(min(fcst_data$Date.reported),max(fcst_data$Date.reported)+28,by=1)),by=c("Date.reported"),all=T)
fwd_fcst[,day:=as.numeric(Date.reported-min(Date.reported))]
fwd_fcst[,weekday:=lubridate::wday(Date.reported)]

fwd_fcst[is.na(N),fcst_mean:=as.numeric(fcst_result$mean),]
fwd_fcst[is.na(N),fcst_lower:=as.numeric(fcst_result$lower),]
fwd_fcst[is.na(N),fcst_upper:=as.numeric(fcst_result$upper),]
fwd_fcst[is.na(N),fit:=opt$par[1]*opt$par[2]^(day),]

fwd_fcst[fcst_mean<0,fcst_mean:=0,]
fwd_fcst[fcst_lower<0,fcst_lower:=0,]
fwd_fcst[fcst_upper<0,fcst_upper:=0,]



plot3 <- ggplot(fwd_fcst,aes(x=Date.reported))+
  geom_line(aes(y=N))+
  geom_point(aes(y=N))+
  geom_ribbon(aes(ymin=fcst_lower,ymax=fcst_upper),alpha=0.2)+
  geom_line(aes(y=fit),colour="red")+
  geom_line(aes(y=fcst_mean))+
  theme_bw()+
  labs(title="YYC Cases w Exponential Fit and ARIMA Fcst",subtitle=paste0("Cases through ", max(yyc$Date.reported)))+
  scale_x_date(name="Date",date_breaks = "1 week")

png(file = "output/yyc_fcst.png", width = 1600, height = 800, res = 200)
print(plot3)
dev.off()



# Alberta -----------------------------------------------------------------

ab_total <- dt[,list(.N),by=c("Date.reported")]
fcst_data <- ab_total[Date.reported>=as.Date("2021-07-01"),]
fcst_data[,day:=as.numeric(Date.reported-min(Date.reported))]
fcst_data[,weekday:=lubridate::wday(Date.reported)]
setorder(fcst_data,Date.reported)

opt<-optim(par=c(
  st=fcst_data[Date.reported==min(Date.reported),N],
  r=1.05
),fn = function(x){
  fcst_data[,fit:=x[1]*x[2]^(day),] #*(ifelse(weekday %in% c(1,2),x[3],1))
  return(sqrt(mean((fcst_data$N-fcst_data$fit)^2)))
})

fcst <- arima(x = fcst_data$N,order = c(0,2,2),seasonal=list(order=c(1,1,0),period=7))
fcst_result <- forecast(fcst,h=28,level = 0.8)

fwd_fcst <- merge(fcst_data,data.table(Date.reported=seq(min(fcst_data$Date.reported),max(fcst_data$Date.reported)+28,by=1)),by=c("Date.reported"),all=T)
fwd_fcst[,day:=as.numeric(Date.reported-min(Date.reported))]
fwd_fcst[,weekday:=lubridate::wday(Date.reported)]

fwd_fcst[is.na(N),fcst_mean:=as.numeric(fcst_result$mean),]
fwd_fcst[is.na(N),fcst_lower:=as.numeric(fcst_result$lower),]
fwd_fcst[is.na(N),fcst_upper:=as.numeric(fcst_result$upper),]
fwd_fcst[is.na(N),fit:=opt$par[1]*opt$par[2]^(day),]

fwd_fcst[fcst_mean<0,fcst_mean:=0,]
fwd_fcst[fcst_lower<0,fcst_lower:=0,]
fwd_fcst[fcst_upper<0,fcst_upper:=0,]

plot4 <- ggplot(fwd_fcst,aes(x=Date.reported))+
  geom_line(aes(y=N))+
  geom_point(aes(y=N))+
  geom_ribbon(aes(ymin=fcst_lower,ymax=fcst_upper),alpha=0.2)+
  geom_line(aes(y=fit),colour="red")+
  geom_line(aes(y=fcst_mean))+
  theme_bw()+
  labs(title="Alberta Cases w Exponential Fit and ARIMA Fcst",subtitle=paste0("Cases through ", max(yyc$Date.reported)))+
  scale_x_date(name="Date",date_breaks = "1 week")

png(file = "output/ab_fcst.png", width = 1600, height = 800, res = 200)
print(plot4)
dev.off()

