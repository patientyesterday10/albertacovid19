library(data.table)
library(ggplot2)
library(lubridate)
library(zoo)
library(patchwork)


# Data Load & Preparation -------------------------------------------------

combined <- data.table(read.csv(paste0("output/ab_covid_time_series.csv")),check.names = T)
combined[,date:=as.Date(date)]
combined[is.na(new_cases),new_cases:=0]
combined[,new_cases_stl:=as.numeric(stl(ts(data=new_cases,frequency=7),s.window = 7)$time.series[,'trend'])]

trend_icu_case_lag <- rbindlist(lapply(seq(min(combined$date),max(combined$date)-60,by=1),FUN = function(x){
  tmp <- combined[date>=x & date<=(x+59),]
  ccf_tmp <- ccf(tmp$ICU,tmp$new_cases,na.action=na.omit,plot=FALSE)
  return(data.table(date=x,lag=ccf_tmp$lag[which.max(ccf_tmp$acf)],pts=nrow(tmp)))
}))
plot_icu_lag <- ggplot(trend_icu_case_lag,aes(x=date,y=lag))+geom_step()+geom_point()+
  scale_y_continuous(breaks=seq(-45,45,by=1))



ccf_cases_icu <- ccf(combined$ICU,combined$new_cases_stl,na.action = na.omit)
lag_cases_icu <- ccf_cases_icu$lag[which.max(ccf_cases_icu$acf)]
combined[,lag_cases_stl:=shift(new_cases_stl,n=lag_cases_icu)]
combined[,lag_positivity:=shift(positivity,n=lag_cases_icu)]

lm_scale <- lm(combined,formula = ICU~lag_cases_stl+lag_positivity+0)
factor <- round(lm_scale$coefficients[1],2)

p1 <- ggplot(combined,aes(x=date))+
  geom_line(aes(y=new_cases_stl),colour="dodgerblue")+
  geom_line(aes(y=ICU/factor),colour="firebrick")+
  scale_y_continuous(name="New Daily Cases",sec.axis = sec_axis(~ .*factor,name = "ICU Patients"))+theme_bw()+
  theme(
    axis.title.y.left=element_text(colour = "dodgerblue"),
    axis.title.y.right=element_text(colour = "firebrick"))+
  labs(title="Alberta: New Cases & Patients in ICU")

p2 <- p1 + geom_line(aes(y=lag_cases_stl),colour="dodgerblue",lty=2)+labs(subtitle=paste0(paste0("Cases with a ",round(lag_cases_icu,0)," day lag.")))
p3 <- ggplot(combined,aes(x=lag_cases_stl,y=ICU))+geom_point(aes(colour=date))+scale_color_viridis_c(option="magma")

combined[,lag_cases_pos_adj:=lag_cases_stl/(1-lag_positivity/100)^2,]

backfit_date <- as.Date("2020-09-01")

lm_fit <- lm(data=combined[date>=backfit_date,],formula = ICU~lag_cases_stl+lag_cases_pos_adj)
lm_recent <- lm(data=combined[date>=as.Date("2021-07-01"),],formula = ICU~lag_cases_stl+lag_cases_pos_adj)

summary(lm_fit)
summary(lm_recent)

latest_date <- max(combined$date)
newdata <- data.table(date=seq(backfit_date,latest_date+lag_cases_icu,by=1),
                      lag_cases_stl=combined[date>=(backfit_date-lag_cases_icu),new_cases_stl],
                      lag_positivity=combined[date>=(backfit_date-lag_cases_icu),positivity],
                      min_icu=combined[date>=(backfit_date-lag_cases_icu),min_icu])
newdata[,lag_cases_pos_adj:=lag_cases_stl/(1-lag_positivity/100)^2,]

pred_hist <- predict(lm_fit,newdata,se.fit = T,interval = "prediction",level = 0.95)
pred_hist <- cbind(pred_hist$fit,newdata)

pred <- predict(lm_recent,newdata,se.fit = T,interval = "prediction",level = 0.95)
pred <- cbind(pred$fit,newdata)

pred_hist[fit<0,fit:=0,]
pred_hist[lwr<0,lwr:=0,]
pred_hist[upr<0,upr:=0,]

fit_perf <- merge(combined[,list(date,ICU)],pred_hist[,list(date,fit)])
fit_perf[,err:=fit-ICU,]
p4fit <- ggplot(fit_perf, aes(x=ICU,y=fit))+geom_point(aes(colour=date))+geom_abline(slope=1,intercept=0)+xlab("ICU (Actual)")+ylab("ICU (Model Predict)")+theme_bw()


png(file = "output/icu_model_fit.png", width = 800, height = 600, res = 150)
print(p4fit)
dev.off()


p4 <- ggplot(combined,aes(x=date))+
  geom_line(aes(y=new_cases_stl,color="New Cases (7d STL)"))+
  geom_line(aes(y=ICU/factor,colour="ICU Patients"))+
  scale_y_continuous(name="New Daily Cases",sec.axis = sec_axis(~ .*factor,name = "ICU Patients"))+theme(
    axis.title.y.left=element_text(colour = "dodgerblue"),
    axis.title.y.right=element_text(colour = "firebrick"))+
  theme_bw()+labs(title="Alberta: New Cases & Patients in ICU, with Rudimentary Forecast",caption="Data Source: Alberta Health\nForecast: Linear Regression - Note recent underprediction")+
  scale_x_date(name="Date",date_breaks="2 weeks",date_labels = "%b-%d")+
  geom_line(data=pred_hist,aes(y=fit/factor,colour="ICU Forecast"),lty=2)+
  geom_ribbon(data=pred_hist,aes(ymin=lwr/factor,ymax=upr/factor,fill="ICU Forecast"),alpha=0.25,show.legend = FALSE)+
  geom_hline(yintercept = 173/factor,colour="firebrick",lwd=0.8,lty=2)+
  theme(
    axis.title.y.left=element_text(colour = "dodgerblue"),
    axis.text.y.left=element_text(colour = "dodgerblue"),
    axis.title.y.right=element_text(colour = "firebrick"),
    axis.text.y.right=element_text(colour = "firebrick"),
    axis.text.x = element_text(angle=90))+
  scale_colour_manual(name=NULL,values=c("ICU Forecast"="red","ICU Patients"="firebrick","New Cases (7d STL)"="dodgerblue"))

png(file = "output/icu.png", width = 1100, height = 500, res = 150)
print(p4)
dev.off()
