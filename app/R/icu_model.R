library(data.table)
library(ggplot2)
library(patchwork)
library(h2o)
library(stringr)

# Data Load & Preparation -------------------------------------------------

combined <- data.table(read.csv(paste0("output/ab_covid_time_series.csv")),check.names = T)
combined[,date:=as.Date(date)]
combined[is.na(new_cases),new_cases:=0]
combined[,new_cases_stl:=as.numeric(stl(ts(data=new_cases,frequency=7),s.window = 7)$time.series[,'trend'])]



# Data Preprocessing ------------------------------------------------------

df <- copy(combined)
df$deaths<-NULL
# Set lags / delay (steps to predict)
num_lags = 16
delay = 7

# Add Future Points
df <- merge(df, data.table(date=seq(max(df$date)+1,max(df$date)+delay,by=1)),by=("date"),all=T)

df1 <- copy(df)

# Created lagged columns for each predictor:
cols <- setdiff(names(df1), "date")
df1 <- cbind(df1,df1[,shift(.SD,n=seq(delay,num_lags,by=1),type="lag",give.names = T),.SDcols=cols])
df1[,diff_new_cases_stl_lag7_14:=new_cases_stl_lag_7-new_cases_stl_lag_14,]
df1[,diff_new_cases_lag7_14:=new_cases_lag_7-new_cases_lag_14,]
df1[,diff_ICU_lag7_14:=ICU_lag_7-ICU_lag_14,]
df1[,diff_positivity_lag_7_14:=positivity_lag_7-positivity_lag_14,]

# Create mean windows
df2 <- copy(df)
newnames <- data.table(expand.grid(lags=seq(delay,num_lags,by=1),col=cols))
df2 <- cbind(df2$date,df2[,shift(.SD,n=delay,type="lag",give.names = F),.SDcols=cols])
colnames(df2) <- colnames(df)
df2 <- df2[,frollmean(.SD,n=seq(delay,num_lags,by=1),align="right"),.SDcols=cols]
colnames(df2)<-paste0(newnames$col,"_avg_window_length_",newnames$lag)

df1[,diff_ICU_lag7_14:=ICU_lag_7-ICU_lag_14,]


df <- cbind(df1,df2)
df <- df[complete.cases(df) | date>=max(combined$date),]

cols_to_use <- colnames(df)[grepl("^ICU$|date|lag|window|diff",colnames(df))]
df <- subset(df,select=cols_to_use)

df[,weights:=1/(1+2*exp(-2*(seq(-1,1,length.out=.N))))^(1/4),]
ggplot(df,aes(x=date,y=weights))+geom_point()

# Modeling ----------------------------------------------------------------

df_train <- df[date < (max(combined$date)-7)]
#df_test <- tail(df_train,nrow(df_train)*0.2)
#df_train <- head(df_train,nrow(df_train)*0.8)
df_validate <- df[date>=(max(combined$date)-7) & date<=max(combined$date)]
#df_recent <- rbindlist(list(df_test,df_validate))
df_future <- df[date>max(combined$date),]

h2o.init()

hf_train = as.h2o(df_train)
#hf_test = as.h2o(df_test)
hf_validate = as.h2o(df_validate)
#hf_recent = as.h2o(df_recent)
hf_future = as.h2o(df_future)

x=colnames(df)[-which(colnames(df) %in% c("ICU","date",("weights")))]
y="ICU"

# Train model over full pandemic:
aml <- h2o.automl(x=x,y=y,training_frame = hf_train,include_algos = c("GLM"),leaderboard_frame = hf_validate,max_runtime_secs = 90,weights_column = 'weights')
lb <- h2o.get_leaderboard(aml)
print(lb)
best_model <- h2o.get_best_model(aml)
head(h2o.varimp(best_model),n = 10)

hf_train_predict= h2o.predict(best_model,hf_train)
#hf_test_predict = h2o.predict(best_model,hf_test)
hf_validate_predict = h2o.predict(best_model,hf_validate)
hf_future_predict = h2o.predict(best_model,hf_future)

df_predict_train <- cbind(df_train,data.table(as.data.frame(hf_train_predict)))
#df_predict_test <- cbind(df_test,data.table(as.data.frame(hf_test_predict)))
df_predict_validate <- cbind(df_validate,data.table(as.data.frame(hf_validate_predict)))
df_predict_future <- cbind(df_future,data.table(as.data.frame(hf_future_predict)))

df_predict <- rbindlist(list(
  cbind(type="Train",model="Full",df_predict_train),
  #cbind(type="Test",model="Full",df_predict_test),
  cbind(type="Test",model="Full",df_predict_validate),
  cbind(type="Forecast",model="Full",df_predict_future)
))

df_predict[,error:=ICU-predict,]
df_predict[!is.na(error) ,error_stl:=as.numeric(stl(ts(data=error,frequency=7),s.window = 7)$time.series[,'trend']),by=c("model")]
p_err_trend <- ggplot(df_predict,aes(x=date,y=error))+geom_point()+geom_line(aes(y=error_stl))
print(p_err_trend)
resid_model <- lm(error_stl~date+new_cases_stl_lag_7+diff_ICU_lag7_14,data = df_predict[date>=(max(combined$date)-30)])
df_pred_resid <- df_predict[date>=(max(combined$date)-30),list(date,positivity_lag_7,new_cases_stl_lag_7,diff_ICU_lag7_14),]
df_pred_resid[,pred_resid:=predict(resid_model,newdata = df_pred_resid)]
df_pred_resid[,transition:=sqrt(seq(0,1.0,length.out=.N)),]
df_pred_resid[,pred_resid_trans:=pred_resid*transition,]

df_predict <- merge(df_predict,df_pred_resid[,list(date,pred_resid,pred_resid_trans),],by=c("date"),all=T)
df_predict[is.na(pred_resid_trans),pred_resid_trans:=0]
df_predict[,pred_corr:=predict+pred_resid_trans,]

fit_error = quantile(df_predict$error,na.rm=TRUE,probs=c(0.05,0.95))
mean(abs(fit_error))

# Sample Errors of Corrected Model & Use for random walk
simple_walk = rbindlist(lapply(seq(1,1000,by=1),FUN = function(x){
  temp_walk=cumsum(sample(df_predict[is.finite(error),]$error,size = 7,replace = T))
  data.table(n=x,t=seq(0,7,by=1),walk=c(0,temp_walk),last=last(temp_walk))
}))

err_band <- rbindlist(lapply(unique(simple_walk$t),function(x){
  data.table(
    t = x,
    lwr = quantile(simple_walk[t==x,]$walk,probs=c(0.05)),
    upr = quantile(simple_walk[t==x,]$walk,probs=c(0.95))
  )
}))
err_band[t==0,lwr:=lwr+fit_error[1],]
err_band[t==0,upr:=upr+fit_error[2],]

rm(simple_walk)
err_band[,date:=seq(max(df_predict$date,na.rm=TRUE)-7,max(df_predict$date,na.rm=TRUE),by=1),]

df_predict <- merge(df_predict,err_band[,list(date,lwr,upr)],by=c("date"),all.x=T)
df_predict[,pred_upr:=pred_corr+upr,]
df_predict[,pred_lwr:=pred_corr+lwr,]
df_predict[,fit_upr:=pred_corr+fit_error[2],]
df_predict[,fit_lwr:=pred_corr+fit_error[1],]
df_predict[fit_lwr<0,fit_lwr:=0]
df_predict[pred_lwr<0,pred_lwr:=0]

# Add blank row for legend alignment:
df_predict <- rbindlist(list(df_predict,data.frame(type="Validate",model="Recent")),fill = TRUE,use.names=TRUE)

p1 <- ggplot(df_predict,aes(x=date))+
  geom_point(aes(y=ICU),alpha=0.2)+
  geom_point(data=combined,aes(y=new_cases/10,color="Cases (new)"),alpha=0.5)+
  geom_line(data=combined,aes(y=new_cases_stl/10,color="Cases (new)"),alpha=0.5)+
  geom_point(data=df_predict[date==max(combined$date),],aes(y=ICU),show.legend = FALSE)+
  geom_line(aes(y=ICU,lty="Actual"),show.legend = F,lwd=0.8)+
  geom_line(data=df_predict[model=="Full",],aes(y=pred_corr,colour=type,group=model),lwd=0.8,show.legend = F)+
  geom_ribbon(data=df_predict,aes(x=date,ymin=pred_lwr,ymax=pred_upr,group=model,fill="Forecast"),alpha=0.25,show.legend = F)+
  geom_ribbon(data=df_predict,aes(x=date,ymin=fit_lwr,ymax=fit_upr,fill="Forecast"),alpha=0.25,show.legend = F)+
  geom_point(data=df_predict[model=="Full" & type=="Forecast",],aes(y=pred_corr,colour=type,group=model))+
  scale_x_date(limits=c(as.Date("2020-09-01"),max(df_predict$date)+180))+
  scale_y_continuous(name="COVID19 ICU Patients",sec.axis = sec_axis(trans=~.*10,name="New Cases"))+
  ggrepel::geom_label_repel(data=df_predict[model=="Full" & type=="Forecast",],aes(y=pred_corr,colour=type,label=paste0(strftime(date,format = "%a %b %d"),": ",round(pred_corr))),nudge_x = -35,max.overlaps = Inf,direction = "y",show.legend = FALSE,size=3)+
  ggrepel::geom_label_repel(data=df_predict[date==max(combined$date),],aes(y=ICU,label=paste0(round(ICU))),nudge_x = 3,max.overlaps = Inf,direction = "x",show.legend = FALSE,size=3,colour="black")+
  theme_bw()+labs(title="ICU Model: Full Pandemic",subtitle=paste(toupper(best_model@algorithm),"with linear residual correction for past 14d"))+
  scale_colour_viridis_d(name=NULL,begin=0,end=0.9)+scale_fill_viridis_d(name=NULL,begin=0,end=0.9)+xlab(NULL)+ylab("COVID19 ICU Patients")

png(file = "output/icu_forecast_old.png", width = 1100, height = 500, res = 100)
print(p1)
dev.off()


png(file = "output/icu_forecast_zoom_old.png", width = 1100, height = 500, res = 100)
print(
  p1+coord_cartesian(xlim=c(max(df_predict$date,na.rm=TRUE)-60,max(df_predict$date,na.rm=TRUE)))
)
dev.off()

png(file="output/icu_forecast_residual.png",width=1200,height=600,res=100)
(ggplot(df_predict,aes(x=ICU,y=predict,colour=type))+
  geom_abline(slope=1,intercept = 0)+
  geom_point()+
  geom_path(group="1",alpha=0.5) + theme_bw() + ylab("Prediction") +
 ggplot(df_predict,aes(x=ICU,y=pred_corr,colour=type))+
    geom_abline(slope=1,intercept = 0)+
    geom_point()+
    geom_path(group="1",alpha=0.5) + theme_bw() + ylab("Prediction (w Residual Correction)")
 )+plot_layout(guides="collect")
dev.off()

# Simple Forecast Model plots

ps1 <- ggplot(df_predict,aes(x=date))+
  geom_point(aes(y=ICU),alpha=0.25)+
  geom_point(data=combined,aes(y=new_cases/10,color="New Cases (STL)",fill="New Cases (STL)"),alpha=0.2)+
  geom_line(data=combined,aes(y=new_cases_stl/10,color="New Cases (STL)"),alpha=0.5,lwd=0.8)+
  geom_point(data=df_predict[date==max(combined$date),],aes(y=ICU,fill="ICU"),show.legend = FALSE)+
  geom_line(aes(y=ICU,lty="Actual",colour="ICU"),show.legend = F,lwd=0.8)+
  geom_line(data=df_predict[date>=max(combined$date),],aes(y=pred_corr,colour="ICU Forecast"),lwd=0.8,show.legend = F)+
  geom_line(data=df_predict[date<=max(combined$date),],aes(y=pred_corr,colour="Model Fit"),lwd=0.8,show.legend = F)+
  geom_ribbon(data=df_predict,aes(x=date,ymin=pred_lwr,ymax=pred_upr,fill="ICU Forecast"),alpha=0.25,show.legend = F)+
  geom_ribbon(data=df_predict[date<=max(combined$date),],aes(x=date,ymin=fit_lwr,ymax=fit_upr,group=model,fill="Model Fit"),alpha=0.25,show.legend = F)+
  geom_point(data=df_predict[model=="Full" & type=="Forecast",],aes(y=pred_corr,colour="ICU Forecast",group=model))+
  scale_x_date(limits=c(as.Date("2020-09-01"),max(df_predict$date)+180))+
  scale_y_continuous(name="COVID19 ICU Patients",sec.axis = sec_axis(trans=~.*10,name="Daily New Cases"))+
  ggrepel::geom_label_repel(data=df_predict[ICU==max(combined$ICU,na.rm=TRUE) |date==max(combined$date),],aes(y=ICU,label=paste0(round(ICU)),colour="ICU"),nudge_x = 3,max.overlaps = Inf,direction = "x",show.legend = FALSE,size=3)+
  theme_bw()+labs(title="Alberta: COVID19 ICU Patients",subtitle=paste0("Forecast using ",toupper(best_model@algorithm)," with linear residual correction for past 14d","\nData Updated to: ",max(combined$date,na.rm=TRUE)))+
  scale_colour_viridis_d(name=NULL,begin=0,end=0.7)+scale_fill_viridis_d(name=NULL,begin=0,end=0.7)+xlab(NULL)+ylab("COVID19 ICU Patients")

ps2 <- ps1 + coord_cartesian(xlim=c(max(df_predict$date,na.rm=TRUE)-30,max(df_predict$date,na.rm=TRUE))) +   ggrepel::geom_label_repel(data=df_predict[model=="Full" & type=="Forecast",],aes(y=pred_corr,colour="ICU Forecast",label=paste0(strftime(date,format = "%a %b %d"),": ",round(pred_corr))),nudge_x = -15, nudge_y=10,max.overlaps = Inf,direction = "y",show.legend = FALSE,size=3)

png(file = "output/icu_forecast.png", width = 1100, height = 500, res = 100)
(ps1 + ps2+labs(title=NULL,subtitle=NULL))+plot_layout(guides="collect",widths = c(1,0.6))
dev.off()

png(file = "output/icu_forecast_zoom.png", width = 1100, height = 500, res = 100)
print(ps2)
dev.off()


#
p <- ggplot(combined,aes(x=date))+
  geom_point(aes(y=ICU),alpha=0.25,show.legend = F)+
  geom_point(data=combined,aes(y=new_cases/10,color="New Cases (STL)",fill="New Cases (STL)"),alpha=0.2)+
  geom_bar(aes(y=deaths,colour="Deaths",fill="Deaths"),stat="identity",show.legend = F)+
  geom_line(data=combined,aes(y=new_cases_stl/10,color="New Cases (STL)"),alpha=0.5,lwd=0.8)+
  geom_line(aes(y=ICU,lty="Actual",colour="ICU"),show.legend = F,lwd=0.8)+
  geom_line(data=combined,aes(y=Non.ICU/10,color="Hospital (Non-ICU)"),alpha=0.5,lwd=0.8)+
  scale_x_date(limits=c(as.Date("2020-09-01"),max(df_predict$date)+180))+
  scale_y_continuous(name="COVID19 ICU Patients & Deaths",sec.axis = sec_axis(trans=~.*10,name="Daily New Cases & Hospitalizations"))+
  ggrepel::geom_label_repel(data=df_predict[ICU==max(combined$ICU,na.rm=TRUE) |date==max(combined$date),],aes(y=ICU,label=paste0(round(ICU)),colour="ICU"),nudge_x = 3,max.overlaps = Inf,direction = "x",show.legend = FALSE,size=3)+
  ggrepel::geom_label_repel(data=combined[date==max(combined$date),],aes(y=Non.ICU/10,label=paste0(round(Non.ICU)),colour="Hospital (Non-ICU)"),nudge_x = 3,max.overlaps = Inf,direction = "x",show.legend = FALSE,size=3)+
  ggrepel::geom_label_repel(data=combined[date==max(combined$date),],aes(y=new_cases/10,label=paste0(round(new_cases)),colour="New Cases (STL)"),nudge_x = 3,max.overlaps = Inf,direction = "x",show.legend = FALSE,size=3)+
  geom_hline(yintercept=173)+
  annotate("label",label="Normal ICU Capacity (173)",x = as.Date("2020-09-01"),y=173,vjust=1.2,hjust=0)+
  theme_bw()+labs(title="Alberta: COVID19 Cases, ICU, Hospitalizations & Deaths",subtitle=paste0("Data Updated to: ",max(combined$date,na.rm=TRUE)))+
  scale_colour_viridis_d(name=NULL,begin=0,end=0.7)+scale_fill_viridis_d(name=NULL,begin=0,end=0.7,guide="none")+xlab(NULL)+ylab("COVID19 ICU Patients")+
  theme(legend.position=c(0.02,0.98),legend.justification =c("left","top"),legend.background = element_rect(color="darkgrey"))

png(file = "output/icu_cases_deaths.png", width = 1100, height = 500, res = 100)
print(p)
dev.off()
