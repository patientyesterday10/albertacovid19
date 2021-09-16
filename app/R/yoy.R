library(data.table)
library(ggplot2)
library(lubridate)
library(zoo)
library(patchwork)


# Data Load & Preparation -------------------------------------------------

combined <- data.table(read.csv(paste0("output/ab_covid_time_series.csv")),check.names = T)
combined[,date:=as.Date(date),]
combined[,yr:=year(date),]
combined[,comp_date:=as.Date(paste0("2021-",month(date),"-",day(date)))]
combined[,new_cases_ma:=rollapply(new_cases,FUN=mean,align="center",width=7,partial=TRUE)]
combined[,deaths_ma:=rollapply(deaths,FUN=mean,align="center",width=7,partial=TRUE)]


last_cases <- combined[date==max(date),]$new_cases_ma
last_hosp <- combined[date==max(date),]$total_hosp
last_icu <- combined[date==max(date),]$ICU
cases_comp_date <- combined[new_cases_ma>=combined[date==max(date),]$new_cases_ma &yr==2020,min(comp_date)]
hosp_comp_date <- combined[total_hosp>=combined[date==max(date),]$total_hosp &yr==2020,min(comp_date)]
icu_comp_date <- combined[ICU>=combined[date==max(date),]$ICU &yr==2020,min(comp_date)]
cases_comp_date_label <- paste("New Cases:\n",combined[comp_date==cases_comp_date & yr==2020,]$date)
hosp_comp_date_label <- paste("Hosp+ICU:\n",combined[comp_date==hosp_comp_date & yr==2020,]$date)
icu_comp_date_label <- paste("ICU:\n",combined[comp_date==icu_comp_date & yr==2020,]$date)



# Create Plots (YoY Comparison) -------------------------------------------

p <- ggplot(combined,aes(x=comp_date,colour=as.factor(yr),group=as.factor(yr)))+
  geom_point(aes(y=new_cases))+
  geom_line(aes(y=new_cases_ma,lty="New Daily Cases (7d MA)"),lwd=0.8)+
  geom_line(aes(y=total_hosp,lty="Total Hosp (Non-ICU+ICU)"),lwd=0.8)+
  geom_line(aes(y=ICU,lty="ICU"),lwd=0.8)+
  scale_x_date(name="Date",limits=as.Date(c("2021-07-01","2022-01-01")),date_breaks = "2 weeks",date_labels = "%b-%d")+
  scale_y_continuous(name="Daily New Cases / People in Hospital (ICU + Non-ICU)",breaks = seq(0,5000,by=100))+
  theme_bw()+
  labs(title="Alberta: COVID19 Daily New Cases & Hospitalizations",caption = "Data Source: Alberta Health",subtitle=max(combined$date))+
  scale_colour_viridis_d(name="Year",option = "A",begin = 0.2,end = 0.8)+
  annotate(geom="line",x=c(max(combined$date),cases_comp_date),y=c(last_cases,last_cases),colour="grey",lty=5)+
  annotate(geom="line",x=c(max(combined$date),hosp_comp_date),y=c(last_hosp,last_hosp),colour="grey",lty=5)+
  annotate(geom="line",x=c(max(combined$date),icu_comp_date),y=c(last_icu,last_icu),colour="grey",lty=5)+
  annotate("point",x=c(hosp_comp_date,cases_comp_date,icu_comp_date),y=c(last_hosp,last_cases,last_icu),size=4,colour="grey")+
  annotate("label",x=c(hosp_comp_date,cases_comp_date,icu_comp_date),y=c(last_hosp,last_cases,last_icu),label=c(hosp_comp_date_label,cases_comp_date_label,icu_comp_date_label),size=3,hjust=-0.2)+
  scale_linetype_discrete(name="Data Type")


dir.create('output',showWarnings=FALSE)
png(file = "output/yoy.png", width = 2000, height = 1000, res = 150)
print(p)
dev.off()


shift <- max(combined$date)-cases_comp_date
start_date <- as.Date("2021-07-01")
combined[,aligned_comp_date:=comp_date]
combined[yr==2020,aligned_comp_date:=comp_date+as.numeric(shift),]

p2 <- ggplot(combined,aes(x=aligned_comp_date,colour=as.factor(yr),group=as.factor(yr)))+
  geom_point(aes(y=new_cases))+
  geom_line(aes(y=new_cases_ma,lty="New Daily Cases (7d MA)"),lwd=0.8)+
  geom_line(aes(y=total_hosp,lty="Total Hosp (Non-ICU+ICU)"),lwd=0.8)+
  geom_line(aes(y=ICU,lty="ICU"),lwd=0.8)+
  scale_x_date(name="Date",limits=as.Date(c("2021-07-01",NA)),date_breaks = "2 weeks",date_labels = "%b-%d")+
  scale_y_continuous(name="Daily New Cases / People in Hospital (ICU + Non-ICU)",breaks = seq(0,5000,by=100))+
  theme_bw()+
  labs(title="Alberta: COVID19 Daily New Cases & Hospitalizations",caption = "Data Source: Alberta Health",subtitle=paste0("2021 vs. 2020 snapshot - aligned based on 7d MA of New Cases (",shift," day shift)\n",max(combined$date)))+
  scale_colour_viridis_d(name="Year",option = "A",begin = 0.2,end = 0.8)+
  scale_linetype_discrete(name="Data Type")

dir.create('output',showWarnings=FALSE)
png(file = "output/yoy_aligned.png", width =1600, height =800, res=150)
print(p2)
dev.off()



# Faceted Version ---------------------------------------------------------

comb_facet<-melt(combined,id.vars=c("date","comp_date","aligned_comp_date","yr"),measure.vars=c("new_cases","new_cases_ma","ICU","total_hosp","deaths","deaths_ma"))

comb_facet[variable %in% c("new_cases","new_cases_ma"),fac:="New Cases"]
comb_facet[variable %in% c("Non.ICU","total_hosp"),fac:="Hospitalizations"]
comb_facet[variable %in% c("ICU"),fac:="ICU"]
comb_facet[variable %in% c("deaths","deaths_ma"),fac:="Deaths"]

comb_facet[,label:=variable,]
comb_facet[variable=="total_hosp",label:="Total Hosp."]
#comb_facet[date<=as.Date("2021-06-30"),yr:=2020,]
#comb_facet[,comp_date:=as.Date(paste0("2021-",month(date),"-",day(date)))]
#comb_facet[yr==2020,aligned_comp_date:=comp_date+as.numeric(shift),]

p3a <- ggplot(comb_facet[fac=="New Cases"],aes(x=aligned_comp_date,y=value,colour=as.factor(yr)))+
  geom_point(data=comb_facet[variable %in% c("new_cases")],show.legend=FALSE,alpha=0.6)+
  geom_line(data=comb_facet[variable %in% c("new_cases_ma")],lwd=0.7)+
  scale_colour_viridis_d(name="Year",option = "A",begin = 0.2,end = 0.8)+
  theme_bw()+facet_grid(~fac,shrink = TRUE)+
  scale_x_date(name="Date",limits=as.Date(c("2021-07-01",NA)),date_breaks = "2 weeks",date_labels = "%b-%d")+
  scale_y_continuous(name="Daily New Cases\n(with 7d MA)",breaks = seq(0,5000,by=200))

p3b <- ggplot(comb_facet[fac=="Hospitalizations"],aes(x=aligned_comp_date,y=value,colour=as.factor(yr)))+
  geom_line(data=comb_facet[variable %in% c("total_hosp"),],lwd=0.7)+
  scale_colour_viridis_d(name="Year",option = "A",begin = 0.2,end = 0.8)+
  theme_bw()+facet_grid(~fac,shrink = TRUE)+
  scale_x_date(name="Date",limits=as.Date(c("2021-07-01",NA)),date_breaks = "2 weeks",date_labels = "%b-%d")+
  scale_y_continuous(name="Total Hosp (incl ICU)",breaks = seq(0,5000,by=250))

p3c <- ggplot(comb_facet[fac=="ICU"],aes(x=aligned_comp_date,y=value,colour=as.factor(yr)))+
  geom_line(data=comb_facet[variable %in% c("ICU"),],lwd=0.7)+
  scale_colour_viridis_d(name="Year",option = "A",begin = 0.2,end = 0.8)+
  theme_bw()+facet_grid(~fac,shrink = TRUE)+
  scale_x_date(name="Date",limits=as.Date(c("2021-07-01",NA)),date_breaks = "2 weeks",date_labels = "%b-%d")+
  geom_hline(yintercept=173,colour="red",lwd=0.7)+
  scale_y_continuous(name="ICU",breaks = seq(0,400,by=10))

p3d <- ggplot(comb_facet[fac=="Deaths"],aes(x=aligned_comp_date,y=value,colour=as.factor(yr)))+
  geom_point(data=comb_facet[variable %in% c("deaths")],show.legend=FALSE)+
  geom_line(data=comb_facet[variable %in% c("deaths_ma")],lwd=0.7)+
  scale_colour_viridis_d(name="Year",option = "A",begin = 0.2,end = 0.8)+
  theme_bw()+facet_grid(~fac,shrink = TRUE)+
  scale_x_date(name="Date*",limits=as.Date(c("2021-07-01",NA)),date_breaks = "2 weeks",date_labels = "%b-%d")+
  scale_y_continuous(name="Daily Deaths",breaks = seq(0,100,by=5))+
  scale_linetype_discrete(name=NULL)

png(file = "output/yoy_aligned_facet.png", width = 1600, height = 1000, res = 150)
(p3a + p3b)/(p3c + p3d) + plot_layout(guides = 'collect')+plot_annotation(title="Alberta: Current COVID19 Situation, Compared to Fall 2020",caption = paste0("2020 Data Aligned based on 7d MA of New Cases (",shift," day shift)\nData Source: Alberta Health\nDate of death is subject to revision per Alberta Health"),subtitle=paste0("Data Updated as of: ",max(combined$date)))
dev.off()



# Projection --------------------------------------------------------------

library(caret)
library(forecast)

timeSlices <- createTimeSlices(1:nrow(combined),initialWindow = 28,horizon=28,fixedWindow=TRUE)

ccf_cases_icu <- ccf(combined$ICU,combined$new_cases,na.action = na.omit)
ccf_hosp_icu <- ccf(combined$ICU,combined$Non.ICU,na.action = na.omit)

lag_cases_icu <- ccf_cases_icu$lag[which.max(ccf_cases_icu$acf)]
lag_hosp_icu <- ccf_hosp_icu$lag[which.max(ccf_hosp_icu$acf)]

combined[,dow:=as.factor(wday(date)),]
combined[is.na(ICU),ICU:=0,]
combined[is.na(Non.ICU),Non.ICU:=0,]
combined[is.na(new_cases),new_cases:=0,]
combined[is.na(new_cases_ma),new_cases_ma:=0,]
combined[is.na(positivity),positivity:=0,]
combined[is.na(active),active:=0,]
combined[,dactive:=c(0,diff(active))]

combined[date<=as.Date("2021-06-01"),delta_pct:=0]
combined[date>=as.Date("2021-08-01"),delta_pct:=1]
combined[date>as.Date('2021-06-01') & date<as.Date("2021-08-01"),delta_pct:=(1-0)/as.numeric(as.Date("2021-08-01")-as.Date('2021-06-01'))*as.numeric(date-as.Date('2021-06-01'))]

combined[,lag_ICU:=shift(ICU,n=lag_cases_icu,type="lead",fill=NA)]
combined[,lag_date:=date+lag_cases_icu]

fit <- train(lag_ICU ~ Non.ICU + new_cases_ma + positivity + active + deaths_ma + delta_pct,
                    data = combined[!is.na(lag_ICU)],
                    method = "glm",
                    preProc = c("center", "scale"),
             trControl=trainControl(method="timeslice",initialWindow=90,horizon=lag_cases_icu,fixedWindow=FALSE))

varImp(fit)

combined[,pred_ICU:=predict(fit, newdata = combined),]
combined[,err:=lag_ICU-pred_ICU,]

mod_err <- quantile(combined$err,probs=c(0.1,0.9),na.rm=TRUE)

p_icu <- ggplot(combined,aes(x=lag_date))+
  geom_ribbon(aes(y=pred_ICU,ymin=pred_ICU+mod_err[1],ymax=pred_ICU+mod_err[2],fill="Prediction"),alpha=0.25,show.legend = FALSE)+
  geom_ribbon(aes(y=lag_ICU,ymin=lag_ICU,ymax=lag_ICU,fill="Actual"),alpha=0.5,show.legend = FALSE)+
  geom_line(aes(y=lag_ICU,colour="Actual"),lwd=0.8)+
  geom_line(aes(y=pred_ICU,colour="Prediction"),lwd=0.8)+
  geom_vline(xintercept=Sys.Date(),lty=3)+
  theme_bw()

png(file = "output/icu_pred.png", width = 1200, height = 800, res = 150)
print(p_icu)
dev.off()
