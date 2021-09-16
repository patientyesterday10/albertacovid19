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

combined <- combined[date>=as.Date("2021-06-01"),]
ymax = max(combined$new_cases,na.rm=T)

annotate_date <- function(d,l){
  obj <- 
  return(obj)
}

p <- ggplot(combined[date>=as.Date("2021-06-01")],aes(x=date))+
  geom_line(aes(y=new_cases,colour="New Cases"))+
  geom_line(aes(y=total_hosp,colour="Hospitalized"))+
  geom_line(aes(y=ICU*5,colour="ICU"))+
  geom_vline(xintercept=as.Date("2021-07-01"))+
  annotate("text",x=as.Date("2021-07-01"),label="Jul 1: Open For Summer",y = ymax*1.5,angle=90,vjust=-0.5,hjust=1)+
  geom_vline(xintercept=as.Date("2021-07-28"))+
  annotate("text",x=as.Date("2021-07-28"),label="Jul 28: Shift to Endemic",y = ymax*1.5,angle=90,vjust=1.5,hjust=1)+
  geom_vline(xintercept=as.Date("2021-08-13"))+
  annotate("text",x=as.Date("2021-08-13"),label="Aug 13: Delay Jul 28th Changes",y = ymax*1.5,angle=90,vjust=1.5,hjust=1)+
  geom_vline(xintercept=as.Date("2021-09-03"))+
  annotate("text",x=as.Date("2021-09-03"),label="Sep 3: Masks Required\nSurgeries Cancelled",y = ymax*1.5,angle=90,vjust=-0.5,hjust=1)+
  theme_minimal()+
  scale_y_continuous(name="New Cases & Hospitalizations",limits=c(0,ymax*1.5),labels=scales::comma,sec.axis = sec_axis(trans=~./5,name="ICU",breaks = seq(0,400,by=50)),breaks = seq(0,4000,by=250))+
  scale_x_date(name=NULL,date_breaks="1 month",date_labels = "%b %e, %Y")+
  geom_hline(aes(colour="ICU",yintercept=c(173*5)),lty=2)+
  annotate("text",y=173*5,label="Normal ICU Capacity\n(173, for all causes)",x = min(combined$date),angle=0,vjust=-0.5,hjust=0)+
  scale_color_viridis_d(option="magma",begin=0.1,end=0.8,name=NULL)+
  theme(legend.position = c(0.03,0.97), legend.justification = c("left","top"), legend.background = element_rect(fill="white",colour="grey20"),panel.background = element_rect(colour="grey20"))+
  labs(caption = "Data courtesy of Alberta Health",title="Alberta's 4th Wave: A Preventable Crisis")

png("output/a_preventable_crisis.png",width=1280,height=720,res=150)
print(p)
dev.off()


library(xkcd)

plot_data <- combined[date>=as.Date("2021-07-25")]
ggplot(plot_data[date>=as.Date("2021-07-25")],aes(x=date))+
  geom_line(aes(y=new_cases,colour="New Cases"))+
  geom_line(aes(y=total_hosp,colour="Hospitalized"))+
  geom_line(aes(y=ICU*5,colour="ICU"))+
  geom_vline(xintercept=as.Date("2021-07-01"))+
  annotate("text",x=as.Date("2021-07-01"),label="Jul 1: Open For Summer",y = ymax*1.5,angle=90,vjust=-0.5,hjust=1)+
  geom_vline(xintercept=as.Date("2021-07-28"))+
  annotate("text",x=as.Date("2021-07-28"),label="Jul 28: Shift to Endemic",y = ymax*1.5,angle=90,vjust=1.5,hjust=1)+
  geom_vline(xintercept=as.Date("2021-08-13"))+
  annotate("text",x=as.Date("2021-08-13"),label="Aug 13: Delay Jul 28th Changes",y = ymax*1.5,angle=90,vjust=1.5,hjust=1)+
  geom_vline(xintercept=as.Date("2021-09-03"))+
  annotate("text",x=as.Date("2021-09-03"),label="Sep 3: Masks Required\nSurgeries Cancelled",y = ymax*1.5,angle=90,vjust=-0.5,hjust=1)+
  theme_xkcd()+
  scale_y_continuous(name="New Cases & Hospitalizations",limits=c(0,ymax*1.5),labels=scales::comma,sec.axis = sec_axis(trans=~./5,name="ICU",breaks = seq(0,400,by=50)),breaks = seq(0,4000,by=250))+
  scale_x_date(name=NULL,date_breaks="1 month",date_labels = "%b %e, %Y")+
  geom_hline(aes(colour="ICU",yintercept=c(173*5)),lty=2)+
  annotate("text",y=173*5,label="Normal ICU Capacity\n(173, for all causes)",x = min(plot_data$date),angle=0,vjust=-0.5,hjust=0)+
  scale_color_viridis_d(option="magma",begin=0.1,end=0.8,name=NULL)+
  theme(legend.position = c(0.03,0.97), legend.justification = c("left","top"), legend.background = element_rect(fill="white",colour="grey20"),panel.background = element_rect(colour="grey20"))+
  labs(caption = "Data courtesy of Alberta Health",title="Alberta's 4th Wave: A Preventable Crisis")
# 
