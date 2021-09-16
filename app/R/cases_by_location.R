# Title     : Plot New Cases by Health Region
# Created by: PatientYesterday10
# Created on: 2020-12-06

source("R/lib/packages.R")
source("R/data_loader.R")
dt <- load_ab_covid_data()

new_cases <- dt[, .N, by = c("Alberta.Health.Services.Zone","Date.reported")]

p1 <- ggplot(new_cases,aes(x=Date.reported,y=N,fill=Alberta.Health.Services.Zone)) +
  geom_bar(stat="identity",color=nord::nord_palettes$polarnight[3],lwd=0.05)+
  theme_bw()+
  scale_y_continuous(name="New Daily Cases",breaks = seq(0,5000,by=100))+
  scale_x_date(name="Date",date_breaks = '1 month',date_labels = '%b %Y',expand=expansion(add=c(1,1)))+
  coord_cartesian(xlim=range(new_cases$Date.reported))+
  scale_fill_nord(name="Health zone",palette = 'aurora')+
  theme(legend.position = c(0.01,0.99),legend.justification = c("left","top"))+
  labs(title='Daily New Cases in Alberta, by zone',
       subtitle=paste0("Updated with data to: ",max(new_cases$Date.reported)),
       caption="Data Source: Alberta Health")

recent_cases <- new_cases[Date.reported>=max(new_cases$Date.reported)-120,]
last_report <- recent_cases[Date.reported==max(recent_cases$Date.reported)]
setorder(last_report,-Alberta.Health.Services.Zone)
last_report[,y:=cumsum(N)]
last_report[,ylab:=y-N/2]

p2 <- ggplot(recent_cases,aes(x=Date.reported,y=N,fill=Alberta.Health.Services.Zone)) +
  geom_bar(stat="identity",color=nord::nord_palettes$polarnight[1],lwd=0.05)+
  theme_bw()+
  scale_y_continuous(name="New Daily Cases",breaks = seq(0,5000,by=100),expand=expansion(add=c(0,200)))+
  scale_x_date(name="Date",date_breaks = '1 month',date_labels = '%b %Y',expand=expansion(add=c(1,10)))+
  coord_cartesian(xlim=range(recent_cases$Date.reported))+
  scale_fill_nord(name="Health zone",palette = 'aurora')+
  theme(legend.position = c(0.01,0.99),legend.justification = c("left","top"))+
  labs(title='Daily New Cases in Alberta, by zone',
       subtitle=paste0("Updated with data to: ",max(recent_cases$Date.reported)),
       caption="Data Source: Alberta Health")+
  geom_label_repel(data=last_report,aes(y=ylab,label=N),show.legend = FALSE,direction = "y",nudge_x = 8,segment.color = nord::nord_palettes$polarnight[3])+
  geom_text_repel(data = last_report[y==max(last_report$y)],aes(label=y,y=y),show.legend = FALSE,direction = "y",nudge_y=5,segment.color = nord::nord_palettes$polarnight[3])


png(file = "output/new_cases_by_region.png", width = 1600, height = 1600, res = 200)
print(p1+p2+plot_layout(nrow = 2))
dev.off()


recent_cases <- recent_cases[Alberta.Health.Services.Zone!="Unknown",]
p3 <- ggplot(recent_cases,aes(x=Date.reported,y=N,fill=Alberta.Health.Services.Zone)) +
  geom_bar(stat="identity",color=nord::nord_palettes$polarnight[1],lwd=0.05)+
  theme_bw()+
  scale_y_continuous(name="New Daily Cases",breaks = seq(0,5000,by=100))+
  scale_x_date(name="Date",date_breaks = '1 month',date_labels = '%b %Y',expand=expansion(add=c(1,10)))+
  coord_cartesian(xlim=range(recent_cases$Date.reported))+
  scale_fill_nord(name="Health zone",palette = 'aurora')+
  labs(title='Daily New Cases in Alberta, by zone',
       subtitle=paste0("Updated with data to: ",max(recent_cases$Date.reported)),
       caption="Data Source: Alberta Health")+
  geom_label_repel(data=last_report,aes(y=N,label=N),show.legend = FALSE,direction = "y",nudge_x = 8,segment.color = nord::nord_palettes$polarnight[3])+
  facet_grid(Alberta.Health.Services.Zone~.)

png(file = "output/facet_cases_by_region.png", width = 1600, height = 1600, res = 200)
print(p3)
dev.off()

# Calgary Zone Cases
yyc_cases <- recent_cases[Alberta.Health.Services.Zone=="Calgary Zone",]
setorder(yyc_cases,Date.reported)
yyc_cases[,ma:=zoo::rollapply(N,width=7,FUN=function(x){mean(x,na.rm=TRUE)},partial=TRUE,fill=NA),]

ref_date <- Sys.Date()-28
nlsfit<-nls(y~a*(1+r)^(x),data = yyc_cases[Date.reported>=ref_date,list(x=as.numeric(Date.reported-ref_date),y=N)],
    start = c(a=yyc_cases[Date.reported == ref_date,]$ma,r=0.1),
    trace = TRUE,control = nls.control(maxiter=1e4,minFactor = 1e-9, tol=1e-4))
pars <- nlsfit$m$getPars()

N = as.numeric(Sys.Date()+21-ref_date)
pred <- data.table(x=ref_date+seq(1,N,by=1),
                   y=pars['a']*(1+pars['r'])^(seq(1,N,by=1)))
pred[,Date.reported:=x,]

yyc_table <-merge(yyc_cases,pred,by=c("Date.reported"),all=TRUE)
write.csv(yyc_table,file="output/yyc_pred.csv",row.names=FALSE)

p4 <- ggplot(yyc_cases,aes(x=Date.reported,y=N)) +
  geom_point(stat="identity",color=nord::nord_palettes$polarnight[1])+
  geom_line(aes(y=ma),lwd=0.8,colour="#219EBC")+
  geom_line(data=pred,aes(x=x,y=y),lty=2,colour="red")+
  theme_bw()+
  scale_y_continuous(name="New Daily Cases",breaks = seq(0,5000,by=100))+
  scale_x_date(name="Date",date_breaks = '1 month',date_labels = '%b %Y',expand=expansion(add=c(1,10)))+
  coord_cartesian(xlim=range(yyc_cases$Date.reported))+
  scale_fill_nord(name="Health zone",palette = 'aurora')+
  labs(title='Daily New Cases in Calgary',
       subtitle=paste0("Updated with data to: ",max(yyc_cases$Date.reported)),
       caption="Data Source: Alberta Health")+
  geom_label_repel(data=last_report[Alberta.Health.Services.Zone=="Calgary Zone",],aes(y=N,label=N),show.legend = FALSE,direction = "y",nudge_x = 8,segment.color = nord::nord_palettes$polarnight[3])

png(file = "output/yyc_cases.png", width=1600,height=800, res=200)
print(p4)
dev.off()


