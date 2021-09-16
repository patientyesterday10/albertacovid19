source("R/lib/packages.R")
source("R/data_loader.R")
dt <- load_ab_covid_data()

cases_by_zone <- dt[,list(new_cases=sum(.N)),by=c("Date.reported","Age.group","Alberta.Health.Services.Zone")]
cases <- dt[,list(new_cases=sum(.N)),by=c("Date.reported","Age.group")]

cases[,Group:="Adults",]
cases[Age.group %in% c("1-4 years","5-9 years","10-19 years"),Group:="Kids",]
cases_by_zone[,Group:="Adults",]
cases_by_zone[Age.group %in% c("1-4 years","5-9 years","10-19 years"),Group:="Kids",]

pct_kids <- dcast(cases[,list(new_cases=sum(new_cases,na.rm=TRUE)),by=c("Group","Date.reported")],Date.reported~Group,value.var = "new_cases")
pct_kids[is.na(Adults),Adults:=0,]
pct_kids[is.na(Kids),Kids:=0,]
pct_kids[,Total:=Kids+Adults,]
pct_kids[,kid_pct:=Kids/Total,]
pct_kids[,kid_pct_stl:=as.numeric(stl(ts(pct_kids$kid_pct,frequency=7),s.window = 7)$time.series[,'trend']),]
pct_kids <- melt(pct_kids,id.vars = c("Date.reported","Total","kid_pct","kid_pct_stl"),measure.vars = c("Kids","Adults"))
pct_kids[,variable:=factor(variable,c("Adults","Kids"))]

png(file = "output/kids_cases.png", width = 1100, height = 500, res = 100)
ggplot(pct_kids[Date.reported>max(pct_kids$Date.reported)-120,],aes(x=Date.reported,fill=variable))+
  geom_bar(stat="identity",position="stack",aes(y=value))+
  geom_point(aes(y=(1-kid_pct)*max(pct_kids$Total),colour="Adults"),show.legend = F)+
  geom_line(aes(y=(1-kid_pct_stl)*max(pct_kids$Total),colour="Adults"),lwd=0.8)+
  geom_point(aes(y=kid_pct*max(pct_kids$Total),colour="Kids <19yo"),show.legend = F)+
  geom_line(aes(y=kid_pct_stl*max(pct_kids$Total),colour="Kids <19yo"),lwd=0.8)+
  ggrepel::geom_label_repel(data=pct_kids[Date.reported==max(pct_kids$Date.reported) & variable=="Kids",],aes(y=kid_pct*max(pct_kids$Total),x=Date.reported,label=paste0(round(kid_pct*100,1),"%"),colour="Kids <19yo",fill=NULL),nudge_x = 3,max.overlaps = Inf,direction = "x",show.legend = FALSE,size=3)+
  scale_y_continuous(name="New Daily Cases",sec.axis = sec_axis(name="% of Cases in Kids (<19yo)",trans = ~./max(pct_kids$Total),labels = scales::percent))+
  scale_fill_viridis_d(direction = -1,begin = 0.2,end=0.8,name="New Cases")+
  scale_colour_viridis_d(begin = 0.2,end=0.8,direction=-1,name="% Cases")+
  theme_bw()+xlab("Date")+
  labs(title="Percentage of New Cases in Kids",subtitle = paste0("Data updated to: ",max(pct_kids$Date.reported)),caption="Data Source: Alberta Health")+
  theme(legend.position=c(0.02,0.98),legend.justification = c("left","top"),legend.box = "horizontal",legend.background = element_rect(color="grey20"))
dev.off()
