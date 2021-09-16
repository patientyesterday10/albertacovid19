source("R/lib/packages.R")
source("R/data_loader.R")
dt <- load_ab_covid_data()

new_cases <- dt[, .N, by = c("Date.reported")]

ref_date <- as.Date("2021-03-01")

new_cases[, days := as.numeric(Date.reported - ref_date),]
max_x <- max(new_cases$days)
max_y <- max(new_cases$N)

setkeyv(new_cases,"Date.reported")

wave1 <- new_cases[Date.reported>=as.Date("2020-03-01") & Date.reported<=as.Date("2020-07-01"),]
wave2 <- new_cases[Date.reported>=as.Date("2020-09-01") & Date.reported<=as.Date("2021-02-08"),]
wave3 <- new_cases[Date.reported>=as.Date("2021-02-08") & Date.reported<Sys.Date(),]

wave1_ref <- min(wave1$Date.reported)
wave2_ref <- min(wave2$Date.reported)+14
wave3_ref <- min(wave3$Date.reported)+14


wave1[,day:=as.numeric(Date.reported-wave1_ref),]
wave2[,day:=as.numeric(Date.reported-wave2_ref),]
wave3[,day:=as.numeric(Date.reported-wave3_ref),]

wave1[,sm:=zoo::rollapply(N,width=7,FUN="mean",partial=TRUE,fill=NA)]
wave2[,sm:=zoo::rollapply(N,width=7,FUN="mean",partial=TRUE,fill=NA)]
wave3[,sm:=zoo::rollapply(N,width=7,FUN="mean",partial=TRUE,fill=NA)]

wave2_mod_rest <- wave2[Date.reported==as.Date("2020-11-24"),]$day
wave2_strict_rest <- wave2[Date.reported==as.Date("2020-12-13"),]$day


acf12 <- ccf(x = wave1$sm,y = wave2$sm,plot = F)
acf12 <- data.table(lag=as.vector(acf12$lag),acf=as.vector(acf12$acf))
lag12 <- acf12$lag[which.max(abs(acf12$acf))]
lag12
wave2[,day:=day+lag12,]

acf23 <- ccf(x = wave2$sm,y = wave3$sm,plot = F,lag.max = 30)
acf23 <- data.table(lag=as.vector(acf23$lag),acf=as.vector(acf23$acf))
lag23 <- acf23$lag[which.max(abs(acf23$acf))]
lag23
wave3[,day:=day+lag12+lag23,]

wave2fit <- optim(par=c(wave2[day==0]$N,0.1,0.1,0),fn=function(p){
  x<-wave2[day>=0 & day<=wave2[which.max(wave2$N),]$day,]$day
  y<-wave2[day>=0 & day<=wave2[which.max(wave2$N),]$day,]$N
  sqrt(mean((y-(p[1]+p[2]*exp(p[3]*(x-p[4]))))^2,na.rm=TRUE))
},lower = c(0,0,0,-30),upper=c(250,200,1,30),method = "L-BFGS-B")

wave2exp <- data.table(day=seq(min(wave2$day),(wave2[which.max(wave2$N),]$day)+28,by=1))
wave2exp[day>0,pred:=wave2fit$par[1]+wave2fit$par[2]*exp(wave2fit$par[3]*(day-wave2fit$par[4]))]

wave2down <- optim(
  par=c(
    wave2[day==wave2[which.max(wave2$N),]$day]$N,
    -0.1,
    wave2[which.max(wave2$N),]$day),
  fn=function(p){
    x<-wave2[day >= wave2[which.max(wave2$N),]$day,]$day
    y<-wave2[day>= wave2[which.max(wave2$N),]$day,]$N
    sqrt(mean((y-(p[1]*exp(p[2]*(x-p[3]))))^2,na.rm=TRUE))
  },
  lower = c(wave2[day==wave2[which.max(wave2$N),]$day]$N*0.8,-0.8,wave2[which.max(wave2$N),]$day-20),
  upper=c(wave2[day==wave2[which.max(wave2$N),]$day]$N*1.2,0,wave2[which.max(wave2$N),]$day+20),method = "L-BFGS-B")

wave2down

wave2recov <- data.table(day=seq(wave2[which.max(wave2$N),]$day,max(wave2$day),by=1))
wave2recov[day>0,pred:= wave2down$par[1]*exp(wave2down$par[2]*(day-wave2down$par[3]))]

wave3fit <- optim(
  par=c(wave3[day==0]$N,1,0.1,0),
  fn=function(p){
      x<-wave3[day>=0,]$day
      y<-wave3[day>=0,]$N
      return(sqrt(sum((y-(p[1]+p[2]*exp(p[3]*(x-p[4]))))^2,na.rm=TRUE)))
    },
lower = c(wave3[day==0]$N*0.9,0,0,-30),
upper=c(wave3[day==0]$N*1.1,200,1,30),
method = "L-BFGS-B")

print(wave3fit)

wave3exp <- data.table(day=seq(min(wave3$day),max(wave3$day)+28,by=1))
wave3exp[day>0,pred:=wave3fit$par[1]+wave3fit$par[2]*exp(wave3fit$par[3]*(day-wave3fit$par[4]))]

wave3rec <- data.table(day=seq(max(wave3$day),max(wave3$day)+90,by=1))
wave3rec[day>0,pred:=wave3exp[day==max(wave3$day),]$pred*exp(wave2down$par[2]*(1.4)*(day-max(wave3$day)))]

wave3recdelay <- data.table(day=max(wave3$day)+seq(7,90+7,by=1))
wave3recdelay[day>0,pred:=(wave3exp[day==(max(wave3$day)+7),]$pred)*exp(wave2down$par[2]*(day-(max(wave3$day)+7)))]

p <- ggplot()+
  geom_point(data=wave1,aes(x=day,y=N,colour="1st Wave"),alpha=0.5)+
  geom_point(data=wave2,aes(x=day,y=N,colour="2nd Wave"),alpha=0.5)+
  geom_point(data=wave3,aes(x=day,y=N,colour="3rd Wave"),alpha=0.5)+
  geom_line(data=wave1,aes(x=day,y=sm,colour="1st Wave"),lwd=1)+
  geom_line(data=wave2,aes(x=day,y=sm,colour="2nd Wave"),lwd=1)+
  geom_line(data=wave3,aes(x=day,y=sm,colour="3rd Wave"),lwd=1.2)+
  #geom_line(data=wave2exp,aes(x=day,y=pred,colour="Exp. Fit"),lwd=0.8,lty=2)+
  #geom_line(data=wave2recov,aes(x=day,y=pred,colour="NPI Impact"),lwd=0.8,lty=2)+
  #geom_line(data=wave3exp,aes(x=day,y=pred,colour="Exp. Fit"),lwd=0.8,lty=2)+
  #geom_line(data=wave3rec,aes(x=day,y=pred,colour="NPI Impact"),lwd=0.8,lty=2)+
  #geom_line(data=wave3recdelay,aes(x=day,y=pred,colour="NPI w Delay"),lwd=0.8,lty=2)+
  geom_vline(lty=3,aes(xintercept=wave2_mod_rest,colour="2nd Wave"),lwd=0.8)+
  geom_vline(lty=3,aes(xintercept=wave2_strict_rest,colour="2nd Wave"),lwd=0.8)+
  scale_colour_manual(name=NULL,values=c(
    "1st Wave"="#feb24c",
    "2nd Wave"="#fd8d3c",
    "3rd Wave"="#bd0026",
    "Exp. Fit"="dodgerblue",
    "NPI Impact"="darkorchid",
    "NPI w Delay"="hotpink"))+
  theme_bw()+
  xlab("Days")+
  ylab("New Daily Cases Reported")+
  labs(title="New Daily Cases by Wave, Alberta")+
  ylim(0,max(wave3$N,wave2$N)*1.1)

graphics.off()

dir.create('output',showWarnings=FALSE)
png(file = "output/waves.png", width = 1600, height = 800, res = 200)
print(p)
dev.off()
