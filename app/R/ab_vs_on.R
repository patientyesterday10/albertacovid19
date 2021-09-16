source("R/lib/packages.R")
source("R/data_loader.R")
dt <- load_ab_covid_data()

ab <- dt[, list(Alberta = .N), by = c("Date.reported")]

r <- httr::GET("https://data.ontario.ca/dataset/f4f86e54-872d-43f8-8a86-3892fd3cb5e6/resource/ed270bb8-340b-41f9-a7c6-e8ef587e6d11/download/covidtesting.csv")
on <- data.table(content(r))
on[is.na(`Confirmed Positive`), `Confirmed Positive` := 0,]
on[is.na(`Presumptive Positive`), `Presumptive Positive` := 0,]
on[is.na(`Total Cases`), `Total Cases` := 0,]

on[, new_cases := c(0, diff(`Total Cases`))]
on <- on[, list(Ontario = new_cases), by = c("Reported Date")]

combined <- merge(ab, on, by.x = c("Date.reported"), by.y = c("Reported Date"), all = TRUE)
combined <- melt(combined, id.vars = "Date.reported")

exp_fit <- function(p, x, y) {
  tmp <- data.table(x = x, y = y)
  tmp[, yfit := p[1] + p[2] * exp(p[3] * x),]
  tmp[, sqerr := (y - yfit)^2]
  result <<- copy(tmp)
  return(sqrt(sum(tmp$sqerr, na.rm = TRUE)))
}

combined[, days := as.numeric(Date.reported - as.Date("2020-09-01")),]
combined <- combined[days >= 0,]
setorder(combined,days)
combined[,ma:=zoo::rollapply(value,width=7,fill=NA,partial=TRUE,FUN=mean),by=c("variable")]

opt_ab <- optim(par = c(113, 1, 1e-10), fn = exp_fit, x = combined[Date.reported >= as.Date("2020-09-01") & variable == "Alberta",]$days, y = combined[Date.reported >= as.Date("2020-09-01") & variable == "Alberta",]$value)
opt_on <- optim(par = c(112, 1, 1e-10), fn = exp_fit, x = combined[Date.reported >= as.Date("2020-09-01") & variable == "Ontario",]$days, y = combined[Date.reported >= as.Date("2020-09-01") & variable == "Ontario",]$value)

pred <- data.table(days = seq(0, max(combined$days) + 30, by = 1))
pred[, Alberta := opt_ab$par[1] + opt_ab$par[2] * exp(opt_ab$par[3] * days)]
pred[, Ontario := opt_on$par[1] + opt_on$par[2] * exp(opt_on$par[3] * days)]
pred <- melt(pred, id.vars = "days")
pred[, Date.reported := as.Date("2020-09-01") + days,]


p <- ggplot(combined[Date.reported>=as.Date("2020-09-01"),])+
  geom_point(aes(x=Date.reported,y=value,colour=variable),alpha=0.5)+
  geom_line(aes(x=Date.reported,y=ma,colour=variable),alpha=0.8,lwd=0.8)+
  theme_bw()+
  geom_line(data=pred[variable=="Onatrio",],aes(x=Date.reported,y=value,colour=variable))+
  ylab("New Cases")+
  scale_y_continuous(name="New Daily Cases")+
  scale_colour_manual(values=c("Alberta"="dodgerblue","Ontario"="seagreen3"))+
labs(title="Alberta vs. Ontario: New Cases",subtitle = "Absolute Cases (Not adjusted for population)")


p2 <- ggplot(combined[Date.reported>=as.Date("2020-09-01"),])+
  geom_point(aes(x=Date.reported,y=value*ifelse(variable=="Alberta",14.57/4.371,1),colour=variable),alpha=0.40)+
  geom_line(aes(x=Date.reported,y=ma*ifelse(variable=="Alberta",14.57/4.371,1),colour=variable),alpha=2,lwd=0.8)+
  theme_bw()+
  theme(axis.title.y.left=element_text(colour="seagreen"),axis.title.y.right=element_text(colour="dodgerblue3"))+
  geom_line(data=pred[variable=="Onatrio",],aes(x=Date.reported,y=value,colour=variable))+
  ylab("New Cases")+
  scale_y_continuous(name="Ontario Cases",sec.axis = sec_axis(trans=~.*(4.371/14.57),name="Alberta Cases"))+
  scale_colour_manual(values=c("Alberta"="dodgerblue","Ontario"="seagreen3"))+
  labs(title="Alberta vs. Ontario: New Cases", subtitle="Normalized for Provincial Population")


png("output/covid_on_ab.png",width=1600,height=800, res=200)
print(p)
dev.off()

png("output/covid_on_ab_normalized.png",width=1600,height=800, res=200)
print(p2)
dev.off()
