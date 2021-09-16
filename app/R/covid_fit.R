source("R/lib/packages.R")
source("R/data_loader.R")

dt <- load_ab_covid_data()

new_cases <- dt[, .N, by = c("Date.reported")]

ref_date <- as.Date("2021-03-01")

new_cases[, days := as.numeric(Date.reported - ref_date),]
max_x <- max(new_cases$days)

# Before New data:

exp_fit <- function(p, x, y) {
  tmp <- data.table(x = x, y = y)
  tmp[, yfit := p[1] + p[2] * exp(p[3] * x),]
  tmp[, sqerr := (y - yfit)^2]
  result <<- copy(tmp)
  return(sqrt(sum(tmp$sqerr, na.rm = TRUE)))
}

back_fit <- function(bfit = as.Date("2021-03-01")) {
  bfit_max_x <- max(new_cases[Date.reported <= bfit,]$days)
  opt <- optim(par = c(new_cases[days == 0 & Date.reported <= bfit,]$N, 1, 1e-10), fn = exp_fit, x = new_cases[days > 0 & Date.reported <= bfit,]$days, y = new_cases[days > 0 & Date.reported <= bfit,]$N)
  result <- merge(result, data.table(x = seq(bfit_max_x, by = 1, length.out = 28 + (max_x - bfit_max_x))), by = c("x"), all = TRUE)
  result[, yfit := opt$par[1] + opt$par[2] * exp(opt$par[3] * x)]
  result[, date := ref_date + x,]
  return(result[, list(date, yfit)])
}

back_fit_weekly <- function() {
  fit_dates <- seq(Sys.Date(), Sys.Date() - 21, by = -7)
  return(rbindlist(lapply(fit_dates, FUN = function(x) {
    tmp <- back_fit(x)
    tmp$fit_date <- x
    return(tmp)
  })))
}

fits <- back_fit_weekly()

setorder(new_cases, days)
new_cases[, smooth := zoo::rollapply(N, width = 7, partial = TRUE, FUN = mean, fill = NA)]

p <- ggplot(new_cases[Date.reported >= as.Date("2020-09-01"),], aes(x = Date.reported, y = N)) +
  #geom_line(data = fits, aes(x = date, y = yfit, colour = as.Date(fit_date), group = fit_date, alpha = fit_date)) +
  geom_point(colour = '#4C566A') +
  geom_line(aes(y = smooth), lwd = 0.8, color = "#219EBC") +
  #geom_line(data = fits[fit_date == max(fits$fit_date),], aes(x = date, y = yfit, colour = as.Date(fit_date)), lwd = 1.0, alpha = 0.8) +
  geom_vline(xintercept = Sys.Date(), lty = 2, colour = "#fd541a") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  xlab("Date") +
  scale_color_viridis_c(end = 0.75, option = "A", guide = FALSE) +
  scale_x_date(date_breaks = "1 week", date_labels = "%a %b %d") +
  coord_cartesian(ylim = c(0, max(new_cases$N) * 1.1)) +
  labs(title = "Alberta - New Daily Cases",
       subtitle = max(dt$Date.reported) + 1)

p2 <- p +
  coord_cartesian(ylim = c(100, max(new_cases$N) * 1.1)) +
  scale_y_log10(
  name="New Cases (Daily)",
  limits=c(100,max(new_cases$N)),
  breaks = c(100, 200, 500, 1000, 1500, 2000, 3000, 4000, 5000)
)

graphics.off()

dir.create('output',showWarnings=FALSE)

png(file = "output/covid_pred.png", width = 1600, height = 1600, res = 200)
print(p + p2 + plot_layout(nrow = 2))
dev.off()

#fits[fit_date == max(fits$fit_date),]


# Cyclicity ---------------------------------------------------------------

new_cases[, dow := wday(Date.reported),]
new_cases[, week := strftime(Date.reported, "%Y-%U"),]
new_cases[, weekday := strftime(Date.reported, "%a")]
setorder(new_cases,week,weekday)

dy <- unique(new_cases[, list(dow, weekday)])
setorder(dy, dow)

new_cases[, weekday := factor(weekday, levels = unique(dy$weekday))]
new_cases[, week := factor(week, levels = unique(new_cases$week)),]
new_cases <- new_cases[week %in% tail(unique(new_cases$week),8),]
p3 <- ggplot(new_cases[days > 0 | is.na(days),], aes(x = weekday, y = N, colour = week, group = week)) +
  geom_point() +
  geom_line() +
  ylab("New Cases") +
  xlab("Weekday")+
  labs(title="Alberta: New Cases by Weekday",
       subtitle=Sys.Date(),
       caption='Data Source: Alberta Health')+
  theme_bw() +
  scale_y_continuous(breaks = seq(0, 5000, by = 100),expand = expansion(add=c(0,200)))+
  geom_label_repel(data = new_cases[week==last(new_cases$week)],aes(label=N),direction = "y")+
  scale_colour_brewer(name="Week",palette = "GnBu")

graphics.off()
png(file = "output/covid_weekly.png", width = 1600, height = 800, res = 200)
print(p3)
dev.off()

# Weekly YYC

new_cases <- dt[Alberta.Health.Services.Zone=="Calgary Zone", .N, by = c("Date.reported")]
new_cases[, days := as.numeric(Date.reported - ref_date),]
new_cases[, dow := wday(Date.reported),]
new_cases[, week := strftime(Date.reported, "%Y-%U"),]
new_cases[, weekday := strftime(Date.reported, "%a")]
setorder(new_cases,week,weekday)

dy <- unique(new_cases[, list(dow, weekday)])
setorder(dy, dow)

new_cases[, weekday := factor(weekday, levels = unique(dy$weekday))]
new_cases[, week := factor(week, levels = unique(new_cases$week)),]
new_cases <- new_cases[week %in% tail(unique(new_cases$week),8),]
p4 <- ggplot(new_cases[days > 0 | is.na(days),], aes(x = weekday, y = N, colour = week, group = week)) +
  geom_point() +
  geom_line() +
  ylab("New Cases") +
  xlab("Weekday")+
  labs(title="Calgary: New Cases by Weekday",
       subtitle=Sys.Date(),
       caption='Data Source: Alberta Health')+
  theme_bw() +
  scale_y_continuous(breaks = seq(0, 5000, by = 100),expand = expansion(add=c(0,200)))+
  geom_label_repel(data = new_cases[week==last(new_cases$week)],aes(label=N),direction = "y")+
  scale_colour_brewer(name="Week",palette = "GnBu")

graphics.off()
png(file = "output/covid_weekly_yyc.png", width = 1600, height = 800, res = 200)
print(p4)
dev.off()
