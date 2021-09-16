source("R/lib/packages.R")

source("R/data_loader.R")
dt <- load_ab_covid_data()


dist <- dt[Date.reported >= (Sys.Date() - 180), list(N = .N), by = c("Age.group", "Date.reported")]
setorder(dist, Age.group)

dist[, Age.group := factor(Age.group, levels = unique(c("Under 1 year", "1-4 years", "5-9 years", unique(dist$Age.group))), ordered = T),]


setorder(dist, "Date.reported")
dist[, cum := cumsum(N), by = c("Age.group")]
daily <- dist[, list(daily_total = sum(N)), by = c("Date.reported")]
daily[, total := cumsum(daily_total),]
dist <- merge(daily, dist, by = c("Date.reported"))
dist[, pct := cum / total,]
dist[, daily_pct := N / daily_total,]

dist[, daily_pct_ma := rollapply(daily_pct, width = 14, partial = T, FUN = "mean"), by = c("Age.group")]

setorder(dist, Age.group)

dist

p <- ggplot(dist, aes(x = Date.reported, y = daily_pct_ma)) +
  geom_line(aes(colour = Age.group)) +
  coord_cartesian(xlim = c(Sys.Date() - 180, max(dist$Date.reported) + 7 * 5)) +
  geom_point(data = dist[Date.reported == max(dist$Date.reported),], aes(color = Age.group)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  scale_y_continuous(labels = scales::percent) +
  geom_label_repel(data = dist[Date.reported == max(dist$Date.reported),], aes(label = Age.group, color = Age.group), hjust = 0, direction = "y", nudge_x = 7 * 5, segment.alpha = 0.4, show.legend = F) +
  ylab("% of new cases, 14 day MA") +
  xlab("Date") +
  labs(title = "Alberta: Percentage of New Daily COVID19 Cases by Age Group", subtitle = Sys.Date()) +
  theme_bw()

png(file = "output/covid_age.png", width = 1600, height = 1200, res = 200)
print(p)
dev.off()


# Cases vs. Population ----------------------------------------------------

options(stringsAsFactors = F)

demo <- data.table(read.csv("data/1710000501_databaseLoadingData.csv"))
demo <- demo[!(Age.group %in% c("All ages", "Median age")), list(Age.group, VALUE)]
demo[Age.group != "0 to 4 years", Age.group := gsub("[4] years$", "9 years", x = Age.group, perl = TRUE)]
demo[Age.group != "5 to 9 years", Age.group := gsub("5 to", "0 to", x = Age.group, perl = TRUE)]
demo[, Age.group := gsub("^([8-9]|100).+", "80+ years", x = Age.group, perl = TRUE)]
demo[, Age.group := gsub(" to ", "-", x = Age.group, perl = TRUE)]

demo <- demo[, list(Pop = sum(VALUE)), by = c("Age.group")]
demo[, pct_pop := Pop / sum(Pop)]
demo[, Data := "Population"]


dt <- data.table(read.csv("~/Downloads/covid19dataexport.csv"))
dt[, Date.reported := as.Date(Date.reported, format = "%Y-%m-%d")]
dist <- dt[Date.reported >= (Sys.Date() - 60) & Age.group != "Unknown", list(N = .N), by = c("Age.group", "Date.reported")]
setorder(dist, Age.group)
dist[, Age.group := factor(Age.group, levels = unique(c("Under 1 year", "1-4 years", "5-9 years", unique(dist$Age.group))), ordered = T),]
dist[Age.group %in% c("Under 1 year", "1-4 years"), Age.group := "0-4 years"]
dist <- dist[, list(Cases = sum(N)), by = c("Age.group")]
dist[, pct_pop := Cases / sum(Cases)]
dist[, Data := "Cases (Past 60d)"]


age_norm <- rbindlist(list(demo, dist), fill = TRUE)

age_norm[, Age.group := factor(as.character(Age.group), levels = unique(as.character(age_norm$Age.group)), unique(as.character(age_norm$Age.group)))]

p4 <- ggplot(age_norm, aes(x = Age.group, y = pct_pop, fill = Data)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  scale_y_continuous("Percentage", labels = scales::percent) +
  theme(legend.position = "bottom") +
  scale_fill_manual(name = NULL, values = c("Cases (Past 60d)" = "#5e81ac", "Population" = "#3b4252")) +
  ggtitle("Percentage of Cases vs. Population", subtitle = "Alberta, Past 60 days") +
  theme_bw()


png(file = "output/covid_age_norm.png", width = 1600, height = 800, res = 200)
print(p4)
dev.off()
