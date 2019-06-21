library(lubridate); library(purrr); library(dplyr); library(stringr)
library(numbers); library(tidyr); library(ggplot2); library(RColorBrewer)
library(ISOweek)
#-----------LIBRARY------------------

study_raw <- read.csv("logs.csv")


# --------------CURRENT---------------------
study_raw$seconds <- study_raw$End..seconds.since.epoch. - study_raw$Start..seconds.since.epoch.

study <- group_by(study_raw, Year, Month, Day) %>%
  summarise(study_hours = sum(seconds) * 0.016666666666667 * 0.016666666666667)


months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Aug", "Sep", "Oct", "Nov", "Dec")
study$Month <- as.factor(study$Month)
levels(study$Month) <- months

ggplot(filter(study, Year != 2019), aes(x=Month,y=study_hours, color = Month)) +
  geom_boxplot() + facet_grid(Year~.) + theme_minimal()

ggplot(filter(study, Year == 2018), aes(x=Month, y=study_hours)) +
  geom_jitter(height = 0.3, aes(color = Month, size = 0.2, alpha = 0.5)) + theme_minimal() +
  scale_color_brewer(palette = "Spectral")
##################### TIME SERIES PLOTTING #########################
study$Month <- as.character(study$Month) # fix factors
study$Year <- as.character(study$Year)
study$Date <- as.Date(with(study, paste(Year, Month, Day, sep="-")), "%Y-%b-%d")
study$Week <- floor_date(study$Date, "week")
study2 <- aggregate(study_hours ~ Week, data=study, FUN=sum)

d <- data.frame(
  date = as.Date(c("2017-09-08", "2018-04-23", "2018-09-8", "2018-12-18", "2019-05-01")),
  event = c("Third Year", "Summer Break", "Fourth Year", "Christmas Break","Vacation")
)

ggplot(data=study2, aes(x=Week, y=study_hours)) + geom_line(color="#3e5f6c", size=1.2, alpha=0.6) +
  scale_x_date(date_labels = "%b %Y", breaks = pretty(study2$Week, n=12)) +
  stat_smooth(color="#da7d48", fill = "#FC4E07", method = "loess", se=F) +
  geom_vline(data=d, mapping=aes(xintercept=date), color="#c93d55", size=1.2, alpha = 0.7) +
  geom_text(data=d, mapping=aes(x=date, y=0, label=event), size=4, angle=90, vjust=-0.4, hjust=0) +
  labs(title = "Pomodoro Hours", x="Date", y="Hours") +
  theme_bw()

ggsave("pomodoro_time.png")
       