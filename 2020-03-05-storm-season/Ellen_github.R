# https://rtweet.info/

library(tidyverse)
library(rtweet)
library(lubridate)
library(tidytext)
library(textdata)
library(tsibble)
library(feasts)
library(scales)
library(ggthemes)
library(stringr)
library(ggimage)
library(patchwork)


ellen <- readRDS("ellen.rds")

###Data Cleaningh --------------------------------------
# clean duplicates
#keep only English tweets and tweets from February

ellen$date  <- as.Date(format(ellen$created_at, "%Y-%m-%d"))

ellen <- ellen %>% 
  filter(ellen$lang == "en" & ellen$date <= as.Date("2020-02-29")) %>% 
  distinct(status_id, .keep_all = TRUE)


### Overall tweet frequency across time ---------------------

# Set up images for use with ggbackground. Add your own to your folder!
img0 = "background0.jpg"
img1 = "background1.jpg"
img2 = "background2.jpg"
img3 = "background3.jpg"
img4 = "background4.jpg"

p0 <- ggplot(ellen, aes(x=date)) + geom_histogram(binwidth = 1) +
  scale_x_date(breaks = "1 day") + 
  labs(title = "Tweets using the #StormEllen hashtag", subtitle = "between 14th of February and 2nd of March") + 
  xlab("Date") + ylab("Number of tweets") +
  theme_calc() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggbackground(p0, img1)

#ggsave("tweets.png", plot = last_plot(), device = "png", path = NULL, scale = 1, width = 6, height = 6, units = "in")


### Frequencies of the target weather words ###---------------------------------

precipitation_lexicon <- c("rain", "rainy", "raining", "drizzle", "drizzling", "wet", "pouring", "pour", "pissing down", "snow", "snowy", "hail", 
                                          "precipitation", "shower", "rainstorm", "deluge", "downpour", "squall", "sleet", "snowing", "sleeting")
                                 
wind_lexicon <- c("stormy", "wind", "blowing", "breeze", "gale", "hurricane", "gust", "gusty", "breezy")                          
  
water_lexicon <- c("water", "river", "waves", "wave", "swell", "swelled", "tide", "deluge", "brook", "stream", "canal", "channel", "creek")

damage_lexicon <- c("damage", "destruction", "damaged", "destroyed", "harm", "harmed", "break", "broken", 
                    "fell", "fallen", "cancel", "cancelled", "submerge", "submerged", "overflow", "overflowed",
                    "drown", "drowned", "flood", "flooding", "flooded")                           

ellen$water <- ellen$text %>% 
  str_count(paste(water_lexicon, collapse = "|"))

ellen$precipitation <- ellen$text %>% 
  str_count(paste(precipitation_lexicon, collapse = "|"))

ellen$wind <- ellen$text %>% 
  str_count(paste(wind_lexicon, collapse = "|"))

ellen$damage <- ellen$text %>% 
  str_count(paste(damage_lexicon, collapse = "|"))

p1 <- ellen %>% 
  group_by(date) %>% 
  summarise(mean = mean(water)) %>% 
  ggplot(aes(x=date, y=mean)) + geom_bar(stat = "identity") +
  scale_x_date(breaks = "1 day") + 
  labs(title = "Water", subtitle = "Example terms: river, waves, swell, deluge") + 
  xlab("Date") + ylab("Average mentions per tweet") +
  theme_calc() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

pb1 <- ggbackground(p1, img1)
pb1
#ggsave("water_mentions.png", plot = last_plot(), device = "png", path = NULL, scale = 1, width = 6, height = 6, units = "in")


p2 <- ellen %>% 
  group_by(date) %>% 
  summarise(mean = mean(damage)) %>% 
  ggplot(aes(x=date, y=mean)) + geom_bar(stat = "identity") +
  scale_x_date(breaks = "1 day") + 
  labs(title = "Damage", subtitle = "Example terms: flood, destroy, break, overflow") + 
  xlab("Date") + ylab("Average mentions per tweet") +
  theme_calc() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

pb2 <- ggbackground(p2, img2)
pb2

#ggsave("damage_mentions.png", plot = last_plot(), device = "png", path = NULL, scale = 1, width = 6, height = 6, units = "in")

p3 <- ellen %>% 
  group_by(date) %>% 
  summarise(mean = mean(wind)) %>% 
  ggplot(aes(x=date, y=mean)) + geom_bar(stat = "identity") +
  scale_x_date(breaks = "1 day") + 
  labs(title = "Wind", subtitle = "Example terms: stormy, windy, gust, gale") + 
  xlab("Date") + ylab("Average mentions per tweet") +
  theme_calc() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

pb3 <- ggbackground(p3, img3)
pb3

#ggsave("wind_mentions.png", plot = last_plot(), device = "png", path = NULL, scale = 1, width = 6, height = 6, units = "in")

p4 <- ellen %>% 
  group_by(date) %>% 
  summarise(mean = mean(precipitation)) %>% 
  ggplot(aes(x=date, y=mean)) + geom_bar(stat = "identity") +
  scale_x_date(breaks = "1 day") + 
  labs(title = "Precipitation", subtitle = "Example terms: snow, rain, drizzle, sleet, downpour") + 
  xlab("Date") + ylab("Average mentions per tweet") +
  theme_calc() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

pb4 <- ggbackground(p4, img4)
pb4
#ggsave("precip_mentions.png", plot = last_plot(), device = "png", path = NULL, scale = 1, width = 6, height = 6, units = "in")



plot <- ((p1 | p4)/
    (p3 | p2)) + plot_annotation(title = "Proportions of tweets mentioning...", subtitle = NULL, caption = NULL,
                                 tag_levels = NULL, tag_prefix = NULL, tag_suffix = NULL,
                                 tag_sep = NULL, theme = NULL)
plot
#ggsave("mentions.png", plot = last_plot(), device = "png", path = NULL, scale = 1, width = 9, height = 9, units = "in")


### Look at weather and tweets ###---------------------------------------
# First run the weather script

weather<- read_csv("weather.csv")

weather$Station <- as.factor(weather$Station)
weather$Date <- as.character(weather$Date)

weather$month <- "Feb"
weather$year <- "2020"

weather$date <- paste(weather$Date, weather$month, weather$year, sep = "-") %>%
  dmy()

weather<- weather %>% dplyr::select(-Date, -month, -year)

weather1 <- weather %>% 
  group_by(date) %>% 
  summarise(Temp_Avg = mean(Temp_Avg), 
            Wind_Max = mean(Wind_Max), Wind_Avg = mean(Wind_Avg), Wind_Min = mean(Wind_Min),
            Pres_Avg = mean(Pres_Avg), Pres_Min = mean(Pres_Min),
            Prec_Avg_mm = mean(Precipitation_mm))

ellen_weather <- weather1 %>%  
  filter(weather1$date >= as.Date("2020-02-15") & weather1$date <= as.Date("2020-02-29"))

### Then match up the data

ellen_daily <- ellen %>% 
  group_by(date) %>% 
  summarise(precip_mentions = mean(precipitation), wind_mentions = mean(wind), water_mentions = mean(water), damage_mentions = mean(damage)  )

ellen_daily <- ellen_daily %>% inner_join(ellen_weather)

#look at correlations

library(Hmisc)
library(corrplot)
library(corrgram)

ellen_cor <- ellen_daily %>% dplyr::select(-date)

corrplot(corrgram(ellen_cor), type = "lower", order = "original", 
         tl.col = "black", tl.srt = 45)

#magic that allows you to have 2 axes!

# For Wind and damage
ylim.prim <- c(0, 0.4)   # range damage tweet proportions
ylim.sec <- c(0, 60)    # range of wind speed

b <- diff(ylim.prim)/diff(ylim.sec)
a <- b*(ylim.prim[1] - ylim.sec[1])

p5 <- ggplot(ellen_daily, aes(date, damage_mentions)) +
  geom_col() +
  geom_line(aes(y = a + Wind_Max*b), color = "blue", size = 1) + 
  coord_cartesian(ylim = c(0, 0.4))  +
  scale_y_continuous("Average damage mentions (grey)", sec.axis = sec_axis(~ (. - a)/b, name = "Maximum wind speed (blue)")) +
  scale_x_date(breaks = "1 day") + 
  theme_calc() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

p5

#ggsave("wind_damage.png", plot = last_plot(), device = "png", path = NULL, scale = 1, width = 6, height = 6, units = "in")

# For Wind and wind
ylim.prim <- c(0, 0.4)   # range damage tweet proportions
ylim.sec <- c(0, 60)    # range of wind speed

b <- diff(ylim.prim)/diff(ylim.sec)
a <- b*(ylim.prim[1] - ylim.sec[1])

p6 <- ggplot(ellen_daily, aes(date, wind_mentions)) +
  geom_col() +
  geom_line(aes(y = a + Wind_Max*b), color = "blue", size = 1) + 
  coord_cartesian(ylim = c(0, 0.4))  +
  scale_y_continuous("Average wind mentions (grey)", sec.axis = sec_axis(~ (. - a)/b, name = "Maximum wind speed (blue)")) +
  scale_x_date(breaks = "1 day") + 
  theme_calc() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

p6

#ggsave("wind_wind.png", plot = last_plot(), device = "png", path = NULL, scale = 1, width = 6, height = 6, units = "in")

plot1 <- (p5 | p6) + 
  plot_annotation(title = "Maximum wind speed and the proportions of tweets mentioning damage and wind")

plot1

#ggsave("wind_speed.png", plot = last_plot(), device = "png", path = NULL, scale = 1, width = 12, height = 6, units = "in")


# for Precipitation and precipitation

ylim.prim <- c(0, 0.4)   # range precipitation tweet proportions
ylim.sec <- c(0, 30)    # range of precipitation

b <- diff(ylim.prim)/diff(ylim.sec)
a <- b*(ylim.prim[1] - ylim.sec[1])

p7 <- ggplot(ellen_daily, aes(date, precip_mentions)) +
  geom_col() +
  geom_line(aes(y = a + Prec_Avg_mm*b), color = "blue", size = 1) + 
  coord_cartesian(ylim = c(0, 0.4))  +
  scale_y_continuous("Average rain mentions (grey)", sec.axis = sec_axis(~ (. - a)/b, name = "Total precipitation (blue)")) +
  scale_x_date(breaks = "1 day") + 
  theme_calc() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
p7

#ggsave("prec_prec.png", plot = last_plot(), device = "png", path = NULL, scale = 1, width = 6, height = 6, units = "in")

# for Precipitation and water
ylim.prim <- c(0, 0.4)   # range water tweet proportions
ylim.sec <- c(0, 30)    # range of precipitation

b <- diff(ylim.prim)/diff(ylim.sec)
a <- b*(ylim.prim[1] - ylim.sec[1])

p8 <- ggplot(ellen_daily, aes(date, water_mentions)) +
  geom_col() +
  geom_line(aes(y = a + Prec_Avg_mm*b), color = "blue", size = 1) + 
  coord_cartesian(ylim = c(0, 0.4))  +
  scale_y_continuous("Average rain mentions (grey)", sec.axis = sec_axis(~ (. - a)/b, name = "Total precipitation (blue)")) +
  scale_x_date(breaks = "1 day") + 
  theme_calc() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

p8

#ggsave("prec_water.png", plot = last_plot(), device = "png", path = NULL, scale = 1, width = 6, height = 6, units = "in")

# for precipitation and damage
ylim.prim <- c(0, 0.4)   # range damage tweet proportions
ylim.sec <- c(0, 30)    # range of precipitation

b <- diff(ylim.prim)/diff(ylim.sec)
a <- b*(ylim.prim[1] - ylim.sec[1])

p9 <- ggplot(ellen_daily, aes(date, damage_mentions)) +
  geom_col() +
  geom_line(aes(y = a + Prec_Avg_mm*b), color = "blue", size = 1) + 
  coord_cartesian(ylim = c(0, 0.4))  +
  scale_y_continuous("Average damage mentions (grey)", sec.axis = sec_axis(~ (. - a)/b, name = "Total precipitation (blue)")) +
  scale_x_date(breaks = "1 day") + 
  theme_calc() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
p9

#ggsave("prec_damage.png", plot = last_plot(), device = "png", path = NULL, scale = 1, width = 6, height = 6, units = "in")

plot2 <- (p7 | p8 |p9 ) + plot_annotation(title = "Total precipitation and proportion of tweets mentioning rain, water and damage")
plot2
#ggsave("precipitation.png", plot = last_plot(), device = "png", path = NULL, scale = 1, width = 18, height = 6, units = "in")


plot1
# overall, may be lagged by 2-3 days, but not enough data to tell.


### Text analysis -----------------------------------------------

devtools::install_github("hrbrmstr/pluralize")

library(ggwordcloud)
library(pluralize)

my_stop_words <- tibble(word = c("https", "t.co", "rt", "amp", "rstats", "gt", "i’m", "it's", "it’s", 
                                 "stormellen", "stormdennis", "stormciara", "dennis", "stormjorge", "ciara", "jorge", "ellen"), 
                        lexicon = "twitter")


#merge the two lists
all_stop_words <- stop_words %>% 
  bind_rows(my_stop_words)

ellen_text <- ellen %>% 
  select(status_id, text, date) %>% 
  unnest_tokens(word, text) %>% 
  filter(is.na(as.numeric(word))) %>% 
  anti_join(all_stop_words, by = "word")

ellen_text$word_sing <- singularize(ellen_text$word)


cloud_words <- ellen_text %>% 
  group_by(word_sing) %>% 
  tally %>% 
  filter(n>40) %>% 
  arrange(desc(n))

pw <-ggplot(cloud_words, aes(label = word_sing, size = n)) +
  geom_text_wordcloud() +
  scale_size_area(max_size = 30) + 
  theme_minimal() +
  labs(title = "40 most common words used in #StormEllen tweets") +
  theme_calc()

ggbackground(pw, img3)

# ggsave("clouds.png", plot = last_plot(), device = "png", path = NULL, scale = 1, width = 6, height = 6, units = "in")

##Sentiment Analysis--------------------------

nrc_words <- ellen_text %>% 
  inner_join(get_sentiments("nrc"), by = "word")

#making it into a chart
chart_words <- nrc_words %>% 
  group_by(sentiment) %>% 
  tally %>% 
  arrange(desc(n))

chart_words$perc <- chart_words$n / sum(chart_words$n) * 100

ps <- ggplot(chart_words, aes(sentiment, perc)) + geom_bar(stat = "identity") +  
  labs(title = "Distribution of sentiment related words in #StormEllen tweets") +
  ylab("Percentage of words") + 
  xlab ("Sentiment expressed") +
  theme_calc() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggbackground(ps, img3)       

#ggsave("sentiment.png", plot = last_plot(), device = "png", path = NULL, scale = 1, width = 6, height = 6, units = "in")
