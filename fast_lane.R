# *************************************************************************
# SETUP ----
# *************************************************************************

rm(list = ls())
# Set the seed to the desired grade
set.seed(100)

# Libraries

pacman::p_load(tidyverse,
               data.table,
               DT,
               visdat,
               lubridate,
               purrr,
               skimr,
               scales,
               DescTools,
               here,
               hms,
               psych,
               zoo,
               tibble)

# Many observations appear in the old version of the data and not the new one
# We want them all
fast_lane <- bind_rows(fread(here("datasets", "price_data_20220823.csv")),
                       fread(here("datasets", "price_data_20220809.csv"))) %>% 
  distinct()

# for later use
weekdays_names <- c("Sunday", "Monday", "Tuesday", "Wednesday",
                    "Thursday", "Friday", "Saturday")

# *************************************************************************
# PART I - EDA ----
# *************************************************************************

## First Look At The Raw Data ----
summary(fast_lane$time)
summary(fast_lane$price)
glimpse(fast_lane)
fast_lane %>% skim
fast_lane %>% Desc
fast_lane %>% select(where(is.numeric)) %>%  describe

ggplot(fast_lane, aes(x=price)) + 
  geom_histogram(binwidth = 5, fill = "navyblue")+
  theme_minimal()+
  scale_y_continuous(expand = c(0,NA)) +
  scale_x_continuous(breaks = 10* 0:11)+
  labs(title = "Histogram of Prices", x = "Price") 
ggsave(bg = "white", filename = here("figures", "price_hist_no_clean_at_all.png"))

# We can see from the histogram and the summaries earlier that we mainly have 7s
# which is the minimal price. to understand the data better - look at the percentage 
# of observations with price 7 and at the distribution of prices for observations
# with surge prices.

mean(fast_lane$price == 7)

fast_lane %>% 
  filter(price > 7) %>%
  ggplot(aes(x=price)) +
  geom_histogram(binwidth = 5, fill = "navyblue")+
  theme_minimal()+
  scale_y_continuous(expand = c(0,NA)) +
  scale_x_continuous(breaks = 10* 0:11)+
  labs(title = "Histogram of Surge Prices", x = "Price")
ggsave(bg = "white", filename = here("figures", "price_hist_no_clean_at_all_only_surge.png"))

fast_lane %>% 
  filter(price>7) %>% 
  Desc()

# Make sure all integer values between 7 and 110 appear in the data.
all(unique(fast_lane$price) %>% sort() == 7:110)


## Create time and price based variables preliminary ----

fast_lane <- fast_lane %>% 
  mutate(surge = if_else(price > 7, 1, 0),
         max_price = if_else(price == 110, 1, 0),
         date = date(time),
         hour = hour(time),
         rounded_time = round_date(time, "5 mins")) 

## Check missing data and organize data to have the same times in every day ----

# check num of obs in each day and explore

obs_num <- fast_lane %>% 
  group_by(date) %>% 
  summarise(obs = n()) %>% 
  arrange(obs)

head(obs_num, n = 10)

summary(obs_num$obs)

# expected number of observations per day:
60*60*24/10

obs_num %>% 
  ggplot(aes(x = obs)) +
  geom_histogram(bins = 10, fill = "navyblue")+
  theme_minimal()+
  scale_y_continuous(expand = c(0,NA))+
  scale_x_continuous(breaks = 1000 * 0:9, labels = 1000 * 0:9) + 
  labs(title = "Distribution of observations number per day",
       x = "Number of daily observations", y = "Number of days")
ggsave(bg = "white", filename = here("figures", "obs_num_per_day_hist.png"))

# Look at the distribution of observations during days with less than 7k obs
low_obs_days <- obs_num %>% 
  filter(obs <= 7000) %>% 
  pull(date)

fast_lane %>% 
  filter(date %in% low_obs_days) %>% 
  ggplot(aes(x = hour)) +
  geom_bar(stat = "count", fill = "navyblue")+
  theme_minimal()+
  scale_y_continuous(expand = c(0,NA))+
  scale_x_continuous(breaks = 2*0:12, labels = 2*0:12)+
  facet_wrap(~date, scales = "fixed")+
  labs(title = "Distribution of observations during low observation days",
       x = "Hour")
ggsave(bg = "white", filename = here("figures", "obs_times_during_low_obs_days.png"))

# We can see that there are chunks of missing data and that the number of hourly 
# observations is not completely stable even during hours where not all the data 
# is missing. To deal with the second problem, and in anticipation of the
# prediction task we will calculate average prices in multiples of 5 mins. 
first_time <- min(fast_lane$rounded_time)
last_time <- max(fast_lane$rounded_time)

fast_lane_clean <- data.frame(
  rounded_time = seq.POSIXt(first_time, last_time, by = "5 min")) %>% 
  left_join(fast_lane) %>% 
  na.omit() %>% 
  group_by(rounded_time) %>% 
  summarise(price = mean(price),
            surge_any = max(surge),
            surge_all = min(surge),
            max_price_any = max(max_price),
            max_price_all = min(max_price)) %>% 
  rename(time = rounded_time) 

## Create more variables ----

fast_lane_clean <- fast_lane_clean %>% 
  mutate(date = date(time),
         month = month(date),
         week = week(date),
         weekday = wday(time),
         friday = if_else(weekday == 6, 1, 0),
         saturday = if_else(weekday == 7, 1, 0),
         weekend = pmax(friday, saturday),
         hour = hour(time),
         time_of_day = as_hms(3600 * (hour + minute(time)/60)),
         elementary_big_hofesh = if_else(date >= "2022-07-01",1,0),
         highschool_big_hofesh = if_else(date >= "2022-06-21",1,0),
         sabbatical = if_else(weekend == 1 | date == "2022-06-05",1,0),
         school_sabbatical = if_else(
           weekend == 1 | date %in% ymd(c("2022-05-19", "2022-06-05", "2022-06-06"))|
             elementary_big_hofesh == 1, 1,0))


## Analyze dates and hours to reduce the dataset ----


fast_lane_clean %>%
  na.omit() %>% 
  group_by(date) %>% 
  summarise(mean_price = mean(price, na.rm = T),
            max_price = max(price, na.rm = T),
            weekday = wday(date, label = T)) %>% 
  ungroup() %>% 
  distinct() %>% 
  group_by(weekday) %>% 
  summarise(Avg_Price = mean(mean_price),
            Max_Price = mean(max_price)) %>%
  pivot_longer(ends_with("price"), names_to = "type", values_to = "Price") %>%
  mutate(type = str_replace(type, "_", ". ")) %>% 
  ggplot(aes(x = factor(weekday), y = Price)) +
  geom_bar(stat = "identity", fill = "navy")+
  theme_minimal() + 
  facet_wrap(~type, scales = "free_y") +
  scale_y_continuous(expand = expansion(mult = c(0,0.1))) +
  geom_text(aes(label = round(Price, 1)), vjust = - 1 ) +
  theme(strip.background = element_blank(), 
        strip.text.x = element_text(face = "bold", size = 12)) +
  labs(x = element_blank(), subtitle = "Mean of Daily Avg. and Maximal Prices By Day of The Week",
       title = "No surge price on the weekend")

ggsave(bg = "white", filename = here("figures", "price_by_day_all_hours.png"))

# We can see that there's never surge pricing on the weekend - therefore for the
# rest of the EDA we will focus on weekdays

fast_lane_weekdays <- fast_lane_clean %>% 
  filter(sabbatical == 0) 

# Look at typical day evolution of price

fast_lane_weekdays %>% 
  group_by(time_of_day) %>% 
  summarise(Price = mean(price, na.rm = T)) %>% 
  ggplot(aes(x = time_of_day, y = Price))+
  geom_line(color = "navyblue", size = 1) +
  theme_minimal() +
  scale_x_time(breaks = hms(7200 *0:12), labels = paste(2*0:12,"00",sep=":")) +
  scale_y_continuous(breaks = 10*0:8, expand = c(0,NA))+
  labs(x = element_blank(), title = "Surge price - mainly in morning rush hour", 
       subtitle = "Avg. price by time of day" )

ggsave(bg = "white", filename = here("figures", "price_by_time_of_day.png"))

  
# We can see that most of the variance in prices occurs in the morning rush hour.
# Therefore we limit the dataset to 6-11 am. Add weather data as well

temp_data <- fread(here("datasets", "temp_dat_220824.csv"))
summary(temp_data)
temp_data<-temp_data[,-1]
names(temp_data) <- c('date', 'time', 'temp')
temp_data <-temp_data %>%
  mutate(date = dmy(date))
class(temp_data$date)


humidity <- fread(here("datasets", "humid_dat_220824.csv"))
humidity <- humidity[,-1]
names(humidity) <- c('date', 'time', 'humidity')
humidity <- humidity %>% 
  mutate(date = dmy(date))

weather <- left_join(temp_data,humidity) %>% 
  mutate(all_time = as.POSIXct(paste(date," ", time), format="%Y-%m-%d %H:%M")) %>% 
  select(!c(date, time))

# merge the data and limit to rush hour, fill NAs in weather vars with linear interpolation

fast_lane_rush <- fast_lane_weekdays %>% 
  filter(hour >= 6 & hour < 11) %>% 
  mutate(ten_min = round_date(time, "10 mins")) %>% 
  left_join(weather, by = c("ten_min" =  "all_time")) %>%
  select(-ten_min) %>% 
  arrange(time) %>% 
  mutate(across(c(temp, humidity), na.approx))

## Explore the final dataset ----

### Prices and time variables ----

# Distribution of prices 
ggplot(fast_lane_rush  , aes(x=price)) + 
  geom_histogram(fill = "navyblue")+
  theme_minimal()+
  scale_y_continuous(expand = c(0,NA))+
  scale_x_continuous(expand = c(0,NA), breaks = 10*0:11)+
  labs(y = "Count", x = "Price", title = "Price distribution")
  
ggsave(bg = "white", filename = here("figures", "price_hist_rush.png"))

# Check distribution of surge prices over all and by time of day
fast_lane_rush %>% 
  na.omit() %>% 
  pivot_longer(starts_with("surge"), names_to = "type",
               values_to = "surge", names_prefix = "surge_",
               names_transform = list(type = str_to_title)) %>% 
  group_by(type, surge) %>% 
  summarise(prop = n()/nrow(fast_lane_rush %>% na.omit)) %>% 
  ggplot(aes(x = factor(surge, labels = c("Base Price", "Surge Price")),fill = type, y = prop ))+
  geom_bar(position = "dodge", stat = "identity")+
  scale_fill_manual(values = c("navyblue", "darkred"), name = "")+
  geom_text(aes(label = percent(prop, accuracy = 0.1)), 
            vjust = - 1, position = position_dodge(.9) ) +
  scale_y_continuous(expand = expansion(mult = c(0,0.1)), breaks = 0.1*0:6,
                     labels = percent(0.1*0:6, accuracy = 1)) +
  theme_minimal() + 
  labs(x = element_blank(), y = element_blank(),
       title = "Surge Price - About Half of The Time During The Morning",
       subtitle = "Any indicated price passed 7 nis during 5 mins. All indicates price stayed over 7 nis through the whole 5 mins.")

ggsave(bg = "white", filename = here("figures", "prop_surge_by_calculation_type.png"))

fast_lane_rush %>% 
  group_by(time_of_day) %>% 
  summarise(surge_all = mean(surge_all, na.rm = T),
            surge_any = mean(surge_any, na.rm = T)) %>% 
  pivot_longer(starts_with("surge"), names_to = "type", 
               names_prefix = "surge_", values_to = "Surge",
               names_transform = list(type = str_to_title) ) %>% 
  ggplot(aes(x = time_of_day, y = Surge, color = type))+
  geom_line( size = 1) +
  scale_color_manual(values = c("navyblue", "darkred"), name = "")+
  theme_minimal() +
  scale_x_time(breaks = hms(3600 *0:12), labels = paste(0:12,"00",sep=":")) +
  scale_y_continuous(breaks = 0.1*0:10, expand = expansion(mult = c(0,0.05)),
                     labels = percent(0.1*0:10, accuracy = 1))+
  labs(x = element_blank(), title = "Percent of Days With Surge Price by Time" )
ggsave(bg = "white", filename = here("figures", "prop_surge_by_calculation_type_and_time.png"))

# Now only use any:
fast_lane_rush %>% 
  na.omit() %>% 
  group_by(surge_any) %>% 
  summarise(prop = n()/nrow(fast_lane_rush %>% na.omit)) %>% 
  ggplot(aes(x = factor(surge_any, labels = c("Base Price", "Surge Price")), y = prop ))+
  geom_bar(position = "dodge", stat = "identity", fill = "navyblue")+
  geom_text(aes(label = percent(prop, accuracy = 0.1)), 
            vjust = - 1, position = position_dodge(.9) ) +
  scale_y_continuous(expand = expansion(mult = c(0,0.1)), breaks = 0.1*0:6,
                     labels = percent(0.1*0:6, accuracy = 1)) +
  theme_minimal() + 
  labs(x = element_blank(), y = element_blank(),
       title = "Surge Price - Approx. Two Thirds Of The Time During The Morning",
       subtitle = "Proportion of 5-minutes preiods during which there was a surge price")
ggsave(bg = "white", filename = here("figures", "prop_surge_any.png"))

# Now by day:
fast_lane_rush %>% 
  na.omit() %>% 
  group_by(weekday) %>% 
  mutate(n_day = n()) %>% 
  group_by(weekday, surge_any) %>% 
  summarise(prop = n()/n_day) %>% 
  ggplot(aes(x = factor(weekday, labels = weekdays_names[1:5]), fill = factor(surge_any, labels = c("Base Price", "Surge Price")), y = prop ))+
  geom_bar(position = "dodge", stat = "identity")+
  geom_text(aes(label = percent(prop, accuracy = 0.1)), 
            vjust = - 1, position = position_dodge(.9) ) +
  scale_fill_manual(values = c("navyblue", "darkred"), name = "")+
  scale_y_continuous(expand = expansion(mult = c(0,0.1)), breaks = 0.1*0:6,
                     labels = percent(0.1*0:6, accuracy = 1)) +
  theme_minimal() + 
  labs(x = element_blank(), y = element_blank(),
       title = "Surge Price By Day Of The Week",
       subtitle = "Proportion of 5-minutes preiods during which there was a surge price")
ggsave(bg = "white", filename = here("figures", "prop_surge_any_by_day.png"))

# Reproduce price by weekday for the final data
fast_lane_rush %>%
  na.omit() %>% 
  group_by(date) %>% 
  summarise(mean_price = mean(price, na.rm = T),
            max_price = max(price, na.rm = T),
            weekday = wday(date, label = T)) %>%
  distinct() %>% 
  ungroup() %>% 
  group_by(weekday) %>% 
  summarise(Avg_Price = mean(mean_price, na.rm = T),
            Max_Price = mean(max_price, na.rm = T)) %>%
  pivot_longer(ends_with("price"), names_to = "type", values_to = "Price") %>%
  mutate(type = str_replace(type, "_", ". ")) %>% 
  ggplot(aes(x = factor(weekday), y = Price)) +
  geom_bar(stat = "identity", fill = "navy")+
  theme_minimal() + 
  facet_wrap(~type, scales = "free_y") +
  scale_y_continuous(expand = expansion(mult = c(0,0.1))) +
  geom_text(aes(label = round(Price, 1)), vjust = - 1 ) +
  theme(strip.background = element_blank(), 
        strip.text.x = element_text(face = "bold", size = 12)) +
  labs(x = element_blank(), title = "Mean of Daily Avg. and Maximal Prices Durind Rush Hours")

ggsave(bg = "white", filename = here("figures", "price_by_day_rush_hours.png"))

# Perhaps the differences between the days is related to an imbalance in the 
# number of school break days by weekday
fast_lane_rush %>% 
  na.omit() %>% 
  group_by(weekday, school_sabbatical) %>% 
  summarise(n()) %>%
  pivot_wider(names_from = "school_sabbatical", values_from = "n()", names_prefix = "sab_") %>% 
  mutate(hofesh_percent = percent(sab_1/(sab_0+sab_1)))

fast_lane_rush %>%
  na.omit() %>% 
  group_by(date, elementary_big_hofesh) %>% 
  summarise(mean_price = mean(price, na.rm = T),
            max_price = max(price, na.rm = T),
            weekday = wday(date, label = T)) %>%
  distinct() %>% 
  ungroup() %>% 
  group_by(weekday, elementary_big_hofesh) %>% 
  summarise(Avg_Price = mean(mean_price, na.rm = T),
            Max_Price = mean(max_price, na.rm = T)) %>%
  pivot_longer(ends_with("price"), names_to = "type", values_to = "Price") %>%
  mutate(type = str_replace(type, "_", ". ")) %>% 
  ggplot(aes(x = factor(weekday), y = Price)) +
  geom_bar(stat = "identity", fill = "navy")+
  theme_minimal() + 
  facet_grid(type~factor(elementary_big_hofesh, labels = c("School Days", "School Breaks")), scales = "free") +
  scale_y_continuous(expand = expansion(mult = c(0,0.1))) +
  geom_text(aes(label = round(Price, 1)), vjust = - 0.5 ) +
  theme(strip.background = element_blank(), 
        strip.text.x = element_text(face = "bold", size = 12),
        strip.text.y = element_text(face = "bold", size = 12)) +
  labs(x = element_blank(), title = "Mean of Daily Avg. and Maximal Prices Durind Rush Hours")

ggsave(bg = "white", filename = here("figures", "price_by_day_and_school_rush_hours.png"))

# check prices by day of the week and hour

fast_lane_rush %>% 
  group_by(weekday, hour) %>% 
  summarise(price = mean(price, na.rm = T)) %>% 
  ggplot(aes(x = factor(hour, labels = paste0(6:10, ":00")),
             fill = factor(weekday, labels = weekdays_names[1:5]), y = price)) +
  geom_bar(stat = "identity", position = "dodge")+
  scale_y_continuous(limits = c(0,95), breaks = 20*0:5)+
  labs(y = "Price", x = element_blank(),
       title = "Avg. Price By Day of the Week and Hour",
       fill = element_blank())+
  theme_minimal()
ggsave(bg = "white", filename = here("figures", "price_by_day_and_hour_hours_on_axis.png"))

fast_lane_rush %>% 
  group_by(weekday, hour) %>% 
  summarise(price = mean(price, na.rm = T)) %>% 
  ggplot(aes(x = factor(weekday, labels = weekdays_names[1:5]),
             fill = factor(hour, labels = paste0(6:10, ":00")), y = price)) +
  geom_bar(stat = "identity", position = "dodge")+
  scale_y_continuous(limits = c(0,95), breaks = 20*0:5)+
  labs(y = "Price", x = element_blank(),
       title = "Avg. Price By Day of the Week and Hour",
       fill = element_blank())+
  theme_minimal()
ggsave(bg = "white", filename = here("figures", "price_by_day_and_hour_days_on_axis.png"))

fast_lane_rush %>% 
  mutate(elementary_big_hofesh =
           factor(elementary_big_hofesh,
                  levels = c(0,1), labels = c("School Days", "School Breaks"))) %>% 
  group_by(weekday, hour, elementary_big_hofesh) %>% 
  summarise(price = mean(price, na.rm = T)) %>% 
  ggplot(aes(x = factor(weekday, labels = weekdays_names[1:5]),
             fill = factor(hour, labels = paste0(6:10, ":00")), y = price)) +
  geom_bar(stat = "identity", position = "dodge")+
  scale_y_continuous(limits = c(0,95), breaks = 20*0:5)+
  labs(y = "Price", x = element_blank(),
       title = "Avg. Price By Day of the Week and Hour",
       fill = element_blank())+
  theme_minimal()+
  facet_grid(~ elementary_big_hofesh) 
ggsave(bg = "white", filename = here("figures", "price_by_day_school_and_hour_days_on_axis.png"))

# Check trends in price over time (in general and by hour)
fast_lane_rush %>% 
  group_by(date, hour) %>% 
  summarise(max_price = max(price, na.rm=T),
            price = mean(price, na.rm = T)) %>% 
  mutate(hour = hms(3600*hour)) %>% 
  ggplot(aes(x = date, y = price))+
  facet_wrap(~factor(hour))+
  geom_point()+
  geom_line() +
  geom_smooth(method= "lm", se = F) +
  scale_y_continuous(breaks = 20 * 0:6, limits = c(0,110), expand = expansion(mult = c(0,0.1)))+
  theme_minimal() +
  labs(title = "Avg. Prices By Date and Time", x = "Date", y = "Avg. Price")
ggsave(bg = "white", filename = here("figures", "price_by_date_and_hour.png"))

fast_lane_rush %>% 
  na.omit() %>% 
  group_by(date) %>% 
  summarise(max_price = max(price, na.rm=T),
            price = mean(price, na.rm = T)) %>%
  ggplot(aes(x = date, y = price))+
  geom_point()+
  geom_line() +
  geom_smooth(method= "lm", se = F) +
  scale_y_continuous(breaks = 20 * 0:6, limits = c(0,60), expand = expansion(mult = c(0,0.1)))+
  theme_minimal()+
  labs(title = "Avg. Prices By Date", x = "Date", y = "Avg. Price")
ggsave(bg = "white", filename = here("figures", "price_by_date.png"))

# Prices by time - school days and school breaks
fast_lane_rush %>% 
  group_by( time_of_day, school_sabbatical) %>%
  summarise(price = mean(price, na.rm = T)) %>% 
  ggplot(aes(x = time_of_day, y = price, color = 
               factor(school_sabbatical, labels = c("School Days", "School Breaks"))))+
  geom_line(size = 1) +
  theme_minimal() + 
  scale_x_time(breaks = hms(1800 *12:25), labels = paste(rep(6:12, each = 2),rep(c("00", "30"),6),sep=":")) +
  scale_color_manual(values = c("navyblue", "darkred"), name = "") +
  scale_y_continuous(limits = c(0,90), breaks = 10*0:9, expand = expansion(mult = c(0,0.025)))+
  labs(x = element_blank(), y = "Price", title = "Prices are higher during school days",
       subtitle = "Main effect is at 7-9 AM")
ggsave(bg = "white", filename = here("figures", "price_time_and_school.png"))

### Prices and weather vars ----
# relationship between temperature and price
fast_lane_rush %>% 
  ggplot(aes(x = temp, y = price)) +
  geom_point() + 
  geom_smooth(method= "lm", se = F) +
  theme_minimal()
ggsave(bg = "white", here("figures", "price_temp_scatter_and_lm.png"))
# separate by hour

fast_lane_rush %>% 
  ggplot(aes(x = temp, y = price)) +
  facet_wrap(~factor(hour, labels = paste0(6:10, ":00")))+
  geom_point() + 
  geom_smooth(method= "lm", se = F) +
  theme_minimal()
ggsave(bg = "white", here("figures", "price_temp_scatter_and_lm_by_hour.png"))

# separate by school sabbatical
fast_lane_rush %>% 
  ggplot(aes(x = temp, y = price)) +
  facet_wrap(~factor(school_sabbatical, labels = c("School Days", "School Breaks")))+
  geom_point() + 
  geom_smooth(method= "lm", se = F) +
  theme_minimal()
ggsave(bg = "white", here("figures", "price_temp_scatter_and_lm_by_school.png"))

# separate by both
fast_lane_rush %>% 
  ggplot(aes(x = temp, y = price)) +
  facet_grid(factor(hour, labels = paste0(6:10, ":00"))~factor(school_sabbatical, labels = c("School Days", "School Breaks")))+
  geom_point() + 
  geom_smooth(method= "lm", se = F) +
  theme_minimal()
ggsave(bg = "white", here("figures", "price_temp_scatter_and_lm_by_school_and_hour.png"))

# relationship between humid and price
fast_lane_rush %>% 
  ggplot(aes(x = humidity, y = price)) +
  geom_point() + 
  geom_smooth(method= "lm", se = F) +
  theme_minimal()
ggsave(bg = "white", here("figures", "price_humid_scatter_and_lm.png"))
# separate by hour

fast_lane_rush %>% 
  ggplot(aes(x = humidity, y = price)) +
  facet_wrap(~factor(hour, labels = paste0(6:10, ":00")))+
  geom_point() + 
  geom_smooth(method= "lm", se = F) +
  theme_minimal()
ggsave(bg = "white", here("figures", "price_humid_scatter_and_lm_by_hour.png"))

# separate by school sabbatical
fast_lane_rush %>% 
  ggplot(aes(x = humidity, y = price)) +
  facet_wrap(~factor(school_sabbatical, labels = c("School Days", "School Breaks")))+
  geom_point() + 
  geom_smooth(method= "lm", se = F) +
  theme_minimal()
ggsave(bg = "white", here("figures", "price_humid_scatter_and_lm_by_school.png"))

# separate by both
fast_lane_rush %>% 
  ggplot(aes(x = humidity, y = price)) +
  facet_grid(factor(hour, labels = paste0(6:10, ":00"))~factor(school_sabbatical, labels = c("School Days", "School Breaks")))+
  geom_point() + 
  geom_smooth(method= "lm", se = F) +
  theme_minimal()
ggsave(bg = "white", here("figures", "price_humid_scatter_and_lm_by_school_and_hour.png"))

## Try to identify outlier dates ----

# compare by weekday

for (i in 1:5){
  assign(paste0("outliers_",weekdays_names[i]),
         fast_lane_rush %>% 
    filter(weekday == i ) %>% 
    ggplot(aes(x = time_of_day, y = price))+ 
    geom_line()+
    scale_y_continuous(limits = c(0,121), breaks =  20*0:6) +
    facet_wrap(~ date, scales = "free")+
    theme_minimal()+
    scale_x_time(breaks = hms(3600 *6:12), labels = paste(6:12, rep("00",6),sep=":")) +
    labs(x = element_blank(), y = "Price", title = paste0("Prices on ", weekdays_names[i]))
    )
}
outliers_Sunday
ggsave(bg = "white", here("figures", "outliers", "outliers_sunday.png"))
outliers_Monday
ggsave(bg = "white", here("figures", "outliers", "outliers_monday.png"))
outliers_Tuesday
ggsave(bg = "white", here("figures", "outliers", "outliers_tuesday.png"))
outliers_Wednesday
ggsave(bg = "white", here("figures", "outliers", "outliers_wednesday.png"))
outliers_Thursday
ggsave(bg = "white", here("figures", "outliers", "outliers_thursday.png"))

# This is a weird day...
fast_lane_rush %>% 
  filter(date == ymd("2022-05-23")) %>% 
  ggplot(aes(x = time, y = price))+ 
  geom_line()+
  scale_y_continuous(limits = c(0,112), breaks =  10*1:11) + 
  theme_classic()


# compare within weeks


for (i in unique(fast_lane_rush$week)){
  assign(paste0("outliers_week_",i),
         fast_lane_rush %>% 
           filter(week == i ) %>% 
           mutate(weekday = wday(date, label = T, abbr = F)) %>% 
           ggplot(aes(x = time_of_day, y = price))+ 
           geom_line()+
           scale_y_continuous(limits = c(0,121), breaks =  20*0:6) +
           facet_wrap(weekday~ date, scales = "free")+
           theme_minimal()+
           scale_x_time(breaks = hms(3600 *6:12), labels = paste(6:12, rep("00",6),sep=":")) +
           labs(x = element_blank(), y = "Price", title = paste0("Prices on Week ", i))
  )
}
outliers_week_20
outliers_week_21
outliers_week_22
outliers_week_23
outliers_week_24
outliers_week_25
outliers_week_26
outliers_week_27
outliers_week_28
outliers_week_29
outliers_week_30
outliers_week_31
outliers_week_32
outliers_week_33
outliers_week_34

# *************************************************************************
# PART II - CASUAL ANALYSIS ----
# *************************************************************************

# We use K-means to try to detect school hours
# We Only use price and school_sabbatical
fast_lane_kmeans <- fast_lane_rush %>% 
  select(price, school_sabbatical) %>% 
  na.omit()%>%
  mutate(across(everything(), scale))  

# save the mean and sd of variables so we can make sense of the centroids later
scaling_param <- fast_lane_rush %>% 
  select(price, school_sabbatical) %>% 
  na.omit() %>% 
  pivot_longer(everything(), values_to = "value", names_to = "var", ) %>% 
  group_by(var) %>% 
  summarise(mean = mean(value), sd = sd(value))

k.max <- 20

# Run K-means and choose number of centroids
wss <- sapply(1:k.max, function(k){
  kmeans(fast_lane_kmeans, k, nstart=50,iter.max = 500 )$tot.withinss})  

ggplot()+aes(x = 1:k.max, y = wss)+
  geom_point()+
  geom_line()+
  labs(x = "Number of clusters K", y = "Total within-clusters sum of squares") +
  theme_classic()
# 4 seems good!

kmeans_4 <- kmeans(fast_lane_kmeans, 4, nstart=50,iter.max = 500)
centers <- kmeans_4$centers
stds <- scaling_param$sd
means <- scaling_param$mean
# Show centers on original scale
centers <- t(t(centers) * stds + means) %>% 
  as.data.frame() 

write.csv(centers %>% mutate(school_sabbatical = round(school_sabbatical,3)), 
          here("kmeans_centers.csv"),
          row.names = F)

# Cluster 1: school days, high price 
# Cluster 2: school breaks, high price 
# Cluster 3: school days, low price 
# Cluster 4: school breaks, low price 

# Add clusters and hours
kmeans_dat_final <- fast_lane_rush %>% 
  select(price, school_sabbatical, time_of_day, date) %>% 
  na.omit()

kmeans_dat_final$cluster <- kmeans_4$cluster

# We can see that times are way less dispersed in clusters 1,2
# That makes sense because clusters 3,4 have times before and after
# peek rush hours. We would take the range containing 80% of times
# that belong to high-price clusters to define peek hours and define
# peek time using this. Then we would run a diff-in-diff regression
# of price on school_break and peek_time and the interaction would
# capture the effect of school drives.

kmeans_dat_final %>%
  group_by(cluster) %>%
  summarise(first_dec = hms(quantile(as.numeric(time_of_day), 0.1)),
            ninth_dec = hms(quantile(as.numeric(time_of_day), 0.9)),
            sd = sd(time_of_day),
            n = n())

# Ranges for clusters 1,2 are very similar therefore we feel quite confident to 
# take the joint range
range_kmeans <- kmeans_dat_final %>%
  filter(cluster < 3) %>% 
  summarise(first_dec = (quantile(as.numeric(time_of_day), 0.1)),
            ninth_dec = (quantile(as.numeric(time_of_day), 0.9))) %>%
  pivot_longer(everything(), names_to = "decile", values_to = "value") %>% 
  pull(value)


# Show times on figure
fast_lane_rush %>% 
  group_by( time_of_day, school_sabbatical) %>%
  summarise(price = mean(price, na.rm = T)) %>% 
  ggplot(aes(x = time_of_day, y = price, color = 
               factor(school_sabbatical, labels = c("School Days", "School Breaks"))))+
  geom_line(size = 1) +
  theme_minimal() + 
  scale_x_time(breaks = hms(1800 *12:25), labels = paste(rep(6:12, each = 2),rep(c("00", "30"),6),sep=":")) +
  scale_color_manual(values = c("navyblue", "darkred"), name = "") +
  scale_y_continuous(limits = c(0,90), breaks = 10*0:9, expand = expansion(mult = c(0,0.025)))+
  labs(x = element_blank(), y = "Price", title = "Prices are higher during school days",
       subtitle = "Dashed lines show peek time selected by Kmeans")+
  geom_vline(xintercept = range_kmeans, size = 1, linetype = "dashed")
ggsave(bg = "white", filename = here("figures", "price_time_and_school_kmeans.png"))
# *************************************************************************
# PART III - COST BENEFIT ANALYSIS ----
# *************************************************************************

# Find minimal time saved at each hour - no weekday separation

fast_lane_CB_1 <- fast_lane_rush %>% 
  filter(hour %in% 7:11 & minute(time) == 0) %>% 
  group_by(time_of_day) %>%
  summarise(min_time_saving = round(0.6 * mean(price, na.rm = T), 1))


# Find minimal time saved at each hour -  weekday separation

# The figure earlier showing prices by days of the week and hour
# demonstrates that there is a lot of variance in price between
# different days of the week. Therefore it might be beneficial to do the 
# cost analysis benefit for each day separately. 


fast_lane_CB_2 <- fast_lane_rush %>% 
  filter(hour %in% 7:11 & minute(time) == 0) %>% 
  mutate(weekday = wday(date, label = T)) %>% 
  group_by(weekday, time_of_day) %>%
  summarise(min_time_saving = round(0.6 * mean(price, na.rm = T), 1)) %>% 
  pivot_wider(names_from = "time_of_day", values_from = "min_time_saving")



# *************************************************************************
# PART IV - PREDICTIONS ----
# *************************************************************************

set.seed(100)

## Prepare validation and training sets ----

### Manipulations common to both train and validation sets ----

master_set <- fast_lane %>% 
  filter(hour >= 6 & hour <= 12) 

#### Add weather predictions ----
# For each 10 minutes predict temp and humid by 4-year avg (2018-2021)
temp_preds <- fread(here("datasets", "predictions_temp.csv")) %>% 
  mutate(all_time = as.POSIXct(paste0("2022-0",month,"-",day," ", time), 
                               format="%Y-%m-%d %H:%M", tz = "UTC")) %>% 
  select(all_time, mean_temp) %>% 
  rename(pred_temp = mean_temp) 
humid_preds <-  fread(here("datasets", "predictions_humidity.csv")) %>% 
  mutate(all_time = as.POSIXct(paste0("2022-0",month,"-",day," ", time), 
                               format="%Y-%m-%d %H:%M", tz = "UTC")) %>% 
  select(all_time, mean_humidity) %>% 
  rename(pred_humid = mean_humidity) 



### Split the data ----

valid_dates <- sample(unique(master_set$date),
                      size = ceiling(0.2 * length(unique(master_set$date))),
                      replace = F)
# In validation set - use past years data to estimate weather variables
valid_set <- master_set %>% 
  filter(date %in% valid_dates) %>% 
  mutate(ten_min = round_date(time, "10 mins")) %>% 
  left_join(temp_preds, by = c("ten_min" =  "all_time")) %>% 
  left_join(humid_preds, by = c("ten_min" =  "all_time")) %>% 
  mutate(date = date(time),
         month = month(date),
         week = week(date),
         weekday = wday(time),
         friday = if_else(weekday == 6, 1, 0),
         saturday = if_else(weekday == 7, 1, 0),
         weekend = pmax(friday, saturday),
         elementary_big_hofesh = if_else(date >= "2022-07-01",1,0),
         highschool_big_hofesh = if_else(date >= "2022-06-21",1,0),
         sabbatical = if_else(weekend == 1 | date == "2022-06-05",1,0),
         school_sabbatical = if_else(
           weekend == 1 | date %in% ymd(c("2022-05-19", "2022-06-05", "2022-06-06"))|
             elementary_big_hofesh == 1, 1,0),
         mins_since_6_am = 60 * (hour - 6) + minute(time) + second(time) / 60,
         hour_fac = factor(hour),
         weekday_fac = factor(weekday * (1 - sabbatical)),
         min_15_fac = factor(mins_since_6_am %/% 15),
         min_20_fac = factor(mins_since_6_am %/% 20),
         min_10_fac = factor(mins_since_6_am %/% 10),
         min_5_fac = factor(mins_since_6_am %/% 5)) %>% 
    rename(temp = pred_temp, humidity = pred_humid) %>% 
  na.omit()


### Three versions of train sets

# Times are not grouped together in the first version
train_set_ungroup <- master_set %>% 
  filter(!date %in% valid_dates)%>% 
  mutate(ten_min = round_date(time, "10 mins")) %>% 
  left_join(weather, by = c("ten_min" =  "all_time")) %>%
  left_join(temp_preds, by = c("ten_min" =  "all_time")) %>% 
  left_join(humid_preds, by = c("ten_min" =  "all_time")) %>% 
  #impute missing temperature
  arrange(time) %>% 
  group_by(date) %>% 
  mutate(across(c(temp, humidity), na.approx)) %>%
  ungroup() %>% 
  mutate(date = date(time),
         month = month(date),
         week = week(date),
         weekday = wday(time),
         friday = if_else(weekday == 6, 1, 0),
         saturday = if_else(weekday == 7, 1, 0),
         weekend = pmax(friday, saturday),
         elementary_big_hofesh = if_else(date >= "2022-07-01",1,0),
         highschool_big_hofesh = if_else(date >= "2022-06-21",1,0),
         sabbatical = if_else(weekend == 1 | date == "2022-06-05",1,0),
         school_sabbatical = if_else(
           weekend == 1 | date %in% ymd(c("2022-05-19", "2022-06-05", "2022-06-06"))|
             elementary_big_hofesh == 1, 1,0),
         mins_since_6_am = 60 * (hour - 6) + minute(time) + second(time) / 60,
         hour_fac = factor(hour),
         weekday_fac = factor(weekday * (1 - sabbatical)),
         min_15_fac = factor(mins_since_6_am %/% 15),
         min_20_fac = factor(mins_since_6_am %/% 20),
         min_10_fac = factor(mins_since_6_am %/% 10),
         min_5_fac = factor(mins_since_6_am %/% 5)) 


# Group times for the rest of the other versions
first_time_train <- min(train_set_ungroup$rounded_time)
last_time_train <- max(train_set_ungroup$rounded_time)

train_set_group <- data.frame(
  rounded_time = seq.POSIXt(first_time_train, last_time_train, by = "5 min")) %>% 
  left_join(train_set_ungroup) %>%
  select(!c(pred_temp, pred_humid)) %>% 
  na.omit() %>% 
  group_by(rounded_time) %>% 
  summarise(price = mean(price),
            surge_any = max(surge),
            surge_all = min(surge),
            max_price_any = max(max_price),
            max_price_all = min(max_price)) %>% 
  rename(time = rounded_time) %>% 
  mutate(ten_min = round_date(time, "10 mins")) %>% 
  left_join(weather, by = c("ten_min" =  "all_time")) %>%
  left_join(temp_preds, by = c("ten_min" =  "all_time")) %>% 
  #impute missing temperature
  arrange(time) %>% 
  mutate(date = date(time),
         month = month(date),
         hour = hour(time),
         week = week(date),
         weekday = wday(time),
         friday = if_else(weekday == 6, 1, 0),
         saturday = if_else(weekday == 7, 1, 0),
         weekend = pmax(friday, saturday),
         elementary_big_hofesh = if_else(date >= "2022-07-01",1,0),
         highschool_big_hofesh = if_else(date >= "2022-06-21",1,0),
         sabbatical = if_else(weekend == 1 | date == "2022-06-05",1,0),
         school_sabbatical = if_else(
           weekend == 1 | date %in% ymd(c("2022-05-19", "2022-06-05", "2022-06-06"))|
             elementary_big_hofesh == 1, 1,0),
         mins_since_6_am = 60 * (hour - 6) + minute(time) + second(time) / 60,
         hour_fac = factor(hour),
         weekday_fac = factor(weekday * (1 - sabbatical)),
         min_15_fac = factor(mins_since_6_am %/% 15),
         min_20_fac = factor(mins_since_6_am %/% 20),
         min_10_fac = factor(mins_since_6_am %/% 10),
         min_5_fac = factor(mins_since_6_am %/% 5)) %>% 
  group_by(date) %>% 
  mutate(across(c(temp, humidity), na.approx)) %>%
  ungroup() 

# In the second version - surge (max_price) is defines as 1 if at any point 
# during the 5 minutes a surge (max) price occurred
train_set_any <- train_set_group %>% 
  rename(surge = surge_any, max_price = max_price_any) %>% 
  select(!ends_with("_all"))

# In the third version - surge (max_price) is defines as 1 if surge (max) price 
# persisted through the whole 5 minutes
train_set_all <- train_set_group %>% 
  rename(surge = surge_all, max_price = max_price_all) %>% 
  select(!ends_with("_any"))

# Option set for for training set

train_op <- c("ungroup", "any", "all")

## Train many models to choose from later ---- 

### Prepare variable lists, interactions and polynomials ----

model_op <-
  c("school_sabbatical + weekday_fac + mins_since_6_am + I(mins_since_6_am^2)",
    "school_sabbatical + weekday_fac + mins_since_6_am + I(mins_since_6_am^2) + I(mins_since_6_am^3)",
    "school_sabbatical*weekday_fac + mins_since_6_am + I(mins_since_6_am^2)",
    "school_sabbatical*weekday_fac + min_15_fac",
    "school_sabbatical*weekday_fac + min_20_fac",
    "weekday_fac + school_sabbatical*poly(mins_since_6_am, degree = 2)",
    "school_sabbatical*weekday_fac + min_15_fac*weekday_fac",
    "school_sabbatical*weekday_fac + min_15_fac*school_sabbatical",
    "school_sabbatical*weekday_fac + mins_since_6_am + I(mins_since_6_am^2) + I(mins_since_6_am^3)",
    "weekday_fac + min_15_fac",
    "weekday_fac * min_15_fac",
    "weekday_fac + min_10_fac",
    "weekday_fac * min_10_fac",
    "school_sabbatical*weekday_fac + mins_since_6_am + I(mins_since_6_am^2) + I(mins_since_6_am^3) + hour_fac",
    "school_sabbatical*weekday_fac + min_15_fac +temp",
    "school_sabbatical*weekday_fac + min_15_fac + humidity",
    "school_sabbatical*weekday_fac +  mins_since_6_am + I(mins_since_6_am^2) + I(mins_since_6_am^3) + hour_fac + humidity",
    "school_sabbatical*weekday_fac + min_15_fac + humidity + temp",
    "school_sabbatical*weekday_fac + min_15_fac + school_sabbatical*humidity",
    "school_sabbatical*weekday_fac + min_20_fac + humidity" ,
    "school_sabbatical*weekday_fac + min_10_fac + humidity",
    "school_sabbatical*weekday_fac + min_5_fac + humidity",
    "school_sabbatical*weekday_fac + min_10_fac",
    "school_sabbatical*weekday_fac + min_5_fac",
    "school_sabbatical*weekday_fac + min_10_fac + humidity + temp",                                                        
    "school_sabbatical*weekday_fac + min_5_fac + humidity + temp",
    "school_sabbatical*weekday_fac + min_10_fac*school_sabbatical",
    "school_sabbatical*weekday_fac + min_5_fac*school_sabbatical",
    "weekday_fac + min_10_fac + temp",                                                                                     
    "weekday_fac + min_5_fac + temp",                                                                                    
    "weekday_fac + min_10_fac + humidity",
    "weekday_fac + min_5_fac + humidity",                                                                                  
    "weekday_fac + min_15_fac + temp",                                                                                     
    "weekday_fac + min_20_fac + temp",                                                                                     
    "weekday_fac + min_15_fac + humidity",                                                                                 
    "weekday_fac + min_20_fac + humidity",                                                                                 
    "weekday_fac + min_10_fac + temp + humidity",                                                                          
    "weekday_fac + min_5_fac + temp + humidity",                                                                           
    "weekday_fac + min_15_fac + temp + humidity",
    "weekday_fac + min_20_fac + temp + humidity",                                                                         
    "school_sabbatical * mins_since_6_am + weekday_fac + hour_fac + I(mins_since_6_am^2) + I(mins_since_6_am^3)",          
    "school_sabbatical * poly(mins_since_6_am, degree = 3) + school_sabbatical * weekday_fac + hour_fac + humidity",
    "school_sabbatical * poly(mins_since_6_am, degree = 2) + school_sabbatical * weekday_fac + hour_fac + humidity",
    "school_sabbatical * poly(mins_since_6_am, degree = 2) + school_sabbatical * weekday_fac + hour_fac")

### Prepare functions running the different types of models ----

#### First option - just predict linearly and correct values out of bound ----
predict_linear <- function(train_dat, test_dat, model_formula){
  model_formula <- as.formula(paste0("price ~ ", model_formula))
  model <- lm(model_formula, data = train_dat)
  test_dat$pred_price <- predict(model, newdata = test_dat)
  test_dat <- test_dat %>% 
    mutate(pred_price = if_else(pred_price > 110 , 110, pred_price),
           pred_price = if_else(pred_price < 7 , 7, pred_price))
  train_dat$pred_price <- predict(model, newdata = train_dat)
  train_dat <- train_dat %>% 
    mutate(pred_price = if_else(pred_price > 110 , 110, pred_price),
           pred_price = if_else(pred_price < 7 , 7, pred_price))
  return(list(train_preds = train_dat,
              test_preds = test_dat,
              model = model))
}

#### Second option - first classify boundaries then predict linearly in the inner part ----

calculate_accuracy <- function(threshold, outcome, prediction){
  # This function calculates accuracy of classification based on the given threshold
  # for predictions from logistic regression
  binary_pred <- as.numeric(prediction > threshold)
  accuracy <- mean(binary_pred == outcome)
  return(accuracy)
}
cv_best_thresh <- function(train_dat, model_formula, k = 5){
  # This function runs k-fold cross validation to determine best
  # threshold for logistic regression classification based on accuracy
  samples <- rep(1:k, ceiling(nrow(train_dat) / k)) [1:nrow(train_dat)]
  samples <- sample(samples, size = length(samples), replace = F)
  train_dat$samples <- samples
  train_dat$preds <- NA
  for(i in 1:k){
    train_dat$preds[train_dat$samples == i] <- predict(
      glm(model_formula, data = train_dat[train_dat$samples != i,], family = "binomial" ),
      type = "response", newdata = train_dat[train_dat$samples == i,])
  }
 thresholds <- seq(0.01, 0.99, by = 0.01) 
 outcome <- model.response(model.frame(model_formula, data = train_dat)) %>% unname()
 accs <- sapply(thresholds, calculate_accuracy, outcome, train_dat$preds)
 return(thresholds[which.max(accs)])

}

predict_boundary_price <- function(train_dat, test_dat, model_formula, boundary){
  # This function takes a training data set, a desired boundary (either "surge"
  # or "max_price") for classification and a model formula, calculates a best
  # threshold for classification based on a 5-fold CV (using the previously 
  # defined cv_best_thresh). Then it predicts whether at certain time the price 
  # would be a boundary price based on this threshold both for the test set. 
  
  model_formula <- as.formula(paste0(boundary, " ~ ", model_formula))
  
  # fit model on all data
  model <- glm(model_formula, data = train_dat, family = "binomial")
  # calculate best threshold
  thresh <- cv_best_thresh(train_dat, model_formula, k = 5)
  test_dat$boundary_pred <- predict(model, newdata = test_dat, type = "response")
  test_dat <- test_dat %>% 
    mutate(boundary_pred = if_else(boundary_pred > thresh , 1, 0))
  train_dat$boundary_pred <- predict(model, newdata = train_dat, type = "response")
  train_dat <- train_dat %>% 
    mutate(boundary_pred = if_else(boundary_pred > thresh , 1, 0))
  return(list(test_preds = test_dat,
              model = model,
              thresh = thresh))
}

predict_price_steps <- function(train_dat, test_dat, model_formula){
  #keep for later
  train_preds <- train_dat
  # First predict surge price
  surge_model <- predict_boundary_price(train_dat, test_dat, model_formula,
                                        boundary = "surge" )
  # Put aside observations in test set where we predict no surge price
  no_surge_test <- surge_model$test_preds %>% 
    filter(boundary_pred == 0) %>% 
    mutate(pred_price = 7) %>% 
    select(!boundary_pred)
  test_dat <- surge_model$test_preds %>% 
    filter(boundary_pred == 1) %>% 
    select(!boundary_pred)
  
  # Now predict max_price
  max_price_model <- predict_boundary_price(train_dat, test_dat, model_formula,
                                            boundary = "max_price" )
  # Put aside observations in test set where we predict max_price
  max_price_test <- max_price_model$test_preds %>% 
    filter(boundary_pred == 1) %>% 
    mutate(pred_price = 110) %>% 
    select(!boundary_pred)
  test_dat <- max_price_model$test_preds %>% 
    filter(boundary_pred == 0) %>% 
    select(!boundary_pred)
  
  # predict the rest linearly
  price_model <- predict_linear(train_dat, test_dat, model_formula)
  
  # combine all test predictions
  test_preds <- bind_rows(price_model$test_preds, no_surge_test, max_price_test)
  
  # Now in order to be able to see how much worse it is in the test set compared to
  # train set - predict for train set
  train_preds$surge_pred <- predict(surge_model$model, newdata = train_preds,
                                          type = "response") 
  train_preds <- train_preds %>% 
    mutate(surge_pred = if_else(surge_pred > surge_model$thresh, 1, 0))
  train_preds_no_surge <- train_preds %>% 
    filter(surge_pred == 0) %>% 
    mutate(pred_price = 7) %>% 
    select(!surge_pred)
  train_preds <- train_preds %>% 
    filter(surge_pred == 1) %>% 
    select(!surge_pred)
  train_preds$max_price_pred <- predict(max_price_model$model, newdata = train_preds,
                                  type = "response")
  train_preds <- train_preds %>% 
    mutate(max_price_pred = if_else(max_price_pred > max_price_model$thresh, 1, 0))
  train_preds_max_price <- train_preds %>% 
    filter(max_price_pred == 1) %>% 
    mutate(pred_price = 110) %>% 
    select(!max_price_pred)
  train_preds <- train_preds %>% 
    filter(max_price_pred == 0) %>% 
    select(!max_price_pred)
  train_preds$pred_price <- predict(price_model$model, newdata = train_preds)
  # combine all train predictions
  train_preds <- bind_rows(train_preds_no_surge, train_preds_max_price, train_preds)
  return(list(train_preds = train_preds, test_preds = test_preds))
  
}

calculate_RMSE <- function(outcome, prediction){
  mse <- mean((outcome - prediction)^2)
  return(sqrt(mse))
}

display_resid_by_time_and_day <- function(dat){
  dat %>%
    mutate(resid = price - pred_price, mins_since_6_am = round(mins_since_6_am)) %>%
    group_by(school_sabbatical, mins_since_6_am, weekday_fac) %>% 
    summarise(resid = mean(resid)) %>% 
    mutate(time = hms(6*60*60 + 60*mins_since_6_am)) %>%
    ggplot(aes(x = time, y = resid, 
               color = factor(school_sabbatical, labels = c("School Days", "School Breaks")))) +
    geom_line() +
    scale_color_manual(values = c("navyblue", "darkred"), name = "") +
    theme_classic()+
    facet_wrap(~weekday_fac)
}
display_resid_by_time <- function(dat){
  dat %>%
    mutate(resid = price - pred_price, mins_since_6_am = round(mins_since_6_am)) %>%
    group_by(school_sabbatical, mins_since_6_am) %>% 
    summarise(resid = mean(resid)) %>% 
    mutate(time = hms(6*60*60 + 60*mins_since_6_am)) %>%
    ggplot(aes(x = time, y = resid,
               color = factor(school_sabbatical, labels = c("School Days", "School Breaks")))) +
    geom_line() +
    scale_color_manual(values = c("navyblue", "darkred"), name = "") +
    theme_classic()
}
display_abs_resid_by_time <- function(dat){
  dat %>%
    mutate(abs_resid = abs(price - pred_price), mins_since_6_am = round(mins_since_6_am)) %>%
    group_by(school_sabbatical, mins_since_6_am) %>% 
    summarise(abs_resid = mean(abs_resid)) %>% 
    mutate(time = hms(6*60*60 + 60*mins_since_6_am)) %>%
    ggplot(aes(x = time, y = abs_resid, 
               color = factor(school_sabbatical, labels = c("School Days", "School Breaks")))) +
    geom_line() +
    scale_color_manual(values = c("navyblue", "darkred"), name = "") +
    theme_classic()
}
pred_op <- c("linear", "steps")


## Test all model*train_set_option*prediction_option ----

config_op_df <- expand_grid(model_op, train_op, pred_op) %>% 
  mutate(train_rmse = NA, validation_rmse = NA)

train_set_list <- list("ungroup" = train_set_ungroup,
                       "any" = train_set_any, "all" = train_set_all)
### Run all configurations and calculate RMSE for both train and validation sets----
# run only once - takes forever!!
if(!file.exists(here("all_models_results.csv"))){

  for(i in 1:nrow(config_op_df)){
    if(config_op_df$pred_op[i] == "linear") {
      pred <- predict_linear(train_dat = train_set_list[[config_op_df$train_op[i]]],
                             test_dat = valid_set,
                             model_formula = config_op_df$model_op[i])
    } else {
      pred <- predict_price_steps(train_dat = train_set_list[[config_op_df$train_op[i]]],
                                  test_dat = valid_set,
                                  model_formula = config_op_df$model_op[i])
    }
    config_op_df$train_rmse[i] <- calculate_RMSE(outcome = pred$train_preds$price,
                                                 prediction = pred$train_preds$pred_price)
    config_op_df$validation_rmse[i] <- calculate_RMSE(outcome = pred$test_preds$price,
                                                      prediction = pred$test_preds$pred_price)
    print(i)
  }
  write_csv(config_op_df, file = here("all_models_results.csv"))

}

### analyze the results a little ----
config_op_df <- read.csv(here("all_models_results.csv")) %>% arrange(validation_rmse)

# most good models don't seem to be overfitted anyway
config_op_df <- config_op_df %>%
  mutate(overfit_per = 100*(validation_rmse - train_rmse)/train_rmse) 

# Look at best models regardless of training set
config_op_df %>% 
  group_by(model_op, pred_op) %>% 
  summarise(validation_rmse = mean(validation_rmse)) %>% 
  arrange(validation_rmse) 

# Step estimation seems to be better by quite a lot
config_op_df %>% 
  group_by(train_op, pred_op) %>% 
  summarise(validation_rmse = mean(validation_rmse)) %>% 
  arrange(validation_rmse) 

# best model
best_model <- config_op_df %>% 
  filter(validation_rmse == min(validation_rmse))

# look at best model without school_sabbatical var in case
# Yaffa Ben-David makes troubles
best_model_no_school <- config_op_df %>% 
  filter(str_detect(model_op, "school_sabbatical", negate = T)) %>% 
  filter(validation_rmse == min(validation_rmse))

#### re-run best models to look a little closer into their results ----

# for best model
best_model_pred <- predict_price_steps(train_dat = train_set_all ,
                                       test_dat = valid_set,
                                       model_formula = best_model[1,"model_op"])

display_resid_by_time(best_model_pred$test_preds)
ggsave(here("figures", "best_model_mean_resid_by_hour.png"))
display_abs_resid_by_time(best_model_pred$test_preds)
ggsave(here("figures", "best_model_mean_abs_resid_by_hour.png"))
display_resid_by_time_and_day(best_model_pred$test_preds)
ggsave(here("figures", "best_model_mean_resid_by_hour_and_weekday.png"))
best_model_pred$test_preds %>% 
  mutate(time = hms(6*60*60 + 60*mins_since_6_am)) %>%
  pivot_longer(cols = c(price, pred_price), names_to = "type", values_to = "price") %>% 
  ggplot(aes(x = time, y = price, color = type) )+
  geom_line() +
  theme_classic()+
  scale_color_manual(values = c("navyblue", "darkred"), name = "") +
  facet_wrap(~date)
ggsave(here("figures", "best_model_prediction_and_price_by_day.png"))

# for best non-school model

best_model_no_school_pred <- predict_price_steps(train_dat = train_set_all ,
                                       test_dat = valid_set,
                                       model_formula = best_model_no_school[1,"model_op"])

display_resid_by_time(best_model_no_school_pred$test_preds)
ggsave(here("figures", "no_school_model_mean_resid_by_hour.png"))
display_abs_resid_by_time(best_model_no_school_pred$test_preds)
ggsave(here("figures", "no_school_model_mean_abs_resid_by_hour.png"))
display_resid_by_time_and_day(best_model_no_school_pred$test_preds)
ggsave(here("figures", "no_school_model_mean_resid_by_hour_and_weekday.png"))

best_model_no_school_pred$test_preds %>% 
  mutate(time = hms(6*60*60 + 60*mins_since_6_am)) %>%
  pivot_longer(cols = c(price, pred_price), names_to = "type", values_to = "price") %>% 
  ggplot(aes(x = time, y = price, color = type) )+
  geom_line() +
  theme_classic()+
  scale_color_manual(values = c("navyblue", "darkred"), name = "") +
  facet_wrap(~date)
ggsave(here("figures", "no_school_model_prediction_and_price_by_day.png"))

## Retrain the selected model on the whole dataset and predict ----

### Prepare final datasets ----

#### New training set with all data (real values for all vars) ----

first_time <- min(master_set$rounded_time)
last_time <- max(master_set$rounded_time)

master_set_all <-  data.frame(
  rounded_time = seq.POSIXt(first_time_train, last_time_train, by = "5 min")) %>% 
  left_join(master_set) %>%
  na.omit() %>% 
  group_by(rounded_time) %>% 
  summarise(price = mean(price),
            surge = min(surge),
            max_price = min(max_price)) %>% 
  rename(time = rounded_time) %>% 
  mutate(ten_min = round_date(time, "10 mins")) %>% 
  left_join(weather, by = c("ten_min" =  "all_time")) %>%
  mutate(date = date(time),
         month = month(date),
         hour = hour(time),
         week = week(date),
         weekday = wday(time),
         friday = if_else(weekday == 6, 1, 0),
         saturday = if_else(weekday == 7, 1, 0),
         weekend = pmax(friday, saturday),
         elementary_big_hofesh = if_else(date >= "2022-07-01",1,0),
         highschool_big_hofesh = if_else(date >= "2022-06-21",1,0),
         sabbatical = if_else(weekend == 1 | date == "2022-06-05",1,0),
         school_sabbatical = if_else(
           weekend == 1 | date %in% ymd(c("2022-05-19", "2022-06-05", "2022-06-06"))|
             elementary_big_hofesh == 1, 1,0),
         mins_since_6_am = 60 * (hour - 6) + minute(time) + second(time) / 60,
         hour_fac = factor(hour),
         weekday_fac = factor(weekday * (1 - sabbatical)),
         min_15_fac = factor(mins_since_6_am %/% 15),
         min_20_fac = factor(mins_since_6_am %/% 20),
         min_10_fac = factor(mins_since_6_am %/% 10),
         min_5_fac = factor(mins_since_6_am %/% 5)) %>% 
  #impute missing temperature
  arrange(time) %>% 
  group_by(date) %>% 
  mutate(across(c(temp, humidity), na.approx)) %>%
  ungroup() 

#### Dataset with future times ----


first_time_pred <- as.POSIXct("2022-09-01 00:00:00", tz = "UTC")
last_time_pred <- as.POSIXct("2022-09-14 00:00:00", tz = "UTC")

pred_set <- data.frame(
  time = seq.POSIXt(first_time_pred, last_time_pred, by = "5 min")) %>% 
  mutate(ten_min = floor_date(time, "10 mins")) %>% 
  left_join(temp_preds, by = c("ten_min" =  "all_time")) %>% 
  left_join(humid_preds, by = c("ten_min" =  "all_time")) %>% 
  mutate(date = date(time),
         month = month(date),
         hour = hour(time),
         week = week(date),
         weekday = wday(time),
         friday = if_else(weekday == 6, 1, 0),
         saturday = if_else(weekday == 7, 1, 0),
         weekend = pmax(friday, saturday),
         elementary_big_hofesh = if_else(date >= "2022-07-01" & date <= "2022-08-31" ,1,0),
         highschool_big_hofesh = if_else(date >= "2022-06-21" & date <= "2022-08-31" ,1,0),
         sabbatical = if_else(weekend == 1 | date == "2022-06-05",1,0),
         school_sabbatical = if_else(
           weekend == 1 | date %in% ymd(c("2022-05-19", "2022-06-05", "2022-06-06"))|
             elementary_big_hofesh == 1, 1,0),
         mins_since_6_am = 60 * (hour - 6) + minute(time) + second(time) / 60,
         hour_fac = factor(hour),
         weekday_fac = factor(weekday * (1 - sabbatical)),
         min_15_fac = factor(mins_since_6_am %/% 15),
         min_20_fac = factor(mins_since_6_am %/% 20),
         min_10_fac = factor(mins_since_6_am %/% 10),
         min_5_fac = factor(mins_since_6_am %/% 5)) %>% 
  rename(temp = pred_temp, humidity = pred_humid) %>% 
  filter(hour >= 6 & hour <= 12)

### Train model and predict ----

model_with_school <- predict_price_steps(master_set_all, pred_set, best_model[1,1])

model_no_school <- predict_price_steps(master_set_all, pred_set, best_model_no_school[1,1])

# Just for curiosity
calculate_RMSE(model_with_school$train_preds$price, model_with_school$train_preds$pred_price )
display_resid_by_time(model_with_school$train_preds)
display_abs_resid_by_time(model_with_school$train_preds)
display_resid_by_time_and_day(model_with_school$train_preds)
model_with_school$train_preds %>% 
  mutate(time = hms(6*60*60 + 60*mins_since_6_am)) %>%
  pivot_longer(cols = c(price, pred_price), names_to = "type", values_to = "price") %>% 
  ggplot(aes(x = time, y = price, color = type) )+
  geom_line() +
  theme_classic()+
  scale_color_manual(values = c("navyblue", "darkred"), name = "") +
  facet_wrap(~date)

#### Predictions ----
preds_with_school <- model_with_school$test_preds %>% 
  select(time, pred_price) %>% 
  arrange(time) %>% 
  rename(partition_datetime = time, mean_price = pred_price)
write.csv(preds_with_school, here("EinBDW22_A3_Muhammad_qais_Ori_Shoham.csv"), row.names = F)

preds_no_school <- model_no_school$test_preds %>% 
  select(time, pred_price) %>% 
  arrange(time) %>% 
  rename(partition_datetime = time, mean_price = pred_price)
write.csv(preds_no_school, here("EinBDW22_A3_Muhammad_qais_Ori_Shoham_2.csv"), row.names = F)
