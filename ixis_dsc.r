# Looking at a retail website's GA data
# author: Ben Armstrong (b.lee.armstrong@gmail.com)

library(tidyverse)
library(lubridate)
library(summarytools)
library(openxlsx)
library(Cairo)

# add Cairo for nicer plots
CairoWin()

# load data

DataAnalyst_Ecom_data_addsToCart = read_csv("C:/Users/ben/Desktop/ixis/DataAnalyst_Ecom_data_addsToCart.csv")
DataAnalyst_Ecom_data_sessionCounts = read_csv("C:/Users/ben/Desktop/ixis/DataAnalyst_Ecom_data_sessionCounts.csv")

# take a quick peek at the data

head(DataAnalyst_Ecom_data_addsToCart)
head(DataAnalyst_Ecom_data_sessionCounts)

# do some descriptive stats to check for missing values and whatnot

dfSummary(DataAnalyst_Ecom_data_sessionCounts)

descr(DataAnalyst_Ecom_data_sessionCounts
      , stats = 'common')

# are there any weird cases?

validation_0 = DataAnalyst_Ecom_data_sessionCounts %>% 
  filter((sessions == 0 & transactions > 0) | (sessions == 0 & QTY > 0))

# there are some cases where there are no sessions but transactions happened
# this is probably bad data and these records will be removed from further analysis

DataAnalyst_Ecom_data_sessionCounts = DataAnalyst_Ecom_data_sessionCounts %>% 
  filter(!((sessions == 0 & transactions > 0) | (sessions == 0 & QTY > 0)))

# what are the most popular browsers, or browser device combos

popular_browsers = DataAnalyst_Ecom_data_sessionCounts %>% 
  count(dim_browser, sort = TRUE)

head(popular_browsers)

popular_browsers_devices = DataAnalyst_Ecom_data_sessionCounts %>% 
  count(dim_browser, dim_deviceCategory, sort = TRUE)

head(popular_browsers_devices)

# and how do these relate to overall sessions, transactions, and quantities?

sessions_by_browser = DataAnalyst_Ecom_data_sessionCounts %>% 
  group_by(dim_browser) %>% 
  summarise(sum_sessions = sum(sessions)
            , mean_sessions = mean(sessions)
            , sum_transactions = sum(transactions)
            , mean_transactions = mean(transactions)
            , sum_QTY = sum(QTY)
            , mean_QTY = mean(QTY)) %>% 
  arrange(desc(sum_sessions))

head(sessions_by_browser)

sessions_by_browser_device = DataAnalyst_Ecom_data_sessionCounts %>% 
  group_by(dim_browser, dim_deviceCategory) %>% 
  summarise(sum_sessions = sum(sessions)
            , mean_sessions = mean(sessions)
            , sum_transactions = sum(transactions)
            , mean_transactions = mean(transactions)
            , sum_QTY = sum(QTY)
            , mean_QTY = mean(QTY)
            , mean_ECR = sum(transactions) / sum(sessions)) %>% 
  arrange(desc(mean_sessions)) 

head(sessions_by_browser_device)

# export this data for ease of presentation

write_csv(sessions_by_browser_device, 'C:/Users/ben/Desktop/ixis/session_browser_summary.csv')

# safari + mobile has the most sessions, but chrome + desktop had the most transactions/quantities
# similarly, safari + desktop has the second most transactions
# chrome + desktop has twice the ECR of Safari + mobile
# are desktops the most active?

device_check = DataAnalyst_Ecom_data_sessionCounts %>% 
  group_by(dim_deviceCategory) %>% 
  summarise(sum_sessions = sum(sessions)
            , mean_sessions = mean(sessions)
            , sum_transactions = sum(transactions)
            , mean_transactions = mean(transactions)
            , sum_QTY = sum(QTY)
            , mean_QTY = mean(QTY)
            , mean_ECR = sum(transactions) / sum(sessions)) %>% 
  arrange(desc(sum_sessions))

head(device_check)

# desktop has a significantly higher ECR

# how has device usage changed over time?

device_check_plot_data = DataAnalyst_Ecom_data_sessionCounts %>% 
  mutate(dim_date_object = as_date(dim_date, format="%m/%d/%y")
         , dim_year = year(dim_date_object)
         , dim_month = month(dim_date_object)
         , dim_date = make_date(dim_year, dim_month, 1)
         , tally = 1)

ggplot(data = device_check_plot_data, aes(x = dim_date, y = tally, fill = dim_deviceCategory)) +
  geom_bar(stat = 'identity'
           , position = 'fill')

# type of device has been relatively steady

# begin preparation of month / device aggregation dataset

summary_0 = DataAnalyst_Ecom_data_sessionCounts %>% 
  mutate(dim_date_object = as_date(dim_date, format="%m/%d/%y")
         , ECR = transactions/sessions
         , dim_year = year(dim_date_object)
         , dim_month = month(dim_date_object)) %>% # add new columns: convert string to date, extract year and month, and create ECR
  group_by(dim_deviceCategory
           , dim_year
           , dim_month) %>% 
  summarize(sum_sessions = sum(sessions)
            , mean_sessions = mean(sessions)
            , max_sessions = max(sessions)
            , min_sessions = min(sessions)
            , sum_transactions = sum(transactions)
            , mean_transactions = mean(transactions)
            , max_transactions = max(transactions)
            , min_transactions = min(transactions)
            , sum_QTY = sum(QTY)
            , mean_QTY = mean(QTY)
            , max_QTY = max(QTY)
            , min_QTY = min(QTY)
            , mean_ECR = mean(ECR, na.rm=TRUE)
            , max_ECR = max(ECR, na.rm=TRUE)
            , min_ECR = min(ECR, na.rm=TRUE)) %>% 
  arrange(dim_year
          , dim_month
          , dim_deviceCategory)

head(summary_0)

# to get the next table, we'll combine the previously created table with
# the addsToCart data, after doing some modifications to them

summary_1a = summary_0 %>% 
  group_by(dim_deviceCategory) %>% 
  mutate(sum_sessions_prior = lag(sum_sessions)
         , relative_diff_sum_sessions = sum_sessions - sum_sessions_prior
         , absolute_diff_sum_sessions = abs(relative_diff_sum_sessions)
         , mean_sessions_prior = lag(mean_sessions)
         , relative_diff_mean_sessions = mean_sessions - mean_sessions_prior
         , absolute_diff_mean_sessions = abs(relative_diff_mean_sessions)
         , max_sessions_prior = lag(max_sessions)
         , relative_diff_max_sessions_prior = max_sessions - max_sessions_prior
         , absolute_diff_max_sessions_prior = abs(relative_diff_max_sessions_prior)
         , min_sessions_prior = lag(min_sessions)
         , relative_diff_min_sessions_prior = min_sessions - min_sessions_prior
         , absolute_diff_min_sessions_prior = abs(relative_diff_min_sessions_prior)
         , sum_transactions_prior = lag(sum_transactions)
         , relative_diff_sum_transactions = sum_transactions - sum_transactions_prior
         , absolute_diff_sum_transactions = abs(relative_diff_sum_transactions)
         , mean_transactions_prior = lag(mean_transactions)
         , relative_diff_mean_transactions = mean_transactions - mean_transactions_prior
         , absolute_diff_mean_transactions = abs(relative_diff_mean_transactions)
         , sum_transactions_prior = lag(sum_transactions)
         , relative_diff_sum_transactions = sum_transactions - sum_transactions_prior
         , absolute_diff_mean_transactions = abs(relative_diff_mean_transactions)
         , max_transactions_prior = lag(max_transactions)
         , relative_diff_max_transactions_prior = max_transactions - max_transactions_prior
         , absolute_diff_max_transactions_prior = abs(relative_diff_max_transactions_prior)
         , min_transactions_prior = lag(min_transactions)
         , relative_diff_min_transactions_prior = min_transactions - min_transactions_prior
         , absolute_diff_min_transactions_prior = abs(relative_diff_min_transactions_prior)
         , sum_QTY_prior = lag(sum_QTY)
         , relative_diff_sum_QTY = sum_QTY - sum_QTY_prior
         , absolute_diff_sum_QTY = abs(relative_diff_sum_QTY)
         , mean_QTY_prior = lag(mean_QTY)
         , relative_diff_mean_QTY = mean_QTY - mean_QTY_prior
         , absolute_diff_mean_QTY = abs(relative_diff_mean_QTY)
         , max_QTY_prior = lag(max_QTY)
         , relative_diff_max_QTY_prior = max_QTY - max_QTY_prior
         , absolute_diff_max_QTY_prior = abs(relative_diff_max_QTY_prior)
         , min_QTY_prior = lag(min_QTY)
         , relative_diff_min_QTY_prior = min_QTY - min_QTY_prior
         , absolute_diff_min_QTY_prior = abs(relative_diff_min_QTY_prior)
         , mean_ECR_prior = lag(mean_ECR)
         , relative_diff_mean_ECR = mean_ECR - mean_ECR_prior
         , absolute_diff_mean_ECR = abs(relative_diff_mean_ECR)
         , max_ECR_prior = lag(max_ECR)
         , relative_diff_max_ECR_prior = max_ECR - max_ECR_prior
         , absolute_diff_max_ECR_prior = abs(relative_diff_max_ECR_prior)
         , min_ECR_prior = lag(min_ECR)
         , relative_diff_min_ECR_prior = min_ECR - min_ECR_prior
         , absolute_diff_min_ECR_prior = abs(relative_diff_min_ECR_prior)) %>% 
  select(dim_deviceCategory
         , dim_year
         , dim_month
         , sum_sessions
         , sum_sessions_prior
         , relative_diff_sum_sessions
         , absolute_diff_sum_sessions
         , mean_sessions
         , mean_sessions_prior
         , relative_diff_mean_sessions
         , absolute_diff_mean_sessions
         , max_sessions
         , max_sessions_prior
         , relative_diff_max_sessions_prior
         , absolute_diff_max_sessions_prior
         , min_sessions
         , min_sessions_prior
         , relative_diff_min_sessions_prior
         , absolute_diff_min_sessions_prior
         , sum_transactions
         , sum_transactions_prior
         , relative_diff_sum_transactions
         , absolute_diff_sum_transactions
         , mean_transactions
         , mean_transactions_prior
         , relative_diff_mean_transactions
         , absolute_diff_mean_transactions
         , max_transactions
         , max_transactions_prior
         , relative_diff_max_transactions_prior
         , absolute_diff_max_transactions_prior
         , min_transactions
         , min_transactions_prior
         , relative_diff_min_transactions_prior
         , absolute_diff_min_transactions_prior
         , sum_QTY
         , sum_QTY_prior
         , relative_diff_sum_QTY
         , absolute_diff_sum_QTY
         , mean_QTY
         , mean_QTY_prior
         , relative_diff_mean_QTY
         , absolute_diff_mean_QTY
         , max_QTY
         , max_QTY_prior
         , relative_diff_max_QTY_prior
         , absolute_diff_max_QTY_prior
         , min_QTY
         , min_QTY_prior
         , relative_diff_min_QTY_prior
         , absolute_diff_min_QTY_prior
         , mean_ECR
         , mean_ECR_prior
         , relative_diff_mean_ECR
         , absolute_diff_mean_ECR
         , max_ECR
         , max_ECR_prior
         , relative_diff_max_ECR_prior
         , absolute_diff_max_ECR_prior
         , min_ECR
         , min_ECR_prior
         , relative_diff_min_ECR_prior
         , absolute_diff_min_ECR_prior) %>% 
  arrange(dim_year
          , dim_month
          , dim_deviceCategory)

summary_1b = DataAnalyst_Ecom_data_addsToCart %>% 
  arrange(dim_year, dim_month) %>% 
  mutate(addsToCart_prior = lag(addsToCart)
         , relative_diff_addsToCart = addsToCart - addsToCart_prior
         , absolute_diff_addsToCart = abs(relative_diff_addsToCart))

# create filter dataset

semi_join_data = summary_1a %>% 
  ungroup() %>% 
  distinct(dim_year, dim_month) %>% 
  arrange(desc(dim_year), desc(dim_month)) %>% 
  slice_head(n = 2)
  

# create month over month comparison

summary_1 = summary_1a %>% 
  left_join(summary_1b, by = c('dim_year', 'dim_month')) %>% 
  arrange(dim_year
          , dim_month
          , dim_deviceCategory) %>% 
  semi_join(semi_join_data, by = c('dim_year', 'dim_month'))
  

head(summary_1)

# how many shopping carts are abandoned? how many sessions generate cart adds?
# and how many sessions lead to transactions?
# we don't have device data for addsToCarts, so we won't be able to break it
# down like that
# also note that with limited data, these calculations are estimates:
# it's possible a cart was added to in one month and purchased in a later month
# this analysis would say that cart was abandoned. 

trends_in_behavior = DataAnalyst_Ecom_data_sessionCounts %>% 
  mutate(dim_date_object = as_date(dim_date, format="%m/%d/%y")
         , dim_year = year(dim_date_object)
         , dim_month = month(dim_date_object)) %>% 
  group_by(dim_year, dim_month) %>% 
  summarize(transactions_count = sum(transactions)
            , sessions_count = sum(sessions)) %>% 
  left_join(DataAnalyst_Ecom_data_addsToCart, by = c('dim_year', 'dim_month')) %>% 
  mutate(abandon_rate = 1 - transactions_count / addsToCart
         , session_to_cart_rate = addsToCart / sessions_count
         , session_to_transaction_rate = transactions_count / sessions_count) %>% 
  arrange(dim_year, dim_month) %>% 
  mutate(dim_day = 1
         , dim_date = make_date(dim_year, dim_month, dim_day))

head(trends_in_behavior)

# are there trends in these rates?

ggplot(data = trends_in_behavior, aes(x = dim_date)) +
  geom_line(aes(y = abandon_rate, color = 'Abandon Rate'), size = 1) +
  geom_line(aes(y = session_to_cart_rate, color = 'Sessions to Cart Rate'),
            size = 1) +
  geom_line(aes(y = session_to_transaction_rate, color = 'Session to Transaction Rate'),
            size = 1) +
  ggtitle('Abandoned Cart Rate, Session to Cart Rate, Session to Transaction Rate') +
  xlab('Date') +
  ylab('Rate') +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(
    values = c(
      'Abandon Rate' = 'darkblue'
      ,
      'Sessions to Cart Rate' = 'red'
      ,
      'Session to Transaction Rate' = 'green'
    )
  ) +
  labs(color = 'Behaviors') 

# ECR is steady
# abandon rate is decreasing
# sessions to cart rate is decreasing

# clean up this dataset for export

trends_in_behavior_clean = trends_in_behavior %>% 
  select(-dim_date, -dim_day)

# write out results to an xlsx file
# NOTE: this section will overwrite the previously existing workbook
# if it exists

excel_doc = createWorkbook(creator = ifelse(.Platform$OS.type == "windows"
                                            , Sys.getenv("USERNAME")
                                            , Sys.getenv("USER"))
                           , title = NULL
                           , subject = NULL
                           , category = NULL)

addWorksheet(excel_doc
             , 'Month_Device_Agg')

# apply formatting to make document easier to read

class(summary_0$sum_sessions) = 'comma'
class(summary_0$max_sessions) = 'comma'
class(summary_0$sum_transactions) = 'comma'
class(summary_0$max_transactions) = 'comma'
class(summary_0$sum_QTY) = 'comma'
class(summary_0$max_QTY) = 'comma'
s1 <- createStyle(numFmt = "#,##0.00")
addStyle(excel_doc, 1, style = s1, rows = 1:(nrow(summary_0) + 1), cols = c(5, 9, 13), gridExpand = TRUE)
s2 <- createStyle(numFmt = "#,##0.000")
addStyle(excel_doc, 1, style = s2, rows = 1:(nrow(summary_0) + 1), cols = c(16, 17), gridExpand = TRUE)
writeData(excel_doc
          , 'Month_Device_Agg'
          , summary_0)
addWorksheet(excel_doc
             , 'Month_Over_Month')
s3 <- createStyle(numFmt = "#,##0")
addStyle(excel_doc, 2, style = s3, rows = 1:(nrow(summary_1) + 1), cols = c(4:7, 12:15, 20:23, 28:31, 36:39, 44:47: 64:67), gridExpand = TRUE)
addStyle(excel_doc, 2, style = s2, rows = 1:(nrow(summary_0) + 1), cols = c(8:11, 24:27, 40:43), gridExpand = TRUE)
writeData(excel_doc
          , 'Month_Over_Month'
          , summary_1)
addWorksheet(excel_doc
             , 'Behavior_Rates')
writeData(excel_doc
          , 'Behavior_Rates'
          , trends_in_behavior_clean)
addWorksheet(excel_doc
             , 'Sessions_by_browser_device')
writeData(excel_doc
          , 'Sessions_by_browser_device'
          , sessions_by_browser_device)
saveWorkbook(excel_doc
             , 'C:/Users/ben/Desktop/ixis/ixis_data_science_challenge.xlsx'
             , overwrite = TRUE)

# to visualize this data (the month device aggregation), we'll add a date column
# to the previous dataset

visualization_data_0 = summary_1 %>% 
  mutate(dim_day = 1
         , dim_date = make_date(dim_year, dim_month, dim_day))

ggplot(data = visualization_data_0,
       aes(x = dim_date, y = mean_sessions, group = dim_deviceCategory)) +
  geom_line(aes(color = dim_deviceCategory), size = 1) +
  geom_point(aes(color = dim_deviceCategory)) +
  ggtitle('Mean Sessions') +
  xlab('Date') +
  ylab('Sessions') +
  theme(plot.title = element_text(hjust = 0.5)) 
ggplot(data = visualization_data_0,
       aes(x = dim_date, y = sum_sessions, group = dim_deviceCategory)) +
  geom_line(aes(color = dim_deviceCategory), size = 1) +
  geom_point(aes(color = dim_deviceCategory)) +
  ggtitle('Sum Sessions') +
  xlab('Date') +
  ylab('Sessions') +
  theme(plot.title = element_text(hjust = 0.5))
ggplot(data = visualization_data_0,
       aes(x = dim_date, y = mean_transactions, group = dim_deviceCategory)) +
  geom_line(aes(color = dim_deviceCategory), size = 1) +
  geom_point(aes(color = dim_deviceCategory)) +
  ggtitle('Mean Transactions') +
  xlab('Date') +
  ylab('Transactions') +
  theme(plot.title = element_text(hjust = 0.5))
ggplot(data = visualization_data_0,
       aes(x = dim_date, y = sum_transactions, group = dim_deviceCategory)) +
  geom_line(aes(color = dim_deviceCategory), size = 1) +
  geom_point(aes(color = dim_deviceCategory)) +
  ggtitle('Sum Transactions') +
  xlab('Date') +
  ylab('Transactions') +
  theme(plot.title = element_text(hjust = 0.5))
ggplot(data = visualization_data_0, aes(x = dim_date, y = mean_QTY, group = dim_deviceCategory)) +
  geom_line(aes(color = dim_deviceCategory), size = 1) +
  geom_point(aes(color = dim_deviceCategory)) +
  ggtitle('Mean QTY') +
  xlab('Date') +
  ylab('QTY') +
  theme(plot.title = element_text(hjust = 0.5))
ggplot(data = visualization_data_0, aes(x = dim_date, y = sum_QTY, group = dim_deviceCategory)) +
  geom_line(aes(color = dim_deviceCategory), size = 1) +
  geom_point(aes(color = dim_deviceCategory)) +
  ggtitle('Sum QTY') +
  xlab('Date') +
  ylab('QTY') +
  theme(plot.title = element_text(hjust = 0.5))

# look at mean ECR by device
# add a crude trendline


ggplot(data = visualization_data_0, aes(x = dim_date, y = mean_ECR, group = dim_deviceCategory)) +
  geom_line(aes(color = dim_deviceCategory), size = 1) +
  geom_point(aes(color = dim_deviceCategory)) +
  stat_smooth(
    method = 'lm',
    geom = 'line',
    alpha = .5,
    se = FALSE,
    size = .5
  ) +
  ggtitle('Mean ECR by Device Type') +
  xlab('Date') +
  ylab('ECR') +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(color = 'Device Type') 

# results of this plot indicate that tablet ECR is not increasing while other
# devices are; is overall tablet usage increasing?

tablet_usage = DataAnalyst_Ecom_data_sessionCounts %>%
  mutate(dim_date_object = as_date(dim_date, format="%m/%d/%y")
         , dim_year = year(dim_date_object)
         , dim_month = month(dim_date_object)
         , tablet_binary = ifelse(dim_deviceCategory == 'tablet', 1, 0)) %>% 
  select(dim_deviceCategory, tablet_binary, dim_year, dim_month) %>% 
  group_by(dim_year, dim_month) %>% 
  summarize(tablet_usage = sum(tablet_binary)) %>% 
  arrange(dim_year, dim_month) %>% 
  mutate(dim_day = 1
         , dim_date_object = make_date(dim_year, dim_month, dim_day))

head(tablet_usage)

ggplot(data = tablet_usage, aes(x = dim_date_object, y = tablet_usage)) +
  geom_line()

# check against all types

all_devices = DataAnalyst_Ecom_data_sessionCounts %>%
  mutate(dim_date_object = as_date(dim_date, format="%m/%d/%y")
         , dim_year = year(dim_date_object)
         , dim_month = month(dim_date_object)) %>% 
  group_by(dim_year, dim_month) %>% 
  count(dim_deviceCategory) %>% 
  mutate(dim_day = 1
         , dim_date_object = make_date(dim_year, dim_month, dim_day)) %>% 
  rename(count = n)

ggplot(data = all_devices,
       aes(x = dim_date_object, y = count, group = dim_deviceCategory)) +
  geom_line(aes(color = dim_deviceCategory), size = 1) +
  geom_point(aes(color = dim_deviceCategory)) +
  ggtitle('Device Type Usage') +
  xlab('Date') +
  ylab('Count') +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(color = 'Device Type') 

# while a lower overall volume, it has grown at roughly the same pace as the
# other devices

# to look at addsToCart data, we'll look at the previously created dataset,
# since it's not broken down by device

visualization_data_1 = summary_1b %>% 
  mutate(dim_day = 1
         , dim_date = make_date(dim_year, dim_month, dim_day))

ggplot(data = visualization_data_1, aes(x = dim_date)) +
  geom_line(aes(y = addsToCart, color = 'Adds to Cart'), size = 1) +
  geom_line(
    aes(y = absolute_diff_addsToCart, color = 'Absolute Difference from Previous Month')
    , size = 1
  ) +
  ggtitle('addsToCart and absolute difference in addsToCart Month over Month') +
  xlab('Date') +
  ylab('Count') +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = c(
    'Adds to Cart' = 'darkblue'
    , 'Absolute Difference from Previous Month' = 'red'
  ))

