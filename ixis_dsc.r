library(tidyverse)
library(lubridate)
library(summarytools)
library(openxlsx)

DataAnalyst_Ecom_data_addsToCart = read_csv("C:/Users/ben/Desktop/ixis/DataAnalyst_Ecom_data_addsToCart.csv")
DataAnalyst_Ecom_data_sessionCounts = read_csv("C:/Users/ben/Desktop/ixis/DataAnalyst_Ecom_data_sessionCounts.csv")

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

# what are the most popular browsers, or browser device combos

popular_browsers = DataAnalyst_Ecom_data_sessionCounts %>% 
  filter(!((sessions == 0 & transactions > 0) | (sessions == 0 & QTY > 0))) %>% # remove bad records
  count(dim_browser, sort = TRUE)

head(popular_browsers)

popular_browsers_devices = DataAnalyst_Ecom_data_sessionCounts %>% 
  filter(!((sessions == 0 & transactions > 0) | (sessions == 0 & QTY > 0))) %>% # remove bad records
  count(dim_browser, dim_deviceCategory, sort = TRUE)

head(popular_browsers_devices)

# and how do these relate to overall sessions, transactions, and quantities?

sessions_by_browser = DataAnalyst_Ecom_data_sessionCounts %>% 
  filter(!((sessions == 0 & transactions > 0) | (sessions == 0 & QTY > 0))) %>% # remove bad records
  group_by(dim_browser) %>% 
  summarise()

summary_0 = DataAnalyst_Ecom_data_sessionCounts %>% 
  filter(!((sessions == 0 & transactions > 0) | (sessions == 0 & QTY > 0))) %>% # remove bad records
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

summary_1 = summary_1a %>% 
  left_join(summary_1b, by = c('dim_year', 'dim_month')) %>% 
  arrange(dim_year
          , dim_month
          , dim_deviceCategory)

# write out results to an xlsx file
excel_doc = createWorkbook(creator = ifelse(.Platform$OS.type == "windows"
                                            , Sys.getenv("USERNAME")
                                            , Sys.getenv("USER"))
                           , title = NULL
                           , subject = NULL
                           , category = NULL)

addWorksheet(excel_doc
             , 'Month_Device_Agg')
writeData(excel_doc
          , 'Month_Device_Agg'
          , summary_0)
addWorksheet(excel_doc
             , 'Month_Over_Month')
writeData(excel_doc
          , 'Month_Over_Month'
          , summary_1)

saveWorkbook(excel_doc
             , 'C:/Users/ben/Desktop/ixis/ixis_data_science_challenge.xlsx'
             , overwrite = TRUE)

# to visualize this data, we'll add a date column to the previous dataset

visualization_data_0 = summary_1 %>% 
  mutate(dim_day = 1
         , dim_date = make_date(dim_year, dim_month, dim_day))

ggplot(data=visualization_data_0, aes(x = dim_date, y = mean_sessions, group = dim_deviceCategory)) +
  geom_line(aes(color = dim_deviceCategory)) +
  geom_point(aes(color = dim_deviceCategory)) +
  ggtitle('Mean Sessions') +
  xlab('Date') +
  ylab('Sessions') +
  theme(plot.title = element_text(hjust = 0.5)) 
ggplot(data=visualization_data_0, aes(x = dim_date, y = sum_sessions, group = dim_deviceCategory)) +
  geom_line(aes(color = dim_deviceCategory)) +
  geom_point(aes(color = dim_deviceCategory)) +
  ggtitle('Sum Sessions') +
  xlab('Date') +
  ylab('Sessions') +
  theme(plot.title = element_text(hjust = 0.5)) 
ggplot(data=visualization_data_0, aes(x = dim_date, y = mean_transactions, group = dim_deviceCategory)) +
  geom_line(aes(color = dim_deviceCategory)) +
  geom_point(aes(color = dim_deviceCategory)) +
  ggtitle('Mean Transactions') +
  xlab('Date') +
  ylab('Transactions') +
  theme(plot.title = element_text(hjust = 0.5)) 
ggplot(data=visualization_data_0, aes(x = dim_date, y = sum_transactions, group = dim_deviceCategory)) +
  geom_line(aes(color = dim_deviceCategory)) +
  geom_point(aes(color = dim_deviceCategory)) +
  ggtitle('Sum Transactions') +
  xlab('Date') +
  ylab('Transactions') +
  theme(plot.title = element_text(hjust = 0.5)) 
ggplot(data=visualization_data_0, aes(x = dim_date, y = mean_QTY, group = dim_deviceCategory)) +
  geom_line(aes(color = dim_deviceCategory)) +
  geom_point(aes(color = dim_deviceCategory)) +
  ggtitle('Mean QTY') +
  xlab('Date') +
  ylab('QTY') +
  theme(plot.title = element_text(hjust = 0.5)) 
ggplot(data=visualization_data_0, aes(x = dim_date, y = sum_QTY, group = dim_deviceCategory)) +
  geom_line(aes(color = dim_deviceCategory)) +
  geom_point(aes(color = dim_deviceCategory)) +
  ggtitle('Sum QTY') +
  xlab('Date') +
  ylab('QTY') +
  theme(plot.title = element_text(hjust = 0.5)) 
ggplot(data=visualization_data_0, aes(x = dim_date, y = mean_ECR, group = dim_deviceCategory)) +
  geom_line(aes(color = dim_deviceCategory)) +
  geom_point(aes(color = dim_deviceCategory)) +
  ggtitle('Mean ECR') +
  xlab('Date') +
  ylab('ECR') +
  theme(plot.title = element_text(hjust = 0.5)) 

# to look at addsToCart data, we'll look at the previously created dataset,
# since it's not broken down by device

visualization_data_1 = summary_1b %>% 
  mutate(dim_day = 1
         , dim_date = make_date(dim_year, dim_month, dim_day))

ggplot(data = visualization_data_1, aes(x = dim_date)) +
  geom_line(aes(y = addsToCart), color = 'red') +
  geom_line(aes(y = absolute_diff_addsToCart), color = 'blue') +
  ggtitle('addsToCart and absolute difference in addsToCart Month over Month') +
  xlab('Date') +
  ylab('Count') +
  theme(plot.title = element_text(hjust = 0.5)) 

