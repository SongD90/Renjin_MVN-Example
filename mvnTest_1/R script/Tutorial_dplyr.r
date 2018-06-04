print("############ dplr #####################")
## dplyr tutorial
 library(dplyr)
# download dataset
 library(downloader)

url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/msleep_ggplot2.csv"
filename <- "msleep_ggplot2.csv"
if (!file.exists(filename)) download(url,filename)
msleep <- read.csv("msleep_ggplot2.csv")
head(msleep)

###

# select (dataset, colname1, colname2, colname3, ..)
sleepData <- select(msleep, name, sleep_total)
head(sleepData)
## select a range of columns
head(select(msleep, name:order, sleep_total))
## To select all columns that start with the character string �sl�, use the function starts_with()
head(select(msleep, starts_with("sl")))

# filter()
## filter(dataset, condition1, condition2, ...)
filter(msleep, sleep_total>=16)
filter(msleep, sleep_total >= 16, bodywt >= 1)
filter(msleep, order %in% c("Perissodactyla", "Primates"))

# pipe operator %>%
msleep %>% select(starts_with("sl")) %>% head

# arrange()
# arrange or re-order rows
msleep %>% arrange(order) %>% head

## Now, we will select three columns from msleep, arrange the rows by the taxonomic order and
## then arrange the rows by sleep_total.
## Finally show the head of the final data frame
msleep %>%
  select(name, order, sleep_total) %>%
  arrange(order, sleep_total) %>%
  head()

## Same as above, except here we filter the rows for mammals that sleep for 16 or
## more hours instead of showing the head of the final data frame
msleep %>%
  select(name, order, sleep_total) %>%
  arrange(order, sleep_total) %>%
  filter(sleep_total>=16)

## Something slightly more complicated: same as above, except arrange the rows in
## the sleep_total column in a descending order. For this, use the function desc()
msleep %>%
  select(name, order, sleep_total) %>%
  arrange(order, desc(sleep_total)) %>%
  filter(sleep_total>=16)

# mutate()
# adde a new column to a dataframe

## create a new column called rem_proportion which is the ratio of rem sleep to total amount of sleep
msleep %>%
  mutate(rem_proportion = sleep_rem / sleep_total) %>%
  head

## add 2 new columns, and the 2nd one is depedened on the 1st one.
msleep %>%
  mutate(rem_proportion = sleep_rem / sleep_total, rem2 = rem_proportion -1) %>%
  head()
# summarise()
# create summaries of the data frame
## mean
msleep %>%
  summarise(avg_sleep = mean(sleep_total))
## other static desc
msleep %>%
  summarise(avg_sleep = mean(sleep_total),
            min_sleep = min(sleep_total),
            max_sleep = max(sleep_total),
            total = n())


# group_by()
# group operations
msleep %>%
  group_by(order) %>%
  summarise(avg_sleep = mean(sleep_total),
            min_sleep = min(sleep_total),
            max_sleep = max(sleep_total),
            total = n())

print(msleep %>%
        group_by(order) %>%
        summarise(avg_sleep = mean(sleep_total),
                  min_sleep = min(sleep_total),
                  max_sleep = max(sleep_total),
                  total = n()))
