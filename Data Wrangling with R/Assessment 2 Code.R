## Setup 
  
#Please run the necessary libraries, un-commend to install
#install.packages("tidyverse")
#install.packages("ds4psy")
#install.packages("randomNames")
library(tidyverse) #covers ggplot2, dplyr,tidyr,readr,readxl, lubridate, stringr, etc
library(ds4psy)
library(randomNames)
library(magrittr)
library(Hmisc)
library(MVN)
library(forecast)
library(outliers)
set.seed(123)

# Creation of multiple synthetic datasets
# Function to create slightly right-skewed data with 5 outlier
right_skewed_data <- function(n,mean,sd,outlier_min,outlier_max){
  rnorm(n, mean=mean, sd=sd) %>% 
    { . * (1+ runif(n, min=0, max=0.3)) } %>% 
    c(., runif(5, min=outlier_min, max=outlier_max))}

example_data_test_result <- right_skewed_data(95, 65, 5,10,100)
summary(example_data_test_result)


# First synthetic dataset `onl_orders`
# Create a rows between 50 and 100
rows1 <- sample(50:100,1)
rows1
# Synthetic dataframe for all online orders
onl_orders <- data.frame(order_num = sample(as.character(10500:10600), rows1, replace=FALSE),
                         username_id = sample(as.character(1:10600), rows1, replace=TRUE),
                         date = sample_date(from = as.Date('2024-04-01'), to = as.Date('2024-04-07'), size = rows1, replace = TRUE),
                         amount = right_skewed_data((rows1-5),30,7.5,0.15,500),
                         transaction_id = sample(as.character(21000:(21000+rows1)), rows1, replace = FALSE),
                         item_number = sample(as.factor(1:42), rows1, replace = TRUE),
                         collected = sample(c("TRUE","FALSE"),rows1,replace = TRUE, prob = c(0.9,0.1)),
                         store_id = paste("AU",sample(c(50:150),rows1,replace = FALSE), sep = ""))

onl_orders$amount <- round(onl_orders$amount,2)
# Add another variable `weekend` - integration standalone as the variable date needs to be created
onl_orders$weekend <- ifelse(onl_orders$date %in% as.Date(c('2024-04-06','2024-04-07')),"YES","NO")
# Create three missing values in random rows of onl_orders$amount
onl_orders[sample(1:80,3), "amount"] <- NA
head(onl_orders,2)


# Second synthethic dataframe `items`
# Synthetic dataframe for all Subway items
# Define all products Subway sells
sandwiches <- c("Black Forest Ham","Buffalo Chicken","B.L.T","Cold Cut Combo","Grilled Chicken","Italien B.M.T","Meatball Marinara","Oven-Roasted Turkey","Oven-Roasted Turkey & Ham","Pizza Sub","Roast Beef","Spicy Italian","Steak & Cheese","Subway Club","Subway Melt","Sweet Onion Chicken Teriyaki","Tuna","Turkey Breast","Veggie Delite")
sides <- c("Fries","Sweet Potato Fried","Caramel Cookie","Chocolate Chip Cookie","Oatmeal Raisin Cookie","White Chip Macadamia Nut Cookie","Dark Chocolate Cherry Cookie","Broccoli Soup","Chicken Noodle Soup","Broccoli Cheddar Soup","Cheese Flatizza","Pepperoni Flatizza","Spicy Italian Flatizza","Veggie Flatizza")
drinks <- c("Coffee","Fountain Drinks","Dasani Water","X2 All Natural Energy","Gatorade","Honest Kids","Bootle Beverage","Low Fat Milk","Juice")

# Define all prices
sandwich_price <- c(7,8,8.3,8.55,8.75,9.75)
side_price <- c(3.25,3.45,4,4.35,4.5,4.95)
drinks_price <- c(2,2.25,2.5,3,3.5,3.75)
item_prices <- c(sample(sandwich_price,length(sandwiches),replace=TRUE),
                 sample(side_price, length(sides), replace=TRUE),
                 sample(drinks, length(drinks), replace=TRUE))
# Define item description
all_items <- c(sandwiches, sides, drinks)

# Defining the necessary vectors 
items <- data.frame(item_number = as.factor(1:length(all_items)),
                    item_description = all_items,
                    item_price = item_prices,
                    rubric = rep(c("Sandwich","Side","Drink"), times = c(length(sandwiches),length(sides),length(drinks))),
                    customer_rating = rnorm(length(all_items), mean = 4, sd = 0.5))
items$customer_rating <- round(items$customer_rating ,2)


# Outliers and missing values
items$customer_rating[sample(1:nrow(items), 5)] <- rnorm(5, mean = 1, sd=0.5)
items[sample(1:length(all_items),3),"customer_rating"] <- NA 
head(items,2)


# Third synthethic dataframe `user`
# Synthetic dataframe for all user data
# Get all unique usernames you find in onl_orders
username <- unique(onl_orders$username_id)
# Generate random first and last name, email provider and postal code of SYD
first_name <- randomNames(length(username),which.names = "first")
last_name <- randomNames(length(username), which.names = "last")
email_provider = c("gmail.com","yahoo.com","gmx.com","icloud.com","me.com","outlook.com","hotmail.com","zoho.com","live.com","fastmail.com","hushmail.com","tutanota.com","usa.com","safe-mail.net","excite.com","bigstring.com","inbox.com","mail.ru","runbox.com","iname.com")
postal_code <- c(2000,2001,2006,2010,2011,2015,2020,2021,2022,2026,2031,2037,2042,2043,2044,2050,2060,2061,2065,2067,2068,2070,2071,2074,2085,2086,2090,2093,2095,2100,2110,2111,2112,2127,2129,2130,2134,2140,2150,2166)

user <- data.frame(username_id = username,
                   first_name = first_name,
                   last_name = last_name,
                   b_day = sample(seq(as.POSIXct("1970/01/01"), as.POSIXct("2008/07/25"), by="day"), length(username), replace = TRUE),
                   email = paste(last_name, sample(email_provider, length(username), replace = TRUE), sep = "@"),
                   postal_code = sample(postal_code, length(username), replace = TRUE ),
                   member = sample(c(TRUE, FALSE), length(username), replace = TRUE),
                   total_orders = round(rnorm(length(username),mean = 15, sd = 4)),
                   last_activity = sample(seq(as.Date("2024/04/01"), as.Date("2024/04/07"), by = "day"), length(username), replace= TRUE))

# Outliers in total_orders and add missing data NA
user$total_orders[sample(1:nrow(user),5)] <- round(rnorm(5, mean=90, sd=8)) 
user[sample(1:length(all_items),6),"email"] <- NA
head(user,2)


# Fourth synthethic dataframe `stores`
# Synthetic dataframe for all Subway_stores
# Extract all unique Store IDs
storeid <- sort(unique(onl_orders$store_id))
# Create the locations
location <- c("Sydney Central", "Redfern","University of Sydney","Darlinghurst","Potts Point","Alexandria","Sydney Domestic Airport","Sydney International Airport","Paddington","Bondi Junction","Bondi Beach","Randwick","Glebe","Newtown","Erskineville","St Peters","Camperdown","North Sydney","Kirribilli","Crows Nest","Chatswood","Willoughby","Lindfield","Killara","Turramurra","Belrose","Frenchs Forest","Mosman","Manly","Fairlight","Brooksvale","Hunters Hill","Gladsville","Ryde","Sydney Olympic Park","Sydney Markets","Summer Hill","Burwood","Homebush","Parramatta","Cabramatta")
# Create the revenue
revenue <- round(right_skewed_data(length(storeid)-5,mean = 200000, sd=20000,outlier_min = 15000, outlier_max = 400000), digits = 2)


# Create the synthetic dataframe
stores <- data.frame(store_id = storeid,
                     location = sample(location, length(storeid), replace = TRUE),
                     revenue = round(revenue, digits = 2),
                     employees = sample(4:20, length(storeid), replace = TRUE),
                     manager_name = randomNames(length(storeid), which.names = "last"),
                     store_type = as.factor(sample(c("express","dine-in","delivery-only"), length(storeid),replace = TRUE)))

# Outliers and missing data
stores$revenue[sample(1:nrow(stores), 5)] <- round(rnorm(5, mean=395000, sd= 10), digits = 2)
stores$employees[sample(1:nrow(stores), 3)] <- NA
head(stores,2)


# 1.5 Fifth synthetic dataframe `transactions`
# Create a synthetic dataset aboout the transactions
# Get transaction id from onl_orders
transactionid <- sort(unique(onl_orders$transaction_id))
payment_type <- c("creditcard","cash","applepay","googleplay","paypal","afterpay","debitcard")
amounts <- round(right_skewed_data(length(transactionid)-5, 30, 7.5, 0.15,500), digits = 2)
# Create the variable transactions
transactions <- data.frame(transaction_id = transactionid,
                           payment_type = as.factor(sample(payment_type, length(transactionid), replace = TRUE)),
                           order_date = sample(seq(ymd_hm('2024-04-01 00:00'), ymd_hm('2024-04-07 23:59'), by = "min"), length(transactionid)),
                           amounts = amounts)
# Add the discount_ref variable , 40% of the user apply one
transactions$discount_ref <- ifelse(
  runif(nrow(transactions)) <= 0.4,
  sapply(transactions$amounts, function(x) {
    discount_amount <- ifelse(x >= 50, round(x * 0.20, 2), round(x * 0.10, 2))
    paste0("ref-", formatC(sample(1000:9999,1), width = 4, flag = "0"), "$", formatC(discount_amount, width = 5, flag = "0"))
  }),
  "ref-not_applied")
# Add the `cancelled` variable                           
transactions$cancelled <- sample(c(TRUE,FALSE), length(transactionid), replace = TRUE, prob = c(0.05,0.95))

# Missing data since outliers are already in the amounts data
transactions$payment_type[sample(1:nrow(transactions),4)] <- NA
head(transactions,2)

# Create a correlation between revenue and employees and customer_rating and item_price
stores <- stores %>% 
  mutate(revenue = revenue + (employees * 10000))
items <- items %>% 
  mutate(customer_rating = customer_rating + log(as.numeric(item_price)))
onl_orders$weekend_numeric <- ifelse(onl_orders$weekend == "YES",1,0)
# Displaying the correlation
par(mfrow = c(1,2))
plot(stores$employees, stores$revenue,
     main = "Corr. Revenue and Employees",
     xlab = "Employees",
     ylab = "Revenue")
plot(items$item_price, items$customer_rating,
     main = "Corr. Customer_rat and Item_pr",
     xlab = "Item Price",
     ylab = "Customer Rating")

onl_orders$weekend_numeric <- NULL

# 2.0 Merge - Creating of merged dataframe `subway_data`
# 2.1 Merging `onl_order` and `user` 
  
# First we merge onl_orders and users
subway_data <- left_join(onl_orders, user, by = "username_id")
head(subway_data,3)

# 2.2 Merging `subway_data` and `items` 
#Then we merge the dataset item_number
subway_data <- left_join(subway_data , items, by = "item_number")
head(subway_data,3)

# Merge stores
subway_data %<>% left_join(stores %>% dplyr::select(store_id, location, revenue, employees, manager_name, store_type), by = "store_id")
# Check result
head(subway_data,3)

# 2.4 Merging `subway_data` and `transaction`
# Merge transactions
subway_data <- left_join(subway_data, transactions, by = "transaction_id")
tail(subway_data,3)

# Check unique dataframe specific variables, everything works so far!
subway_data %>% 
  dplyr::group_by(date) %>% 
  dplyr::select(customer_rating, email, employees, cancelled) %>% 
  head(3)

# 3.0 Understand
# display the structure
str(subway_data)

# 3.1 Converting data into their correct variable types
# Convert the variable types
subway_data$postal_code <- as.character(subway_data$postal_code)
subway_data$item_price <- as.numeric(subway_data$item_price)
subway_data$b_day <- as.Date(subway_data$b_day)
# Get a more concise list of variable types
subway_data %>% 
  select(postal_code,item_number,rubric,collected,weekend) %>% 
  sapply(typeof)

# 3.2 Creating Factor variables and changing their levels
# Label factor variables
subway_data %<>% 
  mutate(payment_type = factor(payment_type, levels = c("creditcard","cash","applepay","googlepay","afterpay","debitcard")),
         store_type = factor(store_type, levels = c("express","dine-in","delivery-only")),
         collected = factor(collected, levels = c("TRUE","FALSE"), labels = c("Collected","Outstanding")),
         member = factor(member, levels = c(TRUE,FALSE),labels = c("Active","Inactive")),
         weekend = factor(weekend, levels = c("NO","YES"), labels = c("Weekday","Weekend")))

factors <- c("payment_type", "store_type", "collected", "member", "weekend")

factors <- c("payment_type", "store_type", "collected", "member", "weekend")

invisible(sapply(factors, function(var) {
  cat("\nFactor Variable:", var, "\nLevels:", levels(subway_data[[var]]), "\n")
}))

subway_data %>% 
  select_if(is.factor) %>% 
  tail(3)


# 4.0 Manipulate Data

# Add the total amount spent at all subways
subway_data %<>% 
  mutate(order_total = amount * total_orders) 
# Add the revenue per head count per subway branch
subway_data %<>% 
  mutate(revenue_per_head = round(revenue / employees, digits = 2))
# Current age of the customer
subway_data %<>% 
  mutate(age = as.numeric(format(Sys.Date(), "%Y")) - as.numeric(format(b_day, "%Y")))

subway_data %>% 
  summarise(minimum_age = min(age[age != 0], na.rm = TRUE),
            median_rev_per_head = round(median(revenue_per_head, na.rm = TRUE), digits = 2),
            max__order_total = round(max(order_total, na.rm = TRUE),digits=0))

subway_data %>% 
  filter(age > 80 |
         revenue_per_head > 55000 |
         total_orders > 100) %>% 
  select(last_name, age, revenue_per_head, location, total_orders)

# 5.1 Scan I
# 5.1.1 Locating and displaying the missing values `NA`
# Scan if values are standalone values
exist_lists <- vapply(subway_data, is.list, logical(1))
exist_lists[exist_lists == TRUE] # set to FALSE, displays all standalone variables

# Scan for missing values in the variables
missing_values <- colSums(is.na(subway_data))
missing_values[missing_values > 0]
subset <- subway_data %>% select(amount,email,item_price,customer_rating,revenue,employees, payment_type, order_total, revenue_per_head)

na_locations <- which(is.na(subway_data), arr.ind = TRUE)
head(na_locations,3)
subway_data[17,4]

# 5.1.2 Cleaning our dataset from missing values `NA`

# Fixing missing values with the `impute()` function
var_numeric <- sapply(subway_data, is.numeric)
subway_data[var_numeric] <- lapply(subway_data[var_numeric], function(x) impute(x, fun = mean))

# Change all missing emails to `Unknown`
subway_data$email[is.na(subway_data$email)] <- "Unknown"
# Change payment_type to its mode
subway_data$payment_type <- impute(subway_data$payment_type, fun = mode)
# Check if it worked
which(is.na(subway_data), arr.ind = TRUE)

# 5.2 Scan II

# 5.2.1 Calculating the `z score` of `amount` and manually detecting outliers
# Detect univariate outliers
z.scores <- subway_data$amount %>% 
  outliers::scores(type = "z")
# Summary
summary(z.scores)

# Manually searching for outliers
q1 <- -0.317898
q3 <- -0.04303
iqr <- q3-q1
lower_fence <- q1 - 1.5 * iqr
upper_fence <- q3 + 1.5 * iqr
cat("Lower Border for Outlier - Upper Border for Outlier\n")
cat(lower_fence,"------------------",upper_fence)

# Histogram 
hist(z.scores, freq = FALSE, breaks = 30, main = "Z-scores Amount")
x <- seq(-15,15, by = 0.001)
lines(x = x, y = dnorm(x), col = 'red')

# 5.2.2 Visualising all outlier of all numeric variables
# Boxplot for all numeric variables
exclude_age <- subway_data %>%  select(-age)
numeric_var <- sapply(exclude_age, is.numeric)
par(mfrow = c(3,3))
for (col in names(exclude_age)[numeric_var]){
  boxplot(exclude_age[[col]], main = col)}

# Apply mean replacement for lower_fence and upper_fence outliers
impute_outliers <- function(x){
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  lower_fence <- q1 - 1.5 * iqr
  upper_fence <- q3 + 1.5 * iqr
  mean_var <- mean(x, na.rm=TRUE)

  x <- ifelse(x < lower_fence | x > upper_fence, mean_var , x)
  return(x)
}
# Apply the imputation to numeric variables three times to ensure it worked
for (i in 1:3){
  subway_data <- subway_data %>% mutate_if(is.numeric, impute_outliers)}

# Check if we were successful
exclude_amount <- subway_data %>%  select(-age)
numeric_var <- sapply(exclude_amount, is.numeric)
par(mfrow = c(3,3))
for (col in names(exclude_amount)[numeric_var]){
  boxplot(exclude_amount[[col]], main = col)}

# 6.0 Transform
# 6.1 Logarithmic Transformation

# This is a chunk where you apply an appropriate transformation to at least one of the variables.
p1 <- ggplot(subway_data, aes(x=revenue)) +
  geom_histogram(bins = 15, color = 'grey')+
  labs(title = "Histogram of Revenue before log10 transformation",
       x = "Revenue")

subway_data <- subway_data %>% mutate(log10_revenue = log10(revenue))
# Visualise with ggplot2
p2 <- ggplot(subway_data, aes(x=log10_revenue)) + geom_histogram(bins = 15, color = 'black') + labs(title = "Histogram of Revenue after log10 transformation",
       x = "Log (base10) Revenue")
# Display before and after
gridExtra::grid.arrange(p1 ,p2, nrow = 2)

# 6.2 Square Root Transformation

subway_data <- subway_data %>% mutate(sqrt_amount = sqrt(amount))
# Create the visualisations of amount before and after square root transformation
o1 <- ggplot(subway_data, aes(x=amount)) + geom_histogram(bins = 15, color='grey') +
  labs(title = "Histogram of Amount before square root transformation",
       x = "Amount")

o2 <- ggplot(subway_data, aes(x=sqrt_amount)) +
  geom_histogram(bins = 15, color='black')+
  labs(title = "Histogram of Amount after square root transformation",
  x = "Square Root of Amount")

gridExtra::grid.arrange(o1 ,o2, nrow = 2) # Display both graphs

# 6.3 Box Cox Transformation

# Use Box Cox transformation on subway_data$revenue_per_head
boxcox_revenue_per_head <- BoxCox(subway_data$revenue_per_head, lambda = "auto")
lambda <- attr(boxcox_revenue_per_head, which = "lambda")
# Create the before and after transformation visualisation
i1 <- hist(subway_data$revenue_per_head, breaks = 25, plot = FALSE)
i2 <- hist(boxcox_revenue_per_head, breaks = 25, plot = FALSE)
# Plot both visualisations next to each other
par(mfrow = c(2,1))
plot(i1, main = "Histogram of `Revenue per Head` before BoxCox transformation", xlab = "Revenue per Head", col = "grey")
plot(i2, main = bquote("Histogram of `Revenue per Head` after BoxCox transformation (" ~ lambda == .(lambda) ~ ")"), xlab = "BoxCox Revenue per Head", col = "green")

# 7.0 Summary statistics
# 7.1 Summary Statistics grouped by `location`

# Just an overview so we drop the created variable mean_amount asap after display
subway_data %>% 
  group_by(location) %>% 
  summarise(mean_amount = round(mean(amount),digits = 2),
            median_item_price = round(median(as.numeric(item_price)),digits = 2),
            min_employ = min(employees),
            max_rev = round(max(revenue),digits = 2), .groups = "drop") %>% 
            head(3)

# 7.2 Summary Statistics ungrouped
subway_data %>% 
  summarise(total_orders = round(sum(total_orders),digits = 0),
            avg_rating = round(mean(customer_rating), digits = 2),
            median_discount = median(amounts[discount_ref != "ref-not_applied"]),
            max_age = max(age),
            min_age = min(age),
            mean_revenue_per_h = round(mean(revenue_per_head), digits = 2),
            .groups = "drop")