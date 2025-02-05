# 0. Setup
  
# Un-commend if needed
# install.packages('RSocrata')
# Please load packages required for producing this report
library(tidyverse) # Covers ggplot2, readr, tidyr, dplyr, etc 
library(ggplot2)
library(forecast)
library(magrittr) # For piping
library(RSocrata) # For webscraping
library(lubridate) # For Date and Time conversion
library(infotheo) # For discretisation techniques, here equal-depth binning
# Seed for reproducibility
set.seed(123)

# 1. Data Description
# 1.1 Data Description Set 1
url <- "https://data.ct.gov/resource/5mzw-sjtu.json" 
complete_data <- read.socrata(url)

# Select random rows once
rows_df <- complete_data %>% slice_sample(n = 10000)

# Ensure town appears just once & drop unnecessary variables
sales_data <- rows_df %>% 
  group_by(town) %>% slice(1) %>% ungroup() %>% 
  select(town, everything(), -c(serialnumber,nonusecode,geo_coordinates.type,geo_coordinates.coordinates,remarks,address,opm_remarks))

# Display the first rows
head(sales_data,5)



# 1.2 Data Description Set 2
# Import dataset 2, webscrap the dataset using RSocrata
url_2 <- "https://data.ct.gov/resource/3udy-56vi.json"
affordable_housing <- read.socrata(url_2)

# Adjust the dataset and display the first 5 rows
affordable_housing %<>% group_by(town) %>% slice(1) %>% ungroup() %>% 
  select(town, everything(), -c(year,code,percent_assisted,deed_restricted))
head(affordable_housing,5)



# 1.3 Data Description Set 3
# Load the third dataset
url_3 <- "https://data.ct.gov/resource/5e73-kfqf.json"
housing_segregation <- read.socrata(url_3)

# Get rid of the first 12 double up rows, rearrange variables and sort by town
housing_segregation %<>%
  rename(town = sub_geography) %>% 
  select(town, everything(),-geography) %>% slice(-c(1:12)) %>%  arrange(town)

# Display the first 5 rows
head(housing_segregation,5)



# 1.4 Data Description Set 4
# Load the fourth dataset as per previous technique
url_4 <- "https://data.ct.gov/resource/stm9-38x4.json"
housing_permits <- read.socrata(url_4)

# Rearrange and drop observations
housing_permits %<>% slice(-1) %>% rename(town = towns) %>% arrange(town)
head(housing_permits,4)

# Change the datasets wide format to long format
housing_permits %<>% 
  pivot_longer(cols = starts_with("X_"),
               names_to = "year",
               names_prefix = "X_",
               values_to = "permits")
head(housing_permits,3)

# Summarise and create new variables to dispaly unique town's
housing_permits %<>% 
  group_by(town, county) %>% 
  summarise(total_permits = sum(as.numeric(permits), na.rm = TRUE),
            peak_year = year[which.max(permits)]) %>% ungroup() %>% arrange(town)
head(housing_permits,4)



# 1.5 Data Description Set 5
# Webscrape data using RSocrata as before and select relevant variables
url_5 <- "https://data.ct.gov/resource/8rr8-a322.json"
temp_df <- read.socrata(url_5)
temp_df <- temp_df[,c(2,4,7,10,13,22)]
head(temp_df,2)

# Change the datasets format from wide to long
values_property <- temp_df %>% 
  pivot_longer(c(residential_net,apartments_net,cip_net,vacant_net,total_real_property),
               names_to = "type_property",
               values_to = "property_value")
head(values_property, 5)

# Create a new variable `avg_property` to display every town just once
avg_property_value <- values_property %>% 
  group_by(town_name) %>% 
  summarise(avg_property = mean(as.numeric(property_value), na.rm = TRUE)) %>% ungroup() %>% rename(town = town_name)
head(avg_property_value,5)

# Identify the common variable of all datasets
common_var <- intersect(sales_data %>% names(), affordable_housing %>% names())
common_var <- intersect(common_var, housing_segregation %>% names())
common_var <- intersect(common_var, housing_permits %>% names())
common_var <- intersect(common_var, avg_property_value %>% names())
common_var



# 2. Understand
# 2.1 Merging the datasets
# Join all datasets to one together
connecticut_df <- sales_data %>% 
  left_join(affordable_housing, by = 'town') %>% 
  left_join(housing_segregation, by = 'town') %>% 
  left_join(housing_permits, by = 'town') %>% 
  left_join(avg_property_value, by = 'town')
head(connecticut_df)

# Display just variable names and type
sapply(connecticut_df[0,],typeof)

new_var_names <- c("Towns","Listed Year","Recorded Date","Assessed Value","Sale Amount","Sale Ratio","Property Type","Residential Type","Census 2010","Gov. Assisted","Rental Assistance","Financed Units","Total Assisted","Gini 2010","Gini 2015","Gini 2020", "County","Total Permits","Peak Permit Year","Average Value")
old_var_names <- names(connecticut_df)
rename_ <- setNames(old_var_names,new_var_names)
connecticut_df <- rename(connecticut_df, !!!rename_)
colnames(connecticut_df)

# Type conversion numeric data
numeric_var <- c("Assessed Value","Sale Amount","Sale Ratio","Census 2010","Gov. Assisted","Rental Assistance","Financed Units","Total Assisted","Gini 2010","Gini 2015","Gini 2020")
connecticut_df[numeric_var] <- vapply(connecticut_df[numeric_var], as.numeric, numeric(nrow(connecticut_df)))
glimpse(connecticut_df)

# Date conversion
connecticut_df %<>% mutate(`Recorded Date` = case_when(is.na(as.Date(`Recorded Date`, format = "%d %b %Y")) ~ as.Date(`Recorded Date`, format = "%d-%b-%Y"), TRUE ~ as.Date(`Recorded Date`, format = "%d %b %Y")))
head(connecticut_df)

# Label and level `Property Type` and `Residential Type`
unique(connecticut_df$`Property Type`)
unique(connecticut_df$`Residential Type`)

connecticut_df %<>% 
  mutate(`Property Type` = factor(`Property Type`,
                                  levels = c("Single Family","Three Family","Condo","Residential","Commercial","Vacant Land"),
                                  labels = c("Single Family Property","Three-Family Property","Condo","Residential Property","Commercial Property","Vacant Land")))

connecticut_df %<>% 
  mutate(`Residential Type` = factor(`Residential Type`,
                                  levels = c("Single Family","Condo","Two Family","Three Family"),
                                  labels = c("Single Household","Condo","Two-Family Home","Three-Family Home")))
connecticut_df %>% 
  select(Towns,`Property Type`,`Residential Type`) %>% head()



# 3.1 Tidy & Manipulate I
# We inspect the connecticut_df dataset
str(connecticut_df)

# Iterate through all values for lists
standalone_val <- vapply(connecticut_df, is.list, logical(1))
list_val <- names(connecticut_df)[standalone_val]
list_val



# 3.2 Tidy & Manipulate II
# This is a chunk where you create/mutate at least one variable from existing variables
connecticut_df %<>%
  mutate(`Average Gini` = rowMeans(select(connecticut_df, starts_with("Gini")), na.rm = TRUE)) %>% 
  select(-c(`Gini 2010`,`Gini 2015`,`Gini 2020`))
connecticut_df$`Average Gini` <- round(connecticut_df$`Average Gini`, digits = 2)
connecticut_df %>% 
  select(Towns,`Assessed Value`,`Total Assisted`,`Average Gini`,`Sale Ratio`) %>% head(4)

# Basic summary statistics on the `Sale Ratio` variable
summary(connecticut_df$`Sale Ratio`)

# Equal depth binning to `Sale Ratio` in three bins
binned <- infotheo::discretize(connecticut_df$`Sale Ratio`, disc = "equalfreq", nbins = 3)
# Convert variable to a factor with labels
connecticut_df$`Price Rubric` <- factor(binned$X,
                              labels = c("Low Priced","Medium Priced","High Priced"))
# Display the result
connecticut_df %>% select(Towns, `Assessed Value`, `Sale Amount`,`Sale Ratio`,`Price Rubric`) %>% head(3)

# Mutate Assited Housing Percentage to the dataframe
connecticut_df %<>% 
  mutate(`Ass. Housing Percentage` = round((`Total Assisted` / `Census 2010`) * 100,digits = 2))
connecticut_df[,c(1,4:5,9,14,17:ncol(connecticut_df))] %>% head(3)

# Correlation between `Average (Property) Value` and `Assessed Value`
log10_assessed_value <- log10(connecticut_df$`Assessed Value`)
log10_average_value <- log10(connecticut_df$`Average Value`)
cor(log10_assessed_value, log10_average_value, use = "complete.obs")

# Let's plot the correlation
ggplot(connecticut_df, aes(x=log10_assessed_value, log10_average_value)) + geom_point() + labs(title = "Correlation between Average (Property) Value and Assessed Value", x = "Assessed Value", y = "Average (Property) Value")

# As no correlation is given, we drop the variable
connecticut_df %<>% select(-`Average Value`)



# 4.1 Scan I
# 4.1.1 Identifying missing and `0` values
# Scan for missing values
missing_values <- colSums(is.na(connecticut_df))
missing_values[missing_values > 0]

# We can scan for Zero values of numerical variables
zero_values <- colSums(connecticut_df == 0, na.rm = TRUE)
zero_values[zero_values > 0]



# 4.1.2 Replacing missing and `0` values
# Replacement of missing data
replace_na <- function(x) {
  proportion <- prop.table(table(x))
  # Replace NA with random sample values based on the proportion of that value within the var
  x[is.na(x)] <- sample(names(proportion), size = sum(is.na(x)),replace = TRUE, prob = proportion)
  return(x)
}

# Apply the function on our qualitative variables
connecticut_df$`Property Type` <- replace_na(connecticut_df$`Property Type`)
connecticut_df$`Residential Type` <- replace_na(connecticut_df$`Residential Type`)

# Check if we were sucessful
summary(connecticut_df$`Property Type`) 
summary(connecticut_df$`Residential Type`)

# Return row with `0` values
connecticut_df %>% 
  select(`Gov. Assisted`, `Rental Assistance`,`Financed Units`,`Total Assisted`) %>%
  filter(`Gov. Assisted` == 0 | `Rental Assistance` == 0 | `Financed Units` == 0 | `Total Assisted` == 0) %>% head(3)

# First we replace all `0` values by `NA`
connecticut_df[connecticut_df == 0] <- NA

# We define the rules as a validator expression
rules <- validate::validator(`Gov. Assisted` + `Rental Assistance` + `Financed Units` == `Total Assisted`,
                             `Gov. Assisted` >= 0,
                             `Rental Assistance` >= 0,
                             `Financed Units` >= 0)

# Now, we use `impute_lr` on `connecticut_df` to fulfill the equation using `rules`
connecticut_df <- deductive::impute_lr(connecticut_df,rules)
connecticut_df %>% 
  select(`Gov. Assisted`, `Rental Assistance`,`Financed Units`,`Total Assisted`) %>%
  filter(`Gov. Assisted` == 0 | `Rental Assistance` == 0 | `Financed Units` == 0 | `Total Assisted` == 0) %>% tail(3)



# 4.2 Scan II
# This is a chunk where you scan the numeric data for outliers 
numeric_var <- sapply(connecticut_df, is.numeric)
par(mfrow = c(3,4))
for (col in names(connecticut_df)[numeric_var]){
  boxplot(connecticut_df[[col]], main = col)}

# First we check what values are considered upper or lower outlier
AV_q1 <- quantile(connecticut_df$`Assessed Value`, 0.25)
AV_q3 <- quantile(connecticut_df$`Assessed Value`, 0.75)
AV_iqr <- AV_q3 - AV_q1
AV_lower_fence <- AV_q1 - 1.5 * AV_iqr
AV_upper_fence <- AV_q3 + 1.5 * AV_iqr
cat("Lower Fence: ", AV_lower_fence, "\nUpper Fence ", AV_upper_fence)

connecticut_df %<>% 
  mutate(`Z-Score Total Permit` = as.vector(scale(`Total Permits`)))

# Filter out z-scores > 3 
connecticut_df %>% select(`Total Permits`, `Z-Score Total Permit`) %>%
  filter(abs(`Z-Score Total Permit`) > 3)

# We save a variable before the transformation, please ignore this for now
before_sale_ratio <- connecticut_df$`Sale Ratio`

# Apply mean replacement for lower_fence and upper_fence outliers
impute_outliers <- function(var){
  q1 <- quantile(var, 0.25, na.rm = TRUE)
  q3 <- quantile(var, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  lower_fence <- q1 - 1.5 * iqr
  upper_fence <- q3 + 1.5 * iqr
  mean_var <- mean(var, na.rm=TRUE)
  
  var <- ifelse(var < lower_fence | var > upper_fence, mean_var , var)
  return(var)
}

# Apply the imputation to numeric variables three times to ensure it worked
for (i in 1:4){
  connecticut_df <- connecticut_df %>% mutate_if(is.numeric, impute_outliers)}

# Check if the standardisation worked
numeric_var <- sapply(connecticut_df, is.numeric)
par(mfrow = c(3,4))
for (col in names(connecticut_df)[numeric_var]){
  boxplot(connecticut_df[[col]], main = col)}

# Visualise Sale Ratio before and after Standardisation
par(mfrow = c(2,1))
hist(before_sale_ratio, main = "Sale Ratio before Standardisation", xlab = "Sale Ratio", breaks = 20)
hist(connecticut_df$`Sale Ratio`, main = "Sale Ration after Standardisation", xlab = "Sale Ration", breaks=20)



# 5. Transform
# 5.1 BoxCox transformation of `Assessed Value`
boxcox_SR <- BoxCox(connecticut_df$`Assessed Value`, lambda = "auto")
lambda <- attr(boxcox_SR, which = "lambda")

# Defining the histogram before the bxcox transformation using ggplot2
graph1 <- ggplot(connecticut_df, aes(`Assessed Value`)) + geom_histogram(bins = 15, color = "red") +
  labs(title = "Histogram of Assessed Value before the Box-Cox Transformation")

# Defining the boxcox of Assessed Value using ggplot2
p1 <- ggplot(boxcox_SR %>% as.data.frame())
graph2 <- p1 + geom_histogram(aes(x = .), bins = 15, color = "black") +
  labs(title = "Histogram of Assessed Value after the Box-Cox Transformation",
       subtitle = bquote(~ lambda -- .(lambda)),
       x = "Transformed Assessed Value")
gridExtra::grid.arrange(graph1 ,graph2, nrow = 2) # Display both graphs

# Update `Assessed Value` with the transformed values
connecticut_df$`Assessed Value` <- boxcox_SR



# 5.2 Square-root transformation of `Ass. Housing Percentage`
# Plot `Total Assisted`
par(mfrow = c(3:4))
for (col in names(connecticut_df)[numeric_var]){
  hist(connecticut_df[[col]], main = col, xlab = col)}

# Subset all right-skewed variables
right_skewed_var <- connecticut_df %>% select(`Gov. Assisted`,`Rental Assistance`,`Financed Units`,`Total Assisted`,`Total Permits`)

# Apply the square root transformation
right_skewed_var %<>% sqrt()

# Display all numerical right-skewed variables to see if their distribution improved
par(mfrow = c(2:3))
for (col in names(right_skewed_var)) {
  hist(right_skewed_var[[col]], freq = FALSE, breaks = 30, main = col, xlab = col)
  
  # This block displays the normal distribution of each graph
  x <- seq(min(right_skewed_var[[col]], na.rm = TRUE), max(right_skewed_var[[col]], na.rm = TRUE), length.out = 100)
  lines(x, dnorm(x, mean = mean(right_skewed_var[[col]], na.rm = TRUE), sd = sd(right_skewed_var[[col]], na.rm = TRUE)), col = 'red')}