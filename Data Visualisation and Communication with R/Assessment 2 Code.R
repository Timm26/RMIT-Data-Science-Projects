# Load all libraries
library(ggplot2) # For visualisation
library(dplyr) # For data preprocessing
library(readr) # For data import
library(scales) # For axis customisation
library(rvest) # For webscraping
# Webscrape the data - Save the url
url <- "https://www.visualcapitalist.com/mapped-home-price-to-income-ratio-of-large-u-s-cities/"
url_page <- read_html(url)
# Extract and clean the table
df <- url_page %>%
  html_element("table") %>%
  html_table(trim = TRUE)

# Step 1: Create a DataFrame of unique States to later map a new Region column to df
state_ <- data_frame(State = c("CA", "NY", "FL", "MA", "WA", "OR", "CO", "AZ", "DC", "TX", "TN", "NV", "NC", 
                               "NM", "VA", "GA", "LA", "MN", "IL", "NE", "OH", "WI", "PA", "KY", "MO", "IN", 
                               "OK", "MD", "KS", "MI"),
                     Region = c(
                       "Pacific West", "Northeast", "South", "Northeast", "Pacific West", "Pacific West",
                       "Mountain West", "Mountain West", "Northeast", "South Central", "South", "Mountain West", 
                       "South", "Mountain West", "South", "South", "South Central", "Midwest", "Midwest", 
                       "Midwest", "Midwest", "Midwest", "Northeast", "South", "Midwest", "Midwest", 
                       "South Central", "Northeast", "Midwest", "Midwest"))

# Step 2: First remove special characters like '$' then join the newly created DataFrame to our existing one `df`
cleaned_df <- df %>% mutate(`Median home price` = as.numeric(gsub("[$,]", "", `Median home price`)),
                            `Median household income` = as.numeric(gsub("[$,]", "", `Median household income`))) %>% 
  left_join(state_, by = "State")

# Step 3: Create a second new variable which groups Price-to-income into five categories
# We need to factor the newly created values, also to ensure R doesn't sort the values alphabetically
cleaned_df <- cleaned_df %>% mutate(Affordability = factor(case_when(`Price-to-income` <= 3 ~ "Very high",
                                                                     `Price-to-income` > 3 & `Price-to-income` <= 5 ~ "High",
                                                                     `Price-to-income` > 5 & `Price-to-income` <= 7 ~ "Medium",
                                                                     `Price-to-income` > 7 & `Price-to-income` <= 9 ~ "Low",
                                                                     TRUE ~ "Very low"),
                                                           levels = c("Very high", "High", "Medium", "Low", "Very low")))

# Step 4: Plot the scatter plot
p1 <- ggplot(cleaned_df, aes(x=`Median household income`, y=`Median home price`, color = Affordability)) + 
  geom_point(size = 3, alpha = 0.8) + # Create a scatter plot with increased point size and transparency
  scale_x_continuous(labels = dollar_format(scale = 1e-3, suffix = "k"), breaks = seq(0, 200000, 25000)) + # Trim the x-label
  scale_y_continuous(labels = dollar_format(scale = 1e-3, suffix = "k"), breaks = seq(0, 1500000, 250000)) + # Trim the y-label
  scale_color_manual(values = RColorBrewer::brewer.pal(5, "PuBu")) + # Use a colorblind-friendly palette
  facet_wrap(~ Region, ncol = 3) + # Create a 2x3 scatter plot figure split by Region
  labs( # Add text elements to the plot
    title = "How does Home Price-to-Income compare in the U.S.?",
    subtitle = "Relationship between Median Home Price and Household Income by Region",
    x = "Median Household Income (USD)",
    y = "Median Home Income (USD)",
    color = "Affordability:",
    caption = "Price-to-Income Ratio: \nVery High: ≤ 3 | High: 3-5 | Medium: 5-7 | Low: 7-9 | Very Low: >9"
  ) + theme_minimal() + # Add a theme for clear and simple plots
  theme(strip.text = element_text(size = 9, face = "bold"), # Customise and position the text elements
        legend.position = "bottom",
        plot.title = element_text(size = 16, face = "bold", hjust = 0),
        plot.subtitle = element_text(size = 12, hjust = 0),
        plot.caption = element_text(size = 7, face = "italic", hjust = 0.5))

# Plot our data
p1