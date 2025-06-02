# =========================
# 📊 Superstore Sales Analysis
# =========================
# Goal: Analyze sales performance and customer behavior to extract useful business insights.
# Tools: R, dplyr, ggplot2, lubridate

# -------------------------
# 🧭 Project Setup
# -------------------------

# Load necessary libraries
library(readr)       
library(dplyr)       
library(ggplot2)     
library(lubridate)  
library(tidyr)  
library(scales)
library(plotly)

# Import data
superstore <- read_csv("superstore.csv")

# Preview and clean column names
glimpse(superstore)
colnames(superstore) <- make.names(colnames(superstore))

# Parse date columns
superstore$Order_Date <- parse_date_time(superstore$Order_Date, orders = c("mdy", "dmy"))
superstore$Ship_Date  <- parse_date_time(superstore$Ship_Date, orders = c("mdy", "dmy"))

# Extract date components
superstore$Order_Month <- month(superstore$Order_Date, label = TRUE, abbr = TRUE)
superstore$Order_Year  <- year(superstore$Order_Date)


# -------------------------
# 🏢 Branch Analysis
# -------------------------

# 1. Which state has the highest sales?
state_sales <- superstore %>%
  group_by(State) %>%
  summarise(Total_Sales = sum(Sales)) %>%
  arrange(desc(Total_Sales))

# View top 10 states
head(state_sales, 10)

ggplot(state_sales[1:10, ], aes(x = reorder(State, Total_Sales), y = Total_Sales)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 10 States by Total Sales", x = "State", y = "Sales") +
  theme_minimal()
ggsave("visuals/sales_by_state.png", width = 8, height = 6)


# 2. Which city has the highest number of transactions?
city_transactions <- superstore %>%
  group_by(City) %>%
  summarise(Transaction_Count = n()) %>%
  arrange(desc(Transaction_Count))

# View top 10 cities
head(city_transactions, 10)

ggplot(city_transactions[1:10, ], aes(x = reorder(City, Transaction_Count), y = Transaction_Count)) +
  geom_col(fill = "darkgreen") +
  coord_flip() +
  labs(title = "Top 10 Cities by Transaction Count", x = "City", y = "Number of Transactions") +
  theme_minimal()
ggsave("visuals/sales_by_city.png", width = 8, height = 6)
 
# -------------------------
# 📦 Product Line Analysis
# -------------------------

# 1. Total sales per category
product_sales <- superstore %>%
  group_by(Category) %>%
  summarise(Total_Sales = sum(Sales)) %>%
  arrange(desc(Total_Sales))

# View result
product_sales

# 2. Total sales per sub-category
subcategory_sales <- superstore %>%
  group_by(Sub_Category) %>%
  summarise(Total_Sales = sum(Sales)) %>%
  arrange(desc(Total_Sales))

head(subcategory_sales, 10)


# -------------------------
# 👥 Customer Segment Analysis
# -------------------------

# 1. Distribution of customer segments across regions
segment_region <- superstore %>%
  group_by(Region, Segment) %>%
  summarise(Transaction_Count = n()) %>%
  arrange(Region, desc(Transaction_Count))

ggplot(segment_region, aes(x = Region, y = Transaction_Count, fill = Segment)) +
  geom_col(position = "fill") +  # "fill" makes it a percentage stacked bar
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Customer Segment Distribution by Region",
       x = "Region",
       y = "Percentage of Transactions",
       fill = "Customer Segment") +
  theme_minimal()
ggsave("visuals/segment_distribution_by_region.png", width = 8, height = 6)

# 2. What customer segment contributes the most to total sales?
segment_sales <- superstore %>%
  group_by(Segment) %>%
  summarise(Total_Sales = sum(Sales)) %>%
  arrange(desc(Total_Sales))
segment_sales

##In addition to total revenue, I also looked at transaction volume 
##to understand which segment is the most active.
#consumer has most with total of 5101
segment_transactions <- superstore %>%
  group_by(Segment) %>%
  summarise(Transaction_Count = n()) %>%
  arrange(desc(Transaction_Count))
segment_transactions


# -------------------------
# 🔁 Customer Behavior Analysis
# -------------------------

# 1. How many times does each customer make a purchase?
customer_freq <- superstore %>%
  group_by(Customer_ID, Customer_Name) %>%
  summarise(Num_Transactions = n()) %>%
  arrange(desc(Num_Transactions))

# Top 10 most frequent buyers
head(customer_freq, 10)

repeat_customers <- customer_freq %>% filter(Num_Transactions > 1)
nrow(repeat_customers)  
# number of repeat customers = 787
total_customers <- nrow(customer_freq)
repeat_rate <- (nrow(repeat_customers) / total_customers) * 100
repeat_rate
# repeat rate = 99.24338

# 2. Who are the top customers by average transaction value?
customer_avg <- superstore %>%
  group_by(Customer_ID, Customer_Name) %>%
  summarise(
    Total_Sales = sum(Sales),
    Num_Transactions = n(),
    Avg_Transaction_Value = Total_Sales / Num_Transactions
  ) %>%
  arrange(desc(Avg_Transaction_Value))

library(knitr)
kable(head(customer_avg, 10), digits = 2, caption = "Top 10 Customers by Avg. Transaction Value")


# -------------------------
# 🕒 Time-Based Analysis
# -------------------------

# 1. How do sales trend vary by day of the week and month?
superstore$Order_Weekday <- wday(superstore$Order_Date, label = TRUE)
superstore$Order_Month <- month(superstore$Order_Date, label = TRUE)
superstore$Order_Year <- year(superstore$Order_Date)

# Total sales by weekday
weekday_sales <- superstore %>%
  group_by(Order_Weekday) %>%
  summarise(Total_Sales = sum(Sales))
weekday_sales

ggplot(weekday_sales, aes(x = Order_Weekday, y = Total_Sales, group = 1)) +
  geom_line(color = "#1f77b4", linewidth = 1) +
  geom_point(color = "#1f77b4", size = 2) +
  labs(
    title = "Daily/Weekly Patterns",
    x = "Day of the Week",
    y = "Total Sales"
  ) +
  scale_y_continuous(labels = comma) +  
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )
ggsave("visuals/sales_by_weekday.png", width = 8, height = 6)


# Total sales by month
monthly_sales <- superstore %>%
  group_by(Order_Month) %>%
  summarise(Total_Sales = sum(Sales))
monthly_sales

ggplot(monthly_sales, aes(x = Order_Month, y = Total_Sales, group = 1)) +
  geom_line(color = "#2ca02c", linewidth = 1) +          
  geom_point(color = "#2ca02c", size = 2) +
  labs(
    title = "Sales Trend by Month",
    x = "Month",
    y = "Total Sales"
  ) +
  scale_y_continuous(labels = comma) +                  
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )
ggsave("visuals/sales_by_month.png", width = 8, height = 6)

# 2. Seasonal sales patterns
superstore$Season <- case_when(
  superstore$Order_Month %in% c("Dec", "Jan", "Feb") ~ "Winter",
  superstore$Order_Month %in% c("Mar", "Apr", "May") ~ "Spring",
  superstore$Order_Month %in% c("Jun", "Jul", "Aug") ~ "Summer",
  superstore$Order_Month %in% c("Sep", "Oct", "Nov") ~ "Fall"
)

sales_by_season <- superstore %>%
  group_by(Season) %>%
  summarise(Total_Sales = sum(Sales))

sales_by_season <- sales_by_season %>%
  mutate(Percentage = round((Total_Sales / sum(Total_Sales)) * 100, 1))

ggplot(sales_by_season, aes(x = "", y = Total_Sales, fill = Season)) +
  geom_col(width = 1) +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(Season, "\n", Percentage, "%")), 
            position = position_stack(vjust = 0.5), color = "white", size = 4) +
  labs(title = "Sales Distribution by Season") +
  scale_fill_manual(values = c("Fall" = "orange", "Winter" = "maroon", 
                               "Spring" = "darkgreen", "Summer" = "steelblue")) +
  theme_void() + theme(legend.position = "none")
ggsave("visuals/sales_by_season_pie.png", width = 7, height = 7)


# 3. How do sales trends differ across regions over the years?
region_year_sales <- superstore %>%
  group_by(Order_Year, Region) %>%
  summarise(Total_Sales = sum(Sales)) %>%
  ungroup()


ggplotly(ggplot(region_year_sales, aes(x = Order_Year, y = Total_Sales, color = Region)) +
           geom_line(size = 1.2) +
           geom_point(size = 2) +
           labs(title = "Sales Trends Across Regions Over the Years",
                x = "Year", y = "Total Sales") +
           scale_y_continuous(labels = scales::comma) +
           theme_minimal())









