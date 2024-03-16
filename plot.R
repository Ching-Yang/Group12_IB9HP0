if (!requireNamespace("RSQLite", quietly = TRUE)) {
  install.packages("RSQLite")
}
library(RSQLite)
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}
library(dplyr)
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
library(ggplot2)

if (!requireNamespace("htmlwidgets", quietly = TRUE)) {
  install.packages("htmlwidgets")
}
library(htmlwidgets)
if (!requireNamespace("DBI", quietly = TRUE)) {
  install.packages("DBI")
}
library(DBI)

if (!requireNamespace("lubridate", quietly = TRUE)) {
  install.packages("lubridate")
}
library(lubridate)

if (!requireNamespace("plotly", quietly = TRUE)) {
  install.packages("plotly")
}
library(plotly)
if (!requireNamespace("webshot", quietly = TRUE)) {
  install.packages("webshot")
  webshot::install_phantomjs()
}
library(webshot)
if (!requireNamespace("leaflet", quietly = TRUE)) {
  install.packages("leaflet")
}
library(leaflet)
if (!requireNamespace("leaflet.extras", quietly = TRUE)) {
  install.packages("leaflet.extras")
}
library(leaflet.extras)



connection <- RSQLite::dbConnect(RSQLite::SQLite(),"ecomm.db")
# Selecting order_id and date of order placed
query <- "SELECT order_id AS Orders, order_date AS Date_Ordered
FROM orders;"

order_df <- dbGetQuery(connection, query)

# Converting date format and creating a new variable for month
order_df <- mutate(order_df, Date_Ordered = as.POSIXct(order_df$Date_Ordered, origin = "1970-01-01", tz = "UTC"),
                   Month = format(Date_Ordered, "%b"))

# Ordering months
order_df$Month <- factor(order_df$Month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

# Plotting number of orders per month
fig <- ggplot(order_df, aes(x = Month)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Orders Per Month", x = "Month", y = "Count")

this_filename_date <- as.character(Sys.Date())
# format the Sys.time() to show only hours and minutes 
this_filename_time <- as.character(format(Sys.time(), format = "%H_%M"))
ggsave(paste0("figures/Order_plot_",
              this_filename_date,"_",
              this_filename_time,".png"))

#Join Customers and Orders_items
joint_cust_query <- "SELECT * FROM Customers CROSS JOIN Order_items"
cust_order_items <- DBI::dbGetQuery(connection, joint_cust_query)

cust_order_items$cust_dob <- as.Date(cust_order_items$cust_dob) #To maintain the data structure of cust_dob table is DATE after joined table 

cust_analysis <- cust_order_items %>%
  mutate(cust_yob = year(cust_dob),
         cust_age = 2024 - cust_yob)

cust_group <- cust_analysis %>% 
  group_by(age_range = cut(cust_age, breaks = c(0, 20, 30, 40, 50, Inf), labels = c("Under 20", "21-30", "31-40", "41-50", "51+"))) %>%
  summarize(total_customers = n(),
            average_age = mean(cust_age))
#Summarise Orders quantity based on Orders
# Define custom age ranges
age_ranges <- c("Under 20", "21-30", "31-40", "41-50", "51+")

# Convert cust_age to factor with custom age ranges
cust_analysis$cust_age_range <- cut(cust_analysis$cust_age, breaks = c(0, 20, 30, 40, 50, Inf), labels = age_ranges, right = FALSE)

# Plot the data as a bar plot
cust_order_plot <- plot_ly(data = cust_analysis, x = ~cust_age_range, y = ~order_item_quantity, type = 'bar', marker = list(color = '#1f77b4')) %>%
  layout(xaxis = list(title = "Age Range"),
         yaxis = list(title = "Total Order Items Quantity"),
         title = "Total Order Items Quantity by Age Range")

this_filename_date <- as.character(Sys.Date())
this_filename_time <- as.character(format(Sys.time(), format = "%H_%M"))
file_name_html <- paste0("figures/cust_order_plot_", this_filename_date, "_", this_filename_time, ".html")
file_name_png <- paste0("figures/cust_order_plot_", this_filename_date, "_", this_filename_time, ".png")

saveWidget(cust_order_plot, file = file_name_html, selfcontained = TRUE)
webshot(file_name_html, file = file_name_png)

query <- "SELECT a.shipment_city AS City, b.Latitude AS shipment_latitude, b.Longitude AS shipment_longitude
FROM shipments a
JOIN maps b ON a.shipment_city = b.Town;"

shipment_df <- dbGetQuery(connection, query)
# Input longitude and latitude details
locations <- data.frame(
  lon = shipment_df$shipment_longitude,
  lat = shipment_df$shipment_latitude
)

# Create a UK map showing where shipments headed
map <- leaflet(locations) %>%
  addTiles() %>%
  addHeatmap(
    lng = ~lon,
    lat = ~lat,
    blur = 0,
    max= 4
 )
this_filename_date <- as.character(Sys.Date())
this_filename_time <- as.character(format(Sys.time(), format = "%H_%M"))
file_name_html <- paste0("figures/map_", this_filename_date, "_", this_filename_time, ".html")
file_name_png <- paste0("figures/map_", this_filename_date, "_", this_filename_time, ".png")

saveWidget(map, file = file_name_html, selfcontained = TRUE)
webshot(file_name_html, file = file_name_png)

# Selecting all orders placed
query <- "SELECT order_id AS Orders, ads_id AS Ads_ID, order_item_quantity AS Quantity_Ordered
FROM order_items;"

order_items_df <- dbGetQuery(connection, query)

# Creating new column for presence of Advertisement
order_items_df <- mutate(order_items_df, Ads_Present = ifelse(is.na(Ads_ID), "No Advertisement", "Advertisement Present"))

# Factorising Ads Present column
order_items_df$Ads_Present <- as.factor(order_items_df$Ads_Present)

# Plot showing the differnce in orders placed with and without advertisements
fig <- ggplot(order_items_df, aes(x = Ads_Present, fill = Ads_Present)) +
  geom_bar() +
  labs(title = "Distribution of Ads Presence", x = "Advertisement", y = "Number of Sales") +
  theme(legend.position = "none")

fig <- ggplotly(fig)
this_filename_date <- as.character(Sys.Date())
this_filename_time <- as.character(format(Sys.time(), format = "%H_%M"))
file_name_html <- paste0("figures/Ads_", this_filename_date, "_", this_filename_time, ".html")
file_name_png <- paste0("figures/Ads_", this_filename_date, "_", this_filename_time, ".png")

saveWidget(fig, file = file_name_html, selfcontained = TRUE)
webshot(file_name_html, file = file_name_png)


if (!requireNamespace("RSQLite", quietly = TRUE)) {
  install.packages("RSQLite")
}
library(RSQLite)
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}
library(dplyr)
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
library(ggplot2)

if (!requireNamespace("htmlwidgets", quietly = TRUE)) {
  install.packages("htmlwidgets")
}
library(htmlwidgets)
if (!requireNamespace("DBI", quietly = TRUE)) {
  install.packages("DBI")
}
library(DBI)

if (!requireNamespace("lubridate", quietly = TRUE)) {
  install.packages("lubridate")
}
library(lubridate)

if (!requireNamespace("plotly", quietly = TRUE)) {
  install.packages("plotly")
}
library(plotly)
if (!requireNamespace("webshot", quietly = TRUE)) {
  install.packages("webshot")
  webshot::install_phantomjs()
}
library(webshot)
if (!requireNamespace("leaflet", quietly = TRUE)) {
  install.packages("leaflet")
}
library(leaflet)
if (!requireNamespace("leaflet.extras", quietly = TRUE)) {
  install.packages("leaflet.extras")
}
library(leaflet.extras)



connection <- RSQLite::dbConnect(RSQLite::SQLite(),"ecomm.db")
# Selecting Product ID, Category, and Order Date
query <- "SELECT a.product_id AS Product_ID, a.products_category AS Category, b.order_date AS Order_Date
FROM Products a
JOIN order_items c ON a.product_id = c.product_id
JOIN Orders b ON b.order_id = c.order_id;"

categories_df <- dbGetQuery(connection, query)

# Manipulating date, creating month column and counting number of times category was ordered in a month
categories_df <- mutate(categories_df, Order_Date = as.POSIXct(categories_df$Order_Date, origin = "1970-01-01", tz = "UTC"),
                   Month = format(Order_Date, "%b")) %>% select(Category, Month) %>% count(Category, Month, name = "Count")

# Ordering months
categories_df$Month <- factor(categories_df$Month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

# Plot showing number of orders placed each month across all categories
fig <- ggplot(categories_df, aes(x = Month, y = Count, group=Category, color=Category)) + geom_point() +geom_line()
fig <- ggplotly(fig)

this_filename_date <- as.character(Sys.Date())
this_filename_time <- as.character(format(Sys.time(), format = "%H_%M"))
file_name_html <- paste0("figures/categories_", this_filename_date, "_", this_filename_time, ".html")
file_name_png <- paste0("figures/categories_", this_filename_date, "_", this_filename_time, ".png")

saveWidget(fig, file = file_name_html, selfcontained = TRUE)
webshot(file_name_html, file = file_name_png)

