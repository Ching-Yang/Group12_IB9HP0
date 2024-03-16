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

# Join "Customers", "Orders", and "Order_items" tables using inner join
joint_cust_query <- "
SELECT 
    c.cust_id,
    c.cust_dob,
    o.order_id,
    o.order_date,
    oi.order_item_quantity,
    oi.order_item_unit_price
FROM 
    order_items AS oi
    JOIN orders AS o ON oi.order_id = o.order_id
    JOIN customers AS c ON o.cust_id = c.cust_id;"


cus_o_oi <- DBI::dbGetQuery(connection, joint_cust_query)

# Maintain "cust_dob" and "order_date" columns to the same data type
cus_o_oi$cust_dob <- as.Date(cus_o_oi$cust_dob)
cus_o_oi$order_date <- as.POSIXct(cus_o_oi$order_date)

# To begin with, we create the new column "cust_year_of_birth" to extract only year of birth data from customer's date of birth, then create another column "cust_age" from cust_yob.
cust_analysis <- cus_o_oi %>%
  mutate(cust_year_of_birth = lubridate::year(cust_dob),
         cust_age = 2024 - cust_year_of_birth)



#Then, we created cust_group data frame to overview current age group of customers
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
cust_order_plot_gg <- ggplot(data = cust_analysis, aes(x = cust_age_range, y = order_item_quantity)) +
  geom_bar(stat = "identity", fill = "#1f77b4") +
  labs(x = "Age Range", y = "Total Order Items Quantity", title = "Total Order Items Quantity by Age Range")

this_filename_date <- as.character(Sys.Date())
# format the Sys.time() to show only hours and minutes 
this_filename_time <- as.character(format(Sys.time(), format = "%H_%M"))
ggsave(paste0("figures/cust_order_plot_",
              this_filename_date,"_",
              this_filename_time,".png"))








# To analyse the platform growth, we can directly use the data from cus_o_oi data frame
order_analysis <- cus_o_oi %>%
  mutate(
    year = lubridate::year(order_date),  # Extract year
    month = lubridate::month(order_date), # Extract month
    order_value = order_item_quantity * order_item_unit_price # to get the order value
  )

# Convert year and month to a date format
order_analysis$date <- as.Date(paste(order_analysis$year, order_analysis$month, "01", sep = "-"), "%Y-%m-%d")

# Generate a line graph to the growth
order_growth_plot <- ggplot(order_analysis, aes(x = date, y = order_value)) +
  geom_line(color = "lightblue") +
  geom_smooth(method = "loess", se = FALSE, color = "skyblue") +  # Add smoother
   geom_area(fill = "lightgrey", alpha = 0.3) +
  labs(x = "Date", y = "Order Value", title = "Order Value Growth per Quarter") +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "3 months") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Display the line plot
ggsave(paste0("figures/order_growth_plot_",
              this_filename_date,"_",
              this_filename_time,".png"))





# Query the data from the database and create a data frame for supplier. 
supplier_query <- "SELECT * FROM Suppliers"

supplier_df <- DBI::dbGetQuery(connection, supplier_query)
# Mutate new column to specify which suppliers define as "Entity" or "Individual"
supplier_df <- supplier_df %>%
  mutate(supplier_type = if_else(
    grepl("PLC|Inc|LLC|Ltd|Group", supplier_name, ignore.case = TRUE),
    "Entity", "Individual"
  ))

#Group the supplier types
supplier_analysis <- supplier_df %>%
  group_by(supplier_type) %>%
  summarize(count = n())

# Plot the counts using ggplot
supplier_plot <- ggplot(supplier_analysis, aes(x = supplier_type, y = count, fill = supplier_type)) +
  geom_bar(stat = "identity") +
  labs(x = "Supplier Type", y = "Count", title = "Supplier Analysis") +
  theme_minimal()

ggsave(paste0("figures/supplier_plot_",
              this_filename_date,"_",
              this_filename_time,".png"))





# Query the data from the database and create a data frame for payment. 
payment_query <- "SELECT * FROM Payments"

payment_df <- DBI::dbGetQuery(connection, payment_query)

# Analyse payment method

#Group the payment types
payment_analysis <- payment_df %>%
  group_by(payment_method) %>%
  summarize(count = n())

# Define colors for each payment method
colors <- c("skyblue", "palegreen2", "pink", "peachpuff1")

# Create pie chart using plotly
pie_chart_payment <- plot_ly(
  payment_analysis,
  labels = ~payment_method,
  values = ~count,
  type = "pie",
  marker = list(colors = colors)
) %>%
  layout(title = "Payment Analysis")

this_filename_date <- as.character(Sys.Date())
this_filename_time <- as.character(format(Sys.time(), format = "%H_%M"))
file_name_html <- paste0("figures/pie_chart_payment_", this_filename_date, "_", this_filename_time, ".html")
file_name_png <- paste0("figures/pie_chart_payment_", this_filename_date, "_", this_filename_time, ".png")

saveWidget(pie_chart_payment, file = file_name_html, selfcontained = TRUE)
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










