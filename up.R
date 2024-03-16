if (!requireNamespace("RSQLite", quietly = TRUE)) {
  install.packages("RSQLite")
}
library(RSQLite)
if (!requireNamespace("readr", quietly = TRUE)) {
  install.packages("readr")
}
library(readr)
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}
library(dplyr)
connection <- RSQLite::dbConnect(RSQLite::SQLite(),"ecomm.db")
old_Advertisements <- dbReadTable(connection, "Advertisements")
old_Customers <- dbReadTable(connection, "Customers")
old_Order_items <- dbReadTable(connection, "Order_items")
old_Orders <- dbReadTable(connection, "Orders")
old_Payments <- dbReadTable(connection, "Payments")
old_Products <- dbReadTable(connection, "Products")
old_Shipments <- dbReadTable(connection, "Shipments")
old_Suppliers <- dbReadTable(connection, "Suppliers")

Advertisements <- readr::read_csv("dataset/Advertisements.csv")
Customers <- readr::read_csv("dataset/Customers.csv")
Order_items <- readr::read_csv("dataset/Order_items.csv")
Orders <- readr::read_csv("dataset/Orders.csv")
Payments <- readr::read_csv("dataset/Payments.csv")
Products <- readr::read_csv("dataset/Products.csv")
Shipments <- readr::read_csv("dataset/Shipments.csv")
Suppliers <- readr::read_csv("dataset/Suppliers.csv")

Advertisements$ads_id <- as.integer(Advertisements$ads_id)
Customers$cust_id <- as.character(Customers$cust_id)
Order_items$order_item_id <- as.integer(Order_items$order_item_id)
Orders$order_id <- as.integer(Orders$order_id)
Payments$payment_id <- as.character(Payments$payment_id)
Products$product_id <- as.integer(Products$product_id)
Shipments$shipment_id <- as.character(Shipments$shipment_id)
Suppliers$supplier_id <- as.character(Suppliers$supplier_id)


new_Advertisements <- anti_join(Advertisements, old_Advertisements, by = "ads_id")
new_Customers <- anti_join(Customers, old_Customers, by = "cust_id")
new_Order_items <- anti_join(Order_items, old_Order_items, by = "order_item_id")
new_Orders <- anti_join(Orders, old_Orders, by = "order_id")
new_Payments <- anti_join(Payments, old_Payments, by = "payment_id")
new_Products <- anti_join(Products, old_Products, by = "product_id")
new_Shipments <- anti_join(Shipments, old_Shipments, by = "shipment_id")
new_Suppliers <- anti_join(Suppliers, old_Suppliers, by = "supplier_id")


this_filename_date <- as.character(Sys.Date())
this_filename_time <- as.character(format(Sys.time(), format = "%H_%M"))



if(nrow(new_Advertisements) == 0) {
  message("No new records to update.")
} else {
  dbWriteTable(connection, "Advertisements", new_Advertisements, append = TRUE, row.names = FALSE)
  
  readr::write_csv(new_Advertisements, paste0("old_dataset/Advertisements_",
                                              this_filename_date,"_",
                                              this_filename_time,".csv"))
}

if(nrow(new_Customers) == 0) {
  message("No new records to update.")
} else {
  dbWriteTable(connection, "Customers", new_Customers, append = TRUE, row.names = FALSE)
  readr::write_csv(new_Customers, paste0("old_dataset/Customers_",
                                         this_filename_date,"_",
                                         this_filename_time,".csv"))
}

if(nrow(new_Order_items) == 0) {
  message("No new records to update.")
} else {
  dbWriteTable(connection, "Order_items", new_Order_items, append = TRUE, row.names = FALSE)
  readr::write_csv(new_Order_items, paste0("old_dataset/Order_items_",
                                           this_filename_date,"_",
                                           this_filename_time,".csv"))
}

if(nrow(new_Orders) == 0) {
  message("No new records to update.")
} else {
  dbWriteTable(connection, "Orders", new_Orders, append = TRUE, row.names = FALSE)
  readr::write_csv(new_Orders, paste0("old_dataset/Orders_",
                                      this_filename_date,"_",
                                      this_filename_time,".csv"))
}

if(nrow(new_Payments) == 0) {
  message("No new records to update.")
} else {
  dbWriteTable(connection, "Payments", new_Payments, append = TRUE, row.names = FALSE)
  readr::write_csv(new_Payments, paste0("old_dataset/Payments_",
                                        this_filename_date,"_",
                                        this_filename_time,".csv"))
}

if(nrow(new_Products) == 0) {
  message("No new records to update.")
} else {
  dbWriteTable(connection, "Products", new_Products, append = TRUE, row.names = FALSE)
  readr::write_csv(new_Products, paste0("old_dataset/Products_",
                                        this_filename_date,"_",
                                        this_filename_time,".csv"))
}

if(nrow(new_Shipments) == 0) {
  message("No new records to update.")
} else {
  dbWriteTable(connection, "Shipments", new_Shipments, append = TRUE, row.names = FALSE)
  readr::write_csv(new_Shipments, paste0("old_dataset/Shipments_",
                                         this_filename_date,"_",
                                         this_filename_time,".csv"))
}

if(nrow(new_Suppliers) == 0) {
  message("No new records to update.")
} else {
  dbWriteTable(connection, "Suppliers", new_Suppliers, append = TRUE, row.names = FALSE)
  readr::write_csv(new_Suppliers, paste0("old_dataset/Suppliers_",
                                         this_filename_date,"_",
                                         this_filename_time,".csv"))
}
