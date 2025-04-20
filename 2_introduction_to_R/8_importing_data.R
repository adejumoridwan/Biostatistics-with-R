
imported_data <- read.csv("2_introduction_to_R/bookstore.csv")

View(imported_data)
str(imported_data)

# # File path on Windows
# 
# C:\Users\Mans\Desktop\MyData\bookstore.csv
# 
# 
# On a Mac it would be
# 
# 
# /Users/Mans/Desktop/MyData/bookstore.csv
# 
# 
# And on Linux:
#   
# /home/Mans/Desktop/MyData/bookstore.csv



# Bookstore example
age <- c(28, 48, 47, 71, 22, 80, 48, 30, 31)
purchase <- c(20, 59, 2, 12, 22, 160, 34, 34, 29)
bookstore <- data.frame(age, purchase)

# Export to .csv:
write.csv(bookstore, "2_introduction_to_R/bookstore.csv")

# Export to .xlsx (Excel):
install.packages("openxlsx")
library(openxlsx)
write.xlsx(bookstore, "2_introduction_to_R/bookstore.xlsx")

