
# Use the airquality data

# Extract the Temp vector:
airquality$Temp
# Compute the mean temperature:
mean(airquality$Temp)

airquality$Temp[5]

# First, we check the order of the columns:
names(airquality)
# We see that Temp is the 4th column.
airquality[5, 4]    # The 5th element from the 4th column,
# i.e. the same as airquality$Temp[5]
airquality[5,]      # The 5th row of the data
airquality[, 4]     # The 4th column of the data, like airquality$Temp
airquality[[4]]     # The 4th column of the data, like airquality$Temp
airquality[, c(2, 4, 6)] # The 2nd, 4th and 6th columns of the data
airquality[, -2]    # All columns except the 2nd one
airquality[, c("Temp", "Wind")] # The Temp and Wind columns

# Using the $ sign

age <- c(28, 48, 47, 71, 22, 80, 48, 30, 31)
purchase <- c(20, 59, 2, 12, 22, 160, 34, 34, 29)
bookstore <- data.frame(age, purchase)

bookstore$age[2] <- 18
# or
bookstore[2, 1] <- 18

bookstore$age <- bookstore$age * 12

bookstore$visit_length <- c(5, 2, 20, 22, 12, 31, 9, 10, 11)
bookstore

# Using conditions

max(airquality$Temp)

which.max(airquality$Temp)

airquality[120,]

airquality[which.max(airquality$Temp),]

airquality$Temp > 90

airquality[airquality$Temp > 90, ]

airquality$Hot <- airquality$Temp > 90

# Useful logical operators when stating conditions in R
a <- 3
b <- 8
a == b     # Check if a equals b
a > b      # Check if a is greater than b
a < b      # Check if a is less than b
a >= b     # Check if a is equal to or greater than b
a <= b     # Check if a is equal to or less than b
a != b     # Check if a is not equal to b
is.na(a)   # Check if a is NA
a %in% c(1, 4, 9) # Check if a equals at least one of 1, 4, 9

which(airquality$Temp > 90)

all(airquality$Temp > 90)

any(airquality$Temp > 90)

sum(airquality$Temp > 90)

mean(airquality$Temp > 90)

# We can combine conditions either using the & or | operator

a <- 3
b <- 8
# Is a less than b and greater than 1?
a < b & a > 1
# Is a less than b and equal to 4?
a < b & a == 4
# Is a less than b and/or equal to 4?
a < b | a == 4
# Is a equal to 4 and/or equal to 5?
a == 4 | a == 5




