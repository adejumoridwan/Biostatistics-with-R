
# A function is a ready-made set of instructions - code - 
# that tells R to do something.

# Compute the mean age of bookstore customers
age <- c(28, 48, 47, 71, 22, 80, 48, 30, 31)
mean(age)

# Note that the code follows the pattern `function_name(variable_name)`: 
# the function's name is `mean` and the variable's name is `age`.

# Compute the correlation between the variables age and purchase
age <- c(28, 48, 47, 71, 22, 80, 48, 30, 31)
purchase <- c(20, 59, 2, 12, 22, 160, 34, 34, 29)
cor(age, purchase)

# Use ?<function-name> to check the documentation of a function
?cor