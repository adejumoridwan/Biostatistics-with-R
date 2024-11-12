# ### Vectors 
# Almost invariably, you'll deal with more than one figure at a time 
# in your analyses. For instance, we may have a list of the ages of 
# customers at a bookstore:

# Instead of adding like this
age_person_1 <- 28
age_person_2 <- 48
age_person_3 <- 47
# ...and so on

# We use a vector, where each item is called an element

age <- c(28, 48, 47, 71, 22, 80, 48, 30, 31)

# We can change the value of an element 
age_months <- age * 12

# We can create a data frame from two vectors

purchase <- c(20, 59, 2, 12, 22, 160, 34, 34, 29)

bookstore <- data.frame(age, purchase)

# Longer vector

distances <- c(687, 5076, 7270, 967, 6364, 1683, 9394, 5712, 5206,
               4317, 9411, 5625, 9725, 4977, 2730, 5648, 3818, 8241,
               5547, 1637, 4428, 8584, 2962, 5729, 5325, 4370, 5989,
               9030, 5532, 9623)
distances