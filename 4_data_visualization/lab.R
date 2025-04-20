
#------------------------------------------------------
# Histogram
#------------------------------------------------------

# Load required library
library(ggplot2)

# Load the ChickWeight dataset (it's built into R)
data(ChickWeight)

# Create the histogram
ggplot(ChickWeight, aes(x = weight)) +
  geom_histogram(binwidth = 20, fill = "skyblue", color = "black") +
  labs(title = "Histogram of Chick Weights",
       x = "Weight (grams)",
       y = "Frequency") +
  theme_minimal()

#----------------------------------------------------
# Bar Chart
#------------------------------------------------------

# Load required libraries
library(dplyr)

# Load the msleep dataset (it's built into R)
data(msleep)

# Create a summary of sleep time by order
sleep_summary <- msleep %>%
  group_by(order) %>%
  summarise(avg_sleep = mean(sleep_total, na.rm = TRUE)) %>%
  arrange(desc(avg_sleep)) |> 
  top_n(5)



# Create the bar chart
ggplot(sleep_summary, aes(x = order, y = avg_sleep, fill = order)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Average Total Sleep Time by Mammalian Order",
       x = "Order",
       y = "Average Total Sleep (hours)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#----------------------------------------------------
# Box Plot
#------------------------------------------------------

# Load the ToothGrowth dataset (built-in)
data("ToothGrowth")

# Create the box plot
ggplot(ToothGrowth, aes(x = supp, y = len, fill = supp)) +
  geom_boxplot() +
  labs(title = "Tooth Growth by Supplement Type",
       x = "Supplement Type",
       y = "Tooth Length",
       fill = "Supplement Type") +
  theme_minimal()

#--------------------------------------------------
# Pie Chart
#------------------------------------------------------

# Create a sample medical dataset
diagnoses <- data.frame(
  Condition = c("Hypertension", "Diabetes", "Asthma", "Arthritis", "Depression"),
  Count = c(150, 100, 75, 50, 25)
)

# Calculate percentages
diagnoses$Percentage <- diagnoses$Count / sum(diagnoses$Count) * 100

# Create the pie chart
ggplot(diagnoses, aes(x = "", y = Count, fill = Condition)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Distribution of Patient Diagnoses",
       fill = "Medical Condition") +
  theme_void() +
  theme(legend.position = "right") +
  geom_text(aes(label = sprintf("%.1f%%", Percentage)), 
            position = position_stack(vjust = 0.5))

#---------------------------------------------------
# Scatter Plot
#------------------------------------------------------

library(MASS)  # For the birthwt dataset

# Load the birthwt dataset (low birth weight study)
data(birthwt)

# Create the scatter plot
ggplot(birthwt, aes(x = lwt, y = bwt)) +
  geom_point(alpha = 0.6) +
  labs(title = "Mother's Weight vs Baby's Birth Weight",
       x = "Mother's Weight (pounds)",
       y = "Baby's Birth Weight (grams)") +
  theme_minimal()


#------------------------------------------------------
# Line Graph
#------------------------------------------------------

library(zoo)

# Load the AirPassengers dataset
df = data(AirPassengers)

# Convert the time series to a data frame
df <- data.frame(
  date = as.Date(time(AirPassengers)),
  passengers = as.vector(AirPassengers)
)

# Create the time series plot
ggplot(df, aes(x = date, y = passengers)) +
  geom_line(color = "red") +
  labs(title = "Monthly Airline Passengers (1949-1960)",
       x = "Date",
       y = "Number of Passengers") +
  theme_minimal()

#------------------------------------------------------
# Frequency Polygon
#----------------------------------------------------------
# Load required libraries
library(ggplot2)

# Load the ToothGrowth dataset
data(ToothGrowth)

# Create the frequency polygon
ggplot(ToothGrowth, aes(x = len)) +
  geom_freqpoly(binwidth = 5, color = "blue") +
  labs(title = "Frequency Polygon of Tooth Length",
       x = "Tooth Length",
       y = "Frequency") +
  theme_minimal()

# Load required libraries
library(ggplot2)

# Load the ToothGrowth dataset
data(ToothGrowth)

# Create the histogram with frequency polygon
ggplot(ToothGrowth, aes(x = len)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 2, fill = "lightblue", color = "black") +
  geom_freqpoly(aes(y = after_stat(density)), binwidth = 2, color = "red") +
  labs(title = "Histogram and Frequency Polygon of Tooth Length",
       x = "Tooth Length",
       y = "Density") +
  theme_minimal()
