smoke <- c("Never", "Never", "Heavy", "Never", "Occasionally",
           "Never", "Never", "Regularly", "Regularly", "No")

table(smoke)

smoke2 <- factor(smoke)

smoke2
levels(smoke2)

# remove the invalid no

smoke2 <- factor(smoke, levels = c("Never", "Occasionally",
                                   "Regularly", "Heavy"),
                 ordered = TRUE)

# Check the results:
smoke2
levels(smoke2)
table(smoke2)

smoke[which(is.na(smoke2))]

smoke2 <- addNA(smoke2)

# change level name

levels(smoke2)[5] <- "Invalid answer"

# drop levels

smoke2 <- factor(smoke, levels = c("Never", "Occasionally",
                                   "Regularly", "Heavy",
                                   "Constantly"),
                 ordered = TRUE)
levels(smoke2)
smoke2 <- droplevels(smoke2)
levels(smoke2)

# change levels order

smoke2 <- factor(smoke2, levels = c("Heavy", "Regularly",
                                    "Occasionally", "Never"))

# Check the results:
levels(smoke2)

# combine levels

levels(smoke2)[1:3] <- "Yes"

# Check the results:
levels(smoke2)

