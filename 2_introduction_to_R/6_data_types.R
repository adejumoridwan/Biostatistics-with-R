
# In programming, a variable's _data type_ describes what kind of object is assigned to it.
# 
# R has six basic data types. For most people, it suffices to know about the first three in the list below:
# 
# * `numeric`: numbers like `1` and `16.823` (sometimes also called `double`).
# * `logical`: true/false values (boolean): either `TRUE` or `FALSE`.
# * `character`: text, e.g. `"a"`, `"Hello! I'm Ada."` and `"name@domain.com"`.
# * `integer`: integer numbers, denoted in R by the letter `L`: `1L`, `55L`.
# * `complex`: complex numbers, like `2+3i`. Rarely used in statistical work.
# * `raw`: used to hold raw bytes. 

# To check what type of object a variable is, 
# you can use the `class` function:

x <- 6
y <- "Scotland"
z <- TRUE
a <- 55L
class(x)
class(y)
class(z)
class(a)

# What happens if we use `class` on a vector?

numbers <- c(6, 9, 12)
class(numbers)

# What of class on objects of different data types

all_together <- c(x, y, z, a)
all_together
class(all_together)

# Types of table

# * `matrix`: a table where all columns must contain objects of the same type (e.g. all `numeric` or all `character`). Uses less memory than other types and allows for much faster computations, but is difficult to use for certain types of data manipulation, plotting and analyses.
# * `data.frame`: the most common type, where different columns can contain different types (e.g. one `numeric` column, one `character` column).
# * `data.table`: an enhanced version of `data.frame`.
# * `tbl_df` ("tibble"): another enhanced version of `data.frame`.

# First, an example of data stored in a matrix:
?WorldPhones
class(WorldPhones)
View(WorldPhones)
# Next, an example of data stored in a data frame:
?airquality
class(airquality)
View(airquality)
# Finally, an example of data stored in a tibble:
library(ggplot2)
?msleep
class(msleep)
View(msleep)

# Coercion

TRUE + 5
v1 <- c(TRUE, 5)
v1

"One" + 5
v2 <- c("One", 5)
v2

as.logical(1)           # Should be TRUE
as.logical("FALSE")     # Should be FALSE
as.numeric(TRUE)        # Should be 1
as.numeric("2.718282")  # Should be numeric 2.718282
as.character(2.718282)  # Should be the string "2.718282"
as.character(TRUE)      # Should be the string "TRUE"


# Where coercion fails

as.numeric("two")                   # Should be 2
as.numeric("1+1")                   # Should be 2
as.numeric("2,718282")              # Should be numeric 2.718282
as.logical("Vaccines cause autism") # Should be FALSE
