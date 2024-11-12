
# A variable is a name used to store data, 
# so that we can refer to a dataset when we write code. 
# As the name *variable* implies, what is stored can change over time.
x <- 4

x + 1
x + x


# In RStudio, you can also create the assignment operator `<-` 
# by using the keyboard shortcut Alt+- 
# (i.e. press Alt and the - button at the same time).

x <- 1 + 2 + 3 + 4

# In rare cases, you may want to switch the direction of the arrow, 
# so that the variable names is on the right-hand side. 
# This is called right-assignment and works just fine too:
2 + 2 -> y

# What's in a name?

# Give clear names instead of single letter names
y <- 100
z <- 20
x <- y - z

income <- 100
taxes <- 20
net_income <- income - taxes

# In the Script panel in RStudio, you can comment and uncomment 
# (i.e. remove the `#` symbol) a row by pressing Ctrl+Shift+C on your keyboard. 
# This is particularly useful if you wish to comment or uncomment several lines - 
# simply select the lines and press Ctrl+Shift+C.