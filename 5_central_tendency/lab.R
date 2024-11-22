library(MASS)
library(psych)

# Arithmetic Mean
mean(birthwt$age)

# Geometric Mean
geometric.mean(birthwt$age)

# Harmonic mean
harmonic.mean(birthwt$age)

library(DescTools)

# Mode
mode(birthwt$age)

# Median
median(birthwt$age)

library(e1071)
skewness(birthwt$age)
