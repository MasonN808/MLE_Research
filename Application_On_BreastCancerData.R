# Wisconsin Breast Cancer Database

# install.packages("mlbench")
library(pracma)
library(mlbench)
data(BreastCancer)
dim(BreastCancer)
levels(BreastCancer$Class)
head(BreastCancer)

# Put BreastCancer data into dataframe
df <- data.frame(BreastCancer)
# Drop the Id column since it wont be used in our model
df <- subset(df, select = -c(Id))

# Remove NA values by removing row
df <- na.omit(df)

# Declare initial values
# Default initial values
init = rep(.1, ncol(df)-1)

# Turn df into a matrix to make compatible with algorithm
df <- data.matrix(df)

# Apply the stochastic newton algo
print(stochastic_newton_algo(df,init)) # Output gives lots on NaNs (could be dividing by 0?)

# Apply the truncated stochastic netwon algo
print(tail(trunc_stochastic_newton_algo(df, 1/4, 1/2, init)))


