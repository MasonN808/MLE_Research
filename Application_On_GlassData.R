# Data set from https://machinelearningmastery.com/machine-learning-datasets-in-r/
library(pracma)
library(mlbench)
# Glass Identification Database
data(Glass)
dim(Glass)
levels(Glass$Type)
head(Glass)

# Put Glass data into dataframe
df <- data.frame(Glass)

# Remove NA values by removing row
df <- na.omit(df)

# Declare initial values
# Default initial values
model1 <- glm(Type~RI + Na + Mg + Al + Si + K + Ca + Ba + Fe, data = Glass, family = binomial)
init = model1$coefficients + rnorm(ncol(df))
# init = rep(.1, ncol(df)-1)

df <- cbind(free = 1, df)

# Turn df into a matrix to make compatible with algorithm
df <- data.matrix(df)

# Apply the stochastic newton algo
stochastic_newton_algo(df,init) # Output gives lots on NaNs (could be dividing by 0?)

# Apply the truncated stochastic netwon algo
print(tail(trunc_stochastic_newton_algo(df, 1/4, 1/2, init)))

model1 <- glm(Type~RI + Na + Mg + Al + Si + K + Ca + Ba + Fe, data = Glass, family = binomial)
model1$coefficients

