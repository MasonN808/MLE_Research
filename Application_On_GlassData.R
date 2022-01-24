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

# init = rep(.1, ncol(df))

model1 <- glm(Type ~ RI + Na + Mg + Al + Si + K + Ca + Ba + Fe, data = df, family = binomial)

init = model1$coefficients + .05*rnorm(ncol(df), 0, 1)

df <- cbind(free = 1, df)
# Turn df into a matrix to make compatible with algorithm
df <- data.matrix(df)
print(df)


# Apply the stochastic newton algo
print(tail(stochastic_newton_algo(df,init))) # Output gives lots on NaNs (could be dividing by 0?)

# Apply the truncated stochastic netwon algo
print(tail(trunc_stochastic_newton_algo(df, 10^(-10), .49, init)))

# model1 <- glm(Type ~  RI + Na + Mg + Al + Si + K + Ca + Ba + Fe, data = Glass, family = binomial)
print(model1$coefficients)
