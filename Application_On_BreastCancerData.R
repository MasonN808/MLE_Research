# Data set from https://machinelearningmastery.com/machine-learning-datasets-in-r/
# install.packages("mlbench")
library(pracma)
library(mlbench)
# Wisconsin Breast Cancer Database
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

df <- cbind(free = 1, df)

# Declare initial values
# Default initial values
init = rep(.1, ncol(df)-1)

# Turn df into a matrix to make compatible with algorithm
df <- data.matrix(df)
print(head(df))

# Apply the stochastic newton algo
print(tail(stochastic_newton_algo(df,init))) # Output gives lots on NaNs (could be dividing by 0?)

# Apply the truncated stochastic netwon algo
print(tail(trunc_stochastic_newton_algo(df, 1/4, 1/2, init)))

model2 <- glm(Class ~ Cl.thickness + Cell.size + Cell.shape + Marg.adhesion + Epith.c.size + Bare.nuclei +Bl.cromatin + Normal.nucleoli + Mitoses, data = BreastCancer, family = binomial)
# print(model2$coefficients)


