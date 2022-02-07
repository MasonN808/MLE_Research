# Data set from https://machinelearningmastery.com/machine-learning-datasets-in-r/
# install.packages("mlbench")
library(pracma)
library(mlbench)
# Wisconsin Breast Cancer Database
data(BreastCancer)
dim(BreastCancer)
print(levels(BreastCancer$Class))
droplevels(BreastCancer$Class)
print(levels(BreastCancer$Class))
head(BreastCancer)

# Put BreastCancer data into dataframe
df <- data.frame(BreastCancer)
# Drop the Id column since it wont be used in our model
df <- subset(df, select = -c(Id))

# Remove NA values by removing row
df <- na.omit(df)

# Add free column of 1s
df <- cbind(free = 1, df)

# Make numeric
df$Class <- as.numeric((df$Class))

# Replace all 2s to 0 in target variable
df["Class"][df["Class"] == "2"] <- "0"

# Make numeric agian (needed)
df$Class <- as.numeric((df$Class))


# Change all values/columns to numeric from levels (just in case)
df[] <- lapply(df, function(x) {
  if(is.factor(x)){
    as.numeric(as.character(x))
  }
  else{
    x
  }
})
sapply(df, class)


# Apply min_max normalization to predictor variables, excluding free column
# stochastic_newton_algo needs normalization to run
df[3:ncol(df)-1] <- min_max_norm(df[3:ncol(df)-1])

# remove column and replace with transformed target variables from {1,2} --> {0,1}
df["Class"] <- droplevels(df["Class"])
temp_df <- df[1:ncol(df)-1]
temp_df[ncol(df)] <- df["Class"]
print(head(temp_df))
temp_targets <- df["Class"]

# Turn df into a matrix to make compatible with algorithm
df <- data.matrix(temp_df)
targets <- data.matrix(temp_df[ncol(df)])
cbind(df, targets)
print(head(df))

# Declare initial values
init = rep(1, ncol(df)-1)

# Stochastic Newton
print(tail(stochastic_newton_algo(df,init))) # Output gives lots on NaNs (could be dividing by 0?)

# Truncated Stochastic Netwon
print(tail(trunc_stochastic_newton_algo(df, 10^(-10), .49, init)))

# Model from R
# model2 <- glm(Class ~ Cl.thickness + Cell.size + Cell.shape + Marg.adhesion + Epith.c.size + Bare.nuclei +Bl.cromatin + Normal.nucleoli + Mitoses, data = BreastCancer, family = binomial)
# print(model2$coefficients)

# Stochastic Gradient Descent
print(tail(sgd2(df)))


