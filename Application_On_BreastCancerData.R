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

# Add free column of 1s
df <- cbind(free = 1, df)

# Declare initial values
init = rep(.1, ncol(df)-1)

# print((df[c("Class")]))


# Remove unwanted levels in Class for numeric computation
df$Class <- as.numeric((df$Class))

print(unique(df[c("Class")]))

# Replace all 2s to 0 in target variable
df["Class"][df["Class"] == "2"] <- "0"

print(unique(df[c("Class")]))
print((df["Class"]))

# Change all values/columns to numeric from levels
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
temp_df <- df[1:ncol(df)-1]
temp_df[ncol(df)] <- droplevels(df["Class"])
print(temp_df)

# Turn df into a matrix to make compatible with algorithm
df <- data.matrix(temp_df)
print((df))

# Stochastic Newton
print(tail(stochastic_newton_algo(df,init))) # Output gives lots on NaNs (could be dividing by 0?)

# Truncated Stochastic Netwon
print(tail(trunc_stochastic_newton_algo(df, 1/4, 1/2, init)))

# Model from R
# model2 <- glm(Class ~ Cl.thickness + Cell.size + Cell.shape + Marg.adhesion + Epith.c.size + Bare.nuclei +Bl.cromatin + Normal.nucleoli + Mitoses, data = BreastCancer, family = binomial)
# print(model2$coefficients)

# Stochastic Gradient Descent
print(tail(sgd2(df)))


