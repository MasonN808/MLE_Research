# Data set from https://machinelearningmastery.com/machine-learning-datasets-in-r/
# install.packages("mlbench")
library(pracma)
library(mlbench)
library(ISLR)
# Wisconsin Breast Cancer Database
data(Default)
dim(Default)
# print(levels(BreastCancer$Class))
# droplevels(BreastCancer$Class)
# print(levels(BreastCancer$Class))
print(head(Default))

# Put BreastCancer data into dataframe
df <- data.frame(Default)

# Remove NA values by removing row
df <- na.omit(df)

# put default as last column
df <- df[,c(2:ncol(df), 1)]

# Add free column of 1s
df <- cbind(free = 1, df)

# Turn all the columns numeric
# turnNumeric <- function(x) (as.numeric(x))
# df[] <- lapply(df, turnNumeric)

# Turn all 1s to 0 and all 2s to 1
transformBinary <- function(x) {
  if (x == "No") {
    return(0)
  }
  else {
    return(1)
  }
}

df$student <- sapply(df$student, transformBinary)
df$default <- sapply(df$default, transformBinary)

cleaned_df <- df

print(head(df))
print(nrow(df))

df_matrix <- data.matrix(df)

# Declare initial values
init = rep(1, ncol(df_matrix)-1)

# Stochastic Newton
print(tail(stochastic_newton_algo(df_matrix,init)))

# Truncated Stochastic Netwon
print(tail(trunc_stochastic_newton_algo(df_matrix, 10^(-10), .49, init)))

# Model from R

n = nrow(df_matrix)

# Stochastic Gradient Descent init, eta = .01, num_iter = n, batch_num = 10, exact = betas
print(tail(sgd2(df_matrix, init, eta = .01, num_iter = n)))

print(tail(sgd_batched(df_matrix, init, eta = .01, num_iter = n, batch_num = 50)))

model2 <- glm(default ~ ., data = cleaned_df, family = "binomial")
print(model2$coefficients)
# summary(model2)



