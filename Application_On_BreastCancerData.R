# Data set from https://machinelearningmastery.com/machine-learning-datasets-in-r/
# install.packages("mlbench")
library(pracma)
library(mlbench)
# Wisconsin Breast Cancer Database
data(BreastCancer)
dim(BreastCancer)
print(head(BreastCancer))

# Put BreastCancer data into dataframe
df <- data.frame(BreastCancer)
# Drop the Id column since it wont be used in our model
df <- subset(df, select = -c(Id))

# Remove NA values by removing row
df <- na.omit(df)

# Add free column of 1s
df <- cbind(free = 1, df)

# Turn all the columns numeric
turnNumeric <- function(x) (as.numeric(x))
df[] <- lapply(df, turnNumeric)

# Turn all 1s to 0 and all 2s to 1
transformClasses <- function(x) {
  return(x-1)
}

df$Class <- sapply(df$Class, transformClasses)

cleaned_df <- df

# Partition the data
training_df <- cleaned_df[1:400,]
testing_df <- cleaned_df[400:nrow(df),]

training_matrix <- data.matrix(training_df)

# Declare initial values
init = rep(1, ncol(training_matrix)-1)
n = nrow(training_matrix)

# Function for calculating probability from coefficients
coefsToLabel <- function(inputs, coefs) {
  target <- inputs[length(inputs)]
  inputs <- inputs[1:length(inputs)-1]
  
  # print(array(unlist(t(coefs))))
  # print(array(unlist(inputs)))
  # Take the dot product of the two vectors to get a real value
  # get the log odds then convert to odds
  # and convert the 1D df objects to arrays
  pi <- exp(array(unlist(t(coefs))) %*% array(unlist(inputs)))
  probability <- pi/(1+pi)

  # Return the predicted class
  if (probability < .5) {
    return(0)
  }
  else {
    return(1)
  }
}

coefsToError <- function(inputs, coefs, correct, total) {
  target <- inputs[length(inputs)]
  inputs <- inputs[1:length(inputs)-1]
  
  # print(array(unlist(t(coefs))))
  # print(array(unlist(inputs)))
  # Take the dot product of the two vectors to get a real value
  # get the log odds then convert to odds
  # and convert the 1D df objects to arrays
  pi <- exp(array(unlist(t(coefs))) %*% array(unlist(inputs)))
  probability <- pi/(1+pi)
  # print(probability)
  # Return the predicted class
  if (probability < .5) {
    if (target == 0) {
      correct = correct + 1
    }
  }
  else {
    if (target == 1) {
      correct = correct + 1
    }
  }
  total = total + 1
  return(c(correct, total))
}

SNA_output <- stochastic_newton_algo(training_matrix,init)
TSNA_output <- trunc_stochastic_newton_algo(training_matrix, 10^(-10), .49, init)
SGD_output <- sgd2(training_matrix, init, eta = .2, num_iter = n)
BSGD_output <- sgd_batched(training_matrix, init, eta = .01, num_iter = n, batch_num = 50)

df_training_errors <- data.frame()
# Plot the errors during learning process
methods <- list(SNA_output, TSNA_output, SGD_output, BSGD_output)
for (method in methods) {
  correct = 0
  total = 0
  errors = c()
  for (i in 1:(nrow(method)-1)) {
    coefs <- array(method[i,])
    output <- coefsToError(training_matrix[i,], coefs, correct, total)
    correct = output[1]
    total = output[2]
    error = 1 - correct/total
    errors <- c(errors, error)
  }
  # Check if df is empty or not
  if (length(df_training_errors) == 0) {
    df_training_errors <- errors
  }
  else {
    print(nrow(df_training_errors))
    print(nrow(errors))
    df_training_errors <- cbind(df_training_errors, errors)
  }
}

col_names <- c("SNA_error", "TSNA_error", "SGD_error", "BSGD_error")
# Rename the columns
for (i in 1:length(col_names)) {
  colnames(df_training_errors)[i] <- col_names[i]
}
# names(df_training_errors) <- col_names

# Make the plots
n <- seq(1, nrow(training_df)-1, by = 1)

#plot the first data series using plot()
plot(n, df_training_errors[,1], type="o", col="forestgreen", pch=".", ylab="Error", lty=1)

#add second data series to the same chart using points() and lines()
points(n, df_training_errors[,2], col="orange", pch=".")
lines(n, df_training_errors[,2], col="orange",lty=1)

#add third data series to the same chart using points() and lines()
points(n, df_training_errors[,3], col="dark red",pch=".")
lines(n, df_training_errors[,3], col="dark red", lty=1)

#add fourth data series to the same chart using points() and lines()
points(n, df_training_errors[,4], col="blue",pch=".")
lines(n, df_training_errors[,4], col="blue", lty=1)

legend(x="right",legend=c("SNA","TSNA","SGD", "BSGD"), col=c("forestgreen","orange","dark red", "blue"),
       pch=c(".",".",".","."),lty=c(1,1,1,1), ncol=1)

p <- ggplot() + 
  geom_boxplot(aes(x = "SNA", y=df_training_errors[,1]), outlier.colour="red", outlier.shape=8,
               outlier.size=1) + 
  geom_boxplot(aes(x = "TSNA", y=df_training_errors[,2]), outlier.colour="red", outlier.shape=8,
               outlier.size=1) +
  geom_boxplot(aes(x = "SGD", y=df_training_errors[,3]), outlier.colour="red", outlier.shape=8,
               outlier.size=1) +
  geom_boxplot(aes(x = "BSGD", y=df_training_errors[,4]), outlier.colour="red", outlier.shape=8,
               outlier.size=1) +
  labs(x = "Methods", y = "Training Error")
print(p)

# Now test accuracy on testing parition
# Stochastic Newton
SNA_coefs <- tail(SNA_output, n=1)
SNA_preds <- unlist(apply(testing_df, MARGIN = 1, FUN = coefsToLabel, coefs=SNA_coefs))

# Truncated Stochastic Newton
TSNA_coefs <- tail(TSNA_output, n=1)
TSN_preds <- unlist(apply(testing_df, MARGIN = 1, FUN = coefsToLabel, coefs=TSNA_coefs))

# Stochastic gradient descent
SGD_coefs <- tail(SGD_output, n=1)
SGD_preds <- unlist(apply(testing_df, MARGIN = 1, FUN = coefsToLabel, coefs=SGD_coefs))

# Batch stochastic gradient descent
BSGD_coefs <- tail(BSGD_output, n=1)
BSGD_preds <- unlist(apply(testing_df, MARGIN = 1, FUN = coefsToLabel, coefs=BSGD_coefs))

model2 <- glm(Class ~ ., data = cleaned_df, family = "binomial")
print(model2$coefficients)

# Combine all the predicted classes among all solution methods
df_preds <- data.frame(SN_preds, TSN_preds, SGD_preds, BSGD_preds, testing_df[1:nrow(testing_df), ncol(testing_df)])

# Get the classification accuracy
getAccuracy <- function(predictions, actuals) {
  correct = 0
  for (i in 1:length(predictions)) {
    if (predictions[i] == actuals[i]) {
      correct = correct + 1
    }
  }
  return(correct)
}

number_correct <- apply(df_preds[,1:ncol(df_preds)-1], MARGIN = 2, FUN = getAccuracy, actuals=df_preds[,ncol(df_preds)])

accuracy <- number_correct/nrow(df_preds)
print(accuracy)
