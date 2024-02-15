
# Install and load the MASS package if not already installed
if (!requireNamespace("MASS", quietly = TRUE)) {
  install.packages("MASS")
}
library(MASS)

# Define the transition matrix in the problem in the video:
P <- matrix(c(3/5, 2/5, 1/2, 1/2), nrow = 2, byrow = TRUE)

# Compute eigenvalues and eigenvectors of the transpose of the transition matrix
eigen_decomp <- eigen(t(P))


# The steady-state probabilities are given by the eigenvector corresponding to eigenvalue 1
pi <- eigen_decomp$vectors[, which.max(eigen_decomp$values == 1)]

# Normalize the probabilities to sum up to 1
pi <- pi / sum(pi)

# Print the steady-state probabilities
print(pi)



#Take n--> inf:

n<-1000
P_temp<-P
for(i in 1:n){
  P_temp<-P_temp%*%P
  print(paste0("Iteration: ", i))
  print(P_temp)
}



##############################First, download stock prices (SPY - S&P500 ETF)
# Load necessary libraries
library(quantmod)
library(dplyr)

# Download SPY stock data
getSymbols("SPY", from = "1900-01-01")

# Convert to dataframe
spy_data <- data.frame(Date = index(SPY), SPY)


# Extract necessary columns
spy_data <- spy_data %>%
  select(Date, SPY.Adjusted) %>%
  rename(Closing_Price = SPY.Adjusted)

# Calculate daily price change
spy_data <- spy_data %>%
  mutate(Daily_Change = Closing_Price - lag(Closing_Price),
         Price_Change_Direction = ifelse(Daily_Change > 0, "UP", "DOWN"),
         Prev_Price_Change_Direction = ifelse(lag(Daily_Change) > 0, "UP", "DOWN"))

# Filter out NA values
spy_data <- na.omit(spy_data)

# Select necessary columns
spy_data <- spy_data %>%
  select(Date, Price_Change_Direction, Prev_Price_Change_Direction)


#############################Now compute the transition probability matrix:
# Load required libraries
library(dplyr)

# Calculate transition probabilities
transition_probabilities <- spy_data %>%
  group_by(Prev_Price_Change_Direction, Price_Change_Direction) %>%
  summarise(transition_count = n()) %>%
  mutate(probability = transition_count / sum(transition_count))

# Create the transition probability matrix P
P <- matrix(0, nrow = 2, ncol = 2)
P[1, 1] <- transition_probabilities$probability[which(transition_probabilities$Prev_Price_Change_Direction == "UP" & transition_probabilities$Price_Change_Direction == "UP")]
P[1, 2] <- transition_probabilities$probability[which(transition_probabilities$Prev_Price_Change_Direction == "UP" & transition_probabilities$Price_Change_Direction == "DOWN")]
P[2, 1] <- transition_probabilities$probability[which(transition_probabilities$Prev_Price_Change_Direction == "DOWN" & transition_probabilities$Price_Change_Direction == "UP")]
P[2, 2] <- transition_probabilities$probability[which(transition_probabilities$Prev_Price_Change_Direction == "DOWN" & transition_probabilities$Price_Change_Direction == "DOWN")]

# Set row names and column names
rownames(P) <- c("UP", "DOWN")
colnames(P) <- c("UP", "DOWN")


####################################### Now run the previous code, only on
#the ACTUAL stock data:


#Approach 1: Eigenvalues of Transpose of Transition Matrix
# Compute eigenvalues and eigenvectors of the transpose of the transition matrix
eigen_decomp <- eigen(t(P))
# The steady-state probabilities are given by the eigenvector corresponding to eigenvalue 1
pi <- eigen_decomp$vectors[, which.max(eigen_decomp$values == 1)]
# Normalize the probabilities to sum up to 1
pi <- pi / sum(pi)
# Print the steady-state probabilities
print(pi)

#Approach 2: Limiting the multiplication of the transition matrix:
#Take n--> inf:
n<-1000
P_temp<-P
for(i in 1:n){
  P_temp<-P_temp%*%P
  print(paste0("Iteration: ", i))
  print(P_temp)
}

print(P_temp)