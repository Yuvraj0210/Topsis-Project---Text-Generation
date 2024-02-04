#Step 0- Install Topsis package
install.packages("topsis")
library(topsis)

#Step 1- Defining Criteria
criteria <- c("grammar", "relevance", "fluency", "diversity", "sentences_flow")

#Step 2- Decision Matrix
decision_matrix <- matrix(c(
  0.751, 0.851, 0.802, 0.901, 0.703,
  0.803, 0.906, 0.752, 0.855, 0.759,
  0.604, 0.708, 0.651, 0.758, 0.800,
  0.706, 0.806, 0.708, 0.802, 0.651
), ncol = 5, byrow = TRUE)

#Step 3-Calculate RMSE for each criterion
rmse <- function(x) sqrt(mean(x^2))

# Step 3.1: Normalize the Matrix using RMSE
normalized_matrix <- decision_matrix / apply(decision_matrix, 2, rmse)

# Step 4: Determine Weighted Scores
# All the categories should have equal importance for this text generation
weights <- c(0.2, 0.2, 0.2, 0.2, 0.2)

#Step 5- Generate Weighted Decision Matrix
#WDM  = Weight* normalised_matrix
# Generate Weighted Decision Matrix
weighted_decision_matrix <- normalized_matrix * weights

#Step 5.1- Display the weighted decision matrix
print("Weighted Decision Matrix:")
print(weighted_decision_matrix)

# Step 6- Identify Ideal and Negative Ideal Solutions
ideal_solution <- apply(weighted_decision_matrix, 2, max)
negative_ideal_solution <- apply(weighted_decision_matrix, 2, min)

# Step 7- Calculate Euclidean Distance from Ideal Solutions
distance_ideal <- sqrt(colSums((weighted_decision_matrix - ideal_solution)^2))
distance_negative_ideal <- sqrt(colSums((weighted_decision_matrix - negative_ideal_solution)^2))

# Step 8- Calculate Performance Score (Relative Closeness)
performance_score <- distance_negative_ideal / (distance_ideal + distance_negative_ideal)

# Step 8.1-Display the Performance Score
print("Performance Score (Relative Closeness):")
print(performance_score)

# Step 9- Rank Alternatives in Increasing Order
ranked_alternatives_increasing <- order(performance_score, decreasing = FALSE)

# Step 9.1- Display the ranked alternatives in increasing order
print("Ranked Alternatives (Increasing Order):")
print(ranked_alternatives_increasing)

