decision_matrix <- matrix(c(-20, 60, 30, -5,
                            40, 50, 35, 0,
                            -50, 100, 45, -10,
                            12, 15, 15, 10), 
                          nrow = 4, 
                          byrow = TRUE,
                          dimnames = list(c("Corn", "Wheat", "Soya Beans", "Do Grazing"),
                                          c("Heavy Rainfall", "Moderate Rainfall", "Light Rainfall", "Drought")))

cat("\nDecision Matrix\n\n")
print(decision_matrix)

laplace <- rowMeans(decision_matrix)

cat("\nLaplace Method\n")
print(laplace)

# Maximization
max_index <- which.max(laplace)

# Minimization
min_index <- which.min(laplace)

op <- sprintf("\nThe optimal crop to plant is %s with a payoff of %.2f\n\n", names(max_index), laplace[max_index])
cat(op)

op <- sprintf("\nThe optimal crop to plant is %s with a cost of %.2f\n\n", names(min_index), laplace[min_index])
cat(op)
