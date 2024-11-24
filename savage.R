# Define decision matrix with row and column names
decision_matrix <- matrix(c(-20, 60, 30, -5,
                            40, 50, 35, 0,
                            -50, 100, 45, -10,
                            12, 15, 15, 10), 
                          nrow = 4, 
                          byrow = TRUE,
                          dimnames = list(c("Corn", "Wheat", "Soya Beans", "Do Grazing"),
                                          c("Heavy Rainfall", "Moderate Rainfall", "Light Rainfall", "Drought")))

cat("\nDecision Matrix\n")
print(decision_matrix)

# Savage method - Maximization
regret_table <- apply(decision_matrix, 2, function(x) max(x) - x)
row_max <- apply(regret_table, 1, max)
optimum <- which.min(row_max)

cat("\nRegret Table and Row Maximums\n")
print(cbind(regret_table, row_min))

cat("\nOptimal Decision\n")
op <- sprintf("\n\tThe decision is to choose %s with a max regret of %.2f\n\n", names(optimum), row_max[optimum])
cat(op)


# Savage method - Minimization
regret_table <- apply(decision_matrix, 2, function(x) x - min(x))
row_min <- apply(regret_table, 1, min)
optimum <- which.max(row_min)

cat("\nRegret Table and Row Minimums\n")
print(cbind(regret_table, row_min))

op <- sprintf("\n\tThe decision is to choose %s with a min regret of %.2f\n\n", names(optimum), row_min[optimum])
cat(op)

