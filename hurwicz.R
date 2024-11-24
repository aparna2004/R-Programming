# Decision Matrix
decision_matrix <- matrix(c(-20, 60, 30, -5,
                            40, 50, 35, 0,
                            -50, 100, 45, -10,
                            12, 15, 15, 10), 
                          nrow = 4, 
                          byrow = TRUE,
                          dimnames = list(c("Corn", "Wheat", "Soya Beans", "Do Grazing"),
                                          c("Heavy Rainfall", "Moderate Rainfall", "Light Rainfall", "Drought")))

cat("\nDecision Matrix:\n")
print(decision_matrix)

# Hurwicz method - Maximization
row_min <- apply(decision_matrix, 1, min)
row_max <- apply(decision_matrix, 1, max)
hurwicz <- apply(decision_matrix, 1, function(x) 0.5 * min(x) + 0.5 * max(x))

# Finding the optimal decision - max
optimum <- which.max(hurwicz)

cat("\n\nHurwicz Table:\n")
print(cbind(decision_matrix, Row_Min = row_min, Row_Max = row_max, Hurwicz = hurwicz))


cat("\nOptimal Decision:\n")
op <- sprintf("\n\tThe optimal decision is to choose %s with a Hurwicz value of %.2f\n\n", names(optimum), hurwicz[optimum])
cat(op)

# Hurwicz method - Maximization
# Finding the optimal decision - min
optimum <- which.min(hurwicz)

cat("\nOptimal Decision:\n")
op <- sprintf("\n\tThe optimal decision is to choose %s with a Hurwicz value of %.2f\n\n", names(optimum), hurwicz[optimum])
cat(op)