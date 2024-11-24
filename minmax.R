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

# Minmax method
minmax <- apply(decision_matrix, 1, max)

cat("\nMinmax Method\n")
print(minmax)

min_index <- which.min(minmax)

op <- sprintf("\nThe option with the lowest max value is %s with a max value of %.2f\n\n", names(min_index), minmax[min_index])
cat(op)
