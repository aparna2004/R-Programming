library(lpSolve)

# objective_coefficients <- c(5, 4)
# 
# # Define the coefficients of the constraints and the right-hand side values
# constraint_coefficients <- matrix(c(6, 4, 1, 2, -1, 1, 0, 1), nrow = 4, byrow = TRUE)
# rhs_values <- c(24, 6, 1, 2)
# constraint_direction <- c("<=", "<=", "<=", "<=")

get_user_input <- function(prompt_text) {

  cat(prompt_text)
  input <- readline(prompt = "")
  
  numeric_input <- as.numeric(unlist(strsplit(input, ",")))
  
  return(numeric_input)
}

objective_coefficients <- get_user_input("Enter the coefficients of the objective function (comma-separated): ")

num_constraints <- as.integer(readline(prompt = "Enter the number of constraints: "))
constraint_coefficients <- matrix(nrow = num_constraints, ncol = length(objective_coefficients))
for (i in 1:num_constraints) {
  constraint_coefficients[i,] <- get_user_input(cat("Enter the coefficients of constraint ", i, " (comma-separated): "))
}

rhs_values <- get_user_input("Enter the right-hand side values of constraints (comma-separated): ")

constraint_direction <- readline(prompt = "Enter the constraint directions (e.g., '<=', '>=', or '==', comma-separated): ")
constraint_direction <- strsplit(constraint_direction, ",")[[1]]

lp_result <- lp("max", objective_coefficients, constraint_coefficients, constraint_direction, rhs_values)

optimal_solution <- lp_result$solution
optimal_objective_value <- lp_result$objval

cat("\n\nOptimal Solution: (", paste(optimal_solution, collapse = ", "), ")\n")
cat("Optimal Objective Function Value: ", optimal_objective_value, "\n")
