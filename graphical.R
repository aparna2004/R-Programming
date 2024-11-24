library(gMOIP)

# constraint_coefficients <- matrix(c(6,4,1,2,-1,1,0,1), ncol = 2, byrow = TRUE)
# rhs <- c(24,6,1,2)
# obj <- c(5,4) 

# Function to take user inputs for coefficients or rhs values
get_user_input <- function(prompt_text) {
  # Prompt user for input
  cat(prompt_text)
  input <- readline(prompt = "")
  
  # Convert input to numeric vector
  numeric_input <- as.numeric(unlist(strsplit(input, ",")))
  
  return(numeric_input)
}

# Prompt user for objective function coefficients
objective_coefficients <- get_user_input("Enter the coefficients of the objective function (comma-separated): ")

# Prompt user for constraint coefficients
num_constraints <- as.integer(readline(prompt = "Enter the number of constraints: "))
constraint_coefficients <- matrix(nrow = num_constraints, ncol = length(objective_coefficients))
for (i in 1:num_constraints) {
  constraint_coefficients[i,] <- get_user_input(paste("Enter the coefficients of constraint ", i, " (comma-separated): "))
}

# Prompt user for right-hand side values of constraints
rhs_values <- get_user_input("Enter the right-hand side values of constraints (comma-separated): ")

# Prompt user for constraint directions
constraint_direction <- readline(prompt = "Enter the constraint directions (e.g., '<=', '>=', or '==', comma-separated): ")
constraint_direction <- strsplit(constraint_direction, ",")[[1]]

p <- plotPolytope(
  constraint_coefficients,
  rhs_values,
  objective_coefficients,
  type = rep("c", ncol(constraint_coefficients)),
  crit = "max",
  faces = rep("c", ncol(constraint_coefficients)),
  plotFaces = TRUE,
  plotFeasible = TRUE,
  plotOptimum = TRUE,
  labels = "coord",
  argsFaces = list(argsGeom_polygon = list(fill = 'blue'))
)

print(p)
