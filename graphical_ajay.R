library(lpSolve)
library(ggplot2)

? <- function(x, y) if (x) y[[1]] else y[[2]]
: <- function(y, z) list(y, z)

get_matrix <- function(n_rows, n_cols) {
  cat("Enter the matrix elements row-wise:")
  input_matrix <- matrix(
    scan(n = n_cols * n_rows),
    nrow = n_rows, ncol = n_cols, byrow = T
  )
  return(input_matrix)
}

feasible_region <- function(obj, mat, dir, rhs, ans) {
  x_vals <- seq(0, 10, by = 0.01)
  y_vals <- seq(0, 10, by = 0.01)
  df <- expand.grid(x = x_vals, y = y_vals)
  df$feasible <- 1
  
  for (i in seq_len(nrow(mat))) {
    l<- F
    if(dir[i] == "<=") {
      l <- mat[i, 1] * df$x + mat[i, 2] * df$y <= rhs[i]
    } else { 
      print("oo")
      l <- mat[i, 1] * df$x + mat[i, 2] * df$y >= rhs[i]
    }
    df$feasible <- df$feasible & l
  }
  
  
  t <- ggplot(df, aes(x, y)) +
    geom_tile(aes(fill = df$feasible), color = "#ffffff") +
    geom_contour(
      data = df[df$feasible, ],
      aes(z = obj[1] * x + obj[2] * y),
      color = "#0000FF") +
    geom_point(aes(x = ans[1], y = ans[2]), color = "#FF0000", size = 3) +
    labs(
      title = "Feasible Region for LP Problem",
      x = "x", y = "y"
    ) +
    theme_minimal()
  
  for (i in seq_len(nrow(mat))) {
    t <- t +
      stat_function(
        fun = function(x, m, b) {
          m * x + b
        },
        args = list(
          m = -mat[i, 1] / mat[i, 2],
          b = rhs[i] / mat[i, 2]
        ),
      )
  }
  t <- t + coord_cartesian(ylim = c(0, 10), xlim = c(0,10))
  
  return(t)
}

lp.constraints <- as.integer(readline("Enter the number of constraints :"))
lp.vars <- as.integer(readline("Enter the number of variables :"))

cat("Enter the objective coeffts :")
obj <- scan(n = lp.vars)
cat("Enter the constraints coeffts :")
mat <- get_matrix(lp.constraints, lp.vars)
dir <- strsplit(
  readline(
    "Enter direction of constraints (<=,=,>=) in space separated list :"), " "
)[[1]]
cat("Enter the RHS :")
rhs <- scan(n = lp.constraints)


result.simplex <- lp("max", obj, mat, dir, rhs)

if (result.simplex$status == 0) {
  cat("The value of decision variables are :")
  print(result.simplex$solution)
  cat("Objective value:")
  print(result.simplex$objval)
} else{
  cat("Infeasible solution.")
}

t <- 10

if (lp.vars == 2) {
  t <- feasible_region(obj, mat, dir, rhs, result.simplex$solution)
  print(t)
} else {
  print("More than 2 decision variables. Graph is not plotted")
}