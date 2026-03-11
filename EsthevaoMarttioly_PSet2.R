########## Computational Methods - Problem Set 2 - Esthevao Marttioly ##########
#### Important: open the Rproject, instead of the script itself
rm(list = ls())       ## Be careful with this! It clears the environment

# renv::init()        ## Freeze the package version == just in the first time
# renv::snapshot()    ## Look the package version   == just in the first time
# renv::restore()     ## To restore, Answer "1"     == just in the first time of a new computer


# Set a seed for future replications
set.seed(20260310)


# Import Packages

library(tibble)
library(tidyverse)
library(ggplot2)
library(pracma)
library(stargazer)


###### Save a theme for the graphs ######
mytheme = theme(legend.position = "bottom",
                plot.title = element_text(size = 12, face = "bold"),
                plot.subtitle = element_text(size = 10),
                panel.background = element_rect(fill = "transparent", colour = "black",
                                                linewidth = 0.5, linetype = "solid"),
                panel.grid.major.y = element_line(colour = "grey", linewidth = 0.5),
                panel.grid.minor.y = element_line(colour = "grey", linewidth = 0.5),
                panel.grid = element_line(colour = "grey98"),
                panel.grid.major.x = element_line(colour = "transparent"),
                panel.grid.minor.x = element_line(colour = "transparent"),
                axis.text = element_text(colour = "black", size = 9),
                strip.background = element_rect(fill = "grey95", colour = "black"),
                strip.text = element_text(colour = "black", size = 9))

colours = c("#85C0F9", "#0F2080", "#A95AA1", "#F5793A", "slateblue2")
# ggsave("output/name.png", width = 5, height = 4)   ## Save the graph into png


####### 1. Interpolating Data #########

# Life expectancy data

LExp_Years = as.numeric(sub(1990, 1991, seq(1940, 2010, 10)))  # Seq 1940 - 2010

LExp_df = tibble(Year = LExp_Years,
                 LExp = c(45.5, 48.0, 52.5, 57.6, 62.5, 66.9, 71.1, 74.4))


# Best Guess for 1996

approx(LExp_Years, LExp_df$LExp, xout = 1996)  # LinearInterp(x, y, predicted) = 69.23


# Cubic Spline Interpolation

LExp_interp = spline(LExp_Years, LExp_df$LExp,
                     xout = seq(LExp_Years[1], max(LExp_Years))) %>% # Year_1 - Year_n
  as_tibble() %>% `colnames<-`(names(LExp_df))


# Graphics

LExp_interp %>%
  ggplot(aes(x = Year, y = LExp)) + mytheme + theme(legend.position = "bottom") +
  geom_line(color = colours[1], linewidth = 1) +
  geom_point(data = LExp_df, aes(x = Year, y = LExp), size = 2) +
  labs(x = "Year", y = "Life Expectancy",
       title = "Cubic Interpolated Life Expectancy in Brazil")

ggsave("output/p2_lifeexp_interp.png", width = 5, height = 4)   ## Save the graph into png



####### 2. Interpolating a Function #########

# Parameters

param_2 = list(xm = 1,
               alpha = 10,
               grid_min = 1,
               grid_max = 5,
               trying_size = 2500)


grid_size_ = c(10, 15, 20, 30, 50)


# Trying points - True Values

x = runif(param_2[["trying_size"]],
          param_2[["grid_min"]],
          param_2[["grid_max"]])


## Pareto PDF Function

dpareto = function(x, list_param = param_2) {
  alpha = list_param[["alpha"]]  # Set parameter
  xm = list_param[["xm"]]        # Set parameter
  
  if (any(abs(x) <= 1e-6)) {      # x cannot contain zero
    stop("Division by Zero: x must not contain zero")
  } else {
    return(alpha * xm^(alpha) / (x^(alpha + 1)))
  }
}


# Graph 
data.frame(x = seq(1, 5, length.out = 100),
           y = dpareto(seq(1, 5, length.out = 100))) %>%
  ggplot(aes(x = x, y = y)) + mytheme +
  geom_line(color = colours[1], linewidth = 1) +
  labs(x = "Grid", y = "Density",
       title = "Pareto Function with the selected values")

ggsave("output/p2_pareto_pdf.png", width = 5, height = 4)   ## Save the graph into png



## Interpolation Function

multi_interp = function(func, grid, trying_points) {
  
  # Fulfilled summary matrix
  summary_interp = matrix(nrow = 3, ncol = 2)
  
  
  # Function Value
  y = func(grid)
  ftrue = func(trying_points)
  
  
  ## Linear
  time_start = Sys.time()
  f_linear = approx(grid, y, xout = trying_points)$y
  summary_interp[1,2] = as.numeric(Sys.time() - time_start, units = "secs")
  
  
  ## Spline
  time_start = Sys.time()
  f_spline = spline(grid, y, xout = trying_points)$y
  summary_interp[2,2] = as.numeric(Sys.time() - time_start, units = "secs")
  
  
  ## Pchip
  time_start = Sys.time()
  f_pchip = pchip(grid, y, trying_points)
  summary_interp[3,2] = as.numeric(Sys.time() - time_start, units = "secs")
  
  
  ## Summarizing
  summary_interp[1,1] = mean((ftrue - f_linear)^2)
  summary_interp[2,1] = mean((ftrue - f_spline)^2)
  summary_interp[3,1] = mean((ftrue - f_pchip)^2)
  
  return(summary_interp)
}


## General Interpolation Function

general_interp = function(func, trying_points,
                          list_param = param_2,
                          grid_size = grid_size_,
                          grid_space = c("equally", "log", "both")) {
  
  # Parameters
  grid_min = list_param[["grid_min"]]
  grid_max = list_param[["grid_max"]]
  
  summary_interp = matrix(nrow = 0, ncol = ifelse(grid_space == "both", 4, 2))
  
  # Changing Grid's Size
  for (i in grid_size) {
    
    # Equally Spaced Grid
    if (grid_space == "equally") {
      t = seq(grid_min, grid_max, length.out = i)
      summary_interp_aux = multi_interp(func, t, trying_points)
      col_names = c("Grid Size", "Method", "MSE Equally", "Time Equally")
    }
    
    
    # Log-Spaced Grid
    if (grid_space == "log") {
      t = exp(seq(log(grid_min), log(grid_max), length.out = i))
      summary_interp_aux = multi_interp(func, t, trying_points)
      col_names = c("Grid Size", "Method", "MSE Log", "Time Log")
    }
    
    
    # Equally and Log Spaced Grid
    if (grid_space == "both") {
      t = seq(grid_min, grid_max, length.out = i)
      t_log = exp(seq(log(grid_min), log(grid_max), length.out = i))
      
      summary_interp_aux = cbind(multi_interp(func, t, trying_points),
                                 multi_interp(func, t_log, trying_points))
  
      col_names = c("Grid Size", "Method", "MSE Equally",
                    "Time Equally", "MSE Log", "Time Log")
    }
    
    # Bind to re-loop
    summary_interp = rbind(summary_interp, summary_interp_aux)
  }
  
  summary_interp =
    cbind.data.frame(rep(grid_size, each = 3),            # Repeat each n 3x
                     rep(c("Linear", "Spline", "Pchip"),  # Repeat N times
                         length(grid_size)),
                     summary_interp) %>% `colnames<-`(col_names)
  
  return(summary_interp)
}


# Run the Function

summary_interp = general_interp(dpareto, x,
                                param_2, grid_size_,
                                grid_space = "both")


stargazer(summary_interp, summary = F, digits = 3, rownames = F)



####### 3. Interpolating another Function #########

# Parameters

param_3 = list(eta = 5,
               xi = 500,
               grid_min = 200,
               grid_max = 1000,
               trying_size = 2500)


# Trying points - True Values

x = runif(param_3[["trying_size"]],
          param_3[["grid_min"]],
          param_3[["grid_max"]])


## New g(x) function

func_g = function(x, list_param = param_3) {
  eta = list_param[["eta"]]  # Set parameter
  xi = list_param[["xi"]]        # Set parameter
  
  if (any(abs(x) <= 1e-6)) {      # x cannot contain zero
    stop("Division by Zero: x must not contain zero")
  } else {
    return(1 / (1 + exp(-eta * log(x/xi))))
  }
}


# Graph
data.frame(x = seq(200, 1000, length.out = 1000),
           y = dpareto(seq(200, 1000, length.out = 1000))) %>%
  ggplot(aes(x = x, y = y)) + mytheme +
  geom_line(color = colours[1], linewidth = 1) +
  labs(x = "Grid", y = "Density",
       title = "Function g(x) with selected values")

ggsave("output/p2_function_g.png", width = 5, height = 4)   ## Save the graph into png



# Run the Function

summary_interp_g = general_interp(func_g, x,
                                  param_3, grid_size_,
                                  grid_space = "both")


stargazer(summary_interp_g, summary = F, digits = 3, rownames = F)



####### 4. Root Finding of a Function #########

# Function
f = function(x) x^3 + 2*x + 5


# Derivative for Newton method
Df = function(x) 3*x^2 + 2


##### Bisection Method #####

bisection = function(func, a, b, tol = 1e-8, max_iter = 1000){
  
  # Necessary condition for bisection
  if(a >= b) stop("Require a < b")
  
  fa = func(a)
  fb = func(b)
  
  if(fa * fb > 0)
    stop("Root is not bracketed: f(a) and f(b) must have opposite signs")
  
  time_start = Sys.time()
  
  iter = 1
  
  while (abs(b - a) > tol && iter < max_iter) {
    
    c = (a+b)/2  # Calculate the midpoint
    fc = func(c)
    
    # Check tolerance
    if (abs(fc) < tol || (b-a)/2 < tol) {
      return(list(root = c, iterations = iter,
                  time = as.numeric(Sys.time() - time_start, units = "secs")))
    }
    
    if (fa * fc < 0) {
      b = c
      fb = fc
    } else {
      a = c
      fa = fc
    }
    
    iter = iter + 1
  }
  
  # Check if the solution converged
  if (iter == max_iter) {
    stop(paste("Bisection method did not converge within", max_iter, "iterations."))
  }
  
  return(list(root = c, iterations = iter,
              time = as.numeric(Sys.time() - time_start, units = "secs")))
  
}



##### Secant Method #####

secant = function(func, x0, x1, tol = 1e-8, max_iter = 1000){
  
  time_start = Sys.time()
  iter = 0
  
  while (abs(x1 - x0) > tol && iter < max_iter) {
    
    f0 = func(x0)
    f1 = func(x1)
    
    if(abs(f1 - f0) < 1e-14) # Avoid division by small values
      stop("Division by zero in secant method")
    
    x2 = x1 - f1 * (x1-x0) / (f1-f0)  # Secant Method formula
    
    # Updating values
    x0 = x1
    x1 = x2
    iter = iter + 1
  }
  
  if (iter == max_iter) {
    stop(paste("Secant method did not converge within", max_iter, "iterations."))
  }
  
  return(list(root = x2, iterations = iter,
              time = as.numeric(Sys.time() - time_start, units = "secs")))
}



##### Newton-Raphson Method #####

newton_raphson = function(func, deriv_func, x0, tol = 1e-8, max_iter = 1000){
  
  time_start = Sys.time()
  iter = 0
  x1 = x0 + 10*tol        # First iteration
  
  while (abs(x1 - x0) > tol && iter < max_iter) {
    
    fx = func(x0)
    dfx = deriv_func(x0)
    
    if(abs(dfx) < 1e-14)  # Avoid division by small values
      stop("Derivative close to zero")
    
    x1 = x0
    x0 = x0 - fx/dfx      # xn+1 = xn + f(xn) / f'(xn)
    
    iter = iter + 1
    
  }
  
  # Check if the solution converged
  if (iter == max_iter) {
    stop(paste("Newton-Raphson method did not converge within", max_iter, "iterations."))
  }
  
  return(list(root = x0, iterations = iter,
              time = as.numeric(Sys.time() - time_start, units = "secs")))
}



### Results

bis = bisection(f, -2, -1)
sec = secant(f, -2, -1)
newt = newton_raphson(f, Df, -1.5)


c("Bisection", "Secant", "Newton") %>%
  cbind.data.frame(rbind.data.frame(bis, sec, newt)) %>%
  `colnames<-`(c("Method", "Root", "Iterations", "Time (sec)")) %>%
  stargazer(summary = F, digits = 3, rownames = F)




