# Import the data and libraries
library(readxl)
library(dplyr)
library(tidyverse)
library(boot)
library(tseries)
library(ggplot2)
library(patchwork)
library(openxlsx)
library(forecast)
# install.packages("urca")
library(urca)
library(progress)

country_Model10_Results <- read_excel("C:/Users/ridhi/Downloads/country_Model10_Results.xlsx",
                                      sheet = "Transformed Data for Model")
summary(country_Model10_Results)

# We will modify the December peak dummy and February peak dummies to instead be using the radial basis functions
# There are some choices to be made about the bandwidth of the kernel
rad_bas_country <- country_Model10_Results
rad_bas_country$month <- c(rep(1:12, 3), 1, 2)
# The extra two rows were check of the data, by taking the average. It will be removed later
summary(rad_bas_country)

# Month behaves as it should. Now, we will make a copy of this data-set, and remove the dummies in the copy.
# No need to set seeds so far, we only utilise the densities.
rad_bas_country <- rad_bas_country %>% select(-Dec_Peak_Dummy, -Feb_Dip_Dummy, -const)
rad_bas_country$Feb_Dip_Dummy <- dnorm(rad_bas_country$month - 2)
rad_bas_country$Dec_Peak_Dummy <- dnorm(rad_bas_country$month - 12)

linear_model_data <- rad_bas_country %>% select(-month)
linear_model_data <- linear_model_data %>% slice(1:(n() - 2))
country_Model10_Results_response <- read_excel("C:/Users/ridhi/Downloads/country_Model10_Results.xlsx",
                                               sheet = "Model Data")
sales <- country_Model10_Results_response$Sales_Volume_Total
rbf_model <- lm(sales ~ ., data = linear_model_data)
summary(rbf_model)
# This leads to a wrong sign

# bandwidth selection with wikipedia recommended optimal bandwidth for KDE, even though we are not doing KDE.
# Make a copy of the data and remove the specified dummies
rad_bas_country_wiki <- rad_bas_country %>% select(-Dec_Peak_Dummy, -Feb_Dip_Dummy)

# Calculate the Wikipedia bandwidth for the 'month' variable
n_month <- length(rad_bas_country_wiki$month) - 2 # Sample size
sigma_hat_month <- sd(rad_bas_country_wiki$month) # Standard deviation
iqr_month <- IQR(rad_bas_country_wiki$month) # Interquartile range
# h_month <- 0.6 * min(sigma_hat_month, iqr_month / 1.34) * n_month^(-1/5) # Bandwidth
h_month <- 2

# Create new dummies using the Wikipedia bandwidth
rad_bas_country_wiki$feb_dummy_wiki <- dnorm((rad_bas_country_wiki$month - 2), sd = h_month)
rad_bas_country_wiki$dec_dummy_wiki <- dnorm((rad_bas_country_wiki$month - 12), sd = h_month)

# Prepare data for the linear model
linear_model_data_wiki <- rad_bas_country_wiki %>% select(-month)
linear_model_data_wiki <- linear_model_data_wiki %>% slice(1:(n() - 2))

# Load sales data
country_Model10_Results_response <- read_excel("C:/Users/ridhi/Downloads/country_Model10_Results.xlsx",
                                               sheet = "Model Data")
sales <- country_Model10_Results_response$Sales_Volume_Total

# Fit the linear model
rbf_model_wiki <- lm(sales ~ ., data = linear_model_data_wiki)

# Display the summary of the model
summary(rbf_model_wiki)
# Wrong siggns again. We were not doing KDE anyways.


# old model for comparison
old_model <- lm(sales ~ . - const, data = country_Model10_Results[1:36, ])
summary(old_model)

############################################################################
# Explore the varince parameter(bandwidth) in 1D and performance
# Define the range of bandwidth values
# We want good coverage between 0 and 1 because we want to have a model which will have the right signs
# We do not care too much about the large values, as a very high bandwidth starts to approach the uniform distribution
# especially, when we restrict ourselves to 12 points which are comparatively close together
bandwidth_values <- c(exp(seq(0, 1.8, by = 0.1)) - 0.9, 6:20)
# Now that I tried the bandwidth, and it is only good to explore until 4

# Initialize lists to store results
summary_results <- list()
all_results <- list()

# Create a copy of the data without the problematic columns
rad_bas_country_wiki_clean <- rad_bas_country_wiki %>%
  select(-feb_dummy_wiki, -dec_dummy_wiki)


# Define bandwidth ranges for December and February
dec_bandwidth_values <- c(exp(seq(0, 1.8, by = 0.07)) - 0.9)
feb_bandwidth_values <- c(exp(seq(0, 1.8, by = 0.1)) - 0.9)

# Define the Normal kernel function
normal_kernel <- function(x, x0, h) {
  u <- (x - x0) / h
  kernel <- dnorm(u)
  return(kernel)
}

# Define the Epanechnikov kernel function
epanechnikov_kernel <- function(x, x0, h) {
  u <- (x - x0) / h
  kernel <- ifelse(abs(u) <= 1, 0.75 * (1 - u ^ 2), 0)
  return(kernel)
}

# Define the Triangular kernel function
triangular_kernel <- function(x, x0, h) {
  u <- abs((x - x0) / h)
  kernel <- ifelse(u <= 1, 1 - u, 0)
  return(kernel)
}

# Define the Biweight kernel function
biweight_kernel <- function(x, x0, h) {
  u <- (x - x0) / h
  kernel <- ifelse(abs(u) <= 1, (15 / 16) * (1 - u ^ 2) ^ 2, 0)
  return(kernel)
}

##############################################################################################
# Create the data frame and find the intersection
generate_kernel_dataframes <- function(kernel_function, kernel_name) {
  sign_results <- data.frame()
  
  # Nested loop over December and February bandwidth values
  for (h_dec in dec_bandwidth_values) {
    for (h_feb in feb_bandwidth_values) {
      # Create new dummies using the specified kernel
      rad_bas_country_wiki_clean$feb_dummy <- kernel_function(rad_bas_country_wiki_clean$month, 2, h_feb)
      rad_bas_country_wiki_clean$dec_dummy <- kernel_function(rad_bas_country_wiki_clean$month, 12, h_dec)
      
      # Prepare data for the linear model
      linear_model_data_temp <- rad_bas_country_wiki_clean %>% select(-month)
      linear_model_data_temp <- linear_model_data_temp %>% slice(1:(n() - 2))
      
      # Fit the linear model
      temp_model <- lm(sales ~ ., data = linear_model_data_temp)
      
      # Extract coefficients and their signs
      coef_signs <- sign(coef(temp_model)) # +1 for positive, -1 for negative
      coef_names <- names(coef_signs)
      
      # Store results
      sign_results <- rbind(
        sign_results,
        data.frame(
          Dec_Bandwidth = h_dec,
          Feb_Bandwidth = h_feb,
          Variable = coef_names,
          Sign = coef_signs
        )
      )
    }
  }
  
  # Filter out the intercept
  sign_results <- sign_results %>% filter(Variable != "(Intercept)")
  
  # Prepare data for plotting
  plot_data <- sign_results %>%
    pivot_wider(names_from = Variable, values_from = Sign)
  
  return(plot_data)
}

# Generate data frames for each kernel
kernel_data_epa <- generate_kernel_dataframes(epanechnikov_kernel, "Epanechnikov")
kernel_data_tri <- generate_kernel_dataframes(triangular_kernel, "Triangular")
kernel_data_biw <- generate_kernel_dataframes(biweight_kernel, "Biweight")
kernel_data_norm <- generate_kernel_dataframes(normal_kernel, "Normal")

positive_signs <- c(
  "TV_GRP_transformed",
  "Outdoor_Spends_transformed",
  "Radio_Spends_transformed",
  "Youtube_Spends_transformed",
  "Direct_Display_Spend_transformed",
  "META_1_Spends_transformed",
  "Programmatic_Video_spends_transformed",
  "META_2_Spends_transformed",
  "dec_dummy"
)

negative_signs <- c("Brand_PH_ATL_Spends_transformed",
                    "Brand_P_ATL_Spends_transformed",
                    "feb_dummy")

# Function to calculate the correct sum
calculate_correct_sum <- function(data) {
  data$positive_sum <- rowSums(data[, positive_signs])
  data$negative_sum <- rowSums(data[, negative_signs])
  data$correct_sum <- data$positive_sum - data$negative_sum
  return(data)
}

# Apply the function to each kernel data frame
kernel_data_epa <- calculate_correct_sum(kernel_data_epa)
kernel_data_tri <- calculate_correct_sum(kernel_data_tri)
kernel_data_biw <- calculate_correct_sum(kernel_data_biw)
kernel_data_norm <- calculate_correct_sum(kernel_data_norm)

# Find optimal bandwidth combinations for each kernel
optimal_bandwidths_epa <- kernel_data_epa %>%
  filter(correct_sum == 12) %>%
  select(Dec_Bandwidth, Feb_Bandwidth)
optimal_bandwidths_tri <- kernel_data_tri %>%
  filter(correct_sum == 12) %>%
  select(Dec_Bandwidth, Feb_Bandwidth)
optimal_bandwidths_biw <- kernel_data_biw %>%
  filter(correct_sum == 12) %>%
  select(Dec_Bandwidth, Feb_Bandwidth)
optimal_bandwidths_norm <- kernel_data_norm %>%
  filter(correct_sum == 12) %>%
  select(Dec_Bandwidth, Feb_Bandwidth)

# Tests for the old model
plot(acf(resid(old_model)))
plot(pacf(resid(old_model)))

# Run ADF test on residuals
adf_test <- adf.test(resid(old_model))
print(adf_test)

# Check if p-value < 0.05 to hint stationarity
if (adf_test$p.value > 0.05) {
  print("Residuals are stationary (ADF test)")
} else {
  print("Residuals are not stationary (ADF test)")
}

# Run KPSS test on residuals
# Check for zero mean of residuals, because there are two KPSEE paradigms, constant mean and trend
mean(resid(old_model))
# Close enough to 0
kpss_test <- ur.kpss(resid(old_model), type = "mu", lags = 'long')  # Type "mu" is for testing level stationarity
summary(kpss_test)

# If p-value > 0.05, residuals are stationary
if (kpss_test@teststat < kpss_test@cval[1, 2]) {
  print("Residuals are stationary (KPSS test)")
} else {
  print("Residuals are not stationary (KPSS test)")
}

# Loop through lags 1 to 12 and perform Ljung-Box test
for (lag in 1:12) {
  ljung_box_test <- Box.test(resid(old_model), lag = lag, type = "Ljung-Box")
  # print(paste("Ljung-Box test for lag", lag))
  # print(ljung_box_test)
  
  # If p-value > 0.05, the residuals do not exhibit autocorrelation
  if (ljung_box_test$p.value > 0.05) {
    print("Residuals are not autocorrelated (Ljung-Box test)")
  } else {
    print("Residuals are autocorrelated (Ljung-Box test)")
  }
}
# Kernel information for all RBFs
rbf_kernels <- list(
  list(
    data = optimal_bandwidths_epa,
    kernel = epanechnikov_kernel,
    name = "Epanechnikov"
  ),
  list(
    data = optimal_bandwidths_tri,
    kernel = triangular_kernel,
    name = "Triangular"
  ),
  list(
    data = optimal_bandwidths_biw,
    kernel = biweight_kernel,
    name = "Biweight"
  ),
  list(
    data = optimal_bandwidths_norm,
    kernel = normal_kernel,
    name = "Normal"
  )
)

# Placeholders for residuals, fitted values and bandwidths
residuals_list <- list()
fitted_values_list <- list()
bandwidths_list <- list()

# Loop through each RBF kernel
for (kernel_info in rbf_kernels) {
  kernel_data <- kernel_info$data
  kernel_function <- kernel_info$kernel
  kernel_name <- kernel_info$name
  print(kernel_name)
  
  # Find the top model (highest adjusted R-squared)
  top_model <- kernel_data %>%
    mutate(adj_R_squared = map_dbl(1:nrow(kernel_data), function(row) {
      feb_band <- kernel_data$Feb_Bandwidth[row]
      dec_band <- kernel_data$Dec_Bandwidth[row]
      
      rad_bas_country_wiki_clean$feb_dummy <- kernel_function(rad_bas_country_wiki_clean$month, 2, feb_band)
      rad_bas_country_wiki_clean$dec_dummy <- kernel_function(rad_bas_country_wiki_clean$month, 12, dec_band)
      
      linear_model_data_temp <- rad_bas_country_wiki_clean %>%
        select(-month) %>%
        slice(1:(n() - 2))
      
      temp_model <- lm(sales ~ ., data = linear_model_data_temp)
      summary(temp_model)$adj.r.squared
    })) %>%
    arrange(desc(adj_R_squared)) %>%
    slice(1)
  
  # Extract bandwidths for the top model
  feb_band <- top_model$Feb_Bandwidth
  dec_band <- top_model$Dec_Bandwidth
  
  # Recreate dummies and calculate residuals
  rad_bas_country_wiki_clean$feb_dummy <- kernel_function(rad_bas_country_wiki_clean$month, 2, feb_band)
  rad_bas_country_wiki_clean$dec_dummy <- kernel_function(rad_bas_country_wiki_clean$month, 12, dec_band)
  
  linear_model_data_temp <- rad_bas_country_wiki_clean %>%
    select(-month) %>%
    slice(1:(n() - 2))
  
  temp_model <- lm(sales ~ ., data = linear_model_data_temp)
  print(summary(temp_model))
  residuals_list[[kernel_name]] <- resid(temp_model)
  fitted_values_list[[kernel_name]] <- fitted(temp_model)
  bandwidths_list[[kernel_name]] <- list(best_feb_bandwidth = feb_band,
                                         best_dec_bandwidth = dec_band)
}

# Loop through each RBF kernel's residuals
for (kernel_name in names(residuals_list)) {
  print(paste("Running tests for", kernel_name, "kernel"))
  
  # Get residuals for the current kernel
  residuals <- residuals_list[[kernel_name]]
  
  print(paste("Auto arima for", kernel_name, "kernel"))
  print(auto.arima(residuals))
  
  # Plot ACF and PACF for residuals
  plot(acf(residuals, main = paste("ACF of", kernel_name, "residuals")))
  plot(pacf(residuals, main = paste("PACF of", kernel_name, "residuals")))
  
  # ADF test on residuals
  adf_test <- adf.test(residuals)
  print(adf_test)
  
  # Check stationarity based on ADF test p-value
  if (adf_test$p.value > 0.05) {
    print(paste(kernel_name, "residuals are stationary (ADF test)"))
  } else {
    print(paste(kernel_name, "residuals are not stationary (ADF test)"))
  }
  
  # KPSS test on residuals
  kpss_test <- ur.kpss(residuals, type = "mu", lags = 'long')  # Type "mu" is for level stationarity
  print(summary(kpss_test))
  
  # Check stationarity based on KPSS test
  if (kpss_test@teststat < kpss_test@cval[1, 2]) {
    print(paste(kernel_name, "residuals are stationary (KPSS test)"))
  } else {
    print(paste(kernel_name, "residuals are not stationary (KPSS test)"))
  }
  
  # Ljung-Box test for residual autocorrelation for lags 1 to 12
  for (lag in 1:12) {
    ljung_box_test <- Box.test(residuals, lag = lag, type = "Ljung-Box")
    
    # Print Ljung-Box test results
    if (ljung_box_test$p.value > 0.05) {
      print(
        paste(
          kernel_name,
          "residuals are not autocorrelated at lag",
          lag,
          "(Ljung-Box test)"
        )
      )
    } else {
      print(
        paste(
          kernel_name,
          "residuals are autocorrelated at lag",
          lag,
          "(Ljung-Box test)"
        )
      )
    }
  }
}

# Kernel information for all RBFs
rbf_kernels <- list(
  list(
    data = optimal_bandwidths_epa,
    kernel = epanechnikov_kernel,
    name = "Epanechnikov"
  ),
  list(
    data = optimal_bandwidths_tri,
    kernel = triangular_kernel,
    name = "Triangular"
  ),
  list(
    data = optimal_bandwidths_biw,
    kernel = biweight_kernel,
    name = "Biweight"
  ),
  list(
    data = optimal_bandwidths_norm,
    kernel = normal_kernel,
    name = "Normal"
  )
)

## Do a small  semi paramteric bootstrap now for the coefficient stability
# we have convergence of mle, which can be applied for the regression
# and that n for convergence is what we are concerned about/ investigating
# Function to create RBF dummies for a given kernel
create_rbf_dummy <- function(month, center, bandwidth, kernel_function) {
  return(kernel_function(month, center, bandwidth))
}

# Function to check the sign constraints of the model coefficients
variables_sign_check <- function(coefficients, sign_constraints) {
  for (var_name in names(coefficients)) {
    if (var_name %in% sign_constraints$positive_signs &&
        coefficients[var_name] <= 0) {
      return(FALSE)
    }
    if (var_name %in% sign_constraints$negative_signs &&
        coefficients[var_name] >= 0) {
      return(FALSE)
    }
  }
  return(TRUE)
}

sign_constraints <- list(
  positive_signs = c(
    "TV_GRP_transformed",
    "Outdoor_Spends_transformed",
    "Radio_Spends_transformed",
    "Youtube_Spends_transformed",
    "Direct_Display_Spend_transformed",
    "META_1_Spends_transformed",
    "Programmatic_Video_spends_transformed",
    "META_2_Spends_transformed",
    "Dec_Peak_Dummy"
  ),
  negative_signs = c(
    "Brand_PH_ATL_Spends_transformed",
    "Brand_P_ATL_Spends_transformed",
    "Feb_Dip_Dummy"
  )
)

tune_bandwidth <- function(data,
                           sales,
                           sign_constraints,
                           kernel_func,
                           feb_bandwidth_range,
                           dec_bandwidth_range) {
  best_bandwidth_feb <- NULL
  best_bandwidth_dec <- NULL
  best_r_squared <- -Inf
  data <- data %>% select(-Feb_Dip_Dummy, -Dec_Peak_Dummy)
  
  # Loop over February bandwidth values
  for (feb_bandwidth in feb_bandwidth_range) {
    # Loop over December bandwidth values
    for (dec_bandwidth in dec_bandwidth_range) {
      # Create the new RBF dummies for February and December using the selected bandwidths
      data_with_rbf <- data
      data_with_rbf$month <- rep(1:12, 3)[1:nrow(data)]
      
      # Create February RBF dummy
      data_with_rbf$Feb_Dip_Dummy <- create_rbf_dummy(data_with_rbf$month, 2, feb_bandwidth, kernel_func)
      # Create December RBF dummy
      data_with_rbf$Dec_Peak_Dummy <- create_rbf_dummy(data_with_rbf$month, 12, dec_bandwidth, kernel_func)
      data_with_rbf <- data_with_rbf %>% select(-month, -Sales_Volume_Total)
      
      # Fit the model with the new RBF dummies
      rbf_model <- lm(sales ~ ., data = data_with_rbf)
      model_summary <- summary(rbf_model)
      
      # Get the R-squared value
      r_squared <- model_summary$r.squared
      
      # Check if the model satisfies the sign constraints and has better R-squared
      if (r_squared > best_r_squared &&
          all(variables_sign_check(rbf_model$coefficients, sign_constraints))) {
        best_r_squared <- r_squared
        best_bandwidth_feb <- feb_bandwidth
        best_bandwidth_dec <- dec_bandwidth
      }
    }
  }
  
  # Return the best bandwidth values for both February and December
  return(
    list(
      best_feb_bandwidth = best_bandwidth_feb,
      best_dec_bandwidth = best_bandwidth_dec
    )
  )
}

# Define the range of bandwidth values to consider
bandwidth_range <- c(exp(seq(0, 1.8, by = 0.1)) - 0.9)
kernesl = list(
  Normal = normal_kernel,
  Epanechnikov = epanechnikov_kernel,
  Triangular = triangular_kernel,
  Biweight = biweight_kernel
)

# Now, we want to sample from the rbfs
parametric_bootstrap_RBFs <- function(fitted_vals,
                                      residual_vals,
                                      data,
                                      n_bootstrap,
                                      sign_constraints,
                                      bandwidth_range,
                                      kernel) {
  set.seed(0)
  # Create a progress bar object
  pb <- progress_bar$new(
    total = n_bootstrap,
    # Total number of iterations
    format = "[:bar] :current/:total (:percent) :eta",
    # Format of the progress bar
    clear = FALSE,
    # Do not clear the progress bar after completion
    width = 60                 # Width of the progress bar
  )
  
  # Get the model coefficients and residuals
  model_fitted_vals <- fitted_vals
  model_residuals <- residual_vals
  bootstrap_results <- vector("list", n_bootstrap)
  
  for (i in 1:n_bootstrap) {
    # Increment progress bar
    pb$tick()
    
    # Simulate new data using the model's coefficients and residuals
    simulated_data <- data
    simulated_data$Sales_Volume_Total <- model_fitted_vals + model_residuals[rep(3 * sample(0:11, 12), each = 3) + rep(1:3, 12)]
    
    old_model_sim <- lm(Sales_Volume_Total ~ ., data = simulated_data)
    # Tune the bandwidth based on the signs and selected kernel
    best_bandwidth <- tune_bandwidth(
      simulated_data,
      simulated_data$Sales_Volume_Total,
      sign_constraints,
      kernel,
      bandwidth_range,
      bandwidth_range
    )
    
    if (is.null(best_bandwidth$best_feb_bandwidth) ||
        is.null(best_bandwidth$best_dec_bandwidth)) {
      bootstrap_results[[i]] <- 0
      next
    }
    
    # Apply RBF dummies with the best bandwidth and fit the model
    data_with_rbf <- simulated_data
    data_with_rbf$month <- rep(1:12, 3)
    
    # Create February RBF dummy
    data_with_rbf$Feb_Dip_Dummy <- create_rbf_dummy(data_with_rbf$month,
                                                    2,
                                                    best_bandwidth$best_feb_bandwidth,
                                                    kernel)
    
    # Create December RBF dummy
    data_with_rbf$Dec_Peak_Dummy <- create_rbf_dummy(data_with_rbf$month,
                                                     12,
                                                     best_bandwidth$best_dec_bandwidth,
                                                     kernel)
    
    data_with_rbf <- data_with_rbf %>% select(-month)
    
    # Fit the model with the selected RBFs
    rbf_model_sim <- lm(Sales_Volume_Total ~ ., data = data_with_rbf)
    bootstrap_results[[i]] <- list(old_one = old_model_sim, new_one = rbf_model_sim)
  }
  
  return(bootstrap_results)
}
n_bootstrap = 10000
# bootstrap_results_OHE <- parametric_bootstrap_RBFs(
#   fitted_values_list$Normal,
#   residuals_list$Normal,
#   country_Model10_Results[1:36, 2:13],
#   n_bootstrap,
#   sign_constraints,
#   bandwidth_range,
#   kernesl$Normal
# )
bootstrap_results_normal <- parametric_bootstrap_RBFs(
  fitted_values_list$Normal,
  residuals_list$Normal,
  country_Model10_Results[1:36, 2:13],
  n_bootstrap,
  sign_constraints,
  bandwidth_range,
  kernesl$Normal
)
bootstrap_results_epan <- parametric_bootstrap_RBFs(
  fitted_values_list$Epanechnikov,
  residuals_list$Epanechnikov,
  country_Model10_Results[1:36, 2:13],
  n_bootstrap,
  sign_constraints,
  bandwidth_range,
  kernesl$Epanechnikov
)
bootstrap_results_biweight <- parametric_bootstrap_RBFs(
  fitted_values_list$Biweight,
  residuals_list$Biweight,
  country_Model10_Results[1:36, 2:13],
  n_bootstrap,
  sign_constraints,
  bandwidth_range,
  kernesl$Biweight
)
bootstrap_results_triangular <- parametric_bootstrap_RBFs(
  fitted_values_list$Triangular,
  residuals_list$Triangular,
  country_Model10_Results[1:36, 2:13],
  n_bootstrap,
  sign_constraints,
  bandwidth_range,
  kernesl$Triangular
)

history_function <- function(x) {
  current_model <- x$new_one
  observed <- current_model$model$Sales_Volume_Total
  data <- model.matrix(current_model)
  coefs_history <- list()
  
  for (i in 20:36) {
    sliced_obs <- observed[1:i]
    sliced_data <- as.data.frame(data[1:i, ])
    sliced_data$Sales_Volume_Total <- sliced_obs  # Ensure response variable is in the data
    temp_model <- lm(Sales_Volume_Total ~ ., data = sliced_data)
    
    temp_coefficients <- coef(temp_model)
    temp_coefficients['MAPE'] = 100 * mean((abs(fitted(temp_model) - sliced_obs)) /
                                       sliced_obs)
    temp_coefficients['RMSE'] =  mean((fitted(temp_model) - sliced_obs) **
                                        2) ** 0.5
    temp_coefficients['NRMSE'] =  temp_coefficients['RMSE'] / (max(sliced_obs) - min(sliced_obs))
    coefs_history[[length(coefs_history) + 1]] <- temp_coefficients
  }
  
  return(do.call(rbind, coefs_history))  # Convert list to matrix at the end
}
plot_metric <- function(metric_name, df_metrics) {
  ggplot(
    df_metrics %>% filter(Variable == metric_name),
    aes(x = Time, y = Value, color = Variable)
  ) +
    geom_line() +
    theme_minimal() +
    labs(title = paste(metric_name, "Over Time"),
         x = "Time",
         y = metric_name) +
    theme(legend.position = "none")
}
plot_coefficient_trends <- function(bootstrap_results) {
  # Gather the good results
  good_indices <- which(as.numeric(lapply(bootstrap_results, function(x)
    ! is.numeric(x))) != 0)
  good_results <- bootstrap_results[good_indices]
  
  num_models <- length(good_results)
  num_slices <- 36 - 20 + 1
  sample_coefs_names <- names(history_function(good_results[[1]])[1, ])
  num_coefs <- length(sample_coefs_names)
  
  # Create an empty 3D array
  coefs_array <- array(
    NA,
    dim = c(num_models, num_slices, num_coefs),
    dimnames = list(
      paste0("Model_", seq_len(num_models)),
      paste0("Time_", 20:36),
      sample_coefs_names
    )
  )
  
  for (m in seq_len(num_models)) {
    coefs_matrix <- history_function(good_results[[m]])
    for (t in seq_len(num_slices)) {
      coefs_array[m, t, ] <- coefs_matrix[t, ]
    }
  }
  
  # Identify coefficient names which are metrics to exclude
  exclude_cols <- c("NRMSE", "RMSE", "MAPE")
  
  # Create a logical mask for columns to exclude
  exclude_mask <- !(dimnames(coefs_array)[[3]] %in% exclude_cols)
  
  # Apply sign transformation only to relevant columns
  signs_coefs_array <- coefs_array  # Copy the original array
  signs_coefs_array[, , exclude_mask] <- sign(coefs_array[, , exclude_mask])
  
  # signs_coefs_array <- sign(coefs_array)
  # # List of coefficients to flip
  # flip_coefficients <- c("Feb_Dip_Dummy",
  #                        "Brand_PH_ATL_Spends_transformed",
  #                        "Brand_P_ATL_Spends_transformed")
  #
  # # Multiply selected coefficients by -1
  # signs_coefs_array[, , flip_coefficients] <- signs_coefs_array[, , flip_coefficients] * -1
  coefs_sign_means <- apply(signs_coefs_array, c(2, 3), mean)
  
  # df <- as.data.frame(coefs_sign_means) %>%
  #   mutate(Time = as.numeric(gsub("Time_", "", rownames(.)))) %>%
  #   pivot_longer(-Time, names_to = "Variable", values_to = "Value")
  
  # Convert to dataframe and reshape
  df <- as.data.frame(coefs_sign_means) %>%
    mutate(Time = as.numeric(gsub("Time_", "", rownames(.)))) %>%
    pivot_longer(-Time, names_to = "Variable", values_to = "Value")
  
  # Define metric and coefficient groups
  metrics <- c("NRMSE", "RMSE", "MAPE")
  excluded_coefs <- c(
    "(Intercept)",
    "Dec_Peak_Dummy",
    "Feb_Dip_Dummy",
    "`(Intercept)`",
    "Brand_PH_ATL_Spends_transformed",
    "Brand_P_ATL_Spends_transformed"
  )
  
  # Filter for metrics
  df_metrics <- df %>%
    filter(Variable %in% metrics)
  
  # Filter for coefficients
  df_coefs <- df %>%
    filter(!Variable %in% c(metrics, excluded_coefs))
  
  # Generate metric plots
  p_nrmse <- plot_metric("NRMSE", df_metrics)
  p_rmse <- plot_metric("RMSE", df_metrics)
  p_mape <- plot_metric("MAPE", df_metrics)
  
  # Arrange metric plots in a vertical layout
  p_metrics <- p_nrmse / p_rmse / p_mape
  
  # Plot for coefficients
  p2 <- ggplot(df_coefs, aes(x = Time, y = Value, color = Variable)) +
    geom_line() +
    facet_wrap( ~ Variable, scales = "free_x") +
    theme_minimal() +
    labs(title = "Coefficient Trends Over Time", x = "Time", y = "Proportion of models with the correct sign") +
    theme(legend.position = "none")
  
  # Print plots
  print(p_metrics)
  print(p2)
  
}
plot_coefficient_trends(bootstrap_results_normal)
plot_coefficient_trends(bootstrap_results_epan)
plot_coefficient_trends(bootstrap_results_biweight)
plot_coefficient_trends(bootstrap_results_triangular)
