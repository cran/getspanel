## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = NA,
  echo = TRUE,
  message = FALSE,
  error = TRUE,
  eval = TRUE,
  out.width = "100%",
  fig.width = 7,
  fig.height = 5,
  dev = "png",
  dpi = 300
)

## -----------------------------------------------------------------------------
devtools::load_all()
set.seed(99726)
# Simulation parameters (panel structure and data generation)
n_ids <- c(3)
n_times <- c(10)
beta <- c(0.3, 0.7, -.3, 0, 0) # the betas for the coefficients
sigma <- 0.5
fe_sigma <- 5
# Treatment parameters (imposed treatments to be detected)
treatment_params_list <- list(
  tribble(
    ~id, ~type, ~location, ~magnitude,
    0.3, "trendbreak", 0.5, 2,
    0.7, "step", 0.3, 2
  )
)
# Benchmark parameters (getspanel parameters to be varied)
n_rep <- 1
methods <- c("both")
t.pvals <- c(0.05, 0.01, 0.001)

## ----warning=FALSE------------------------------------------------------------
results <- run_simulation_study(
  n_ids, n_times, beta, sigma, fe_sigma, treatment_params_list, n_rep, methods, t.pvals, print.searchinfo = FALSE, plot = FALSE
)
results

## -----------------------------------------------------------------------------
metrics <- metrics_summary(results)
metrics

## -----------------------------------------------------------------------------
plot_metrics(metrics$per_simulation, metrics = c("gauge", "potency"), plot_type = "scatter")

## -----------------------------------------------------------------------------
input_data <- create_input_data(
  n_id = 3,
  n_time = 10,
  treatment_params = tribble( ~ id, ~ type, ~ location, ~ magnitude),
  fe_sigma = 5,
  beta = c(0.3, 0.7, -0.3, 0, 0),
  sigma = 0.5,
  plot = TRUE
)

## -----------------------------------------------------------------------------
treatment_params <- tribble(
  ~id, ~type, ~location, ~magnitude,
  0.3, "trendbreak", 0.5, 2,
  0.7, "step", 0.3, 2
)
single_run <- run_single_model(
  sim_id = 1,
  n_id = 3,
  n_time = 10,
  method = "both",
  treatment_params,
  fe_sigma = 5,
  beta = c(0.3, 0.7, -0.3, 0, 0),
  sigma = 0.5,
  t.pval = 0.05,
  max.block.size = 30,
  print.searchinfo = TRUE,
  plot = TRUE
)
single_run

## -----------------------------------------------------------------------------
extracted_treatments <- extract_treatments(single_run)
extracted_treatments$true_treatments
extracted_treatments$detected_treatments
matched_treatments <- optimal_match_treatments(extracted_treatments$true_treatments, extracted_treatments$detected_treatments, tolerance = 2)
matched_treatments

## -----------------------------------------------------------------------------
set.seed(99726)
# Simulation parameters (panel structure and data generation)
n_ids <- c(2,5,10)
n_times <- c(20,30,100)

sigma <- 0.5

# Treatment parameters (imposed treatments to be detected)
treatment_params_list <- list(
  tribble(
    ~id, ~type, ~location, ~magnitude,
    0.3, "trendbreak", 0.5, 2,
    0.7, "step", 0.3, 4 # ,
    #0.45, "impulse", -0.3, 2.5
  )
)
# Benchmark parameters (getspanel parameters to be varied)
n_rep <- 5
methods <- c("both")
t.pvals <- c(0.05, 
             #0.01, 
             0.001)

# not yet in there
# ar
# impulses
# max.block.size
# more treatments
# check TIS location and treatment table again
# steps should be % of unit FE

beta <- c(0.3, 0.7, -.3, 0, 0) # the betas for the coefficients
fe_sigma <- 5



