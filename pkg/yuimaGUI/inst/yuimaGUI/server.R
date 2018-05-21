options(shiny.maxRequestSize = 100*1024^2)
options("getSymbols.warning4.0"=FALSE)

server <- function(input, output, session) {
  
  # session$onSessionEnded(function() {
  #   stopApp()
  # })

  source("server/settings.R", local = TRUE)
  source("server/functions.R", local = TRUE)

  source("server/home/home.R", local = TRUE)
  
  source("server/load_data/finance.R", local = TRUE)
  source("server/load_data/your_file.R", local = TRUE)
  
  source("server/eda/clustering.R", local = TRUE)
  source("server/eda/changepoint_non_parametric.R", local = TRUE)
  source("server/eda/changepoint_parametric.R", local = TRUE)
  source("server/eda/llag_and_corr.R", local = TRUE)

  source("server/modeling/univariate_start_estimation.R", local = TRUE)
  source("server/modeling/univariate_set_model.R", local = TRUE)
  source("server/modeling/univariate_results.R", local = TRUE)
  
  source("server/modeling/multivariate_start_estimation.R", local = TRUE)
  source("server/modeling/multivariate_results.R", local = TRUE)

  source("server/simulation/univariate_estimated.R", local = TRUE)
  source("server/simulation/univariate_non_estimated.R", local = TRUE)
  source("server/simulation/univariate_results.R", local = TRUE)
  
  source("server/simulation/multivariate_estimated.R", local = TRUE)
  source("server/simulation/multivariate_non_estimated.R", local = TRUE)
  source("server/simulation/multivariate_results.R", local = TRUE)
  
  source("server/finance/profit_and_loss.R", local = TRUE)
  
}




