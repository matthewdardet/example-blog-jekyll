#' @title GOV 1347: Introductory Blog Post/Laboratory Session
#' @author Matthew E. Dardet
#' @date November 4, 2024

####----------------------------------------------------------#
#### Preamble
####----------------------------------------------------------#

# Load libraries.
library(data.table)
library(mlr3verse)
library(tidyverse)

####----------------------------------------------------------#
#### Read, clean, merge data.
####----------------------------------------------------------#

# Read presidential popular vote. 
d_pv_natl <- read_csv("data/02/popvote_1948_2020.csv")
d_pv_state <- read_csv("data/02/state_popvote_1948_2020.csv")
d_pv_state[d_pv_state$state == "District Of Columbia",]$state <- "District of Columbia"

# Read number of electors by state-year. 
d_ec <- read_csv("data/02/corrected_ec_1948_2024.csv")

# Read polling data.
d_poll_natl <- read_csv("data/02/national_polls_1968-2024.csv")
d_poll_state <- read_csv("data/02/state_polls_1968-2024.csv")

# Read expert ratings. 
d_cook <- read_csv("data/02/CPR_EC_Ratings.csv") |> 
  select(year = Cycle, state = State, rating = Rating)
d_cook_24 <- data.frame(
  state = c("California", "Colorado", "Connecticut", "Delaware", "District of Columbia", "Hawaii", "Illinois",
            "Maine (01)", "Maryland", "Massachusetts", "New Jersey", "New York", "Oregon", "Rhode Island",
            "Vermont", "Washington", 

            "Maine", "Minnesota", "Nebraska (02)", "New Hampshire", "New Mexico", "Virginia", 
            "Arizona", "Georgia", "Michigan", "Nevada", "North Carolina", "Pennsylvania", "Wisconsin",
            "Florida", "Iowa", "Maine (02)", "Texas",
            
            "Alabama", "Alaska", "Arkansas", "Idaho", "Indiana", "Kansas", "Kentucky", "Louisiana", "Mississippi", "Missouri", "Montana",
            "Nebraska", "Nebraska (01)", "Nebraska (03)", "North Dakota", "Ohio", "Oklahoma", "South Carolina",
            "South Dakota", "Tennessee", "Utah", "West Virginia", "Wyoming"), 
  rating = c(rep("Solid D", 16), 
             rep("Likely D", 6),
             rep("Toss Up", 7), 
             rep("Likely R", 4), 
             rep("Solid R", 23)),
  year = rep(2024, 56)
)
d_cook <- rbind(d_cook, d_cook_24)

# Read economic data. 
d_econ <- read_csv("data/02/fred_econ.csv")

# Read state-level demographics data.
d_demog <- read_csv("data/02/demographics.csv")

# Create national-level dataset.
d_poll_natl <- d_poll_natl |> 
  mutate(party = case_when(party == "REP" ~ "R", 
                           .default = "D"), 
         poll_date = as.Date(poll_date, "%m/%d/%y"),
         poll_month = month(poll_date)) |> 
  filter(party == "D")

d_poll_natl_weeks <- d_poll_natl |> # Get polling averages by week. 
  drop_na(weeks_left) |> 
  group_by(party, year, weeks_left, poll_support) |>
  summarize(poll_support = mean(poll_support, na.rm = T)) |> 
  distinct(weeks_left, .keep_all = TRUE) |> 
  ungroup() |> 
  pivot_wider(names_from = weeks_left, values_from = poll_support, names_prefix = "poll_weekleft_")

d_poll_natl_months <- d_poll_natl |> # Get polling averages by month.
  drop_na(poll_month) |> 
  group_by(party, year, poll_month, poll_support) |>
  arrange(poll_month) |>
  summarize(poll_support = mean(poll_support, na.rm = T)) |>
  distinct(poll_month, .keep_all = TRUE) |>
  ungroup() |>
  pivot_wider(names_from = poll_month, values_from = poll_support, names_prefix = "poll_month_")

# Ad hoc carry-forward imputation. 
colSums(is.na(d_poll_natl_weeks))
for (i in 31:36) {
  current_col <- paste0("poll_weekleft_", i)
  previous_col <- paste0("poll_weekleft_", i - 1)
  
  d_poll_natl_weeks[[current_col]][is.na(d_poll_natl_weeks[[current_col]])] <- 
    d_poll_natl_weeks[[previous_col]][is.na(d_poll_natl_weeks[[current_col]])]
}

colSums(is.na(d_poll_natl_months))
for (i in 3:2) {
  current_col <- paste0("poll_month_", i)
  previous_col <- paste0("poll_month_", i + 1)
  
  d_poll_natl_months[[current_col]][is.na(d_poll_natl_months[[current_col]])] <- 
    d_poll_natl_months[[previous_col]][is.na(d_poll_natl_months[[current_col]])]
}

d_n <- d_pv_natl |> 
  mutate(incumbent = as.numeric(incumbent),
         party = case_when(party == "democrat" ~ "D",
                           .default = "R")) |> 
  filter(party == "D") |> 
  left_join(d_econ[d_econ$quarter == 2,], by = "year") |> 
  left_join(d_poll_natl_weeks, by = "year") |>
  left_join(d_poll_natl_months, by = "year") |> 
  arrange(year) |> 
  mutate(pv_lag1 = lag(pv, 1), 
         pv_lag2 = lag(pv, 2), 
         pv2p_lag1 = lag(pv2p, 1), 
         pv2p_lag2 = lag(pv2p, 2)) |>
  mutate(GDP_x_incumbent = GDP * incumbent,
         GDP_growth_x_incumbent = GDP_growth_quarterly * incumbent,
         RDPI_x_incumbent = RDPI * incumbent,
         RDPI_growth_x_incumbent = RDPI_growth_quarterly * incumbent,
         CPI_x_incumbent = CPI * incumbent,
         unemployment_x_incumbent = unemployment * incumbent,
         sp500_close_x_incumbent = sp500_close * incumbent) |> 
  select(year, pv, pv2p, pv_lag1, pv2p_lag1, pv_lag2, pv2p_lag2, incumbent, GDP, GDP_growth_quarterly, RDPI, RDPI_growth_quarterly, CPI, unemployment, sp500_close,
         GDP_x_incumbent, GDP_growth_x_incumbent, RDPI_x_incumbent, RDPI_growth_x_incumbent, CPI_x_incumbent, unemployment_x_incumbent, sp500_close_x_incumbent,
         all_of(paste0("poll_month_", 2:11)), all_of(paste0("poll_weekleft_", 0:36))) |> 
  filter(year >= 1968) 

# Create state-level dataset. 
# Find states with 2024 polling data. 
states.2024 <- d_poll_state[d_poll_state$year == 2024,]$state |> unique()

d_poll_state <- d_poll_state |> 
  mutate(party = case_when(party == "REP" ~ "R", 
                           .default = "D"), 
         poll_date = as.Date(poll_date, "%m/%d/%y"),
         poll_month = month(poll_date)) |> 
  filter(party == "D", state != "Maine Cd 2", state != "Nebraska Cd 2", state %in% states.2024, 
         year >= 1988) 

d_poll_state_weeks <- d_poll_state |> # Get polling averages by week.
  drop_na(weeks_left) |> 
  group_by(state, year, weeks_left, poll_support) |>
  summarize(poll_support = mean(poll_support, na.rm = T)) |> 
  distinct(weeks_left, .keep_all = TRUE) |> 
  ungroup() |> 
  pivot_wider(names_from = weeks_left, values_from = poll_support, names_prefix = "poll_weekleft_")

d_poll_state_months <- d_poll_state |> # Get polling averages by month.
  drop_na(poll_month) |> 
  group_by(state, year, poll_month, poll_support) |>
  arrange(poll_month) |>
  summarize(poll_support = mean(poll_support, na.rm = T)) |>
  distinct(poll_month, .keep_all = TRUE) |>
  ungroup() |>
  pivot_wider(names_from = poll_month, values_from = poll_support, names_prefix = "poll_month_")

# Ad hoc carry-forward imputation.
colSums(is.na(d_poll_state_weeks))
for (i in 2:36) {
  current_col <- paste0("poll_weekleft_", i)
  previous_col <- paste0("poll_weekleft_", i - 1)
  
  d_poll_state_weeks[[current_col]][is.na(d_poll_state_weeks[[current_col]])] <- 
    d_poll_state_weeks[[previous_col]][is.na(d_poll_state_weeks[[current_col]])]
}

colSums(is.na(d_poll_state_months))
for (i in 9:2) {
  current_col <- paste0("poll_month_", i)
  previous_col <- paste0("poll_month_", i + 1)
  
  d_poll_state_months[[current_col]][is.na(d_poll_state_months[[current_col]])] <- 
    d_poll_state_months[[previous_col]][is.na(d_poll_state_months[[current_col]])]
}

d_s <- d_poll_state_weeks |> 
  left_join(d_poll_state_months, by = c("state", "year")) |> 
  left_join(d_pv_state |> select(year, state, D_pv, D_pv2p, D_pv_lag1, D_pv2p_lag1, D_pv_lag2, D_pv2p_lag2), by = c("state", "year")) |>
  left_join(d_n |> select(year, incumbent), by = "year") |> 
  left_join(d_cook, by = c("state", "year")) |> 
  mutate(rating = as.numeric(as.factor(rating))) |> 
  arrange(state, year)

for (state in unique(d_s$state)) {
  d_s[["D_pv_lag1"]][d_s$state == state & d_s$year == 2024] <- d_s[["D_pv"]][d_s$state == state & d_s$year == 2020]
  d_s[["D_pv_lag2"]][d_s$state == state & d_s$year == 2024] <- d_s[["D_pv"]][d_s$state == state & d_s$year == 2016]
  d_s[["D_pv2p_lag1"]][d_s$state == state & d_s$year == 2024] <- d_s[["D_pv2p"]][d_s$state == state & d_s$year == 2020]
  d_s[["D_pv2p_lag2"]][d_s$state == state & d_s$year == 2024] <- d_s[["D_pv2p"]][d_s$state == state & d_s$year == 2016]
}
  
d_s <- d_s |> 
  select(year, state, D_pv, D_pv2p, D_pv_lag1, D_pv2p_lag1, D_pv_lag2, D_pv2p_lag2, incumbent, rating, 
         # GDP, GDP_growth_quarterly, RDPI, RDPI_growth_quarterly, CPI, unemployment, sp500_close,
         # GDP_x_incumbent, GDP_growth_x_incumbent, RDPI_x_incumbent, RDPI_growth_x_incumbent, CPI_x_incumbent, unemployment_x_incumbent, sp500_close_x_incumbent,
         all_of(paste0("poll_month_", 2:11)), all_of(paste0("poll_weekleft_", 0:36)))

####----------------------------------------------------------#
#### Part I. National Popular Vote 
####----------------------------------------------------------#

# Set learning task for regression-based prediction of national two-party vote share. 
d_n <- d_n |> select(-year, -pv)
tsk_natl = as_task_regr(d_n[-15,], target = "pv2p", id = "vote")
set.seed(02138)
splits = partition(tsk_natl)
measures = msrs(c("regr.bias", "regr.mae", "regr.mse", "regr.rmse"))

# Graph learner. 
graph_ensemble_regr = gunion(list(
  po("learner", lrn("regr.cv_glmnet", s = "lambda.min")),
  po("learner", lrn("regr.rpart", cp = 0.01)),
  po("learner", lrn("regr.ranger")),
  po("learner", lrn("regr.svm")),
  po("learner", lrn("regr.xgboost")),
  po("learner", lrn("regr.nnet"))
)) %>>%
  po("regravg", 6)

graph_ensemble_regr$plot()

ensemble_unweighted_natl = as_learner(graph_ensemble_regr, id = "ensemble_unweighted_natl")

# Set resampling strategy.
rcv25  = rsmp("repeated_cv", folds = 10, repeats = 5) 

# Graph ensemble (unweighted):
set.seed(02138)
ensemble_unweighted_natl$train(tsk_natl)
ensemble_unweighted_natl$predict_newdata(d_n[15,])

# Get split conformal confidence bounds. 
m <- 1e3
d.split <- rep(NA, m) 
for (i in 1:m) {
  alpha <- 0.05 
  n <- dim(tsk_natl$data())[1]
  split.index <- sample(1:n, size = n/2, replace = FALSE)
  split = partition(tsk_natl)
  split$train <- split.index
  split$test <- (1:n)[-split.index]
  ensemble_unweighted_natl$train(tsk_natl, row_ids = split$train)
  pred.split <- ensemble_unweighted_natl$predict(tsk_natl, row_ids = split$test)
  r <- abs(pred.split$truth - pred.split$response)
  k <- ceiling((n/2 + 1)*(1 - (alpha)))
  d <- sort(r)[k]
  d.split[i] <- quantile(r, (1-alpha))  
}
d.natl <- median(d.split)

# Concatenate national results. 
natl_results <- data.frame("year" = 2024, 
                           "state" = "National Popular Vote",
                           "pred" = ensemble_unweighted_natl$predict_newdata(d_n[15,])$response |> round(2)) |> 
  mutate(lo = pred - d.natl, 
         up = pred + d.natl) |> 
  select(state, lo, pred, up)
natl_results







####----------------------------------------------------------#
#### Part II. Electoral College (State-Level Forecasts)
####----------------------------------------------------------#
# Set learning task for regression-based prediction of national two-party vote share. 
indices.2024 <- which(d_s$year == 2024)
d_s_t <- d_s |> 
  mutate(state_int = as.integer(as.factor(state))) |> 
  select(-year, -state, -D_pv, -paste0("poll_weekleft_", 11:36), -paste0("poll_month_", 2:7))
tsk_state = as_task_regr(d_s_t[-indices.2024,], target = "D_pv2p", id = "state_vote")
set.seed(02138)
splits = partition(tsk_state)
measures = msrs(c("regr.bias", "regr.mae", "regr.mse", "regr.rmse"))

#### Part II.A. First Pass. 
# Graph ensemble (unweighted):
ensemble_unweighted_state = as_learner(graph_ensemble_regr, id = "ensemble_unweighted_state")
set.seed(02138)
ensemble_unweighted_state$train(tsk_state)
ensemble_unweighted_state$predict_newdata(d_s_t[indices.2024,])$response

# Get split conformal confidence bounds.
m <- 1e3
d.split <- rep(NA, m) 
for (i in 1:m) {
  alpha <- 0.05 
  n <- dim(tsk_state$data())[1]
  split.index <- sample(1:n, size = n/2, replace = FALSE)
  split = partition(tsk_natl)
  split$train <- split.index
  split$test <- (1:n)[-split.index]
  ensemble_unweighted_state$train(tsk_state, row_ids = split$train)
  pred.split <- ensemble_unweighted_state$predict(tsk_state, row_ids = split$test)
  r <- abs(pred.split$truth - pred.split$response)
  k <- ceiling((n/2 + 1)*(1 - (alpha)))
  d <- sort(r)[k]
  d.split[i] <- quantile(r, (1-alpha))  
}
d.state <- median(d.split)

# Concatenate state-level results. 
state_results <- data.frame("state" = d_s$state[indices.2024], 
                            "pred" = ensemble_unweighted_state$predict_newdata(d_s_t[indices.2024,])$response |> round(2)) 
state_results <- state_results |> 
  mutate(state_abb = state.abb[match(state_results$state, state.name)]) |> 
  mutate(lo = pred - d.state, 
         up = pred + d.state) |>
  select(state, state_abb, lo, pred, up)
state_results

results <- rbind(natl_results, 
                 state_results |> select(-state_abb))
results



####----------------------------------------------------------#
#### Part III. Electoral College (State-Level Forecasts)
####            Prediction Evaluation    
####----------------------------------------------------------#

state_outcomes <- read_csv("data/02/state_votes_pres_2024.csv")

state_outcomes$`Geographic Name`

# (1.) Bias 

results <- results |> 
  left_join(state_outcomes, by = c("state" = "Geographic Name")) |> 
  slice(-1) |> 
  mutate(harris_votes = as.numeric(`Kamala D. Harris`), 
         trump_votes = as.numeric(`Donald J. Trump`),
         harris_2pv = 100 * harris_votes/(harris_votes + trump_votes))

mean(results$harris_2pv - results$pred)

# (2.) MSE 

mean((results$harris_2pv - results$pred)^2)

# (3.) RMSE 

sqrt(mean((results$harris_2pv - results$pred)^2))

# (4.) MAE 

mean(abs(results$harris_2pv - results$pred))

# (5.) Confusion Matrix 
results <- results |> 
  mutate(pred_class = as.factor(case_when(pred > 50 ~ "DEM", 
                                          .default = "REP")),
         result_class = as.factor(case_when(harris_2pv > 50 ~ "DEM", 
                                            .default = "REP")))

table("Actual" = results$result_class, 
      "Prediction" = results$pred_class)

library(caret)

confusionMatrix(table("Actual" = results$result_class, 
                      "Prediction" = results$pred_class))




















  # Graveyard of fancy code :(

# ## Initial benchmarking test. 
# design = benchmark_grid(tsk_state, learners_state_1, rcv25)
# bmr = benchmark(design)
# results <- bmr$aggregate(measures)
# results[,regr.bias := abs(regr.bias)] # Change bias to absolute value. 
# 
# (setorder(results, regr.bias)) # XGboost wins.
# (setorder(results, regr.mae)) # SVM wins.
# (setorder(results, regr.mse)) # XGBoost wins.
# (setorder(results, regr.rmse)) # XGBoostSVM wins.

# # Initial benchmarking test. 
# design = benchmark_grid(tsk_natl, learners_natl_1, rcv25)
# bmr = benchmark(design)
# results <- bmr$aggregate(measures)
# results[,regr.bias := abs(regr.bias)] # Change bias to absolute value. 
# 
# (setorder(results, regr.bias)) # Ranger wins.
# (setorder(results, regr.mae)) # Ranger wins.
# (setorder(results, regr.mse)) # Ranger wins.
# (setorder(results, regr.rmse)) # Ranger wins.


# ## Part II.B. Second Pass with Tuned Models. 
# 
# # Tune hyperparameters for decision tree. 
# tnr_grid_search = tnr("grid_search", resolution = 5, batch_size = 10)
# mlr_tuning_spaces$get("regr.rpart.default")
# lrn_rpart = lrn("regr.rpart", 
#                 cp = to_tune(1e-04, 0.1, logscale = TRUE),
#                 minbucket = to_tune(1, 64, logscale = TRUE),
#                 minsplit = to_tune(2, 128, logscale = TRUE))
# at_rpart = auto_tuner(tuner = tnr_grid_search, 
#                       learner = lrn_rpart, 
#                       resampling = rcv25,
#                       measure = measures$regr.mse)
# at_rpart$train(tsk_state)
# at_rpart$tuning_instance$result
# # cp = -9.21034; minbucket = 2.087194; minsplit =  2.77648  
# # MSE: 13.97209
# 
# # Tune hyperparameters for ranger. 
# mlr_tuning_spaces$get("regr.ranger.default")
# tnr_random = tnr("random_search", batch_size = 50)
# lrn_ranger = lrn("regr.ranger", 
#                  mtry.ratio = to_tune(0.0, 1, logscale = FALSE),
#                  sample.fraction = to_tune(0.1, 1, logscale = FALSE),
#                  num.trees = to_tune(1, 2000, logscale = FALSE)
#               )
# at_ranger = auto_tuner(tuner = tnr_grid_search,
#                        learner = lrn_ranger, 
#                        resampling = rcv25,
#                        measure = measures$regr.mse,
#                        terminator = trm("evals", n_evals = 50))
# at_ranger$train(tsk_state)
# at_ranger$tuning_instance$result
# # mtry.ratio = 1; sample.fraction = 1; num.trees = 501
# # MSE: 9.618081
# 
# # Tune hyperparameters for XGBoost.
# mlr_tuning_spaces$get("regr.xgboost.default")
# lrn_xgboost = lrn("regr.xgboost", 
#                   eta = to_tune(1e-04, 1, logscale = TRUE),
#                   nrounds = to_tune(1, 5000, logscale = FALSE),
#                   max_depth = to_tune(1, 20, logscale = FALSE),
#                   colsample_bytree = to_tune(0.1, 1, logscale = FALSE),
#                   colsample_bylevel = to_tune(0.1, 1, logscale = FALSE),
#                   lambda = to_tune(1e-03, 1000, logscale = TRUE),
#                   alpha = to_tune(1e-03, 1000, logscale = TRUE),
#                   subsample = to_tune(0.1, 1, logscale = FALSE)
# )
# at_xgboost = auto_tuner(tuner = tnr_grid_search, 
#                         learner = lrn_xgboost, 
#                         resampling = rcv25,
#                         measure = measures$regr.mse, 
#                         terminator = trm("evals", n_evals = 50))
# at_xgboost$train(tsk_state)
# at_xgboost$tuning_instance$result
# # alpha = 0 ; colsample_bylevel = 0.55; colsample_bytree = 0.775 ; eta = -2.302585 ; lambda = 3.453878 ; max_depth = 16 ; nrounds = 3751 ; subsample = 0.55 
# # MSE: 8.642474
# 
# # Tune hyperparameters for SVM. 
# mlr_tuning_spaces$get("regr.svm.default")
# lrn_svm = lrn("regr.svm", 
#               cost = to_tune(1e-04, 10000, logscale = TRUE),
#               type = "eps-regression"
# )
# at_svm = auto_tuner(tuner = tnr_grid_search, 
#                     learner = lrn_svm, 
#                     resampling = rcv25,
#                     measure = measures$regr.mse, 
#                     terminator = trm("evals", n_evals = 50))
# at_svm$train(tsk_state)
# at_svm$tuning_instance$result
# # cost = 4.60517
# # MSE: 9.132871
# 
# # Get predictions with tuned models. 
# results_tuned_indiv = data.frame("state" = d_s$state[indices.2024], 
#                                  "svm_D_pv2p" = at_svm$predict_newdata(d_s_t[indices.2024,])$response,
#                                  "rpart_D_pv2p" = at_rpart$predict_newdata(d_s_t[indices.2024,])$response,
#                                  "ranger_D_pv2p" = at_ranger$predict_newdata(d_s_t[indices.2024,])$response,
#                                  "xgboost_D_pv2p" = at_xgboost$predict_newdata(d_s_t[indices.2024,])$response)
# write_csv(results_tuned_indiv, "data/02/state_results_tuned_2024.csv")
# 
# ## Part II.C. Third Pass with Tuned Models and Different Ensembles. 
# graph_ensemble_tuned_lm = ppl("stacking", 
#                            base_learners = list(
#                              lrn("regr.cv_glmnet", s = "lambda.min", id = "cv_glmnet"),
#                              lrn("regr.rpart", cp = 0.01, id = "tree"),
#                              lrn("regr.ranger", id = "ranger"),
#                              lrn("regr.ranger", mtry.ratio = 1, sample.fraction = 1, num.trees = 501, id = "ranger_tuned"), 
#                              lrn("regr.svm", id = "svm"),
#                              lrn("regr.svm", cost = 4.60517, type = "eps-regression", id = "svm_tuned"),
#                              lrn("regr.xgboost", id = "xgboost"),
#                              lrn("regr.xgboost", alpha = 0, colsample_bylevel = 0.55, colsample_bytree = 0.775, lambda = 3.453878, max_depth = 16, nrounds = 3751, subsample = 0.55, id = "xgboost_tuned"),
#                              lrn("regr.nnet", id = "nnet"),
#                              ensemble_pipe_regr_unweighted_1
#                            ), 
#                            super_learner = lrn("regr.lm"),
#                            method = "cv")
# 
# graph_ensemble_tuned_cvglmnet = ppl("stacking", 
#                            base_learners = list(
#                              lrn("regr.cv_glmnet", s = "lambda.min", id = "cv_glmnet"),
#                              lrn("regr.rpart", cp = 0.01, id = "tree"),
#                              lrn("regr.ranger", id = "ranger"),
#                              lrn("regr.ranger", mtry.ratio = 1, sample.fraction = 1, num.trees = 501, id = "ranger_tuned"), 
#                              lrn("regr.svm", id = "svm"),
#                              lrn("regr.svm", cost = 4.60517, type = "eps-regression", id = "svm_tuned"),
#                              lrn("regr.xgboost", id = "xgboost"),
#                              lrn("regr.xgboost", alpha = 0, colsample_bylevel = 0.55, colsample_bytree = 0.775, lambda = 3.453878, max_depth = 16, nrounds = 3751, subsample = 0.55, id = "xgboost_tuned"),
#                              lrn("regr.nnet", id = "nnet"),
#                              ensemble_pipe_regr_unweighted_1
#                            ), 
#                            super_learner = lrn("regr.cv_glmnet"),
#                            method = "cv")
# 
# graph_ensemble_tuned_ranger = ppl("stacking",
#                            base_learners = list(
#                              lrn("regr.cv_glmnet", s = "lambda.min", id = "cv_glmnet"),
#                              lrn("regr.rpart", cp = 0.01, id = "tree"),
#                              lrn("regr.ranger", id = "ranger"),
#                              lrn("regr.ranger", mtry.ratio = 1, sample.fraction = 1, num.trees = 501, id = "ranger_tuned"), 
#                              lrn("regr.svm", id = "svm"),
#                              lrn("regr.svm", cost = 4.60517, type = "eps-regression", id = "svm_tuned"),
#                              lrn("regr.xgboost", id = "xgboost"),
#                              lrn("regr.xgboost", alpha = 0, colsample_bylevel = 0.55, colsample_bytree = 0.775, lambda = 3.453878, max_depth = 16, nrounds = 3751, subsample = 0.55, id = "xgboost_tuned"),
#                              lrn("regr.nnet", id = "nnet"),
#                              ensemble_pipe_regr_unweighted_1
#                            ), 
#                            super_learner = lrn("regr.ranger"),
#                            method = "cv")
# 
# graph_ensemble_tuned_xgboost = ppl("stacking", 
#                                    base_learners = list(
#                                      lrn("regr.cv_glmnet", s = "lambda.min", id = "cv_glmnet"),
#                                      lrn("regr.rpart", cp = 0.01, id = "tree"),
#                                      lrn("regr.ranger", id = "ranger"),
#                                      lrn("regr.ranger", mtry.ratio = 1, sample.fraction = 1, num.trees = 501, id = "ranger_tuned"), 
#                                      lrn("regr.svm", id = "svm"),
#                                      lrn("regr.svm", cost = 4.60517, type = "eps-regression", id = "svm_tuned"),
#                                      lrn("regr.xgboost", id = "xgboost"),
#                                      lrn("regr.xgboost", alpha = 0, colsample_bylevel = 0.55, colsample_bytree = 0.775, lambda = 3.453878, max_depth = 16, nrounds = 3751, subsample = 0.55, id = "xgboost_tuned"),
#                                      lrn("regr.nnet", id = "nnet"),
#                                      ensemble_pipe_regr_unweighted_1
#                                    ), 
#                                    super_learner = lrn("regr.xgboost"),
#                                    method = "cv")
# 
# graph_ensemble_tuned_svm = ppl("stacking", 
#                                base_learners = list(
#                                  lrn("regr.cv_glmnet", s = "lambda.min", id = "cv_glmnet"),
#                                  lrn("regr.rpart", cp = 0.01, id = "tree"),
#                                  lrn("regr.ranger", id = "ranger"),
#                                  lrn("regr.ranger", mtry.ratio = 1, sample.fraction = 1, num.trees = 501, id = "ranger_tuned"), 
#                                  lrn("regr.svm", id = "svm"),
#                                  lrn("regr.svm", cost = 4.60517, type = "eps-regression", id = "svm_tuned"),
#                                  lrn("regr.xgboost", id = "xgboost"),
#                                  lrn("regr.xgboost", alpha = 0, colsample_bylevel = 0.55, colsample_bytree = 0.775, lambda = 3.453878, max_depth = 16, nrounds = 3751, subsample = 0.55, id = "xgboost_tuned"),
#                                  lrn("regr.nnet", id = "nnet"),
#                                  ensemble_pipe_regr_unweighted_1
#                                ), 
#                                super_learner = lrn("regr.svm"),
#                                method = "cv")
# 
# graph_ensemble_tuned_nnet = ppl("stacking",
#                                 base_learners = list(
#                                   lrn("regr.cv_glmnet", s = "lambda.min", id = "cv_glmnet"),
#                                   lrn("regr.rpart", cp = 0.01, id = "tree"),
#                                   lrn("regr.ranger", id = "ranger"),
#                                   lrn("regr.ranger", mtry.ratio = 1, sample.fraction = 1, num.trees = 501, id = "ranger_tuned"), 
#                                   lrn("regr.svm", id = "svm"),
#                                   lrn("regr.svm", cost = 4.60517, type = "eps-regression", id = "svm_tuned"),
#                                   lrn("regr.xgboost", id = "xgboost"),
#                                   lrn("regr.xgboost", alpha = 0, colsample_bylevel = 0.55, colsample_bytree = 0.775, lambda = 3.453878, max_depth = 16, nrounds = 3751, subsample = 0.55, id = "xgboost_tuned"),
#                                   lrn("regr.nnet", id = "nnet"),
#                                   ensemble_pipe_regr_unweighted_1
#                                 ), 
#                                 super_learner = lrn("regr.nnet"),
#                                 method = "cv")
# 
# super_lm = as_learner(graph_ensemble_tuned, resample = rcv25, id = "super_lm")
# super_cv_glmnet = as_learner(graph_ensemble_tuned, resample = rcv25, id = "super_cv_glmnet")
# super_ranger = as_learner(graph_ensemble_tuned, resample = rcv25, id = "super_ranger")
# super_xgboost = as_learner(graph_ensemble_tuned, resample = rcv25, id = "super_xgboost")
# super_svm = as_learner(graph_ensemble_tuned, resample = rcv25, id = "super_svm")
# super_nnet = as_learner(graph_ensemble_tuned, resample = rcv25, id = "super_nnet")
# 
# graph_ensemble_super_final = gunion(list(
#   po("learner", graph_ensemble_tuned_lm),
#   po("learner", graph_ensemble_tuned_cvglmnet),
#   po("learner", graph_ensemble_tuned_ranger),
#   po("learner", graph_ensemble_tuned_xgboost),
#   po("learner", graph_ensemble_tuned_svm),
#   po("learner", graph_ensemble_tuned_nnet)
# )) %>>%
#   po("regravg", 6)
# 
# final_super = as_learner(graph_ensemble_super_final, resample = rcv25, id = "final_super")
# 
# final_learners = lrns(c("regr.featureless", 
#                         "regr.lm", 
#                         "regr.cv_glmnet", 
#                         "regr.rpart", 
#                         "regr.ranger",
#                         "regr.svm", 
#                         "regr.xgboost", 
#                         "regr.nnet"))
# final_learners$ranger_tuned = lrn("regr.ranger", mtry.ratio = 1, sample.fraction = 1, num.trees = 501)
# final_learners$xgboost_tuned = lrn("regr.xgboost", alpha = 0, colsample_bylevel = 0.55, colsample_bytree = 0.775, lambda = 3.453878, max_depth = 16, nrounds = 3751, subsample = 0.55)
# final_learners$svm_tuned = lrn("regr.svm", cost = 4.60517, type = "eps-regression")
# final_learners$ensemble_unweighted = ensemble_pipe_regr_unweighted_1
# final_learners$super_lm = super_lm
# final_learners$super_cv_glmnet = super_cv_glmnet
# final_learners$super_ranger = super_ranger
# final_learners$super_xgboost = super_xgboost
# final_learners$super_svm = super_svm
# final_learners$super_nnet = super_nnet
# final_learners$final_super = final_super
# 
# # Initial benchmarking test. 
# design_final = benchmark_grid(tsk_natl, final_learners, rcv25)
# bmr_final = benchmark(design_final)
# results_final <- bmr_final$aggregate(measures)
# results_final[,regr.bias := abs(regr.bias)] # Change bias to absolute value. 
# 
# results_final$learner_id = c("baseline", "lm", "cv_glmnet", "tree", "randomforest", "svm", "xgboost", "nnet", 
#                              "ranger_tuned", "xgboost_tuned", "svm_tuned", 
#                              "unweighted_ensemble_untuned", 
#                              "weighted_tuned_super_lm", "weighted_tuned_super_cv_glmnet", "weighted_tuned_super_ranger", "weighted_tuned_super_xgboost", "weighted_tuned_super_svm", "weighted_tuned_super_nnet", 
#                              "weighted_tuned_super_final_avg")
# 
# write_csv(results_final, "data/02/final_results_2024.csv")
# 
# (setorder(results_final, regr.bias)) # CV GLMNET wins.
# (setorder(results_final, regr.mae)) # Ranger wins.
# (setorder(results_final, regr.mse)) # Ranger wins.
# (setorder(results_final, regr.rmse)) # Ranger wins.
# 
# # So let's go with ranger for everything then...
# 
# lrn_ranger_state_final = lrn("regr.ranger")
# set.seed(02138)
# lrn_ranger_state_final$train(tsk_state)
# 
# state_results_1 <- data.frame("state" = d_s$state[indices.2024], 
#                               "ranger_D_pv2p" = lrn_ranger_state_final$predict_newdata(d_s_t[indices.2024,])$response)
# state_results_1 



# for both national and state models 
# get polling errors from 2022 and 2020 
# predict models on training data to get super learning weights 


# graph_ensemble_tuned_1 = gunion(list(
#   po("learner_cv", lrn("regr.cv_glmnet", s = "lambda.min"), id = "cv_glmnet"),
#   po("learner_cv", lrn("regr.rpart", cp = 0.01), id = "rpart"),
#   po("learner", lrn("regr.ranger"), id = "ranger"),
#   po("learner_cv", lrn("regr.ranger", mtry.ratio = 1, sample.fraction = 1, num.trees = 501), id = "ranger_tuned"),
#   po("learner", lrn("regr.svm"), id = "svm"),  
#   po("learner_cv", lrn("regr.svm", cost = 4.60517, type = "eps-regression"), id = "svm_tuned"),
#   po("learner", lrn("regr.xgboost"), id = "xgboost"),
#   po("learner_cv", lrn("regr.xgboost", alpha = 0, colsample_bylevel = 0.55, colsample_bytree = 0.775, lambda = 3.453878, max_depth = 16, nrounds = 3751, subsample = 0.55), id = "xgboost_tuned"),
#   po("learner_cv", lrn("regr.nnet"), id = "nnet"), 
#   po("learner_cv", ensemble_pipe_regr_unweighted_1, id = "og_ensemble")
# )) 
# %>>% po("featureunion")

# # Conduct feature importance analyses. 
# flt_gain = flt("information_gain")
# flt_gain$calculate(tsk_natl)
# as.data.table(flt_gain)
# 
# lrn("regr.ranger")$param_set$levels$importance
# ranger_importance = lrn("regr.ranger", importance = "permutation")
# flt_ranger_importance = flt("importance", learner = ranger_importance)
# flt_ranger_importance$calculate(tsk_natl)
# as.data.table(flt_ranger_importance)
# 
# as.data.table(lrn("regr.lm")$param_set)[,.(id, class, lower, upper, nlevels)]

# (setorder(results[results$resampling_id == "subsampling"], regr.bias)) # Ranger
# (setorder(results[results$resampling_id == "subsampling"], regr.mae)) # Ranger
# (setorder(results[results$resampling_id == "subsampling"], regr.mse)) # Ranger
# (setorder(results[results$resampling_id == "subsampling"], regr.rmse)) # Ranger
# 
# (setorder(results[results$resampling_id == "cv"], regr.bias)) # RPART, nnet 
# (setorder(results[results$resampling_id == "cv"], regr.mae)) # Ranger 
# (setorder(results[results$resampling_id == "cv"], regr.mse)) # Ranger
# (setorder(results[results$resampling_id == "cv"], regr.rmse)) # Ranger
# 
# (setorder(results[results$resampling_id == "repeated_cv"], regr.bias)) # CV_glmnet
# (setorder(results[results$resampling_id == "repeated_cv"], regr.mae)) # Ranger
# (setorder(results[results$resampling_id == "repeated_cv"], regr.mse)) # Ranger
# (setorder(results[results$resampling_id == "repeated_cv"], regr.rmse)) # Ranger wins!