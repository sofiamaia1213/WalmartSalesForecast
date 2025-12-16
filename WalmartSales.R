# 
# ## Libraries I need
# library(tidyverse)
# library(vroom)
# library(tidymodels)
# library(DataExplorer)
# 
# ## Read in the Data
# train <- vroom("GitHub/WalmartSalesForecast/train.csv")
# test <- vroom("GitHub/WalmartSalesForecast/test.csv")
# features <- vroom("GitHub/WalmartSalesForecast/features.csv")
# 
# 
# ### Impute Missing Markdowns
# features <- features %>%
#   mutate(across(starts_with("MarkDown"), ~ replace_na(., 0))) %>%
#   mutate(across(starts_with("MarkDown"), ~ pmax(., 0))) %>%
#   mutate(
#     MarkDown_Total = rowSums(across(starts_with("MarkDown")), na.rm = TRUE),
#     MarkDown_Flag = if_else(MarkDown_Total > 0, 1, 0),
#     MarkDown_Log   = log1p(MarkDown_Total)
#   ) %>%
#   select(-MarkDown1, -MarkDown2, -MarkDown3, -MarkDown4, -MarkDown5)
# 
# ## Impute Missing CPI and Unemployment
# feature_recipe <- recipe(~., data=features) %>%
#   step_mutate(DecDate = decimal_date(Date)) %>%
#   step_impute_bag(CPI, Unemployment,
#                   impute_with = imp_vars(DecDate, Store))
# imputed_features <- juice(prep(feature_recipe))
# 
# ########################
# ## Merge the Datasets ##
# ########################
# 
# fullTrain <- left_join(train, imputed_features, by=c("Store", "Date")) %>%
#   select(-IsHoliday.y) %>%
#   rename(IsHoliday=IsHoliday.x) %>%
#   select(-MarkDown_Total)
# fullTest <- left_join(test, imputed_features, by=c("Store", "Date")) %>%
#   select(-IsHoliday.y) %>%
#   rename(IsHoliday=IsHoliday.x) %>%
#   select(-MarkDown_Total)
# plot_missing(fullTrain)
# plot_missing(fullTest)
# 
# 
# ## Adding Prophet Model ##
# library(prophet)
# 
# ## Choose Store and Dept
# store <-  1
#   dept <- 3
# 
# ## Filter and Rename to match prophet syntax
# sd_train <- fullTrain %>%
# filter(Store==store, Dept==dept) %>%
# rename(y=Weekly_Sales, ds=Date)
# sd_test <- fullTest %>%
# filter(Store==store, Dept==dept) %>%
# rename(ds=Date)
# 
# ## Fit a prophet model
# prophet_model <- prophet() %>%
# add_regressor("Fuel_Price") %>%
# add_regressor("CPI") %>%
# add_regressor("Unemployment") %>%
# fit.prophet(df=sd_train)
# 
# ## Predict Using Fitted prophet Model1
# fitted_vals <- predict(prophet_model, df=sd_train) #For Plotting Fitted Values
# test_preds <- predict(prophet_model, df=sd_test) #Predictions are called "yhat"
# 
# ## Plot Fitted and Forecast on Same Plot5
# ggplot() +
# geom_line(data = sd_train, mapping = aes(x = ds, y = y, color = "Data")) +
# geom_line(data = fitted_vals, mapping = aes(x = as.Date(ds), y = yhat, color = "Fitted")) +
# geom_line(data = test_preds, mapping = aes(x = as.Date(ds), y = yhat, color = "Forecast")) +
# scale_color_manual(values = c("Data" = "black", "Fitted" = "blue", "Forecast" = "red")) +
# labs(color="")
# 
# ## Predict using Prophet Model
# predict(prophet_model, df=new_data)
# 
# ##################################
# ## Loop Through the Store-depts ##
# ## and generate predictions.    ##
# ##################################
# all_preds <- tibble(Id = character(), Weekly_Sales = numeric())
# n_storeDepts <- fullTest %>% distinct(Store, Dept) %>% nrow()
# cntr <- 0
# for(store in unique(fullTest$Store)){
# 
#   store_train <- fullTrain %>%
#     filter(Store==store)
#   store_test <- fullTest %>%
#     filter(Store==store)
# 
#   for(dept in unique(store_test$Dept)){
# 
#     ## Filter Test and Training Data
#     dept_train <- store_train %>%
#       filter(Dept==dept)
#     dept_test <- store_test %>%
#       filter(Dept==dept)
# 
#     ## If Statements for data scenarios
#     if(nrow(dept_train)==0){
# 
#       ## Predict 0
#       preds <- dept_test %>%
#         transmute(Id=paste(Store, Dept, Date, sep="_"),
#                   Weekly_Sales=0)
# 
#     } else if(nrow(dept_train) < 10 && nrow(dept_train) > 0){
# 
#       ## Predict the mean
#       preds <- dept_test %>%
#         transmute(Id=paste(Store, Dept, Date, sep="_"),
#                   Weekly_Sales=mean(dept_train$Weekly_Sales))
# 
#     } else {
# 
#       ## Fit a penalized regression model
#       my_recipe <- recipe(Weekly_Sales ~ ., data = dept_train) %>%
#         step_mutate(Holiday = as.integer(IsHoliday)) %>%
#         step_date(Date, features=c("month","year")) %>%
#         step_rm(Date, Store, Dept, IsHoliday)
#       prepped_recipe <- prep(my_recipe)
#       tst <- bake(prepped_recipe, new_data=dept_test)
# 
#       my_model <- rand_forest(mtry=3,
#                               trees=100,
#                               min_n=5) %>%
#         set_engine("ranger") %>%
#         set_mode("regression")
# 
#       my_wf <- workflow() %>%
#         add_recipe(my_recipe) %>%
#         add_model(my_model) %>%
#         fit(dept_train)
# 
#       preds <- dept_test %>%
#         transmute(Id=paste(Store, Dept, Date, sep="_"),
#                   Weekly_Sales=predict(my_wf, new_data = .) %>%
#                     pull(.pred))
# 
#     }
# 
#     ## Bind predictions together
#     all_preds <- bind_rows(all_preds,
#                            preds)
# 
#     ## Print out Progress
#     cntr <- cntr+1
#     cat("Store", store, "Department", dept, "Completed.",
#         round(100 * cntr / n_storeDepts, 1), "% overall complete.\n")
# 
#   } ## End Dept Loop
# 
# } ## End Store Loop
# 
# ## Write out after each store so I don't have to start over
# vroom_write(x=all_preds,
#             file=paste0("GitHub/WalmartSalesForecast/Predictions.csv"), delim=",")
# 
library(tidyverse)
library(vroom)
library(tidymodels)
library(DataExplorer)
library(prophet)
library(ranger)
library(lubridate)

# Read data
train <- vroom("GitHub/WalmartSalesForecast/train.csv")
test <- vroom("GitHub/WalmartSalesForecast/test.csv")
features <- vroom("GitHub/WalmartSalesForecast/features.csv")

# Impute MarkDowns
features <- features %>%
  mutate(across(starts_with("MarkDown"), ~replace_na(.,0))) %>%
  mutate(across(starts_with("MarkDown"), ~pmax(.,0))) %>%
  mutate(
    MarkDown_Total = rowSums(across(starts_with("MarkDown")), na.rm = TRUE),
    MarkDown_Flag = if_else(MarkDown_Total > 0, 1, 0),
    MarkDown_Log   = log1p(MarkDown_Total)
  ) %>%
  select(-starts_with("MarkDown"))

# Impute CPI and Unemployment
feature_recipe <- recipe(~., data=features) %>%
  step_mutate(DecDate = decimal_date(Date)) %>%
  step_impute_bag(CPI, Unemployment, impute_with = imp_vars(DecDate, Store))
imputed_features <- juice(prep(feature_recipe))

# Merge datasets
fullTrain <- left_join(train, imputed_features, by=c("Store", "Date")) %>%
  select(-IsHoliday.y) %>% rename(IsHoliday=IsHoliday.x)
fullTest <- left_join(test, imputed_features, by=c("Store", "Date")) %>%
  select(-IsHoliday.y) %>% rename(IsHoliday=IsHoliday.x)

# Precompute Store-Dept aggregates
dept_stats <- fullTrain %>%
  group_by(Store, Dept) %>%
  summarize(
    Mean_Sales = mean(Weekly_Sales, na.rm=TRUE),
    .groups = "drop"
  )

all_preds <- tibble(Id = character(), Weekly_Sales = numeric())
n_storeDepts <- fullTest %>% distinct(Store, Dept) %>% nrow()
cntr <- 0

for(store in unique(fullTest$Store)){
  store_train <- fullTrain %>% filter(Store==store)
  store_test  <- fullTest %>% filter(Store==store)
  
  for(dept in unique(store_test$Dept)){
    dept_train <- store_train %>% filter(Dept==dept) %>% arrange(Date)
    dept_test  <- store_test %>% filter(Dept==dept) %>% arrange(Date)
    
    dept_train <- left_join(dept_train, dept_stats, by=c("Store","Dept"))
    dept_test  <- left_join(dept_test, dept_stats, by=c("Store","Dept"))
    
    # Handle sparse data
    if(nrow(dept_train)==0){
      preds <- dept_test %>%
        transmute(Id=paste(Store, Dept, Date, sep="_"), Weekly_Sales=0)
    } else if(nrow(dept_train) < 10){
      preds <- dept_test %>%
        transmute(Id=paste(Store, Dept, Date, sep="_"), Weekly_Sales=mean(dept_train$Weekly_Sales))
    } else {
      # Prepare Random Forest features (original ones only)
      my_recipe <- recipe(Weekly_Sales ~ ., data = dept_train) %>%
        step_mutate(Holiday = as.integer(IsHoliday)) %>%
        step_rm(Date, Store, Dept, IsHoliday)
      
      # Tuned Random Forest
      my_model <- rand_forest(
        mtry = 5,       # tune: try sqrt(p) or p/3
        trees = 500,    # more trees
        min_n = 3       # smaller leaf size
      ) %>%
        set_engine("ranger", sample.fraction=0.7) %>%
        set_mode("regression")
      
      my_wf <- workflow() %>%
        add_recipe(my_recipe) %>%
        add_model(my_model) %>%
        fit(dept_train)
      
      rf_preds <- predict(my_wf, new_data = dept_test) %>% pull(.pred)
      
      # Prophet model
      prophet_train <- dept_train %>% rename(y = Weekly_Sales, ds = Date)
      prophet_test  <- dept_test %>% rename(ds = Date)
      
      m <- prophet(
        yearly.seasonality = TRUE,
        weekly.seasonality = TRUE,
        seasonality.mode = "multiplicative",
        changepoint.prior.scale = 0.05
      )
      
      # Add original regressors
      m <- m %>%
        add_regressor("Fuel_Price") %>%
        add_regressor("CPI") %>%
        add_regressor("Unemployment")
      
      m <- fit.prophet(m, prophet_train)
      prophet_preds <- predict(m, prophet_test)$yhat
      
      # Blend RF + Prophet
      blended_preds <- 0.5 * rf_preds + 0.5 * prophet_preds
      
      preds <- dept_test %>%
        transmute(Id = paste(Store, Dept, Date, sep="_"),
                  Weekly_Sales = blended_preds)
    }
    
    all_preds <- bind_rows(all_preds, preds)
    
    cntr <- cntr+1
    cat("Store", store, "Department", dept, "Completed.",
        round(100 * cntr / n_storeDepts, 1), "% overall complete.\n")
  }
}

vroom_write(all_preds, file="GitHub/WalmartSalesForecast/PredictionsRF.csv", delim=",")
