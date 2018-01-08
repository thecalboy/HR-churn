install.packages('readxl')
install.packages('h2o')
install.packages('lime')
library(readxl)
library(h2o)
library(lime)
library(tidyquant)
hr_data_raw = read_excel("Employment.xlsx")
hr_data = hr_data_raw %>%
  mutate_if(is.character, as.factor) %>%
  select(Attrition, everything())
h2o.init()
h2o.no_progress()
hr_data_h2o <- as.h2o(hr_data)

split_h2o <- h2o.splitFrame(hr_data_h2o, c(0.7, 0.15), seed = 1234 )

train_h2o <- h2o.assign(split_h2o[[1]], "train" ) 
valid_h2o <- h2o.assign(split_h2o[[2]], "valid" ) 
test_h2o  <- h2o.assign(split_h2o[[3]], "test" )

y <- "Attrition"
x <- setdiff(names(train_h2o), y)
automl_models_h2o <- h2o.automl(
  x = x, 
  y = y,
  training_frame    = train_h2o,
  leaderboard_frame = valid_h2o,
  max_runtime_secs  = 30
)
automl_leader <- automl_models_h2o@leader
pred_h2o <- h2o.predict(object = automl_leader, newdata = test_h2o)
test_performance <- test_h2o %>%
  tibble::as_tibble() %>%
  select(Attrition) %>%
  add_column(pred = as.vector(pred_h2o$predict)) %>%
  mutate_if(is.character, as.factor)
confusion_matrix <- test_performance %>%
  table() 
confusion_matrix
tn <- confusion_matrix[1]
tp <- confusion_matrix[4]
fp <- confusion_matrix[3]
fn <- confusion_matrix[2]

accuracy <- (tp + tn) / (tp + tn + fp + fn)
misclassification_rate <- 1 - accuracy
recall <- tp / (tp + fn)
precision <- tp / (tp + fp)
null_error_rate <- tn / (tp + tn + fp + fn)

tibble(
  accuracy,
  misclassification_rate,
  recall,
  precision,
  null_error_rate
) %>% 
  transpose() 
class(automl_leader)
model_type.H2OBinomialModel <- function(x, ...) {
  # Function tells lime() what model type we are dealing with
  # 'classification', 'regression', 'survival', 'clustering', 'multilabel', etc
  #
  # x is our h2o model
  
  return("classification")
}
# Setup lime::predict_model() function for h2o
predict_model.H2OBinomialModel <- function(x, newdata, type, ...) {
  # Function performs prediction and returns dataframe with Response
  #
  # x is h2o model
  # newdata is data frame
  # type is only setup for data frame
  
  pred <- h2o.predict(x, as.h2o(newdata))
  
  # return probs
  return(as.data.frame(pred[,-1]))
  
}
predict_model(x = automl_leader, newdata = as.data.frame(test_h2o[,-1]), type = 'raw') %>%
  tibble::as_tibble()
explainer <- lime::lime(
  as.data.frame(train_h2o[,-1]), 
  model          = automl_leader, 
  bin_continuous = TRUE)
explanation <- lime::explain(
  as.data.frame(test_h2o[1:10,-1]), 
  explainer    = explainer, 
  n_labels     = 1, 
  n_features   = 4,
  kernel_width = 0.5)
plot_features(explanation) +
  labs(title = "HR Predictive Analytics: LIME Feature Importance Visualization",
       subtitle = "Hold Out (Test) Set, First 10 Cases Shown")

attrition_critical_features <- hr_data %>%
  tibble::as_tibble() %>%
  select(Attrition, TrainingTimesLastYear, JobRole, OverTime) %>%
  rowid_to_column(var = "Case")
attrition_critical_features
p <- ggplot(hr_data, aes(x=Attrition, y = TrainingTimesLastYear))+
  geom_violin(trim = TRUE, fill = 'blue', color= "darkred")
p

p + geom_jitter(shape = 16, position = position_jitter(0.5),color = "orange")

attrition_critical_features %>%
  mutate(OverTime = ifelse(OverTime == "No", 0,1)) %>%
  ggplot(aes(Attrition, OverTime))+
  geom_jitter(alpha = 0.5, color = "orange")+
  geom_violin(alpha = 0.7, fill = "purple", color = "darkred")+
  theme_tq()+
  labs(
    title = "Prevalance of Over Time is higher in Attrition = Yes",
    subtitle = "Suggests that increased overtime is related to higher attrition"
  )


attrition_critical_features %>%
  group_by(JobRole, Attrition) %>% 
  summarize(
    total = n()) %>%
  spread(key = Attrition, value = total) %>%
  mutate(pct_attrition = Yes / (Yes + No)) %>%
  ggplot(aes(x = forcats::fct_reorder(JobRole, pct_attrition), y = pct_attrition)) +
  geom_bar(stat = "identity", alpha = 1, fill = palette_light()[[1]]) +
  expand_limits(y = c(0, 1)) +
  coord_flip() +
  theme_tq() +
  labs(
    title = "Attrition Varies By Job Role",
    subtitle = "Sales Rep, Lab Tech, HR, Sales Exec, and Research Scientist have much higher turnover",
    y = "Attrition Percentage (Yes / Total)",
    x = "JobRole")