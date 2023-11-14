
# load packages -----------------------------------------------------------

library("tidyverse"); theme_set(theme_minimal())
library("here")
library("caret")
library("kableExtra")
library("keras")
library("tensorflow")
library("reticulate")


# read data ---------------------------------------------------------------

sn_attempts <- read_rds("sn_attempts.rds") %>%
  filter(division != "youth") %>%
  select(name_born, name, nation, bw, group, division, age, max_sn,
         max_total, sn_career_make_perc, sn_avg_rank, comp_avg_rank,
         years_wl, sn_attempt, sn_perc, make) %>%
  mutate() %>%
  group_by(name_born) %>%
  filter(n() > 6) %>%
  ungroup()

set.seed(123)
#If you do not have an ID per row, use the following code to create an ID
tb <- sn_attempts %>% mutate(id = row_number())
#Create training set
train <- tb %>% sample_frac(.7)
#Create test set
test  <- anti_join(tb, train, by = 'id') %>% select(-id)
train <- select(train, -id)


# Build Model -------------------------------------------------------------

train_matrix <- model.matrix(make ~ sn_perc + bw + age + sn_career_make_perc, train)[, -1]

# initialize model

nn_mod <- keras_model_sequential() %>%
  layer_dense(units = 32, input_shape = ncol(train_matrix), activation = "relu") %>%
  layer_dropout(.25) %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dropout(.25) %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dropout(.25) %>%
  layer_dense(units = 1, activation = "sigmoid")

# summary(nn_mod)

nn_mod %>% compile(loss = "mse",
                   optimizer = optimizer_rmsprop(),
                   metrics = c("accuracy"))
                   # metrics=tf.keras.metrics.BinaryAccuracy(threshold = .65))

train_label <- train %>% pull(make)


set.seed(123L)
# Actually train our model! This step will take a while
trained_nn <- nn_mod %>% fit(
  x = train_matrix, # using for prediction
  y = train_label, # predicting
  batch_size = 20, # how many samples to pass to our model at a time
  epochs = 45, # how many times we'll look @ the whole dataset
  validation_split = 0.3) # how much data to hold out for testing as we go along



# how well did our trained model do?
# trained_nn

# plot how our model performance changed during training
plot(trained_nn)

# Matrix by observation -- neural network
train_preds <- nn_mod %>% predict(train_matrix) %>% as.numeric()

test_matrix <- model.matrix(make ~ sn_perc + bw + age + career_make_perc, test)[, -1]
test_preds <- nn_mod %>% predict(test_matrix) %>% as.numeric()


# mean(((train %>% pull(price)) - train_preds)^2)
# mean(((test %>% pull(price)) - test_preds)^2)



test %>%
  mutate(pred = test_preds) %>%
  ggplot(aes(sn_perc, pred, color = as.factor(make))) +
    geom_point(alpha = .5)

train %>%
  mutate(pred = train_preds) %>%
  ggplot(aes(sn_perc, pred, color = as.factor(make))) +
    geom_point(alpha = .15)

all_pred <- test %>%
  mutate(pred = test_preds, train_test = "test") %>%
  bind_rows(mutate(train, pred = train_preds, train_test = "train"))

all_pred %>%
  filter(name %in% c("PIZZOLATO Antonino", "VALLENILLA SANCHEZ Keydomar Giovanni", "TIAN Tao")) %>%
  ggplot(aes(sn_perc, pred, color = as.factor(make))) +
  geom_point() +
  facet_wrap(vars(name))





# Predict For Athlete -----------------------------------------------------

nino <- sn_attempts %>%
  group_by(name_born) %>%
  filter(n() > 6) %>%
  arrange(years_wl) %>%
  slice(n()) %>%
  filter(name_born == "PIZZOLATO Antonino 1996-08-20") %>%
  mutate(years_wl = 11.4, sn_kg = sn_perc * max_sn) %>%
  slice(rep(1, each = 15)) %>%
  mutate(sn_kg = 166:180, sn_perc = sn_kg / max_sn, sn_attempt = factor(1, levels = c(1,2,3))) %>%
  relocate(make, .after = sn_kg)

nino <- nino %>%
  slice(rep(1:n(), each = 3)) %>%
  mutate(sn_attempt = as.factor(rep(1:3, each = 15)))

tian <- sn_attempts %>%
  group_by(name_born) %>%
  filter(n() > 6) %>%
  arrange(years_wl) %>%
  slice(n()) %>%
  filter(name_born == "TIAN Tao 1994-04-08") %>%
  mutate(years_wl = 8.5, sn_kg = sn_perc * max_sn) %>%
  slice(rep(1, each = 15)) %>%
  mutate(sn_kg = 171:185, sn_perc = sn_kg / max_sn, sn_attempt = factor(1, levels = c(1,2,3))) %>%
  relocate(make, .after = sn_kg)

tian <- tian %>%
  slice(rep(1:n(), each = 3)) %>%
  mutate(sn_attempt = as.factor(rep(1:3, each = 15)))

nino_matrix <- model.matrix(make ~ . - name_born - nation - group - name - sn_kg - gender - division, nino)[, -1]

nino_preds <- nn_mod %>% predict(nino_matrix) %>% as.numeric()



sn_attempts %>%
  group_by(name_born) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  view()








