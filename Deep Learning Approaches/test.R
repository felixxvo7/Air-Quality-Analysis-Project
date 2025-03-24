install.packages(c("keras", "tensorflow"))

install.packages("devtools")

library("devtools")

devtools::install_github("rstudio/keras", dependencies = TRUE)
devtools::install_github("rstudio/tensorflow", dependencies = TRUE)

library(keras)
library(tensorflow)
install_keras()
install_tensorflow()

tensorflow::install_tensorflow()
tensorflow::tf_config()


# Just create a model
model <- keras_model_sequential()
model %>% layer_dense(units = 16, activation = "relu", input_shape = c(10))

# Try compiling
model %>% compile(optimizer = "adam", loss = "binary_crossentropy")

summary(model)



library(keras3)
library(tidyverse)

model <- keras_model_sequential() %>%
  layer_dense(units = 2, activation = "relu", name = "layer1") %>%
  layer_dense(units = 3, activation = "relu", name = "layer2") %>%
  layer_dense(units = 4, name = "layer3")

# Call model on a test input
x <- op_ones(c(3, 3))
y <- model(x)

# Compile & train
model |> compile(...)
model |> fit(...)

summary(model)


model <- keras_model_sequential(input_shape = 4) %>%
  layer_dense(units = 2, activation = "relu")
summary(model)

model %>% compile(
  loss = 'mse',
  optimizer = optimizer_rmsprop(),
  metrics = c('accuracy')
)

history <- model %>% fit(
  x_train, y_train,
  epochs = 2, batch_size = 1,
  validation_split = 0.2
)

## ----setup, include=FALSE-------------------------------------------------
tensorflow::as_tensor(1)


## -------------------------------------------------------------------------
library(tensorflow)
library(keras3)
mnist <- dataset_mnist()
train_images <- mnist$train$x
train_labels <- mnist$train$y
test_images <- mnist$test$x
test_labels <- mnist$test$y


## -------------------------------------------------------------------------
str(train_images)
str(train_labels)


## -------------------------------------------------------------------------
str(test_images)
str(test_labels)


## -------------------------------------------------------------------------
# model <- keras_model_sequential(list(
#   layer_dense(units = 512, activation = "relu"),
#   layer_dense(units = 10, activation = "softmax")
# ))
model <- keras_model_sequential(input_shape = c(784))
model %>%
  layer_dense(units = 256, activation = 'relu') %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 10, activation = 'softmax')


## -------------------------------------------------------------------------
compile(model,
        optimizer = "rmsprop",
        loss = "sparse_categorical_crossentropy",
        metrics = "accuracy")


## -------------------------------------------------------------------------
train_images <- array_reshape(train_images, c(60000, 28 * 28))
train_images <- train_images / 255
test_images <- array_reshape(test_images, c(10000, 28 * 28))
test_images <- test_images / 255


## -------------------------------------------------------------------------
fit(model, train_images, train_labels, epochs = 5, batch_size = 128)
