library(keras3)

# Load the MNIST dataset
mnist <- dataset_mnist()
train_images <- mnist$train$x
train_labels <- mnist$train$y
test_images <- mnist$test$x
test_labels <- mnist$test$y

# Inspect the data
str(train_images)
str(train_labels)
str(test_images)
str(test_labels)

# Build the model
model <- keras_model_sequential() %>%
  layer_dense(units = 256, activation = 'relu', input_shape = c(784)) %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 10, activation = 'softmax')

# Compile the model
compile(model,
        optimizer = "rmsprop",
        loss = "sparse_categorical_crossentropy",
        metrics = "accuracy")

# Reshape and normalize the data
train_images <- array(train_images, dim = c(60000, 28 * 28))
train_images <- train_images / 255
test_images <- array(test_images, dim = c(10000, 28 * 28))
test_images <- test_images / 255

# Train the model
fit(model, train_images, train_labels, epochs = 5, batch_size = 128)


Sys.setenv("RETICULATE_PYTHON"="managed")
library(keras3)
op_convert_to_tensor("Hello World!")