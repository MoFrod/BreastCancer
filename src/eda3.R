# Perform LDA
lda_fit <- linDA(variables = bc_data_red[,1:6], group = bc_data_red$Class)

# Extract discriminant functions and print to screen
lda_fit$functions

# Plot discriminant functions


# Perform QDA
qda_fit <- quaDA(variables = bc_data_red[,1:6], group = bc_data_red$Class, functions = TRUE)

# Extract discriminant functions and print to screen
qda_fit$functions

# Plot discriminant functions