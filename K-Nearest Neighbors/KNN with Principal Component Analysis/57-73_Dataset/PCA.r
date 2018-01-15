#read in columns you wanted to do pca on
#assume cols is these columns
scaled_cols <- scale(cols)
norm_pca <- prcomp(scaled_cols)
#see how many principle components are greater than 1
plot(norm_pca)
#x is number of principle components
reduced_matrix <- as.matrix(scaled_cols) %*% as.matrix(norm_pca[,1:x])

#reduced_matrix now contains the reduced dataset.