# load data from previous sections
load(url("https://userpage.fu-berlin.de/soga/300/30100_data_sets/pca_food_30300.RData"))

food.pca.eigen <- eigen(cov(food.pca))
pca.loading <- food.pca.eigen$vectors[,1:2] # select the first two principal components

pca.scores <- food.pca %*% pca.loading
rownames(pca.scores) <- seq(1, nrow(pca.scores))

# Plot the scores
plot(pca.scores, 
     xlab = expression('Z'[1]), 
     ylab = expression('Z'[2]), 
     main = 'Score plot')
abline(h = 0, col = "blue")
abline(v = 0, col = "green")

# Plot the scores as points
text(pca.scores[,1]+0.2,
     pca.scores[,2],
     rownames(pca.scores),
     col="blue", cex=0.6)

loading.vector <- food.pca.eigen$vectors
rownames(loading.vector) <- colnames(food.pca)

# Plot the loading vector
plot(loading.vector, 
     xlab = expression('p'[1]), 
     ylab = expression('p'[2]), 
     main = 'Loading plot',
     ylim = c(-1,1),
     xlim = c(-1,1))
abline(h = 0, col = "blue")
abline(v = 0, col = "green")

# Plot the loadings as points
text(loading.vector[,1]+0.1,
     loading.vector[,2]+0.1,
     rownames(loading.vector),
     col="blue", cex=1.2)



# Correlation BiPlot
pca.sd <- sqrt(food.pca.eigen$values) # standardize to sd = 1
loading.vector <- food.pca.eigen$vectors
rownames(loading.vector) <- colnames(food.pca)

# Plot
plot(pca.scores,
     xlab = expression('p'[1]), 
     ylab = expression('p'[2]))
abline(h = 0, col = "blue")
abline(v = 0, col = "green")

# This is to make the size of the lines more apparent
factor <- 0.5

# Plot the variables as vectors
arrows(0,0,loading.vector[,1]*pca.sd[1]/factor,
       loading.vector[,2]*pca.sd[2]/factor,
       length = 0.1,
       lwd=  2,
       angle = 20,
       col = "red")

# Plot annotations
text(loading.vector[,1]*pca.sd[1]/factor*1.2,
     loading.vector[,2]*pca.sd[2]/factor*1.2,
     rownames(loading.vector),
     col = "red",
     cex = 1.2)


