library(ggplot2)
library(dplyr)
library(tidyr)
library(faux)


df <- rnorm_multi( n = 10000,
                   mu= c(0,200),
                   sd = c(1,3),
                   r= 0.7 ,
                   varnames=c("A","B"),
                   empirical=FALSE)


## de easy way
pca <- prcomp(df,scale=T)

## manual
df_center= scale(as.matrix(df),center=T,scale=T)

cov_matrix = ( t(df_center) %*% as.matrix(df_center) ) / (dim(df)[1] - 1)

eigs = eigen(cov_matrix)


# ## plots
# 
# plot(df_center) +
#   arrows(0,0,pca$rotation[1,],pca$rotation[2,], col="red",lw=2)
# 
# plot(df_center) +
# arrows(0,0,eigs$vectors[1,],eigs$vectors[2,], col="blue",lw=2)


biplot(pca)


pca$sdev * sqrt(500)

