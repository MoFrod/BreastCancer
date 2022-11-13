# Check size of data
dim(BC1)

# Summarise data
summary(BC1) # 53.83% are malignant

# Quick plot of data
pairs(BC1[2:10]) # Remove ID column as it represents meta-information and the response variable (M/B)
# Malignant only for cell thickness 10, cell size 5, 6 and 10, cell shape 10, marg.adhesion 7, 8, 9, epith.c.size 9, bare.nuclei 6 and 9, BI.cromatin 8, 9, 10, normal.nuclei 9 and 10, mitoses 4, 6, and 9
# Potentially linear relationship between cell size and shape
# Mitoses more consistent at lower levels for all 9 categories

# Filter for class type
M <- filter(BC1, Class == "malignant")
summary(M)
B <- filter(BC1, Class == "benign")
summary(M) # Mean/Median are generally smaller (only marg.adhesion, epith.c.size and bare.nuclei have max of 10)

# Understand the variance of the categories
apply(BC1[,2:10], 2, var) # Remove ID and Class, Bare.nuclei has largest var and epith.c.size has lowest.
apply(M[,2:10], 2, var) 
apply(B[,2:10], 2, var) # Very low variances in comparison to M

# Observe the correlations
cor(BC1[,2:10])

# Quickplot heatmap
BC2 <- data.matrix(BC1) # Convert into numeric matrix
heatmap(BC2[,-1])

# GG density plots of variables
BC1 %>%
  ggplot(aes(x = Cl.thickness, colour = Class)) + geom_density(kernel = "gaussian", size = 1.5) + labs(x = "Clump Thickness", y = "Density", legend = "Class") + scale_color_brewer(palette = "Paired")

BC1 %>%
  ggplot(aes(x = Cell.size, colour = Class)) + geom_density(kernel = "gaussian", size = 1.5) + labs(x = "Cell Size", y = "Density", legend = "Class") + scale_color_brewer(palette = "Paired")

BC1 %>%
  ggplot(aes(x = Cell.shape, colour = Class)) + geom_density(kernel = "gaussian", size = 1.5) + labs(x = "Cell shape", y = "Density", legend = "Class") + scale_color_brewer(palette = "Paired")

BC1 %>%
  ggplot(aes(x = Marg.adhesion, colour = Class)) + geom_density(kernel = "gaussian", size = 1.5) + labs(x = "Marginal Adhesion", y = "Density", legend = "Class") + scale_color_brewer(palette = "Paired")

BC1 %>%
  ggplot(aes(x = Epith.c.size, colour = Class)) + geom_density(kernel = "gaussian", size = 1.5) + labs(x = "Single Epithelial Cell Size", y = "Density", legend = "Class") + scale_color_brewer(palette = "Paired")

BC1 %>%
  ggplot(aes(x = Bare.nuclei, colour = Class)) + geom_density(kernel = "gaussian", size = 1.5) + labs(x = "Bare Nuclei", y = "Density", legend = "Class") + scale_color_brewer(palette = "Paired")

BC1 %>%
  ggplot(aes(x = Bl.cromatin, colour = Class)) + geom_density(kernel = "gaussian", size = 1.5) + labs(x = "Bland Chromatin", y = "Density", legend = "Class") + scale_color_brewer(palette = "Paired")

BC1 %>%
  ggplot(aes(x = Normal.nucleoli, colour = Class)) + geom_density(kernel = "gaussian", size = 1.5) + labs(x = "Normal Nucleoli", y = "Density", legend = "Class") + scale_color_brewer(palette = "Paired")

BC1 %>%
  ggplot(aes(x = Mitoses, colour = Class)) + geom_density(kernel = "gaussian", size = 1.5) + labs(x = "Mitoses", y = "Density", legend = "Class") + scale_color_brewer(palette = "Paired")

