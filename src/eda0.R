# Check size of data
dim(BC1)

# Summarise data
summary(BC1) # total summaries
describe(BC1)

## Boxplot
# Plot spread of characteristics by class
ggplot(BC1, aes(x = Class, y = Cl.thickness)) + geom_boxplot(alpha = 0.3, color = "#1f78b4") + labs(x = "Class", y = "Clump Thickness (scale)", title = "Clump Thickness by Class") + theme(legend.position = "none") + expand_limits(y = 0)
ggplot(BC1, aes(x = Class, y = Cell.size)) + geom_boxplot(alpha = 0.3, color = "#1f78b4") + labs(x = "Class", y = "Cell Size (scale)", title = "Cell Size by Class") + theme(legend.position = "none") + expand_limits(y = 0)
ggplot(BC1, aes(x = Class, y = Cell.shape)) + geom_boxplot(alpha = 0.3, color = "#1f78b4") + labs(x = "Class", y = "Cell Shape (scale)", title = "Cell Shape by Class") + theme(legend.position = "none") + expand_limits(y = 0)
ggplot(BC1, aes(x = Class, y = Marg.adhesion)) + geom_boxplot(alpha = 0.3, color = "#1f78b4") + labs(x = "Class", y = "Marginal Adhesion (scale)", title = "Marginal Adhesion  by Class") + theme(legend.position = "none") + expand_limits(y = 0)
ggplot(BC1, aes(x = Class, y = Epith.c.size)) + geom_boxplot(alpha = 0.3, color = "#1f78b4") + labs(x = "Class", y = "Single Epitheial Cell Size (scale)", title = "Single Epitheial Cell Size by Class") + theme(legend.position = "none") + expand_limits(y = 0)
ggplot(BC1, aes(x = Class, y = Bare.nuclei)) + geom_boxplot(alpha = 0.3, color = "#1f78b4") + labs(x = "Class", y = "Bare Nuclei (scale)", title = "Bare Nuclei by Class") + theme(legend.position = "none") + expand_limits(y = 0)
ggplot(BC1, aes(x = Class, y = Bl.cromatin)) + geom_boxplot(alpha = 0.3, color = "#1f78b4") + labs(x = "Class", y = "Bland Cromatin (scale)", title = "Bland Cromatin by Class") + theme(legend.position = "none") + expand_limits(y = 0)
ggplot(BC1, aes(x = Class, y = Normal.nucleoli)) + geom_boxplot(alpha = 0.3, color = "#1f78b4") + labs(x = "Class", y = "Normal Nucleoli (scale)", title = "Normal Nucleoli by Class") + theme(legend.position = "none") + expand_limits(y = 0)
ggplot(BC1, aes(x = Class, y = Mitoses)) + geom_boxplot(alpha = 0.3, color = "#1f78b4") + labs(x = "Class", y = "Mitoses (scale)", title = "Mitoses by Class") + theme(legend.position = "none") + expand_limits(y = 0)

# Quick plot of data
pairs(BC1[2:11]) # Remove ID column as it represents meta-information and the response variable (M/B)
# Malignant only for cell thickness 10, cell size 5, 6 and 10, cell shape 10, marg.adhesion 7, 8, 9, epith.c.size 9, bare.nuclei 6 and 9, BI.cromatin 8, 9, 10, normal.nuclei 9 and 10, mitoses 4, 6, and 9
# Potentially linear relationship between cell size and shape
# Mitoses more consistent at lower levels for all 9 categories

# Percentage of Malignant per Benign
(239/444 * 100) #53.83%

# Filter for class type
M <- filter(BC1, Class == "malignant")
summary(M)
describe(M)
B <- filter(BC1, Class == "benign")
summary(B) # Mean/Median are generally smaller (only marg.adhesion, epith.c.size and bare.nuclei have max of 10)
describe(B)

# Understand the variance of the categories
apply(BC1[,2:10], 2, var) # Remove ID and Class, Bare.nuclei has largest var and epith.c.size has lowest.
apply(M[,2:10], 2, var) 
apply(B[,2:10], 2, var) # Very low variances in comparison to M

# Observe the correlations
(cor_bc <- cor(BC1[,2:10]))

# Plot correlations
corrplot(cor_bc, order = "hclust", tl.cex = 0.7) # Shows bivariate relationship among the characteristics. Highly correlated features are likely to provide redundant information.

# Quickplot heatmap
BC2 <- data.matrix(BC1) # Convert into numeric matrix
heatmap(BC2[,-1]) # This doesn't tell us much more.

# Sample variance of data
s <- var(BC2)

# Calculate total variation
s_sq = diag(s) # Extract diagonal elements

(total_variation = sum(s_sq))
#33283.24

# Generalised variance 
det(s) #55382860

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

