# Load mlbench package as specified in provided project description
library(mlbench)

# Load the data specified in project description
data("BreastCancer")

# Create tibble for initial investigation 
BC1 <- as_tibble(BreastCancer)

# Confirm variable type in BC1
ls(BC1) # List the categories

class(BC1$Id) # Character
class(BC1$Cl.thickness) # Ordered Factor
class(BC1$Cell.size) # Ordered Factor
class(BC1$Cell.shape) # Ordered Factor
class(BC1$Marg.adhesion) # Ordered Factor
class(BC1$Epith.c.size) # Ordered Factor
class(BC1$Bare.nuclei) # Factor
class(BC1$Bl.cromatin) # Factor
class(BC1$Normal.nucleoli) # Factor
class(BC1$Mitoses) # Factor
class(BC1$Class) # Factor

# Change factors into quantitative variables
BC1$Cl.thickness <- as.numeric(BC1$Cl.thickness)
BC1$Cell.size <- as.numeric(BC1$Cell.size)
BC1$Cell.shape <- as.numeric(BC1$Cell.shape)
BC1$Marg.adhesion <- as.numeric(BC1$Marg.adhesion)
BC1$Epith.c.size <- as.numeric(BC1$Epith.c.size)
BC1$Bare.nuclei <- as.numeric(BC1$Bare.nuclei)
BC1$Bl.cromatin <- as.numeric(BC1$Bl.cromatin)
BC1$Normal.nucleoli <- as.numeric(BC1$Normal.nucleoli)
BC1$Mitoses <- as.numeric(BC1$Mitoses)

# Remove all rows where there are missing values
BC1 <- na.omit(BC1) # 16 removed, matching R Documentation on the Wisconsin Breast Cancer Database

# Check numerical version of class
c <- as.numeric(BC1$Class)
head(c, 20) # Benign = 1, Malignant = 2

