# Packages.

package_names <- c("haven", "ggplot2", "lavaan", "brunnermunzel", "mgcv", 
                   "splines", "rlang", "psych", "glmnet", "foreign", 
                   "viridis", "RColorBrewer", "cowplot", "ggrepel",
                   "lavaan", "bayestestR", "scales", "mlr3measures",
                   "mice", "ProDenICA", "CCA", "tidyr")

for (i in package_names){
  if ( !requireNamespace( i, 
                          quietly = F )) {
    install.packages( i )
  }
  library( i, character.only = TRUE )}

# Load data.
data_path <- "C:/Users/lintu/OneDrive/Desktop/PTO tutkimus/PLE prevalence among adolescents entering psychiatric services/PTO_KKT_STUDYBASELINE.sav"
df <- read_sav(data_path, encoding = "UTF-8")

# Misc options
cols <- brewer.pal(n = 8, name = "Dark2")[1:3]
names(cols) <- c("Total", "Male", "Female")
theme_set(theme_bw())
options(ggplot2.discrete.colour= cols)
par(family = "serif")
pqbItems <- read_lines("C:/Users/lintu/OneDrive/Desktop/PTO tutkimus/PQ-B_items.txt")
bdiItems <- read_lines("C:/Users/lintu/OneDrive/Desktop/PTO tutkimus/BDI_items.txt")

# Data modifications -------------

# 1. Change PQ-B 8 out of custom sums (Therman publication)
PQBvars <- paste("PQB", 1:21, sep = "")
BDIvars <- paste("BDI", 1:21, sep = "")
PQBvars_cust <- PQBvars[-which(PQBvars %in% c( "PQB7", "PQB12" ) ) ]
df$PQBsum_cust <- apply(df[,PQBvars_cust], MARGIN = 1, sum, na.rm = F) # Custom distress sum
df$sex <- factor(df$Gender_Male1_Female2, levels = c(1,2), labels = c("Male", "Female"))

#missing pattern
if(FALSE){md.pattern(df[,c(paste("PQB",1:21, sep = ""))])}
