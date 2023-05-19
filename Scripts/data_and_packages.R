# Packages.

package_names <- c("haven", "ggplot2", "lavaan", "brunnermunzel", "mgcv", 
                   "splines", "rlang", "psych", "glmnet")

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
color_palette <- c("blue","red", "violet", "#222222",  "#666666",  "#AAAAAA", "#CCCCCC", "#EEEEEE")
theme_set(theme_bw())
options(ggplot2.discrete.colour= color_palette)


# Data modifications -------------

# 1. Change PQ-B 8 out of custom sums (Therman publication)
PQBvars <- paste("PQB", 1:21, sep = "")
PQBvars_cust <- PQBvars[-which(PQBvars == "PQB8")]
df$PQBsum_cust <- apply(df[,PQBvars_cust], MARGIN = 1, sum, na.rm = T) # Custom distress sum
df$sex <- factor(df$Gender_Male1_Female2, levels = c(1,2), labels = c("Male", "Female"))


