# Suicidiality replication study - keep it short.

# Data pre-processing: -----


SuicidalityDF <- df[ , c(
  BDIvars, "sex", "Age_in_years", PQBvars
)]
N <- dim(SuicidalityDF) # N = 509
N_omit <- dim(na.omit(SuicidalityDF)) # 328 after removing cases with incomplete observations.
SuicidalityDF <- na.omit(SuicidalityDF)

# First (Therman et al) we combined low PQ-B Distress values of 'no distress' with 'almost no distress'.
# This was done because previous research showed that PQ-B item threshold of low distress endorsements
# are close to each other, indicating low differentiability between the two endorsement categories.

SuicidalityDF[ , PQBvars ] <- sapply( df[ , PQBvars],
                                      FUN = function( x ) {
                                        mapvalues( x, from = 0:5, c(1,1:5) )
                                        } )

# Second, we used the BDI item 9 to measure suicidal intentions. This was done as in Granö et al., 2015 by
# dichotomizing the BDI item 9 as 0 'no suicidal ideation' 1 'suicidal ideation'.

SuicidalityDF$BDI9dic <- mapvalues( SuicidalityDF$BDI9, 0:3 , c( 0, 1, 1, 1 ) )

# Preliminary check of polychoric correlations:
cor_auto( SuicidalityDF[ , c( PQBvars, "BDI9dic" ) ] )

# Third we check dimensions of variation.

fa.parallel( cor_auto(SuicidalityDF[,c( PQBvars_cust, BDIvars )]), cor = "poly")


suicidalityModel <- '
Depression =~ BDI1 + BDI2 + BDI3 + BDI4 + BDI5 + BDI6 + 
              BDI7 + BDI8 + BDI10 + BDI11 + BDI12 + 
              BDI13 + BDI14 + BDI15 + BDI16 + BDI17 + BDI18 + 
              BDI19 + BDI20 + BDI21

# This is just a compromise and would not be a believable
GeneralPsy =~ PQB1 + PQB2 + PQB4 + PQB5 + PQB6 + 
                PQB8 + PQB9 + PQB10 + PQB11 + PQB13 + PQB14 + 
                PQB15 + PQB16 + PQB17 + PQB18 + PQB21

VisHalluc =~ PQB3 + PQB19 + PQB20 

# Factors uncorrelated:
VisHalluc ~~ 0*GeneralPsy + 0 * Depression
GeneralPsy ~~ 0*Depression

BDI9dic ~ VisHalluc + Depression + GeneralPsy
'

# SEM just breaks...
SEMresult <- sem(suicidalityModel, data = SuicidalityDF, ordered = T)
inspect(SEMresult, "est")
