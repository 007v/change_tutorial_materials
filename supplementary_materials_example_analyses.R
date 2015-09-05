## Bodo Winter & Martijn Wieling
## Supplementary materials for paper on time series analysis for language evolution research
## Created: September 5, 2015

## Load in data:

IL <- read.csv('example1_iterated.csv')
dyads <- read.csv('example2_dyads.csv')


##------------------------------------------------------------------
## Analysis of IL experiment:
##------------------------------------------------------------------

## First, plot each chain:

plot(1, 1, xlim = c(0, 11), ylim = c(0, 10), type = 'n')	# creates empty plot
for (i in 1:6) {	# loop through chains and plot each as separate line
	points(IL[IL$chain == i, ]$t, IL[IL$chain == i, ]$y, type = 'l')
	}

## Load mixed model library:

library(lme4)

## Construct mixed model with random slopes for time by chain:

xmdl <- lmer(y ~ t + (1+t|chain), data = IL, REML = F)
# Explanation:
# - mixed model modeling y as a function of time (t)
# - with random intercepts (by-chain variation in y)
# - and random slopes for time (by-chain variation in the slope of t)
# - and a random intercept / slope correlation
# - we set REML = F because we want to perform likelihood ratio tests

## The same model with decorrelated random effects structure:

xmdl2 <- lmer(y ~ t + (1|chain) + (0+t|chain), data = IL, REML = F)
# Explanation:
# - same as above but without the (unecessary) random intercept / slope correlation

## Check the model:

summary(xmdl)	# notice the correlation term in the random effects component (close to 0)
summary(xmdl2)	# notice that there are now separate lines for random intercepts and slopes
# notice also that the estimated random slope variance is rather small (= 0.023) in comparison

## Check whether the random slope is needed by means of a likelihood ratio test ...
## ... to do this, we need to construct a comparison model without random slopes:

xmdl2_noslopes <- lmer(y ~ t + (1|chain), data = IL, REML = F)

## Then test the two models against each other:

anova(xmdl2_noslopes, xmdl2, test = 'Chisq')
# There is a significant difference, indicating that the slopes do improve the fit
# The model with slopes ("xmdl2") has a lower AIC (= better fit) ... 
# ... than the model with slopes ("xmdl2_noslopes" )

## Test the effect of time by constructing a comparison model (in this case a null modl) without time:

xmdl_null <- lmer(y ~ 1 + (1|chain) + (0+t|chain), data = IL, REML = F)
anova(xmdl_null, xmdl2, test = 'Chisq')
# Explanation:
# - "1" represents the intercept and in this case indicates that no other fixed effects are in the model
# - notice that when testing for a fixed effect, we keep the random effects structure the same
# - interpretation: the model with the time variable "t" is significantly better than the model without

## We investigate the residuals of the final model (for explanation, see Winter, 2013):

hist(residuals(xmdl2))	# good
qqnorm(residuals(xmdl2)); qqline(residuals(xmdl2))	# beautiful
plot(fitted(xmdl2), residuals(xmdl2))	# good
# Explanation:
# - since the residuals look approximately normal (as revealed through histograms and Q-Q plots) ...
# ... there is no problem with the assumption of normality
# - as the residual plot looks blob-like, there is no problem with the assumption of homoskedasticity

## Inspect the random effects estimates:

coef(xmdl2)		# chain 3 has a really low intercept (negative), chain 6 has a very shallow slope of t



##------------------------------------------------------------------
## Analysis of repeated interaction experiments (dyads) using GCA:
##------------------------------------------------------------------

## First, plot each dyad:

plot(1, 1, xlim = c(0, 11), ylim = c(0, 10), type = 'n')	# creates empty plot
for (i in 1:6) {	# loop through chains and plot each as separate line
	points(dyads[dyads$dyad == i, ]$t, dyads[dyads$dyad == i, ]$y, type = 'l')
	}

## Load mixed model library:

library(lme4)

## Center time variable:

dyads$t_c <- dyads$t - mean(dyads$t)

## Create quadratic time variable:

dyads$t_c2 <- dyads$t_c ^ 2

## Show that they are indeed uncorrelated:

cor(dyads$t_c, dyads$t_c2)	# r = 0

## Compare this to the un-centered case:

cor(dyads$t, dyads$t^2)		# r = 0.97

## Create a mixed model:

xmdl <- lmer(iconicity ~ t_c + t_c2 +	# fixed effects
	(1|dyad) + (0+t_c|dyad) + (0+t_c2|dyad),		# random effects
	data = dyads, REML = F)

## Create model without quadratic random slope and without linear random slope:

xmdl_red1 <- lmer(iconicity ~ t_c + t_c2 +	# fixed effects
	(1|dyad) + (0+t_c|dyad),		# random effects
	data = dyads, REML = F)
xmdl_red2 <- lmer(iconicity ~ t_c + t_c2 +	# fixed effects
	(1|dyad),		# random effects
	data = dyads, REML = F)
anova(xmdl_red2, xmdl_red1, xmdl, test = 'Chisq')
# Explanation:
# - the linear slope model differs from the one without slopes; thus the linear slopes are warranted
# - the quadratic slopes do not differ from the model with only linear slopes ...
# ... hence we choose the simpler model

xmdl <- xmdl_red1	# make model with linear random slopes the main model

## Inspect the random effects estimates:

coef(xmdl)	# notice that "t_c" is different for the different dyads
# "t_c2" is the same because we dropped the random slopes for the quadratic effect

## Test quadratic and linear effects using likelihood ratio tests:

xmdl_noQR <- lmer(iconicity ~ t_c +	# fixed effects
	(1|dyad) + (0+t_c|dyad),		# random effects
	data = dyads, REML = F)
xmdl_noLIN <- lmer(iconicity ~ 1 +	# fixed effects
	(1|dyad) + (0+t_c|dyad),		# random effects
	data = dyads, REML = F)
anova(xmdl_noLIN, xmdl_noQR, xmdl, test = 'Chisq')
# Explanation:
# - the null model (intercept only) differs from the linear model, hence the linear effect is significant
# - the linear model differs from the quadratic model, hence the quadratic effect is significant
# - notice that we kept the random effects structure constant across the comparisons

## Inspect residuals (assumptions):

hist(residuals(xmdl))	# good
qqnorm(residuals(xmdl)); qqline(residuals(xmdl))	# o.k.
plot(fitted(xmdl), residuals(xmdl))	# o.k.

## In this case, visualizing the data obviously revealed that a quadratic effect is appropriate ...
## ... in other cases however, it is not obvious what order of polynomials is supported by the data
## To demonstrate, we show how to choose a polynomial form for the data:

all_polys <- data.frame(poly(1:10, degree = 4))
all_polys <- do.call('rbind', replicate(10, all_polys, simplify = F))	# match length of dataframe
dyads <- cbind(dyads, all_polys)	# this depends on the dataset being sorted

## Models with different polynomials:

xmdl1 <- lmer(iconicity ~ X1 + (1|dyad), data = dyads, REML = F)
xmdl2 <- lmer(iconicity ~ X1 + X2 + (1|dyad), data = dyads, REML = F)
xmdl3 <- lmer(iconicity ~ X1 + X2 + X3 + (1|dyad), data = dyads, REML = F)
xmdl4 <- lmer(iconicity ~ X1 + X2 + X3 + X4 + (1|dyad), data = dyads, REML = F)

## Test them against each other:

anova(xmdl1, xmdl2, xmdl3, xmdl4, test = 'Chisq')
# only the quadratic model differs significantly from the linear
# thus there is no support for polynomials above order 2



##------------------------------------------------------------------
## Analysis of repeated interaction experiment (dyads) using GAMs:
##------------------------------------------------------------------

## @ Martijn: this is where your analysis comes in...




