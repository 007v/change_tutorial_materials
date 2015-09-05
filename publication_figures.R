## Bodo Winter & Martijn Wieling
## Publication-ready plots for paper
## Created: September 5, 2015

## Load in data:

IL <- read.csv('example1_iterated.csv')
dyads <- read.csv('example2_dyads.csv')



##------------------------------------------------------------------
## Figure 1: Single chain, simple linear effect
##------------------------------------------------------------------

## Generate data for simple linear regression:

set.seed(53)
t <- 1:10
y <- 0.63 * t + rnorm(length(t), sd = 1.5) + 1.5

## The actual plot:

quartz('', 8, 5.5); par(mai = c(1, 1.5, 0.75, 0.5))
plot(t, y, xlim = c(-0.5, 10.5), ylim = c(0, 10),
	xlab = '', ylab = '', xaxt = 'n', yaxt = 'n', xaxs = 'i', type = 'n')
axis(side = 1, at = 0:10, labels = 0:10, font = 2, cex.axis = 1.25, lwd = 2)
axis(side = 2, at = seq(0, 10, 2.5), las = 2, font = 2, cex.axis = 1.25, lwd = 2)
mtext('Compositionality', side = 2, line = 3.5, cex = 1.8, font = 2)
mtext('Time / Generation', side = 1, line = 3, cex = 1.8, font = 2)
mtext('One chain', side = 3, line = 0.8, cex = 2.25, font = 2)
abline(v = 0, lwd = 2, lty = 2)	# y-axis
abline(lm(y~t), lwd = 4)	# regression line
points(t, y, type = 'b', lwd = 2, pch = 22, bg = 'gray', cex = 1.25)	# actual data
box(lwd = 2)



##------------------------------------------------------------------
## Figure 2: Multiple chains, plotting random effects
##------------------------------------------------------------------

## Perform mixed model for plotting:

library(lme4)
xmdl <- lmer(y ~ t + (1+t|chain), IL)

## The plot:

quartz('', 8, 5.5); par(mai = c(1, 1.5, 0.75, 0.5))
plot(IL$t, IL$y, xlim = c(-0.5, 10.5), ylim = c(0, 10),
	xlab = '', ylab = '', xaxt = 'n', yaxt = 'n', xaxs = 'i', type = 'n')
axis(side = 1, at = 0:10, labels = 0:10, font = 2, cex.axis = 1.25, lwd = 2)
axis(side = 2, at = seq(0, 10, 2.5), las = 2, font = 2, cex.axis = 1.25, lwd = 2)
mtext('Compositionality', side = 2, line = 3.5, cex = 1.8, font = 2)
mtext('Time / Generation', side = 1, line = 3, cex = 1.8, font = 2)
mtext('Multiple chains', side = 3, line = 0.8, cex = 2.25, font = 2)
abline(v = 0, lwd = 2, lty = 2)	# y-axis
abline(a = fixef(xmdl)[1], b = fixef(xmdl)[2], lwd = 4)	# plotting fixed effect line
for(i in 1:nchain){	# plotting random effects
	abline(a = coef(xmdl)$chain[i, 1],
		b = coef(xmdl)$chain[i, 2], col = 'darkgray', lwd = 3)
	}
box(lwd = 2)



##------------------------------------------------------------------
## Figure 3a, 3b: Repeated interaction experiment (dyads) double-plot
##------------------------------------------------------------------

## Mixed model to get coefficients for plotting:

library(lme4)
dyads$t2 <- dyads$t^2
xmdl <- lmer(iconicity ~ t + t2 +	# fixed effects
	(1+t+t2|dyad), dyads)			# random effects

## Plotting window parameters:

quartz('', 11, 5)
par(mfrow = c(1, 2), omi = c(0.75, 1.25, 0.5, 0), mai = c(0.25, 0.25, 0.25, 0.25))

## Figure 3a:

plot(dyads$t, dyads$y, xlim = c(0.5, 10.5), ylim = c(0, 10),
	xlab = '', ylab = '', xaxt = 'n', yaxt = 'n', xaxs = 'i', type = 'n')
sapply(1:10, FUN = function(x){points(dyads[dyads$dyad == x, ]$iconicity, type = 'l', lwd = 2)})
axis(side = 1, at = 1:10, labels = 1:10, font = 2, cex.axis = 1.25, lwd = 2)
axis(side = 2, at = seq(0, 10, 2.5), las = 2, font = 2, cex.axis = 1.25, lwd = 2)
mtext('Iconicity', side = 2, line = 3.5, cex = 2, font = 2)
mtext('Time / Interaction round', side = 1, line = 3, cex = 1.8, font = 2)
mtext('Raw data', side = 3, line = 0.8, cex = 2.25, font = 2)
text(1.1, 9.75, labels = '(a)', font = 2, cex = 1.5)
box(lwd = 2)

## Figure 3b:

plot(dyads$t, dyads$y, xlim = c(0.5, 10.5), ylim = c(0, 10),
	xlab = '', ylab = '', xaxt = 'n', yaxt = 'n', xaxs = 'i', type = 'n')
for(i in 1:nchain){	# predictions for different chains
	yvals <- coef(xmdl)$dyad[i, 1] + coef(xmdl)$dyad[i, 2] * 1:10 + coef(xmdl)$dyad[i, 3] * ((1:10)^2)
	points(1:10, yvals, col = 'darkgray', lwd = 3, type = 'l')
	}
points(1:10, fixef(xmdl)[1] + (fixef(xmdl)[2] * 1:10) + (fixef(xmdl)[3] * ((1:10)^2)),
	type = 'l', lwd = 4)		# fixed effect line
axis(side = 1, at = 1:10, labels = 1:10, font = 2, cex.axis = 1.25, lwd = 2)
mtext('Time / Interaction round', side = 1, line = 3, cex = 1.8, font = 2)
mtext('Growth Curve Analysis', side = 3, line = 0.8, cex = 2.25, font = 2)
text(1.1, 9.75, labels = '(b)', font = 2, cex = 1.5)
box(lwd = 2)
## Additional point discussed in the paper:
points(10, fixef(xmdl)[1] + (fixef(xmdl)[2] * 10) + (fixef(xmdl)[3] * ((10)^2)),
	cex = 2, pch = 19)




##------------------------------------------------------------------
## Figure 4a, 4b: Non-linear shaps, GCA vs. GAM comparison
##------------------------------------------------------------------

## Create an exponential data:

set.seed(42)
N <- 250
t <- sample(seq(-10, 10, 0.01), N)
y <- 0.85 * exp(t) + rnorm(length(t), sd = max(y) / 30)

## Create a really complex wiggly shape:

set.seed(42)
N <- 250
t2 <- sample(seq(1, 10, 0.001), N)
y2 <- exp(cos(t2 * 2)) + exp(t2 / 10) + rnorm(length(t2), sd = 0.5)

## Polynomial fit for exponential model:
## (let's go with quartic for demonstration purposes)

xmdl1 <- lm(y ~ t + I(t^2) + I(t^3))
xvals <- seq(min(t), max(t), length.out = 1000)
yvals <- coef(xmdl1)[1] + xvals * coef(xmdl1)[2] + (xvals^2) * coef(xmdl1)[3] +
	(xvals^3)*coef(xmdl1)[4]

## Polynomial fit for sinusoidal data:

xmdl2 <- lm(y2 ~ t2 + I(t2^2) + I(t2^3))
xvals2 <- seq(min(t2), max(t2), length.out = 1000)
yvals2 <- coef(xmdl2)[1] + xvals2 * coef(xmdl2)[2]+ (xvals2^2) * coef(xmdl2)[3] +
	(xvals2^3) * coef(xmdl2)[4]

## Simple regression smoother:

library(mgcv)
s1 <- gam(y ~ s(t))
s2 <- gam(y2 ~ s(t2))

## Plotting window parameters:

quartz('', 11, 5)
par(mfrow = c(1, 2), omi = c(0.75, 1.25, 0.5, 0), mai = c(0.25, 0.25, 0.25, 0.25))

## Figure 4a, exponential:

plot(t, y, xlim = c(min(t) - 0.05 * diff(range(t)), max(t) + 0.05 * diff(range(t))),
	xlab = '', ylab = '', xaxt = 'n', yaxt = 'n', xaxs='i', type = 'n')
points(t, y, pch = 19, col = rgb(0, 0, 0, 0.1), cex = 1.25)
points(xvals, yvals, type = 'l', lwd = 3, lty = 2)
mtext('Arbitrary scale', side = 2, line = 1.5, cex = 1.8, font = 2)
mtext('Time', side = 1, line = 1.5, cex = 1.8, font = 2)
mtext("Shape #1", side = 3, line = 0.8, cex = 2.25, font = 2)
text(min(t) + 0.025 * diff(range(t)), 0.975 * max(y), labels = '(a)', font = 2, cex = 1.5)
points(sort(t), s1$fitted.values[order(t)], lwd = 3, type = 'l')
box(lwd = 2)

## Figure 4b, sinusoidal:

plot(t2, y2, xlim = c(min(t2) - 0.05 * diff(range(t2)), max(t2) + 0.05 * diff(range(t2))),
	xlab = '', ylab = '', xaxt = 'n', yaxt = 'n', xaxs = 'i', type = 'n')
points(t2, y2, pch = 19, col = rgb(0, 0, 0, 0.1), cex = 1.25)
points(xvals2, yvals2, type = 'l', lwd = 3, lty = 2)
mtext('Time', side = 1, line = 1.5, cex = 1.8, font = 2)
mtext("Shape #2", side = 3, line = 0.8, cex = 2.25, font = 2)
text(min(t2) + 0.025 * diff(range(t2)), 0.975 * max(y2), labels = '(b)', font = 2, cex = 1.5)
points(sort(t2), s2$fitted.values[order(t2)], lwd = 3, type = 'l')
box(lwd = 2)



##------------------------------------------------------------------
## Figure 5a, 5b: GAM fits and factor smooths for repeated interaction data:
##------------------------------------------------------------------

## The GAM:

library(mgcv)
dyads$dyad <- as.factor(dyads$dyad)
xmdl <- bam(iconicity ~ s(t, k = 5) + s(t, dyad, k = 5, bs = 'fs', m = 1),
	data = dyads)		# @Martijn: What does the warning message say here?

## Get fixed effect prediction and confidence interval:

gam_preds <- as.data.frame(predict.gam(xmdl, newdata = data.frame(t = 1:10, dyad = 1), se.fit = T))
gam_preds$UL <- gam_preds$fit + 1.96 * gam_preds$se.fit
gam_preds$LL <- gam_preds$fit - 1.96 * gam_preds$se.fit

## Extract smooth terms (factor smooths for dyads):

fsmooths <- get_modelterm(xmdl, 2)$fit	# second term based on summary(xmdl), 30 data points per dyad
dyad_id <- cut(1:300, 10)
all_dyads <- unique(dyad_id)

## Plotting window parameters:

quartz('', 11, 5)
par(mfrow = c(1, 2), omi = c(0.75, 1.25, 0.5, 0), mai = c(0.25, 0.25, 0.25, 0.25))

## Figure 5a:

plot(dyads$t, dyads$y, xlim = c(0.5, 10.5), ylim = c(0, 10),
	xlab = '', ylab = '', xaxt = 'n', yaxt = 'n', xaxs = 'i', type = 'n')
polygon(c(1:10, rev(1:10)), c(gam_preds$UL, rev(gam_preds$LL)), col = 'darkgray', border = F)
points(1:10, gam_preds$fit, type = 'l')
axis(side = 1, at = 1:10, labels = 1:10, font = 2, cex.axis = 1.25, lwd = 2)
axis(side = 2, at = seq(0, 10, 2.5), las = 2, font = 2, cex.axis = 1.25, lwd = 2)
mtext('Iconicity', side = 2, line = 3.5, cex = 2, font = 2)
mtext('Time / Interaction round', side = 1, line = 3, cex = 1.8, font = 2)
mtext('GAM predictions', side = 3, line = 0.8, cex = 2.25, font = 2)
text(1.1, 9.75, labels = '(a)', font = 2, cex = 1.5)
box(lwd = 2)

## Figure 5b:

plot(dyads$t, dyads$y, xlim = c(0.5, 10.5), ylim = c(-2, 2),
	xlab = '', ylab = '', xaxt = 'n', yaxt = 'n', xaxs = 'i', type = 'n')
for(i in 1:10){	# predictions for different chains
	points(seq(1, 10, length.out = 30), fsmooths[dyad_id == unique(dyad_id)[i]],
		lwd = 3, type = 'l', lty = i)
	}
axis(side = 1, at = 1:10, labels = 1:10, font = 2, cex.axis = 1.25, lwd = 2)
mtext('Time / Interaction round', side = 1, line = 3, cex = 1.8, font = 2)
mtext('Dyad smooths', side = 3, line = 0.8, cex = 2.25, font = 2)
text(1.1, 9.75, labels = '(b)', font = 2, cex = 1.5)
box(lwd = 2)




##------------------------------------------------------------------
## Figure 6: Auto-correlation structure for the GAM:
##------------------------------------------------------------------

## The GAM:

library(mgcv)
dyads$dyad <- as.factor(dyads$dyad)
xmdl <- bam(iconicity ~ s(t, k = 5) + s(round, dyad, k = 5, bs = 'fs', m = 1),
	data = dyads)

## Get auto-correlation values:

acf_vals <- acf(residuals(xmdl), plot = F, lag.max = 9)
	# @ Martijn: but wait, these aren't sorted, are they???
acf_sig_vals <- qnorm((1 + 0.95)/2)/sqrt(acf_vals$n.used)
# http://r.789695.n4.nabble.com/acf-Significance-td895634.html

## The plot:

quartz('', 8, 5.5); par(mai = c(1, 1.5, 0.75, 0.5))
plot(1, 1, xlim = c(-0.5, 9.5), ylim = c(-0.3, 1.1),
	xlab = '', ylab = '', xaxt = 'n', yaxt = 'n', xaxs = 'i', type = 'n')
axis(side = 1, at = 0:10, labels = 0:10, font = 2, cex.axis = 1.25, lwd = 2)
axis(side = 2, at = seq(-0.2, 1, 0.2), las = 2, font = 2, cex.axis = 1.25, lwd = 2)
mtext('Correlation coefficient (r)', side = 2, line = 3.5, cex = 1.8, font = 2)
mtext('Lag', side = 1, line = 3, cex = 1.8, font = 2)
mtext('Auto-correlation of residuals', side = 3, line = 0.8, cex = 2.25, font = 2)
abline(h = 0 + acf_sig_vals, lty = 2)
abline(h = 0 - acf_sig_vals, lty = 2)
segments(x0 = 0:9, x1 = 0:9, y0 = 0, y1 = acf_vals$acf[, , 1], lwd = 2)
abline(h = 0)
box(lwd = 2)


