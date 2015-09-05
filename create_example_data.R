## Bodo Winter & Martijn Wieling
## June 7, 2015; Changed September 5, 2015
## Generating data for change analysis paper

##------------------------------------------------------------------
## Create dataset #1: iterated learning, simple linar effect
##------------------------------------------------------------------

## Generate data for simple linear regression:

set.seed(53)
t <- 1:10
y <- 0.63 * t + rnorm(length(t), sd = 1.5) + 1.5

## Generate data structure for mixed model:

nchain <- 6
xdf <- data.frame(t = rep(t, nchain))
xdf$chain <- gl(nchain, length(t))

## Generate data:

set.seed(63)
# grand mean
xdf$y <- 2.5
# random intercepts
xdf$y <- xdf$y + rnorm(nchain, sd = 0.5)[xdf$chain]
# random slopes
xdf$y <- xdf$y + (0.83 + rnorm(nchain, sd = 1.5)[xdf$chain]) * xdf$t	
# random error and constrain to positive values
xdf$y <- abs(xdf$y + rnorm(nrow(xdf)))
# so that it fits into the range 1-10
xdf$y <- xdf$y * 0.5

## Write to table:

write.table(xdf, 'example1_iterated.csv', row.names = F, sep = ',')



##------------------------------------------------------------------
## Create dataset #2: repeated interaction, quadratic effect
##------------------------------------------------------------------

## Code originally generated for a course on May 9, 2014 (Brussels workshop)
## 10 dyads for 10 rounds; iconicity index ranges between 0 and 10

## Create function for generating a random 2nd order polynomial:

create10points <- function(b1, b2){			# b1 and b2 are the coefficients
	resp <- b1 * 1:10 - b2 * (-5:4)^2
	}

## Generate 10 dyads:

set.seed(42)
all_resp <- c()
for (i in 1:10) {
	all_resp <- c(all_resp,
		create10points(b1 = rnorm(1, mean = 4, sd = 1.5),
			b2 = rnorm(2, mean = 6, sd = 2)))
	}

## Add more noise to make it seem more realistic:

all_resp <- all_resp + rnorm(length(all_resp), sd = 10)

## Normalize all dyads to the iconicity index range (-10,10):

all_resp <- all_resp + abs(min(all_resp))
all_resp <- all_resp / max(all_resp) * 10

## Put everything together into a dataframe:

mydf <- data.frame(dyad = rep(1:10, each = 10),
	t = rep(1:10, 10),
	iconicity = all_resp)

## Write to table:

write.table(mydf, 'example2_dyads.csv', row.names = F, sep = ',')


