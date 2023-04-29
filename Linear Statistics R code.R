# Usage: Run the entire script to generate the experimental design, 
# fit a linear model to it, and evaluate the model using diagnostic 
# plots and statistical tests.

#          A: 2 min cooktime vs 4 min cooktime     | -1 vs 1
#          B: .5 tsp- vs 1.5 tsp baked baing soda  | -1 vs 1
#          C: 70 mL water vs 100 mL water          | -1 vs 1
# Response Y: max grams of force to cut the noodle

# importing libraries
library(FrF2)   # for generating fractional factorial designs
library(ggplot2)   # for creating visualizations
library(nortest)   # for performing statistical tests 


# Function to provide regular Fractional Factorial 2-level designs
f2_design <- FrF2(nruns=8,nfactors=3,replications=2,randomize=FALSE)
#cubePlot(c(2266,3043,3574,3369,2449,2640,2795,2474), c(-1,1), c(-1,1), c(-1,1))


first_run <-  c(2266,3043,3574,3369,2449,2640,2795,2474)   # The results from the first run of the experiment.
second_run <- c(4425,4011,2829,3149,3034,3097,3067,2860)   # The results from the second run of the experiment.


y <-  c(y.first,y.second) # The response variable Y, which is the maximum grams of force needed to cut a noodle.

f2_design <- add.response(f2_design, y) # Add the response variable Y to the experimental design.
f2_design

fitted <- lm(y ~ (A+B+C)^3, data=f2_design) # Fit a linear model to the experimental design.
summary(fitted)

MEPlot(fitted)   # Create a main effects plot to visualize the effects of each factor on the response variable.
IAPlot(fitted)   # Create interaction effect plots to visualize the interaction between factors.
effects <- 2*fitted$coeff # Calculate the effects of each factor on the response variable.

# Evaluating the model using diagnostic plots and statistical tests

ggplot(fitted, aes(.fitted, .stdresid)) + geom_point(pch = 21) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(se = FALSE, col = "red", size = 0.5, method = "loess") +
  labs(x = "Fitted values", y = "Standardized residuals",
       title = "Fitted values vs. Standardized residuals fitted model",
       subtitle = deparse(fitted$call))

# Creating a QQ plot to check the normality assumption of the model
ggplot(fitted, aes(sample = .stdresid)) +
  stat_qq(pch = 19) + 
  geom_abline(intercept = 0, slope = 1, linetype = "dotted") +
  labs(x = "Theoretical quantiles", y = "Standardized residuals", 
       title = "Normal Q-Q fitted model", subtitle = deparse(fitted$call))

# Performing an Anderson-Darling test to assess normality of the residuals
ad.test(rstudent(fitted))
