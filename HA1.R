################# Part 1: Loading the data ###################################################

# Csv file "nfl2016.csv" is read from R working direrctory and stored in
# Data frame "nfl2016", we define ";" as separator (csv file is of this form)
nfl2016 = read.csv("nfl2016.csv", sep = ";")

################# Part 2: Linear Fit #########################################################

# Constructing a vector with regressors of interest
reg=c(3,5,6)

# Linear regression model with an intercept and regressors: "Passing.Yards"+"Sacks"+"SB.Win.11y"
# The result is stored in the variable "Points.lm"
Points.lm = lm(Points ~ 1 + Passing.Yards + Sacks + SB.Win.11y, nfl2016)

# a summary of given model is displayed
summary(Points.lm)

# As it can be observed from the summary, regressors "SB.Win.11y" and "Sacks" are
# not significant. For the Forward and Backward Selection of
# regressors one could use their p-value as criterion. 
# As an approximation to the AIC Criterion (Akaike Information Criterion) Literature suggest 
# To use a p-value of < 0.1573 as criterion to include regressors. 
# As a criterial p-value one should not use a too low threshold value, since the exclusion of
# certain regressors can have a negative impact on the p-values of other regressors 
# Also one should avoid leaving out too many regressors

########## Part 3: Forward Selection ########################################################

# The linear regression model is tested separately with an intercept and one 
# regressor of interest at a time. If the p-value of a regressor is lower than 0.1573, then 
# the regressor is selected for the final model

reg_forw=c()                          # defining variable "reg_forw" as a vector
for (j in reg){                       # outter loop that just runs n times where n is the number of regressors
  reg_forw_temp=c()                   # defining/reseting variable "reg_forw_temp" as a vector
  p_old=1                             # defining/reseting p_old to 1 as 100%, as in the inner loop this value is set to p-value of the current regressor
  for (i in reg){                     # inner loop, runs the function lm for each regressor
      if (is.null(reg_forw)) {        # checks if there are already selected regressors, that qualify for the forward selection
        regressors=c(4,i)             # sets which variables are included in the lm to be run
      } else { 
          if (max(reg_forw==i)!=1){   # checks that already selected regressors by forward selection are not tested again
          regressors=c(4,i,reg_forw)  # sets with which variables the lm should be run
          } else {next}               # forces inner for-loop to go to next iteration, if the currect regressor was already selected
      }
      Points.FS.lm = lm(Points ~ ., data = nfl2016[,regressors])      # runs the lm with prespecified regressors
      p=(summary(Points.FS.lm)$coefficients[colnames(nfl2016)[i],4])  # stores the p-value of the current regressor as variable "p"
      if (p<=0.1573 & p<p_old){                                       # checks whether the p-value of the current regressor is lower than the desired 0.1573 and the 
                                                                      # p-value of previous regressor
        p_old=p                       # current regressors p-value is given to variable "p-old" so the next regressors p-value can be compared
        reg_forw_temp=i               # since this regressor currently qualifies to be in the selection, it is temporarily stored
      }
  }
  reg_forw=c(reg_forw,reg_forw_temp)  # the regressor with the lowest p-value (which is lower than 0.1573) from the inner loop is
                                      # stored in the final selection of regressors that were selected with the
                                      # forward selection method
}

# Finally we compute the model which has been selected. We need to make sure that if NO regressor has met our criterion
# we use a model only containing an intercept. Therefore we adjust our dataframe to either the whole dataset
# (no regressor selected) or to the dataframe only containing the corresponding columns.

if (is.null(reg_forw)){
  Points.FS.lm = lm(Points ~ 1, data = nfl2016)
} else {
  Points.FS.lm=Points.FS.lm = lm(Points ~ ., data = nfl2016[,c(4,reg_forw)])
}


# Display summary of the final model
summary(Points.FS.lm)

# As a comparison and justification of our apporach, we used the in-built step function
# to conduct our forward selection as well, which uses the AIC approach

# We initiate an linear regression model with only an intercept
intercept.ml = lm(Points ~ 1, nfl2016)

# We set the bounds of the step function as: lower model only containing an intercept, and upper model
# containing all regressors
Points.FS.Steps.lm = step(intercept.ml, scope = list(lower = intercept.ml, upper = Points.lm), direction = "forward")

# Finally we compare both approaches using an analysis of variance
anova(Points.FS.Steps.lm, Points.FS.lm)

# as it can be seen, the same results are provided

################# Part 4: Backward Selection ################################################

# Basically the algorithm for the Forward Selection was reversed and implemented as the Backward Selection
# with only minor changes, which are indicated and explained below


reg_back=reg                    # Starting point is not a model with only the intecept, but the full model with all regressors
for (j in reg){
  reg_back_tmp=c()
  p_old=0                       # since elimination of the regressors with the highest p-value is the goal, starting point of comparison is 0%
  for(i in reg){
    if (is.null(reg_back)){     # in this implemenation the model only works with at least one regressor
      break                     # therefore the for-loop is stopped
    } else {
      if(max(reg_back==i)!=1){  # we are only interested in regressors that are currently still present in the model
        next                    # if it is not the case, next regressor is tested (next iteration of the for-loop)
      }
    }
    Points.BS.lm = lm(Points ~ ., data = nfl2016[,c(4,reg_back)])   # runs the function lm with prespecified regressors
    p=(summary(Points.BS.lm)$coefficients[colnames(nfl2016)[i],4])  # stores the p-value of the current regressor as variable "p"
    if(p>=0.1573 & p>p_old){                                        # We are interested which regressor should be kicked out of the model
      p_old=p
      reg_back_tmp=i
    }
  }
  if(is.null(reg_back_tmp)){next} else {                            # next iteration of the outter for-loop is launched, when no regressor was selected
    reg_back=reg_back[reg_back!=reg_back_tmp]                       # selected regressor by the inner for-loop is kicked out of the model 
  }
}

# As above we compute the model which has been selected. We need to make sure that if NO regressor has met our criterion
# we use a model only containing an intercept. Therefore we adjust our dataframe to either the whole dataset
# (no regressor selected) or to the dataframe only containing the corresponding columns.

if (is.null(reg_back)){
  Points.BS.lm = lm(Points ~ 1, data = nfl2016)
} else {
    Points.BS.lm = lm(Points ~ ., data = nfl2016[,c(4,reg_back)])
}

# Display summary of the final model
summary(Points.BS.lm)

# As before: As a comparison and justification of our apporach, we used the in-built step function
# to conduct our backward selection as well, which uses the AIC approach

# Setting the step function for Backward Selection and starting with a model with all regressors
Points.BS.Steps.lm = step(Points.lm, direction = 'backward')

# Finally we compare both approaches using an analysis of variance
anova(Points.BS.Steps.lm,Points.BS.lm)


################# Part 5: Analysis of the residuals #########################################

# We are interested in the distribution of the residuals, as normally distributed residuals indicate
# that the mean of the difference between the predicted values and the actual points are not far 
# from zero.

# Both methods return the same final model with only one regrossor. So either model can
# be plotted:

par(mfrow = c(1, 2))                # Graph parameters are specified, namely that two figures will be plotted

plot(Points.FS.lm, which = c(1,2))  # Plot residuals vs fitted values and st. residuals vs theor. quantiles 

# R generates quantiles for a standard normal distribution.
# Left graph shows if there are some non-linear relationsships that have not been explained
# by the model, in our case it is hard to argue that there is some non-linear leftover relationship.

# Right graph plots normal theoretical quantiles vs. the st residuals of the model. So for normality
# we should expect the points to be on the line (at least most of the points should be), however in
# our case we rather sense there is some excess kurtosis

# To examine the normality of the residuals we can also look at their pdf where we use a histogram for comparison

# Getting residuals from the model and calculating standardized residuals
resid.lm = resid(Points.FS.lm)
st.resid.lm = (resid.lm - mean(resid.lm))/sd(resid.lm)

# Ploting st. residuals in the histogram and adding the normal curve
hist(st.resid.lm, freq = F,xlim=c(-5,5), breaks=5)
curve(dnorm, add = TRUE, col="red", lw=2)

# The better the bars fit into the shape of the normal distribution, the more confident
# one can be, that the residuals are distributed normally.
# as before we seem to have at least some excess kurtosis

################# Part 6: Concluding output #################################################

summary(Points.FS.lm)

# There is only one regressor left over in the model, however this regressor is highly significant
# and therefore we can conclude that "Passing.Yards" has an significant positive influence on "Points".

# R-squared is roughly 0.5 and therefore about half of the total variation is explained by the model, which
# should be seen as an accaptable result for a model with only one regressor in a case where one could 
# expect many variables to be influencing the dependent variable.