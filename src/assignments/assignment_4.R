# Assignment 4

## You can choose between two research questions.

### 1. Does the interest in politics, together with the time spent online
###    looking for political information, predict the probability of watching
###    political shows during election time?
### 2. Does the interest in politics together with interest in elections
###    predict the probability of watching political shows during the election
###    time?
media_elec_data <- bootcamp2021::MediaElecData

## 1. Formulate a hypothesis.

## 2. Recode the variables appropriately (don’t forget to set the reference
##    category)
summary(media_elec_data)

# nolint start
media_elec_data$TVtime <- relevel(as.factor(media_elec_data$TVtime), ref = "0")
media_elec_data$ONLINEtime <- as.factor(media_elec_data$ONLINEtime)
media_elec_data$ElecInt <- as.factor(media_elec_data$ElecInt)
media_elec_data$age <- as.factor(media_elec_data$age)
media_elec_data$gender <- as.factor(media_elec_data$gender)
media_elec_data$edu <- as.factor(media_elec_data$edu)
# nolint end

summary(media_elec_data)

## 3. Specify nested models adding one predictor per time and the five control
##    variables (ProbLeft, ProbRight, age, gender, edu) in the last nested
##    model.
mod_tvtime1 <- glm(TVtime ~ ProbLeft,
                   family = binomial(link = logit),
                   data = media_elec_data)
jtools::summ(mod_tvtime1)

mod_tvtime2 <- glm(TVtime ~ ProbLeft + ProbRight,
                   family = binomial(link = logit),
                   data = media_elec_data)
jtools::summ(mod_tvtime2)

mod_tvtime3 <- glm(TVtime ~ ProbLeft + ProbRight + age,
                   family = binomial(link = logit),
                   data = media_elec_data)
jtools::summ(mod_tvtime3)

mod_tvtime4 <- glm(TVtime ~ ProbLeft + ProbRight + age + gender,
                   family = binomial(link = logit),
                   data = media_elec_data)
jtools::summ(mod_tvtime4)

mod_tvtime5 <- glm(TVtime ~ ProbLeft + ProbRight + age + gender + edu,
                   family = binomial(link = logit),
                   data = media_elec_data)
jtools::summ(mod_tvtime5)

## 4. Compare models and select the best one.
texreg::screenreg(list(mod_tvtime1,
                       mod_tvtime2,
                       mod_tvtime3,
                       mod_tvtime4,
                       mod_tvtime5))

## 5. Estimate predicted values and residuals.
mod_tvtime3_predictions <- predict(mod_tvtime3, type = "response")
mod_tvtime3_residuals <- residuals(mod_tvtime3, type = "response")

## 6. Make a binned plot and comment on it.
bootcamp2021::binnedplot(mod_tvtime3_predictions, mod_tvtime3_residuals)

## 7. Interpret odds ratios.
mod_tvtime3$coefficients

## 8. Interpret log-odds.
exp(mod_tvtime3$coefficients)

## 9. Conclude by stating whether your hypothesis is supported or not.
