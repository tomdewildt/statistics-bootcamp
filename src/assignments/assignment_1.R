# Assignment 1

## 1. Start with loading the data and exploring it.

### Load dataset
data(centrality, package = "bootcamp2021")

### Explore dataset
head(centrality)
summary(centrality)
bootcamp2021::descriptives(centrality, omit_nonnumeric = TRUE)

## 2. Use one of the centralities: advice, friendship, or adverse and use that
##    as the dependent variable you want to explain.

### Dependent variable: advice

## 3. Build several regression models, with varying levels of complexity.
##    Experiment with interactions and transformations.
mod_1 <- lm(advice ~ activity_preference + conscientiousness +
                     extraversion, data = centrality)
summary(mod_1)

mod_2 <- lm(advice ~ activity_preference + conscientiousness +
                     extraversion + neuroticism, data = centrality)
summary(mod_2)

mod_3 <- lm(advice ~ activity_preference + conscientiousness +
                     extraversion + neuroticism +
                     agreeableness, data = centrality)
summary(mod_3)

mod_4 <- lm(advice ~ activity_preference + conscientiousness +
                     extraversion + neuroticism +
                     agreeableness + openness_experience, data = centrality)
summary(mod_4)

mod_5 <- lm(advice ~ activity_preference * conscientiousness +
                     extraversion * neuroticism +
                     agreeableness * openness_experience, data = centrality)
summary(mod_5)

## 4. Look at the diagnostic plots to check for anything fishy.
par(mfrow=c(2, 2))
plot(mod_1)
plot(mod_2)
plot(mod_3)
plot(mod_4)
plot(mod_5)

## 5. Compare multiple models based on theory or field expertise and compare the
##    models based on quantitative measures.
anova(mod_1, mod_2, mod_3, mod_4, mod_5)
bootcamp2021::compareLM(mod_1, mod_2, mod_3, mod_4, mod_5)
