# Assignment 3 (Part I)

## 1. Provide three reasons why causal inference are so important to both
##    science and practice.

### Helps understanding the underlying mechanism of a relationship, increases
### certainty of conclusion and increases credibility of arguments and
### conclusion.

## 2. Explain why experiment is seen as the golden standard of drawing causal
##    inferences.

### Conducting experiments with random assignment is one of the best methods to
### create a similar environment with only one variable of interest different
### between treatment and non-treatment groups.

## 3. Provide three disadvantages of using an experiment versus other research
##    methods.

### It is difficult to generalize findings from an experiment to the real
### world. Can be difficult to implement an experiment due to various factors
### like feasibility, ethics or costs. A well designed experiment tends to
### require more resources than other research methods.

## 4. Explain what type I and type II error are and how they related to
##    statistical power.

### Type I error: falsely rejecting the null hypothesis.
### Type II error: falsely accepting the null hypothesis.
### Statistical power: the probability of correctly rejecting a null hypothesis.

## 5. In the lecture, we learned that ANOVA is a subset of OLS regression.
##    Please write the regression formula for an experimental design with 3
##    group (baseline, group 1, group 2) and no covariate.

### Yi = α + β1dummy1 + β2dummy2 + εi

## 6. Explain with the regression formula above why the mean of the baseline
##    group is always equal to the intercept of the regression model (tips:
##    baseline group is coded as 0 in both dummy variables).

### When the baseline group is active dummy 1 and dummy 2 are both 0, therefore
### Y is equal to the intercept.

## 7. You are the manager of talent management. You would like to know if data
##    analytic training can improve employee performance. You have 1560
##    employees that are clustered into 52 teams, with 30 employees in each
##    team. Each of these teams has its own manager and own office. You randomly
##    assign 780 employees to the experimental group (i.e., receive data
##    analytic training) and 780 employees to the control group (i.e., no data
##    analytic training).

### a. What are the null and alternative hypotheses of this study?

#### H0: μ = μ1
#### H1: μ ≠ μ1

### b. What are the potential problems of the design above?

#### Measurements of employees in the same team are related to each other
#### because they have the same manager and work in the same office. Violating
#### the independence assumption.

### c. How can the potential problems above be solved?

#### Sample a subset of employees for this study (i.e. only one employee per
#### team).

## 8. Use the following data to perform a one-way ANOVA by hand
##    (alpha level = .05)
n_groups <- 3
g1 <- c(43, 23, 45, 59, 43, 60, 77)
g2 <- c(52, 44, 52, 46, 38, 32, 53)
g3 <- c(75, 77, 74, 99, 84, 80, 89)

### SS Total
sst <- sum((g1 - mean(c(g1, g2, g3)))^2) +
       sum((g2 - mean(c(g1, g2, g3)))^2) +
       sum((g3 - mean(c(g1, g2, g3)))^2)

### SS Residual
ssr <- sum((g1 - mean(g1))^2) +
       sum((g2 - mean(g2))^2) +
       sum((g3 - mean(g3))^2)

### SS Model
ssm <- sst - ssr

### DF Residual
dfr <- length(g1) + length(g2) + length(g3) - n_groups

### DF Model
dfm <- n_groups - 1

### MS Residual
msr <- ssr / dfr

### MS Model
msm <- ssm / dfm

### F
f <- msm / msr

### \ Source   \    SS   \  df  \    MS   \   F   \
### \ -------- \ ------- \ ---- \ ------- \ ----- \
### \ Model    \ 5771.14 \   2  \ 2885.57 \ 19.79 \
### \ Residual \ 2625.14 \  18  \  145.84 \       \
### \ Total    \ 8396.29 \      \         \       \

# Assignment 3 (Part II)

## 1. Please test whether the amount of funding received by the control group
##    and the amount of funding received by experimental group 1 are
##    statistically different. Please conduct the appropriate analysis using R
##    for this question.
experiment <- read.csv("./data/experiment.csv",
                       header = TRUE,
                       colClasses = c("factor", "numeric", "numeric"))

### a. Should you conduct one sample t-test, independent sample t-test, or
###    paired samples t-test? Please explain why.

#### An independent sample t-test should be conducted because you need to
#### compare the mean between two groups.

### b. Does the assumptions of normality and homogeneity of variance hold?
# nolint start
experiment_group_1_2 <- experiment[which(experiment$Condition == 1:2), ]
experiment_group_1_2$Residual <- residuals(lm(Funding ~ Condition,
                                              data = experiment_group_1_2))
# nolint end

#### Test Normality
shapiro.test(experiment_group_1_2$Residual[experiment_group_1_2$Condition == 1])
shapiro.test(experiment_group_1_2$Residual[experiment_group_1_2$Condition == 2])

#### Test homogeneity
car::leveneTest(experiment_group_1_2$Funding ~ experiment_group_1_2$Condition,
                center = "median")

#### The normality assumption is violated for group 1 and 2 because the results
#### of the shapiro test are statistically significant. The assumptions for
#### homogeneity holds because the results for the levene test are not
#### statistically significant.

### c. Please report the results of the t-test and your conclusion.

#### Non-parametric test
with(experiment_group_1_2, wilcox.test(Funding ~ Condition,
                                       conf.int = TRUE,
                                       paired = FALSE))

#### A non-parametric test must be conducted since the normality assumption is
#### violated. The results indicated that there is no statistically significant
#### difference between the control and the intuition group.

## 2. Please test whether the amount of funding received by the control group,
##    the intuition group, and the "act like a scientist" group are
##    statistically different. Please conduct the appropriate analysis using R
##    for this question.

### a. We do not hypothesize which group would attract higher amount of funding.
###    In this case, should we perform a post-hoc analysis or planned contrast
###    analysis? Please explain why.

#### A post-hoc analysis should be performed for this question because there is
#### no prior hypothesis for the differences between groups.

### b. Which multiple comparison test should we use if we prefer a conservative
###    test?

#### Bonferroni is one of the most conservative tests. Alternatively,
#### the Benjamin-Hochberg test is less conservative and more widely accepted.

### c. Please report the results of the ANOVA and your conclusion.
# nolint start
experiment$Residual <- residuals(aov(Funding ~ Condition, data = experiment))
# nolint end

#### Test normality
shapiro.test(experiment$Residual[experiment$Condition == 1])
shapiro.test(experiment$Residual[experiment$Condition == 2])
shapiro.test(experiment$Residual[experiment$Condition == 3])

#### Test homogeneity
car::leveneTest(experiment$Funding ~ experiment$Condition, center = "median")

#### Non-parametric test
kruskal.test(Funding ~ Condition, data = experiment)

#### The shapiro test is statistically significant for group 1, 2 and 3.
#### Therefore we need to conduct a non-parametric test to compare the mean of
#### the three groups. The result of the kruskal test show that there is at
#### least one statistically significant difference among the three groups with
#### respect to the level of funding received.

## 3. We think that firm age might influence funding. Please test whether the
##    amount of funding received by the control group, the intuition group, and
##    the “act like a scientist” group are statistically different while
##    controlling for firm age. Please conduct an ANCOVA test for this question
##    and report the results.
ancova <- aov(Funding ~ Firm.age + Condition, data = experiment)
summary(ancova)

### The results of the ANCOVA analysis show that the difference of funding
### received by group 1, 2 and 33 remains statistically significant after
### including the firm age effect into the model.
