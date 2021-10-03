# Assignment 2 (Part I)

## 1. Load the dataset dozen from the bootcamp2021 package.
data(dozen, package = "bootcamp2021")

## 2. For each dataset, calculate descriptive statistics.
result <- matrix(data = NA, nrow = length(dozen), ncol = 5)

rownames(result) <- names(dozen)
colnames(result) <- c("mean_x", "mean_y", "sd_x", "sd_y", "cor_xy")

for (i in seq_along(dozen)) {
  means <- apply(dozen[[i]], 2, mean)
  sds <- apply(dozen[[i]], 2, sd)
  cor <- cor(dozen[[i]]$x, dozen[[i]]$y)

  result[i, ] <- c(means, sds, cor)
}

result

## 3. Compare the results, is there something you notice about this?

### All the datasets are very similar, there is very little difference in mean,
### standard deviation and correlation between the x and y values.

## 4. Make a scatterplot between x and y, again for each dataset separately. Is
##    there anything special you notice?
par(mfrow = c(4, 3))
for (i in seq_along(dozen)) {
  plot(
    dozen[[i]]$x, dozen[[i]]$y,
    main = names(dozen)[i],
    xlab = "x",
    ylab = "y",
  )
}

### The datasets form different shapes, like: stars, cirlces, horizontal lines,
### vertical lines, etc.

# Assignment 2 (Part II)

## 1. Load the dataset loans from the bootcamp2021 package.
data(loans, package = "bootcamp2021")

## 2. Pick a handful of variables that are interesting to you and might be
##    related to each other.

### annual_income, total_credit_limit, total_debit_limit, tax_liens,
### public_record_bankrupt
loans_small <- loans[, c(5, 17, 31, 37, 38)]

## 3. Explore the variables; look at the FiveNumber Summary, calculate
##    descriptive measures, calculate correlations, make informative plots,
##    search for outliers, et cetera.
summary(loans_small)

bootcamp2021::descriptives(loans_small)

par(mfrow = c(1, 1))
bootcamp2021::corrMat(loans_small)

outliertree::outlier.tree(loans_small)

## 4. Build a regression model (or a couple of them). Make sure to look at the
##    diagnostic plots after running the model. Try to find some interesting
##    relations.
mod_income <- lm(annual_income ~ total_credit_limit + total_debit_limit +
                                 tax_liens + public_record_bankrupt,
                 data = loans)
summary(mod_income)

par(mfrow = c(2, 2))
plot(mod_income)
