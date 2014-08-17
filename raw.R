req.pack <- function (name, load=TRUE) {
  if (!name %in% installed.packages()) {
    install.packages(name)
  }
  if (load) {
    require(name, character.only = TRUE)
  }
}
# leaps gives access to regsubsets to perform
# best model search, backward|forward stepwise search
req.pack("leaps")

# gives access to vif
req.pack("car")

# load the data
data(mtcars)
dim(mtcars)

# explore the data
head(mtcars)
mtcars$am <- factor(x=mtcars$am, labels=c("Automatic", "Manual"))

# explore the mpg against am
plot(mtcars$am, mtcars$mpg)
# median looks quite different, so let's see if it's statically significant
# assuming mpg to be N(u, s)
t.test(mtcars[mtcars$am == "Automatic",]$mpg, mtcars[mtcars$am == "Manual",]$mpg)
# it's statically significant 0.0013 that their mean are differents
# also the .95 conf int doesn't contain 0, so the difference cannot inverse it self

# now let's see how am can predict mpg
simple.fit <- lm(mpg ~ am, data=mtcars)
summary(simple.fit)


# find the best models for mpg using backward stepwise reg
best.fits <- regsubsets(mpg ~ ., data=mtcars, force.in=c("amManual"))
summary(best.fits)

# best fits but forcing am in
summary(regsubsets(mpg ~ ., data=mtcars, method="forward", force.in = c("amManual")))
summary(regsubsets(mpg ~ ., data=mtcars, method="backward", force.in = c("amManual")))

# including hp is still low in R²
summary(lm(mpg ~ am + hp, mtcars))

# hp and wt is not considered because it removes am 
summary(lm(mpg ~ am + hp + wt, mtcars))

# cyl and wt is not considered because it removes am 
summary(lm(mpg ~ am + cyl + wt, mtcars))

#let's see with wt alone
# wt is always picked early
plot(mtcars$wt, mtcars$mpg, col=mtcars$am)

# regarding fit with am + 2 others the forward and backward stepwise reg are diverging
# forward select hp and wt
# backward select qsec and wt

fit.am.wt <- lm(mpg ~ wt * am, data=mtcars)
summary(fit.am.wt)
abline(coef(fit.am.wt)[1], coef(fit.am.wt)[2])
abline(coef(fit.am.wt)[1]+coef(fit.am.wt)[3], coef(fit.am.wt)[2]+coef(fit.am.wt)[4])

cor(mtcars$hp, mtcars$qsec)

fit.am.wt.qsec.hp <- lm(mpg ~ wt + qsec + hp + am, data=mtcars)
summary(fit.am.wt.qsec.hp)
plot(fit.am.wt.qsec.hp)

# hp is not significant here...
fit.wt.qsec.am <- lm(mpg ~ wt + qsec + am, data=mtcars)
summary(fit.wt.qsec.am)
plot(fit.wt.qsec.am)

# looking at the plot against wt, 
# it looks like a linear interaction with am could be interesting to look at
fit.wt.qsec.am.int <- lm(mpg ~ wt*am + qsec, data=mtcars)
summary(fit.wt.qsec.am.int)
plot(fit.wt.qsec.am.int)

anova(fit.wt.qsec.am, fit.wt.qsec.am.int)
# including the interaction sounds good and significant


#let's look at the vif
vif(fit.wt.qsec.am.int)
vif(fit.wt.qsec.am)
