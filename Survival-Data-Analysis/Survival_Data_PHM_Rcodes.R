# --------------- Survival Data Analysis ---------------
# Exploration of Proportional Hazards Model

# Load library
library(coxed); library(survival); library(ggplot2)

# Set seed (57217)
set.seed(57217)

# --------------- Generate a survival dataset ---------------
simdata <- sim.survdata(N = 42, T = 45, num.data.frames = 1)$data
head(simdata, n = 5)

# --------------- EDA ---------------
summary(simdata)
cor(simdata[, c("X1", "X2", "X3")])
pairs(simdata[,1:3])
km_fit <- survfit(Surv(y, failed) ~ 1, data=simdata)
plot(km_fit, xlab="Time", ylab="Survival Probability", 
     main="Kaplan-Meier Survival Curve")

# Histograms for each covariate
ggplot(simdata, aes(x=X1)) + 
  geom_histogram(binwidth=1) + 
  ggtitle("Distribution of X1") + 
  theme_classic() 
ggplot(simdata, aes(x=X2)) + 
  geom_histogram(binwidth=1) + 
  ggtitle("Distribution of X2") + 
  theme_classic()
ggplot(simdata, aes(x=X3)) + 
  geom_histogram(binwidth=1) + 
  ggtitle("Distribution of X3") + 
  theme_classic()

# Histogram for survival times
ggplot(simdata, aes(x=y)) + 
  geom_histogram(binwidth=1) + 
  ggtitle("Distribution of Survival Times") + 
  theme_classic()

# --------------- Fitting using Proportional Hazards Model ---------------
# Proportional Hazards Model 
phm <- coxph(formula = Surv(y, failed) ~ X1 + X2 + X3, 
             data = simdata, method = "breslow")
summary(phm)

# Reduced model (both direction)
reduced_model <- step(phm, direction = "both")

# Reduced model (backward direction)
reduced_model2 <- step(phm, direction = "backward")

# Reduced model (forward direction)
nullphm <- coxph(formula = Surv(y, failed) ~ 1, 
                 data = simdata, method = "breslow")
reduced_model3 <- step(nullphm, direction = "forward", 
                       scope = ~ X1 + X2 + X3)

summary(reduced_model)

# --------------- Adequacy of the Reduced Model ---------------
plot(cox.zph(reduced_model))
cox.zph(reduced_model)

# Deviance Residuals Plot (Reduced Model)
deviance_residuals <- resid(reduced_model, type="deviance")
plot(deviance_residuals, main="Deviance Residuals", 
     ylab="Residuals", xlab="Index")
abline(h=0, col="red")


# --------------- Additional Analysis --------------- 
# Martingale residuals
scatter.smooth(simdata$X1, resid(reduced_model))

# Full model (Wald's test)
stagecov <- phm$var[1:3,1:3]
(X.w <- phm$coef[1:3]%*%solve(stagecov)%*%(phm$coef[1:3])) # test statistics
1-pchisq(X.w,4) # p-value

# Other approaches
summary(coxph(formula = Surv(y, failed) ~ X1 * X2 * X3, 
              data = simdata, method = "breslow"))
summary(coxph(formula = Surv(y, failed) ~ X1 * X2 + X3, 
              data = simdata, method = "breslow"))
summary(coxph(formula = Surv(y, failed) ~ X1 + X2 * X3, 
              data = simdata, method = "breslow"))




