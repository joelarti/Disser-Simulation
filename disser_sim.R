
# Packages
```{r}
library(MASS)
library(car)
library(dplyr)
library(openxlsx)
```

# IR detector
```{r}
detect_ir <- function(dat){
  topp <- subset(dat, y == 1)
  bott <- subset(dat, y == 0)
  return(round(nrow(topp)/nrow(bott),2))
}
```

# Function for prediction accuracy
```{r}
accuracy <- function(model){
  prob <- predict(model, type = "response")
  pre_class <- ifelse(prob > 0.5, 1, 0)
  return(mean(pre_class == data$y))
}
```

# trans x2 to dummy
```{r}
trans <- function(datta){
  datta$dummi <- ifelse(datta$x2 >= mean(datta$x2), 1, 0)
  datta <- subset(datta, select = c(x1, dummi))
  names(datta) <- c("x1", "x2")
  return(datta)
}
```

# Inverse logit
```{r}
inv.logit <- function(x){
  return(exp(x)/(1 + exp(x))) 
}
```

# MCAR
```{r}
mcar <- function(datta){
  datta$miss <- rbinom(n, 1, prob = 0.30)
  datta <- subset(datta, miss == 0)
}
```

# MAR
```{r}
mar <- function(datta){
  datta1 <- subset(datta, x2 == 1)
  datta0 <- subset(datta, x2 == 0)
  datta1$miss <- rbinom(nrow(datta1), 1, prob = 0.225)
  datta0$miss <- rbinom(nrow(datta0), 1, prob = 0.075)
  datta <- rbind(datta1, datta0)
  datta <- subset(datta, miss == 0)
}
```

# MNAR
```{r}
mnar <- function(datta){
  datta1 <- subset(datta, y ==1)
  datta0 <- subset(datta, y == 0)
  datta1$miss <- rbinom(nrow(datta1), 1, prob = 0.225)
  datta0$miss <- rbinom(nrow(datta0), 1, prob = 0.075)
  datta <- rbind(datta1, datta0)
  datta <- subset(datta, miss == 0)
}
```

# IR 20%
```{r}
ir20 <- function(datta){
  datta1 <- subset(datta, y == 1)
  datta0 <- subset(datta, y == 0)
  u <- round(n/1.2)
  v <- n - u
  datta1 <- datta1[sample(nrow(datta1), v), ]
  datta0 <- datta0[sample(nrow(datta0), u), ]
  datta <- rbind(datta1, datta0)
}
```

# IR 10%
```{r}
ir10 <- function(datta){
  datta1 <- subset(datta, y == 1)
  datta0 <- subset(datta, y == 0)
  u <- round(n/1.1)
  v <- n - u
  datta1 <- datta1[sample(nrow(datta1), v), ]
  datta0 <- datta0[sample(nrow(datta0), u), ]
  datta <- rbind(datta1, datta0)
}
```

# IR 40%
```{r}
ir40 <- function(datta){
  datta1 <- subset(datta, y == 1)
  datta0 <- subset(datta, y == 0)
  u <- round(n/1.4)
  v <- n - u
  datta1 <- datta1[sample(nrow(datta1), v), ]
  datta0 <- datta0[sample(nrow(datta0), u), ]
  datta <- rbind(datta1, datta0)
}
```

# IR 30%
```{r}
ir30 <- function(datta){
  datta1 <- subset(datta, y == 1)
  datta0 <- subset(datta, y == 0)
  u <- round(n/1.3)
  v <- n - u
  datta1 <- datta1[sample(nrow(datta1), v), ]
  datta0 <- datta0[sample(nrow(datta0), u), ]
  datta <- rbind(datta1, datta0)
}
```

# IR 50%
```{r}
ir50 <- function(datta){
  datta1 <- subset(datta, y == 1)
  datta0 <- subset(datta, y == 0)
  u <- round(n/1.5)
  v <- n - u
  datta1 <- datta1[sample(nrow(datta1), v), ]
  datta0 <- datta0[sample(nrow(datta0), u), ]
  datta <- rbind(datta1, datta0)
}
```

# Complete data
```{r}
real <- function(datta){
  datta <- datta[sample(nrow(datta), n), ]
  return(datta)
}
```

# Missing mecha with real NAs
```{r}
mcar2 <- function(datta){
  datta$miss <- rbinom(nrow(datta), 1, prob = 0.30)
  datta <- datta %>% mutate(y2 = ifelse(miss == 0, y, NA))
}

mar2 <- function(datta){
  datta1 <- subset(datta, x2 == 1)
  datta0 <- subset(datta, x2 == 0)
  
  datta1$miss <- rbinom(nrow(datta1), 1, prob = 0.225)
  datta1 <- datta1 %>% mutate(y2 = ifelse(miss == 0, y, NA))
  
  datta0$miss <- rbinom(nrow(datta0), 1, prob = 0.075)
  datta0 <- datta0 %>% mutate(y2 = ifelse(miss == 0, y, NA))
  
  datta <- rbind(datta1, datta0)
}

mnar2 <- function(datta){
  datta1 <- subset(datta, y ==1)
  datta0 <- subset(datta, y == 0)
  
  datta1$miss <- rbinom(nrow(datta1), 1, prob = 0.225)
  datta1 <- datta1 %>% mutate(y2 = ifelse(miss == 0, y, NA))
  
  datta0$miss <- rbinom(nrow(datta0), 1, prob = 0.075)
  datta0 <- datta0 %>% mutate(y2 = ifelse(miss == 0, y, NA))
  
  datta <- rbind(datta1, datta0)
}
```

# LG Imputation algorithm
```{r}
imp <- function(datta, x1, x2){
  m <- glm(y ~ x1*x2, data = na.omit(datta), family = binomial (link = logit), maxit = 100)
  pr <- predict(m, type = "response", newdata = data.frame(x1 = datta$x1, x2 = datta$x2))
  cl <- ifelse(pr > 0.03, 1, 0)
  return(cl)
}

imp_process <- function(datta){
  datta$imputed <- imp(datta)
  datta <- datta %>% mutate(y3 = ifelse(is.na(y2) == TRUE, imputed, y))
}
```

# Oversampling
```{r}
over <- function(data){
  uu <- round(0.23 * n)
  uu2 <- round(0.77 * n)
  data1 <- subset(data, y == 1)
  data0 <- subset(data, y == 0)
  data11 <- data1[sample(nrow(data1), uu, replace = TRUE), ]
  data00 <- data0[sample(nrow(data0), uu2, replace = TRUE), ]
  data <- as.data.frame(rbind(data11, data00))
  return(data)
}
```

# Specifications
```{r}
setwd("/Users/lartisan/Desktop/SimResults3")
set.seed(12343)
reps <- 1200
results <- matrix(NA, nrow = reps, ncol = 22)

b0 <- -1.78
b1 <- 0.06
b2 <- 0.3
b3 <- 0.03
n <- 100

samples = 200000

x1 <- round(runif(samples, 18, 80))
x2 <- sample(c(0,1), samples, replace = TRUE)
x1 <- scale(x1, center = TRUE, scale = F)
xb <- b0 + b2*x2+ b1*x1 + b3*x2*x1
p <- inv.logit(xb)

for(i in 1:reps){
  y <- rbinom(n = samples, size = 1, prob = p)
  my_data <- data.frame(x1 = x1, x2 = x2, y = y )
  
  #data <- imp_process(mnar2(ir50(my_data)))
  data <- mcar(ir10(my_data))
  # data <- over(data)
  
  mod1 <- glm(y ~ x1*x2, data = data, family = binomial (link = logit), maxit = 100)
  # use y3 in imputation model
  lrt <- car::Anova(mod1)
  results[i, 1] <- mod1$coef[1]
  results[i, 2] <- mod1$coef[2]
  results[i, 3] <- mod1$coef[3]
  results[i, 4] <- mod1$coef[4]
  results[i, 5] <- summary(mod1)$coefficient[4, 4]
  results[i, 6] <- lrt$"Pr(>Chisq)"[3] 
  results[i, 7] <- 1
  results[i, 8] <- n
  results[i, 9] <- mean(data$y)
  results[i, 10] <- summary(mod1)$coefficient[1, 2]
  results[i, 11] <- summary(mod1)$coefficient[2, 2]
  results[i, 12] <- summary(mod1)$coefficient[3, 2]
  results[i, 13] <- summary(mod1)$coefficient[4, 2]
  results[i, 14] <- nrow(data)
  results[i, 15] <- summary(mod1)$coefficient[2, 4]
  results[i, 16] <- summary(mod1)$coefficient[3, 4]
  results[i, 17] <- lrt$"Pr(>Chisq)"[1]
  results[i, 18] <- lrt$"Pr(>Chisq)"[2]
  results[i, 19] <- accuracy(mod1)
  results[i, 20] <- detect_ir(data)
  results[i, 21] <- as.numeric(mod1$conv)
  results[i, 22] <- mod1$iter
  
  results <- as.data.frame(results)
  names(results) <- c("b0", "b1", "b2", "b3", "wald_b3", "lrt_b3", "cond", "sample", "prop_1",
                      "se_b0", "se_b1", "se_b2", "se_b3", "sample2", "wald_b1", "wald_b2",
                      "lrt_b1", "lrt_b2", "accuracy", "obs_ir", "conv", "iter")
}
print(round(colMeans(results),3))
reps - sum(results$converge) # Not converged
table(data$y2)

detect_ir(my_data) # TRUE IR
detect_ir(data)
print(round(mean(my_data$y), 2))
nrow(data)

wbb <- createWorkbook("remedies")
addWorksheet(wbb, "Raw Results")
writeData(wbb, sheet = 1, results)
saveWorkbook(wbb, "result1_.xlsx", overwrite = TRUE)
```