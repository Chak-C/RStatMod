## ----setup_def_Fin, include=FALSE---------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----setup2_libs_Fin, include=FALSE-------------------------------------------------------------------------------------------------------
library(knitr)
library(caTools)
library(tidyverse)
library(psych)
library(corrplot)
library(GGally)
library(ggplot2)
library(gridExtra)
library(MASS)
library(dplyr)
library(stats)
library(car)
library(caret)
library(grid)
library(kableExtra)
library(naniar)
library(mice)
library(leaps)
library(glmnet)
library(mgcv)
library(splines)
library(gam)
library(tree)
library(randomForest)
library(gbm)
library(nnet)
library(keras)
library(tensorflow)
library(naivebayes)
library(class)
library(pROC) #citation
library(e1071)


## ----setup3_Fin, include=FALSE------------------------------------------------------------------------------------------------------------
setwd('C:/DEV/STAT3001/final_Assign/Data')
set.seed(211)

train <- read.csv("train.csv", header=T)
test <- read.csv("test.csv", header=T)


## ----output, eval = FALSE, include = FALSE------------------------------------------------------------------------------------------------
## # change the model, get the pred, pass to output (for price_sR), get csv
## pred <- predict(tre.bag,newdata = ntest)
## output <- pred^2 * sign(pred)
## f_output <- cbind(test$id,output)
## write.csv(f_output, file = "submition.csv", row.names = FALSE)


## ----checkMissing_Fin, echo = FALSE, out.length = "70%"-----------------------------------------------------------------------------------
Missing <- paste0((round(colSums(is.na(train)) / nrow(train),3) * 100),'%')
Missing_1 <- data.frame(Missing[1:8]) %>% t()
Missing_2 <- data.frame(Missing[9:16]) %>% t()

colnames(Missing_1) <- c('id','price','school','lat','lon','rate','hoa','hvac')
colnames(Missing_2) <- c('garage','view','year','bath','bed','stories','lot','living')
rownames(Missing_1) <- 'Missing (%)'
rownames(Missing_2) <- 'Missing (%)'

kable(Missing_1, align = c("c"), caption = "Percentage of Missing Data") %>%
  kable_styling(latex_options = "HOLD_position")

kable(Missing_2, align = c("c")) %>%
  kable_styling(latex_options = "HOLD_position")


## ----mcar_test_Fin, echo = FALSE, out.length = "70%"--------------------------------------------------------------------------------------
temp <- mcar_test(train)
temp <- data.frame(round(temp,0))


## ----fill_missing1_Fin, cache = TRUE, echo = TRUE, results = "hide", warning = FALSE------------------------------------------------------
# Imputation on missing data
impute <- mice(train, m = 7, method = c("pmm"), seed = 211)

## ----fill_missing2_Fin, echo = FALSE------------------------------------------------------------------------------------------------------
# Access the completed imputed data
data <- complete(impute)

## ----testing_setup, include = FALSE-------------------------------------------------------------------------------------------------------
t_impute <- mice(test, m = 7, method = c("pmm"), seed = 211)
ntest <- complete(t_impute)

ntest$rate <- abs(ntest$rate)^(1/2) * sign(ntest$rate)
ntest$living <- abs(ntest$living)^(1/2) * sign(ntest$living)
ntest$lot <- abs(ntest$lot)^(1/2) * sign(ntest$lot)
colnames(ntest)[colnames(ntest) == "rate"] <- "rate_sR"
colnames(ntest)[colnames(ntest) == "living"] <- "living_sR"
colnames(ntest)[colnames(ntest) == "lot"] <- "lot_sR"


## ----checkSummary1_Fin, echo = FALSE, out.width = "30%", out.length = "30%"---------------------------------------------------------------
#describe from psych package
con_cov <- c("price","lat","lon","rate","garage","year","bath","bed","stories","lot","living")
summary <- describe(data[which(colnames(data) %in% con_cov)]) %>% 
  select_if(is.numeric) %>% t() %>% 
  as.data.frame() %>%
  mutate_if(is.numeric, round, 1)
summary <- summary %>% 
  kable(align = "c", caption = "Summary Statistics for Continuous Variables in Train Dataset") %>%
  kable_styling(latex_options = "HOLD_position")

summary


## ----checkSummary2_Fin, echo = FALSE, out.width = "30%", out.length = "30%"---------------------------------------------------------------
data_sim <- data[!(names(data) %in% c("id", "school","hoa","hvac","view","lat","lon","year",
                                      "bath","bed","stories","garage"))]

data_sim$price_sR <- abs(data_sim$price)^(1/2) * sign(data_sim$price)
data_sim$rate_sR <- abs(data_sim$rate)^(1/2) * sign(data_sim$rate)
data_sim$living_sR <- abs(data_sim$living)^(1/2) * sign(data_sim$living)
data_sim$lot_sR <- abs(data_sim$lot)^(1/2) * sign(data_sim$lot)

summary <- describe(data_sim[which(colnames(data_sim) %in% c("price_sR","rate_sR","lot_sR","living_sR"))]) %>% 
  select_if(is.numeric) %>% t() %>% 
  as.data.frame() %>%
  mutate_if(is.numeric, round, 1)


## ----refresh1_FIN, echo = FALSE-----------------------------------------------------------------------------------------------------------
data$price <- abs(data$price)^(1/2) * sign(data$price)
data$rate <- abs(data$rate)^(1/2) * sign(data$rate)
data$living <- abs(data$living)^(1/2) * sign(data$living)
data$lot <- abs(data$lot)^(1/2) * sign(data$lot)

colnames(data)[colnames(data) == "price"] <- "price_sR"
colnames(data)[colnames(data) == "rate"] <- "rate_sR"
colnames(data)[colnames(data) == "living"] <- "living_sR"
colnames(data)[colnames(data) == "lot"] <- "lot_sR"


## ----refresh2, echo = FALSE---------------------------------------------------------------------------------------------------------------
set.seed(659)
lm.one <- data[which(data$hvac == 0 & data$view == 0),]
lm.two <- data[which(data$hvac != 0 & data$view == 0),]
lm.three <- data[which(data$hvac == 0 & data$view != 0),]
lm.four <- data[which(data$hvac != 0 & data$view != 0),]

indices <- sample(nrow(lm.one))
fold1 <- data[indices[1:round(0.8*nrow(lm.one))], ]
val1 <- data[indices[(round(0.8*nrow(lm.one))+1):nrow(lm.one)], ]
indices <- sample(nrow(lm.two))
fold2 <- data[indices[1:round(0.8*nrow(lm.two))], ]
val2 <- data[indices[(round(0.8*nrow(lm.two))+1):nrow(lm.two)], ]
indices <- sample(nrow(lm.three))
fold3 <- data[indices[1:round(0.8*nrow(lm.three))], ]
val3 <- data[indices[(round(0.8*nrow(lm.three))+1):nrow(lm.three)], ]
indices <- sample(nrow(lm.four))
fold4 <- data[indices[1:round(0.8*nrow(lm.four))], ]
val4 <- data[indices[(round(0.8*nrow(lm.four))+1):nrow(lm.four)], ]

#og <- data
val <- rbind(val1,val2,val3,val4)
data <- rbind(fold1,fold2,fold3,fold4)


## ----cat_var1_Fin, echo = FALSE, fig.height = 2.2, fig.width= 6---------------------------------------------------------------------------
par(mfrow=c(1,4))
barplot(table(data$school), main = "school")
barplot(table(data$hoa), main = "hoa")
barplot(table(data$hvac), main = "hvac")
barplot(table(data$view), main = "view")
title(main = "Table 3.1: Distriution of Binary Variables", line = -1, outer = TRUE)


## ----cor_1_FIN, echo = FALSE--------------------------------------------------------------------------------------------------------------
temp <- round(cor(data[2:16],method="pearson"),2)

kable(temp, align = c("c"), caption = "Pearson Correlation") %>%
  kable_styling(latex_options = c("striped", "scale_down", "HOLD_position"))


## ----np_baseline_Fin, echo = FALSE, cache = TRUE------------------------------------------------------------------------------------------
n <- nrow(data) #no. of data
K <- 10 #cv count
errs <- rep(NA,K)
errs2 <- rep(NA,K)

fold <- sample(rep(1:K, each=n/K))

for(k in 1:K) {
  df.train <- data[fold != k,]
  df.test <- data[fold == k,]
  
  pred1 <- mean(df.train$price_sR)
  pred2 <- median(df.train$price_sR)
  
  errs[k] <- mean((df.test$price_sR-pred1)^2) 
  errs2[k] <- mean((df.test$price_sR-pred2)^2) 
}


## ----pred_setup, echo = FALSE-------------------------------------------------------------------------------------------------------------
pr <- data[which(!colnames(data) %in% c("id",'school'))]


## ----lm_cv1_Fin, cache = TRUE, echo = FALSE, fig.height = 4, fig.width = 10---------------------------------------------------------------
set.seed(83)

lm.one <- pr[which(pr$hvac == 0 & pr$view == 0),]
lm.two <- pr[which(pr$hvac != 0 & pr$view == 0),]
lm.three <- pr[which(pr$hvac == 0 & pr$view != 0),]
lm.four <- pr[which(pr$hvac != 0 & pr$view != 0),]

K <- 10

n <- nrow(lm.one)
fold1 <- sample(rep(1:K, each=n/K))
n <- nrow(lm.two)
fold2 <- sample(rep(1:K, each=n/K))
n <- nrow(lm.three)
fold3 <- sample(rep(1:K, each=n/K))
n <- nrow(lm.four)
fold4 <- sample(rep(1:K, each=n/K))

cv.mse <- matrix(NA,K,13)
val.mse <- rep(1,K)

for(k in 1:K) {
  df.train <- rbind(lm.one[fold1 != k,],lm.two[fold2 != k,],lm.three[fold3 != k,],lm.four[fold4 != k,])
  df.test <- rbind(lm.one[fold1 == k,],lm.two[fold2 == k,],lm.three[fold3 == k,],lm.four[fold4 == k,])
  
  X <- as.matrix(df.train[which(!colnames(df.train) %in% c("price_sR"))])
  Y <- as.matrix(df.train$price_sR)
  sm <- summary(regsubsets(X, Y, nvmax= 13))
  
  #find best performing subset
  for(i in 1:13) {
    temp <- sm$outmat[i,]
    f <- paste(names(temp[which(temp=='*')]),collapse = " + ")
    f <- paste('price_sR ~', f)
    
    cv_mod <- glm(as.formula(f), data = df.train)
    
    pred <- predict(cv_mod, newdata = df.test)
    cv.mse[k,i] <- mean((pred-df.test$price_sR)^2)
    
    if(i==5) {
      cv.mod <- cv_mod
    }
  }
  
  pred <- predict(cv.mod, newdata = val)
  mse <- mean((pred-val$price_sR)^2)
  val.mse[k] <- mse
  if(min(val.mse)==mse) {
    f.mse <- mse
    lm.mod <- cv.mod
    cv.f.mod <- summary(cv.mod)$coefficients
  }

}

par(mfrow=c(1,2))
plot(1:13,cv.mse[1,], type = "b", lwd = 2, ylab = "CV MSE", xlab = 'Number of variables in Subset', ylim = c(0.15,0.35))
for(i in 2:K){ 
  lines(cv.mse[i,], type="b", lwd=2, col=i) 
} #3 performs best


cv.est <- apply(cv.mse, 2, mean)
cv.sd <- apply(cv.mse, 2, sd)/sqrt(K)

plot(1:13, cv.est, lwd=3, type="b", ylim=c(0.20, 0.33),
ylab="CV MSE Estimate", xlab="Number of variables in Subset")
points(1:13, cv.est + cv.sd, pch=25, col="red")
points(1:13, cv.est - cv.sd, pch=24, col="red")

#real <- predct(cv.mod)
title(main = "Figure 1: Cross-validation MSE and Estimates on Subset Selection", line = -1, outer = TRUE)


## ----las_cv1, cache=TRUE, echo = FALSE, fig.height = 3.5, fig.width = 9-------------------------------------------------------------------
set.seed(83)

X <- as.matrix(pr[ , !colnames(pr) %in% "price_sR"])
Y <- pr$price_sR

valx <- as.matrix(val[ , !colnames(val) %in% c("price_sR","school","id")])
valy <- val$price_sR

las_mod <- glmnet(X,Y,alpha=1,standardize=FALSE)

par(mfrow=c(1,2))
plot(las_mod, xvar="lambda")
title(main = 'Figure 2: Covariate Coefficients and Lambda', line = 3)

las_mod <- cv.glmnet(X, Y, alpha = 1, standardize = FALSE)
lambda <- log(las_mod$lambda.min)
pred <- predict(las_mod, newx = valx, s = lambda)
mse <- round(mean((pred - valy)^2),4)
cofs <- coef(las_mod)

plot(las_mod)
title(main = 'Figure 3: MSE and Lambda', line = 3)

abline(v=lambda, col="cyan3", lwd=3)
abline(h=las_mod$cvm[which.min(las_mod$cvm)], lwd=3,
col="dark orange")


## ----pred1_Fin, echo = FALSE, fig.width = 8, fig.height = 4-------------------------------------------------------------------------------
par(mfrow=c(1,2))
pred_int <- predict(lm.mod, newdata = val)

# Create a scatter plot of actual vs. predicted values
plot(pred_int, val$price_sR, xlab = "Actual Values", ylab = "Predicted Values", main = "Linear Tuned")

abline(a=-0.1, b=0.868, col = "red",lty=2)

# Create a scatter plot of actual vs. predicted values
plot(pred, val$price_sR, xlab = "Actual Values", ylab = "Predicted Values", main = "Lasso Regularized")

abline(a=-0.1, b=0.868, col = "red",lty=2)

title(main='Figure 4: Actual vs Predicted Values',line = -1, outer=TRUE)


## ----poly_setup_Fin, cache = TRUE, echo = FALSE, fig.height = 3, fig.width = 10-----------------------------------------------------------
degrees <- 1:10
polyy <- function(variable) {
  train_mse <- sapply(degrees, function(degree) {
    formula <- as.formula(paste("price_sR ~ poly(",variable,",", degree, ")"))
    mod <- lm(formula, data = data)
    res <- predict(mod) - data$price_sR
    mean(res^2)
  })
  test_mse <- sapply(degrees, function(degree) {
    formula <- as.formula(paste("price_sR ~ poly(",variable,",", degree, ")"))
    mod <- lm(formula, data = data)
    res <- predict(mod, newdata = val) - val$price_sR
    mean(res^2)
  })
  
  mse_data <- data.frame(degree = degrees, 
                         train_mse = train_mse, 
                         test_mse = test_mse)
  
  # Combine the MSE data into a data frame
  mse_data <- data.frame(
    degree = degrees,
    train_mse = train_mse,
    test_mse = test_mse
  )
  
  return(mse_data)
}

par(mfrow=c(1,3))

mse_data <- polyy('bath')

irreducible_error <- min(mse_data$test_mse)
train_mse <- mse_data$train_mse
test_mse <- mse_data$test_mse

plot(degrees, train_mse, type = "l", col = "blue", xlab = "Model flexibility", ylab = "Mean squared error",
     main = "bath", ylim = c(min(train_mse)-0.005,max(test_mse)+0.005))
lines(degrees, test_mse, col = "red")
abline(h = irreducible_error, col = "black", lty = "dashed")
legend("topright", legend = c("Training MSE", "Testing MSE", "Irreducible Error"),
       col = c("blue", "red", "black"), lty = c(1, 1, 2))

mse_data <- polyy('rate_sR')

irreducible_error <- min(mse_data$test_mse)
train_mse <- mse_data$train_mse
test_mse <- mse_data$test_mse

plot(degrees, train_mse, type = "l", col = "blue", xlab = "Model flexibility", ylab = "Mean squared error",
     main = "rate_sR", ylim = c(min(train_mse)-0.005,max(test_mse)+0.005))
lines(degrees, test_mse, col = "red")
abline(h = irreducible_error, col = "black", lty = "dashed")
legend("topright", legend = c("Training MSE", "Testing MSE", "Irreducible Error"),
       col = c("blue", "red", "black"), lty = c(1, 1, 2))

mse_data <- polyy('living_sR')

irreducible_error <- min(mse_data$test_mse)
train_mse <- mse_data$train_mse
test_mse <- mse_data$test_mse

plot(degrees, train_mse, type = "l", col = "blue", xlab = "Model flexibility", ylab = "Mean squared error",
     main = "living_sR", ylim = c(min(train_mse)-0.005,max(test_mse)+0.005))
lines(degrees, test_mse, col = "red")
abline(h = irreducible_error, col = "black", lty = "dashed")
legend("topright", legend = c("Training MSE", "Testing MSE", "Irreducible Error"),
       col = c("blue", "red", "black"), lty = c(1, 1, 2))

title(main = 'Figure 5: Training and Test MSEs for Polynomial Models of Varing Covariates', line = -1, outer = TRUE)


## ----bsp_gam_Fin, cache = TRUE, echo = FALSE----------------------------------------------------------------------------------------------
# returns best gam mod
bosp <- function(data, samples) {
  f <- paste0("price_sR ~ lon + ns(lat) + ns(rate_sR) + hoa + garage + year + ns(bath) + ns(living_sR)")
  f.mse <- 0.5 #baseline (mean/median prediction)
  gam.mod <- NA
  
  for (i in 1:samples) {
    set.seed(i)
    # sample with replacement
    bt_train <- data[sample(nrow(data), replace = TRUE), ]
    
    # gam mod
    bt_mod <- gam::gam(as.formula(f), data = bt_train)
    
    # mse
    mse <- mean((predict(bt_mod,newdata=val)-val$price_sR)^2)
    
    if(mse < f.mse) {
      f.mse <- mse
      gam.mod <- bt_mod
    }
  }
  
  return(gam.mod)
}

gam.mod <- bosp(data,1000)
f.mse <- mean((predict(gam.mod,newdata=val)-val$price_sR)^2)

## ----plot_gam_Fin, echo = FALSE, fig.height = 6, fig.width = 12---------------------------------------------------------------------------
par(mfrow=c(2,4))
plot(gam.mod, se = TRUE, col = "red")
title(main="Figure 6: Splines on Covariates Used", line = -1, outer = TRUE)


## ----tree_setup1_fin,echo = FALSE---------------------------------------------------------------------------------------------------------
tre <- cv.tree(tree(price_sR ~ ., data = pr), K = 10)

## ----tree_setup2_fin, include = FALSE-----------------------------------------------------------------------------------------------------
png('cv_tree.png',height = 350, width = 480, units = "px")
plot(tre$size, tre$dev, type="b") #its 10
abline(v=10, col="cyan3", lwd=3)

dev.off()

tre <- prune.tree(tree(price_sR ~ ., data = pr), best=10)

pred <- predict(tre,newdata=val)
mse <- mean((pred-val$price_sR)^2)



## ----tree_bag_fin, cache = TRUE, include = FALSE------------------------------------------------------------------------------------------
set.seed(83)

tre.bag <- randomForest(price_sR ~ ., data = pr, mtry = 13, ntree = 200)

pred1 <- predict(tre.bag,newdata=val)
mse1 <- round(mean((pred1-val$price_sR)^2),3)

## ----tree_rf_fin, cache = TRUE, include = FALSE-------------------------------------------------------------------------------------------
set.seed(83)

tre.raf <- randomForest(price_sR ~ ., data = pr, mtry = 4, importance = TRUE, ntree = 200)

pred2 <- predict(tre.raf,newdata=val)
mse2 <- round(mean((pred2-val$price_sR)^2),3)

## ----tree_bot_fin, cache = TRUE, include = FALSE------------------------------------------------------------------------------------------
set.seed(83)

tre.bot <- gbm(price_sR ~., data=pr, distribution="gaussian", n.trees=5000, 
               interaction.depth=4)

pred3 <- predict(tre.bot,newdata=val)
mse3 <- round(mean((pred3-val$price_sR)^2),3)


## ----interpret_tre_fin, include = FALSE---------------------------------------------------------------------------------------------------
png("inte_tre.png",height = 300, width = 480, units = "px")
varImpPlot(tre.bag)
dev.off()


## ----tree_plot_fin, echo = FALSE----------------------------------------------------------------------------------------------------------
plot(tre.bag,lwd=2,main='',ylim = c(0.05,0.22))
par(new = TRUE)
plot(tre.raf,lwd=2,col=2,main='',ylim = c(0.05,0.22))

test_error <- NULL
for (i in 1:200) {
  pred <- predict(tre.bot, n.trees = i, newdata = pr)
  test_error <- c(test_error, mean((pred - pr$price_sR)^2))
}

lines(test_error, lwd = 2, col = 3)
legend("topright", legend = c("Bagging", "randomForest: m = sqrt(p)", "Boosting: Depth = 4"),
       col = c(1,2,3),lty = c(2, 2, 2))
title(main = "Figure 9: Test Errors vs No. of Trees: Ensemble Methods", line = -2, outer = TRUE)


## ----nn_base, results = 'hide'------------------------------------------------------------------------------------------------------------
nn.base <- nnet(price_sR ~ ., size=25, data = pr, linout = TRUE)

## ----nn_base2, include = FALSE------------------------------------------------------------------------------------------------------------
pred <- predict(nn.base, newdata = val)
mse <- mean((pred-val$price_sR)^2)


## ----nn_setup, cache = TRUE, echo = FALSE, messages = FALSE, warning = FALSE--------------------------------------------------------------
set.seed(83)

temp <- as.matrix(data)
temp <- temp[ , !colnames(temp) %in% c("price_sR","id","school")]
dimnames(temp) <- NULL
tempX <- as.matrix(data)
tempX <- tempX[,2]
dimnames(tempX) <- NULL

valX <- as.matrix(val)
valX <- valX[ , !colnames(valX) %in% c("price_sR","id","school")]
dimnames(valX) <- NULL
valY <- as.matrix(val)
valY <- valY[,2]
dimnames(valY) <- NULL

ntestX <- as.matrix(ntest)
ntestX <- ntestX[ , !colnames(ntestX) %in% c("id")]
dimnames(ntestX) <- NULL


## ----nn_2Lay, cache = TRUE, echo = FALSE, messages = FALSE, warning = FALSE, fig.show='hold', result = 'hide'-----------------------------
set.seed(83)
nn.first <- keras_model_sequential() %>%
  layer_dense(units = 100, activation = "relu", input_shape = c(13)) %>%
  #layer_batch_normalization() %>%
  #layer_dropout(rate = 0.4) %>%
  layer_dense(units = 50, activation = "relu") %>%
  #layer_dropout(rate = 0.3) %>%
  #layer_dense(units = 20, activation = "relu", kernel_regularizer = regularizer_l1(0.001)) %>%
  #layer_dropout(rate = 0.2) %>%
  layer_dense(units = 1)

# Compile the model
nn.first %>% compile(
  loss = "mse",
  optimizer = optimizer_adam(learning_rate = 0.005),
  metrics = "mae"
)


hist <- nn.first %>% fit(temp, tempX, epochs = 100, batch_size = 32, validation_split = 0.3)

pred <- nn.first %>% predict(valX)
mse1 <- round(mean((pred-valY)^2),3)

f1 <- nn.first %>% count_params()

## ----nnplot_1, include = FALSE------------------------------------------------------------------------------------------------------------
png("nn_1.png", height = 300, width = 550, units = "px")
plot(hist)
dev.off()


## ----nn_reg, cache = TRUE, echo = FALSE, messages = FALSE, warning = FALSE, fig.show='hold', result = 'hide'------------------------------
set.seed(83)
nn.second <- keras_model_sequential() %>%
  layer_dense(units = 100, activation = "relu", input_shape = c(13)) %>%
  #layer_batch_normalization() %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 50, activation = "relu", kernel_regularizer = regularizer_l2(0.001)) %>%
  layer_dropout(rate = 0.3) %>%
  #layer_dense(units = 20, activation = "relu", kernel_regularizer = regularizer_l1(0.001)) %>%
  #layer_dropout(rate = 0.2) %>%
  layer_dense(units = 1)

# Compile the model
nn.second %>% compile(
  loss = "mse",
  optimizer = optimizer_adam(learning_rate = 0.002),
  metrics = "mae"
)


hist <- nn.second %>% fit(temp, tempX, epochs = 100, batch_size = 32, validation_split = 0.3)

pred <- nn.second %>% predict(ntestX)

pred <- nn.second %>% predict(valX)
mse2 <- round(mean((pred-valY)^2),3)

f2 <- nn.second %>% count_params()

## ----nnplot_2, include = FALSE------------------------------------------------------------------------------------------------------------
png("nn_2.png", height = 300, width = 550, units = "px")
plot(hist)
dev.off()


## ----nn_noBNCB, cache = TRUE, echo = FALSE, messages = FALSE, warning = FALSE, fig.show='hold', result = 'hide'---------------------------
set.seed(83)
nn.third <- keras_model_sequential() %>%
  layer_dense(units = 100, activation = "relu", input_shape = c(13)) %>%
  #layer_batch_normalization() %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 50, activation = "relu", kernel_regularizer = regularizer_l2(0.001)) %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 20, activation = "relu", kernel_regularizer = regularizer_l1(0.001)) %>%
  layer_dropout(rate = 0.1) %>%
  layer_dense(units = 1)

# Compile the model
nn.third %>% compile(
  loss = "mse",
  optimizer = optimizer_adam(learning_rate = 0.002),
  metrics = "mae"
)


hist <- nn.third %>% fit(temp, tempX, epochs = 100, batch_size = 32, validation_split = 0.3)

pred <- nn.third %>% predict(valX)
mse3 <- round(mean((pred-valY)^2),3)

f3 <- nn.third %>% count_params()

## ----nnplot_3, include = FALSE------------------------------------------------------------------------------------------------------------
png("nn_3.png", height = 300, width = 550, units = "px")
plot(hist)
dev.off()


## ----nn_fin, cache = TRUE, echo = FALSE, messages = FALSE, warning = FALSE, fig.show='hold', result = 'hide'------------------------------
set.seed(83)
nn.dropout <- keras_model_sequential() %>%
  layer_dense(units = 100, activation = "relu", input_shape = c(13)) %>%
  layer_batch_normalization() %>%
  #layer_dropout(rate = 0.4) %>%
  layer_dense(units = 50, activation = "relu", kernel_regularizer = regularizer_l2(0.001)) %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 20, activation = "relu", kernel_regularizer = regularizer_l1(0.001)) %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 1)

# Compile the model
nn.dropout %>% compile(
  loss = "mse",
  optimizer = optimizer_adam(learning_rate = 0.005),
  metrics = "mae"
)


hist <- nn.dropout %>% fit(temp, tempX, epochs = 100, batch_size = 32, validation_split = 0.3,
                           callbacks = list(callback_early_stopping(patience = 15), callback_reduce_lr_on_plateau()))

pred <- nn.dropout %>% predict(valX)
mse4 <- round(mean((pred-valY)^2),3)

f4 <- nn.dropout %>%count_params()

## ----nnplot_last, include = FALSE---------------------------------------------------------------------------------------------------------
png("nn_last.png", height = 350, width = 550, units = "px")
plot(hist)
dev.off()


## ----clas_setup_Fin, echo = FALSE, results = 'hide'---------------------------------------------------------------------------------------
cl <- data[which(!colnames(data) %in% c("id",'price_sR'))]


## ----nc_baseline_FIN, include = FALSE, cache = TRUE---------------------------------------------------------------------------------------
n <- nrow(cl)
K <- 10
acc <- rep(NA,K)
acc2 <- rep(NA,K)

fold <- sample(rep(1:K, each = n/K))

for(k in 1:K) {
  
  temp <- cl
  temp$school <- as.factor(temp$school)
  df.train <- temp[fold != k,]
  df.test <- temp[fold == k,]
  
  temp <- naive_bayes(school ~ ., data = df.train)
  pred <- predict(temp, newdata = df.test)

  acc[k] <- sum(pred == df.test$school) / nrow(df.test)
  
  df.train$school = as.numeric(df.train$school)
  df.test$school = as.numeric(df.test$school)
  df.trainX <- df.train[,-1]
  df.trainY <- df.train[,1]
  df.testX <- df.test[,-1]
  df.testY <- df.test[,1]
  
  pred <- knn(train = df.trainX, test = df.testX, cl = df.trainY, k = 5) ##check appendix, 5 is this best
  acc2[k] <- sum(as.numeric(pred) == df.testY) / length(df.testY)
}


## ----lm_fs, eval=FALSE--------------------------------------------------------------------------------------------------------------------
## mod <- glm(school ~ ., data = cl, family = binomial(link = 'cauchit'))
## mod <- step(mod, direction = "backward")

## ----lm_fs2, include=FALSE----------------------------------------------------------------------------------------------------------------
mod <- glm(school ~ ., data = cl, family = binomial(link = 'cauchit'))
mod <- step(mod, direction = "backward")

## ----lm_fs3, echo = FALSE-----------------------------------------------------------------------------------------------------------------
print(formula(mod))


## ----lm_btfn, cache=TRUE, echo = FALSE----------------------------------------------------------------------------------------------------
f <- formula(mod)

acc_bs <- rep(0,1000)

# Perform B bootstrap replicates
for (i in 1:1000) {
  set.seed(i)
  # Sample with replacement from the original data
  bt_train <- cl[sample(nrow(cl), replace = TRUE), ]
  
  # Fit the model to the bootstrapped data
  bt_mod <- glm(f, data = bt_train, family = binomial(link = 'cauchit'))
  
  pred <- predict(bt_mod, newdata = val, type = 'response')
  pred <- ifelse(pred > 0.5, 1, 0)
  
  confusion <- table(pred,val$school)
  
  bt_acc <- (confusion[1,1]+confusion[2,2])/(confusion[1,1]+confusion[1,2]+confusion[2,1]+confusion[2,2])
  
  acc_bs[i] <- bt_acc
  
  if(max(acc_bs) == bt_acc) {
    f.lm.mod <- bt_mod
  }
}



## ----lm_bt1, cache = TRUE, echo = FALSE---------------------------------------------------------------------------------------------------
f.lm.mod <- bosp(cl, 1000, 0.5, 'cauchit')


## ----lm_bt2, cache = TRUE, include=FALSE--------------------------------------------------------------------------------------------------
pred <- predict(f.lm.mod, newdata = val, type = 'response')

threshold <- seq(0.01, 0.8, by=0.01)
n.t <- length(threshold)
overall <- rep(0, n.t)
fp <- rep(0, n.t)
fn <- rep(0, n.t)

for(i in 1:n.t){
  pred_lo <- ifelse(pred > i/100, 1, 0)
  confusion <- table(pred_lo,val$school)
  
  #fp
  t1 <- tryCatch({
    confusion['0',]
  }, error = function(err) {
    c(0, 0)
  })
  
  #fn
  t2 <- tryCatch({
    confusion['1',]
  }, error = function(err) {
    c(0, 0)
  })
  
  overall[i] <- (t1[2]+t2[1])/(sum(t1)+sum(t2))
  fp[i] <- ifelse(t1[2]==0, 0, t1[2]/(sum(t1)))
  fn[i] <-ifelse(t2[1]==0, 0, t2[1]/(sum(t2)))
}

png("lm_thres1.png", height = 300, width = 500)
plot(threshold,overall,type='l',lwd=3,ylim=c(0,0.8), ylab="error rate")
lines(threshold, fp, col="orange", lwd=3)
lines(threshold, fn, col="blue", lwd=3)
legend("topright", legend=c("overall", "false.pos", "false.neg"), col=c("black", "orange", "blue"),
lty=c(1,1,1), lwd=3 )
dev.off()


## ----lm_roc, include = FALSE--------------------------------------------------------------------------------------------------------------
png("lm_roc.png", height = 300, width = 500)
par(mfrow=c(1,2))

pred_lo <- ifelse(pred > 0.5, 1, 0)
plot(roc(val$school,pred_lo), main = "Threshold: 0.5", xlab = "False Positive Rate", ylab = "True Positive Rate", print.auc = TRUE)

pred_lo <- ifelse(pred > 0.6, 1, 0)
plot(roc(val$school,pred_lo), main = "Threshold: 0.6", xlab = "False Positive Rate", ylab = "True Positive Rate", print.auc = TRUE)

dev.off()

pred_lo <- ifelse(pred > 0.5, 1, 0)
confusion <- table(pred_lo,val$school)
t1 <- confusion['0',]
t2 <- confusion['1',]


## ----lda_setup, include = FALSE, cache = TRUE---------------------------------------------------------------------------------------------
n <- nrow(cl)
K <- 5
set.seed(824)
fold <- sample(rep(1:K, each=n/K))

best_mean_err_ld <- 1

unused_cov <- colnames(cl)
unused_cov <- unused_cov[which(!unused_cov == 'school')]
used_cov <- NULL

covs <- colnames(cl)

switch <- TRUE

while(switch) {
  switch <- FALSE
  
  if(length(unused_cov) == 0) {
    break
  }
  
  add_cov <- NULL
  
  for(cov in unused_cov) {
    errs <- rep(NA,K)
    
    cur_cov <- c(used_cov, cov)
    f <- paste0("school~",paste(cur_cov,collapse="+"))
    
    for(k in 1:K) {
      df.train <- cl[fold != k,]
      df.test <- cl[fold == k,]
      
      p0 <- sum(df.train$school == 0) / nrow(df.train)
      p1 <- sum(df.train$school == 1) / nrow(df.train)
      
      temp_mod <- lda(as.formula(f), data = df.train, prior = c(p0, p1))
      
      lda_pred <- predict(temp_mod, newdata = df.test)$class
      actual <- df.test$school
      
      # confusion matrix
      confusion <- table(lda_pred,actual)
      
      # error rate [Miss classification]
      errs[k] <- (confusion[1,2]+confusion[2,1])/(confusion[1,1]+confusion[1,2]+confusion[2,1]+confusion[2,2])
    }
    
    mean_err <- mean(errs)
    
    if(mean_err < best_mean_err_ld) {
      print(paste0(cov,' ',mean_err))
      best_mean_err_ld <- mean_err
      ld_sd <- sqrt(sum((errs - mean_err)^2) / (K - 1))
      add_cov <- cov
      switch <- TRUE
    }
  }
  
  used_cov <- c(used_cov,add_cov)
  unused_cov <- unused_cov[unused_cov!=add_cov]
}


## ----lda_finalMod, echo = FALSE-----------------------------------------------------------------------------------------------------------
p0 <- sum(val$school == 0) / nrow(val)
p1 <- sum(val$school == 1) / nrow(val)


f1 <- paste0("school~",paste(used_cov,collapse="+"))
temp_mod <- lda(as.formula(f1), data = val, prior = c(p0, p1))

lda_pred <- predict(temp_mod, newdata = val)$class
actual <- val$school

# confusion matrix
confusion_lda <- table(lda_pred,actual)

# error rate [Miss classification]
err_lda <- (confusion[1,2]+confusion[2,1])/(confusion[1,1]+confusion[1,2]+confusion[2,1]+confusion[2,2])


## ----qda_setup, include = FALSE-----------------------------------------------------------------------------------------------------------
n <- nrow(cl)
K <- 10
set.seed(824)
fold <- sample(rep(1:K, each=n/K))

best_mean_err_qd <- 1

unused_cov <- colnames(cl)
unused_cov <- unused_cov[which(!unused_cov == 'school')]
used_cov <- NULL

covs <- colnames(cl)

switch <- TRUE

while(switch) {
  switch <- FALSE
  
  if(length(unused_cov) == 0) {
    break
  }
  
  add_cov <- NULL
  
  for(cov in unused_cov) {
    errs <- rep(NA,K)
    
    cur_cov <- c(used_cov, cov)
    f <- paste0("school~",paste(cur_cov,collapse="+"))
    
    for(k in 1:K) {
      df.train <- cl[fold != k,]
      df.test <- cl[fold == k,]
      
      p0 <- sum(df.train$school == 0) / nrow(df.train)
      p1 <- sum(df.train$school == 1) / nrow(df.train)
      
      temp_modX <- qda(as.formula(f), data = df.train, prior = c(p0, p1))
      
      qda_pred <- predict(temp_modX, newdata = df.test)$class
      actual <- df.test$school
      
      # confusion matrix
      confusion <- table(qda_pred,actual)
      
      # error rate [Miss classification]
      errs[k] <- (confusion[1,2]+confusion[2,1])/(confusion[1,1]+confusion[1,2]+confusion[2,1]+confusion[2,2])
    }
    
    mean_err <- mean(errs)
    
    if(mean_err <= best_mean_err_qd) {
      print(paste0(cov,' ',mean_err))
      best_mean_err_qd <- mean_err
      qd_sd <- sqrt(sum((errs - mean_err)^2) / (K - 1))
      add_cov <- cov
      switch <- TRUE
    }
  }
  
  used_cov <- c(used_cov,add_cov)
  unused_cov <- unused_cov[unused_cov!=add_cov]
}


## ----qda_finalMod, echo = FALSE-----------------------------------------------------------------------------------------------------------
p0 <- sum(val$school == 0) / nrow(val)
p1 <- sum(val$school == 1) / nrow(val)


f2 <- paste0("school~",paste(used_cov,collapse="+"))
temp_modX <- qda(as.formula(f2), data = val, prior = c(p0, p1))

qda_pred <- predict(temp_modX, newdata = val)$class
actual <- val$school

# confusion matrix
confusion_qda <- table(qda_pred,actual)

# error rate [Miss classification]
err_qda <- (confusion[1,2]+confusion[2,1])/(confusion[1,1]+confusion[1,2]+confusion[2,1]+confusion[2,2])


## ----lm_ffplot, include = FALSE-----------------------------------------------------------------------------------------------------------
png("conf_lq.png", height = 300, width = 500)
par(mfrow=c(1,2))
ctable <- as.table(matrix(c(confusion_lda[1,1], confusion_lda[1,2], confusion_lda[2,1], confusion_lda[2,2]), nrow = 2, byrow = TRUE,
                          dimnames = list(c("0","1"),c("0","1"))))
names(dimnames(ctable)) <- c("Predicted","Actual")
fourfoldplot(ctable, color = c("#CC6666", "#99CC99"),
             conf.level = 0, margin = 1, main = "Method: LDA")

ctable <- as.table(matrix(c(confusion_qda[1,1], confusion_qda[1,2], confusion_qda[2,1], confusion_qda[2,2]), nrow = 2, byrow = TRUE,
                          dimnames = list(c("0","1"),c("0","1"))))
names(dimnames(ctable)) <- c("Predicted","Actual")
fourfoldplot(ctable, color = c("#CC6666", "#99CC99"),
             conf.level = 0, margin = 1, main = "Method: QDA")
dev.off()


## ----lq_roc, include = FALSE--------------------------------------------------------------------------------------------------------------
png("lq_roc.png", height = 300, width = 500)
par(mfrow=c(1,2))

plot(roc(val$school,as.numeric(lda_pred)-1), main = "Method: LDA", xlab = "False Positive Rate", 
     ylab = "True Positive Rate", print.auc = TRUE)

plot(roc(val$school,as.numeric(qda_pred)-1), main = "Method: QDA", xlab = "False Positive Rate", 
     ylab = "True Positive Rate", print.auc = TRUE)

dev.off()


## ----decision_bound, include = FALSE, message = FALSE, warning = FALSE--------------------------------------------------------------------
x1_grid <- seq(min(data$lat), max(data$lat), length.out = 100)
x2_grid <- seq(min(data$lon), max(data$lon), length.out = 100)
grid <- data.frame(cbind(x1_grid,x2_grid))
colnames(grid) <- c('lat','lon')

predictions <- predict(temp_mod, newdata = grid)
class_probabilities <- predictions$posterior

s1 <- temp_mod$scaling[1]
s2 <- temp_mod$scaling[2]
ld <- as.vector(temp_mod$means[1, ]) %*% temp_mod$scaling
linear_discriminant <- as.vector(grid$lat) * s2 + as.vector(grid$lon) * s1 + ld
px <- predict(temp_mod, newdata = grid, type = "var")$x[, 1]

confidence_intervals <- 1.96 * sqrt(sum((px-mean(px))^2)/99)/10
lower_bound <- linear_discriminant - confidence_intervals
upper_bound <- linear_discriminant + confidence_intervals


png("db.png", height = 300, width = 500)

plot(data$lat, data$lon, col = ifelse(data$school==1, 3,4), pch = 19, xlab = "lat", ylab = "lon")
lines(x1_grid, linear_discriminant, col = "red", lwd = 2)
lines(x1_grid, lower_bound, col = 1, lty = 2)
lines(x1_grid, upper_bound, col = 1, lty = 2)

legend("topright", legend = c("School Nearby","School Not Nearby"),
       col = c(3,4), lty = c(1, 1))

dev.off()


## ----CIs, include = FALSE-----------------------------------------------------------------------------------------------------------------
ld_d <- best_mean_err_ld - 1.96 * ld_sd / 2.236
ld_u <- best_mean_err_ld + 1.96 * ld_sd / 2.236

qd_d <- best_mean_err_qd - 1.96 * qd_sd / 3.162
qd_u <- best_mean_err_qd + 1.96 * qd_sd / 3.162

rates <- 1-acc_bs
lm_sd <- sqrt(sum((rates - mean(rates))^2) / 999)
lm_d <- mean(rates) - 1.96 * lm_sd / 31.622
lm_u <- mean(rates) + 1.96 * lm_sd / 31.622


## ----ctree_setup1_fin,results = 'hide'----------------------------------------------------------------------------------------------------
tre <- cv.tree(tree(as.factor(school) ~ ., data = cl), K = 10)

## ----ctree_setup2_fin, include = FALSE----------------------------------------------------------------------------------------------------
png('cv_clas_tree.png',height = 350, width = 480, units = "px")
plot(tre$size, tre$dev, type="b") #its 11
abline(v=7, col="cyan3", lwd=3)

dev.off()

tre <- prune.tree(tree(as.factor(school) ~ ., data = cl), best=11)

pred <- predict(tre,newdata=val, type = 'class')
pred <- as.numeric(pred)-1
actual <- val$school
confusion <- table(pred,actual)
      
acc <- (confusion[1,1]+confusion[2,2])/1000



## ----cl_tree_bag_fin, cache = TRUE, include = FALSE---------------------------------------------------------------------------------------
set.seed(83)

tre.bag <- randomForest(as.factor(school) ~ ., data = cl, mtry = 13, ntree = 200)

pred1 <- predict(tre.bag,newdata=val, type = 'class')
confusion1 <- table(pred1,actual)
acc1 <- (confusion1[1,1]+confusion1[2,2])/1000

## ----cl_tree_rf_fin, cache = TRUE, include = FALSE----------------------------------------------------------------------------------------
set.seed(83)
class_labels <- levels(data$Class)

tre.raf <- randomForest(as.factor(school) ~ ., data = cl, mtry = 4, importance = TRUE, ntree = 200)

pred2 <- predict(tre.raf,newdata=val, type = 'class')
confusion2 <- table(pred2,actual)
acc2 <- (confusion2[1,1]+confusion2[2,2])/1000

## ----cl_tree_bot_fin, cache = TRUE, include = FALSE---------------------------------------------------------------------------------------
set.seed(83)

tre.bot <- gbm(school ~., data=cl, distribution="bernoulli", n.trees=5000, 
               interaction.depth=4)

pred3 <- predict(tre.bot,newdata=val, type = 'response')
pred3 <- ifelse(pred3 > 0.5, 1, 0)
confusion3 <- table(pred3,actual)
acc3 <- (confusion3[1,1]+confusion3[2,2])/1000


## ----interpret_cl_tre_fin, include = FALSE------------------------------------------------------------------------------------------------
png("cv_inte_tre.png",height = 300, width = 480, units = "px")
plot(tre)
text(tre, pretty = 0)
dev.off()


## ----cl_tree_plot_fin, echo = FALSE, warning = FALSE, message = FALSE---------------------------------------------------------------------
par(mar=c(5,2,2,5),mfrow = c(2,3))
ctable <- as.table(matrix(c(confusion1[1,1], confusion1[1,2], confusion1[2,1], confusion1[2,2]), nrow = 2, byrow = TRUE,
                          dimnames = list(c("0","1"),c("0","1"))))
names(dimnames(ctable)) <- c("Predicted","Actual")
fourfoldplot(ctable, color = c("#CC6666", "#99CC99"),
             conf.level = 0, margin = 1)

ctable <- as.table(matrix(c(confusion2[1,1], confusion2[1,2], confusion2[2,1], confusion2[2,2]), nrow = 2, byrow = TRUE,
                          dimnames = list(c("0","1"),c("0","1"))))
names(dimnames(ctable)) <- c("Predicted","Actual")
fourfoldplot(ctable, color = c("#CC6666", "#99CC99"),
             conf.level = 0, margin = 1)

ctable <- as.table(matrix(c(confusion3[1,1], confusion3[1,2], confusion3[2,1], confusion3[2,2]), nrow = 2, byrow = TRUE,
                          dimnames = list(c("0","1"),c("0","1"))))
names(dimnames(ctable)) <- c("Predicted","Actual")
fourfoldplot(ctable, color = c("#CC6666", "#99CC99"),
             conf.level = 0, margin = 1)

plot(roc(val$school,as.numeric(pred1)-1), main = "Method: Bagging", xlab = "False Positive Rate", 
     ylab = "True Positive Rate", print.auc = TRUE)

plot(roc(val$school,as.numeric(pred2)-1), main = "Method: Random Forest", xlab = "False Positive Rate", 
     ylab = "True Positive Rate", print.auc = TRUE)

plot(roc(val$school,as.numeric(pred3)-1), main = "Method: Boosting", xlab = "False Positive Rate", 
     ylab = "True Positive Rate", print.auc = TRUE)

title(main = "Figure 16: Summary for Classification Trees", line = -1, outer = TRUE)


## ----svm_base-----------------------------------------------------------------------------------------------------------------------------
svm.mod <- svm(as.factor(school) ~ ., data = cl, kernel = 'linear', cost = 10, scale = FALSE)

## ----svm_base_results, echo = FALSE-------------------------------------------------------------------------------------------------------
temp1 <- val
temp1$school <- as.factor(temp1$school)

ypred <- predict(svm.mod, temp1)
tab <- table(predict=ypred, truth=temp1$school)


## ----svm_tune, cache = TRUE, echo = FALSE-------------------------------------------------------------------------------------------------
lin_svm <- tune(svm, as.factor(school) ~ ., data=cl, kernel ="linear",
     ranges=list(cost=c(0.001, 0.01, 0.1, 1, 5, 10, 100) ))

rad_svm <- tune(svm, as.factor(school) ~ ., data=cl, kernel ="radial",
     ranges=list(cost=c(0.001, 0.01, 0.1, 1, 5, 10, 100) ))

pol_svm <- tune(svm, as.factor(school) ~ ., data=cl, kernel ="polynomial",
     ranges=list(cost=c(0.001, 0.01, 0.1, 1, 5, 10, 100) ))


## ----svm_confusion, echo = FALSE----------------------------------------------------------------------------------------------------------
temp <- lin_svm$best.model

pred1 <- predict(temp, temp1)
confusion1 <- table(predict=pred1, truth=temp1$school)

temp <- rad_svm$best.model

pred2 <- predict(temp, temp1)
confusion2 <- table(predict=pred2, truth=temp1$school)

temp <- pol_svm$best.model

pred3 <- predict(temp, temp1)
confusion3 <- table(predict=pred3, truth=temp1$school)


## ----svm_plot_fin, include = FALSE, warning = FALSE, message = FALSE----------------------------------------------------------------------
png('smv_sum.png', height = 600, width = 1000, units = "px")

par(mar=c(5,2,2,5),mfrow = c(2,3))
ctable <- as.table(matrix(c(confusion1[1,1], confusion1[1,2], confusion1[2,1], confusion1[2,2]), nrow = 2, byrow = TRUE,
                          dimnames = list(c("0","1"),c("0","1"))))
names(dimnames(ctable)) <- c("Predicted","Actual")
fourfoldplot(ctable, color = c("#CC6666", "#99CC99"),
             conf.level = 0, margin = 1)

ctable <- as.table(matrix(c(confusion2[1,1], confusion2[1,2], confusion2[2,1], confusion2[2,2]), nrow = 2, byrow = TRUE,
                          dimnames = list(c("0","1"),c("0","1"))))
names(dimnames(ctable)) <- c("Predicted","Actual")
fourfoldplot(ctable, color = c("#CC6666", "#99CC99"),
             conf.level = 0, margin = 1)

ctable <- as.table(matrix(c(confusion3[1,1], confusion3[1,2], confusion3[2,1], confusion3[2,2]), nrow = 2, byrow = TRUE,
                          dimnames = list(c("0","1"),c("0","1"))))
names(dimnames(ctable)) <- c("Predicted","Actual")
fourfoldplot(ctable, color = c("#CC6666", "#99CC99"),
             conf.level = 0, margin = 1)

plot(roc(val$school,as.numeric(pred1)-1), main = "Function: Linear", xlab = "False Positive Rate", 
     ylab = "True Positive Rate", print.auc = TRUE)

plot(roc(val$school,as.numeric(pred2)-1), main = "Function: Radial", xlab = "False Positive Rate", 
     ylab = "True Positive Rate", print.auc = TRUE)

plot(roc(val$school,as.numeric(pred3)-1), main = "Function: Polynomial", xlab = "False Positive Rate", 
     ylab = "True Positive Rate", print.auc = TRUE)

dev.off()


## ----cnn_base, results = 'hide'-----------------------------------------------------------------------------------------------------------
nn.base <- nnet(as.factor(school) ~ ., size=25, data = cl, decay=5e-4, maxit=200)

## ----cnn_base2, include = FALSE-----------------------------------------------------------------------------------------------------------
pred <- predict(nn.base, newdata = val)
pred <- ifelse(pred > 0.5, 1, 0)

actual <- val$school
confusion <- table(pred,actual)



## ----cnn_setup, cache = TRUE, echo = FALSE, messages = FALSE, warning = FALSE-------------------------------------------------------------
set.seed(83)

temp <- as.matrix(data)
temp <- temp[ , !colnames(temp) %in% c("price_sR","id","school")]
dimnames(temp) <- NULL
tempX <- as.matrix(data)
tempX <- tempX[,3]
dimnames(tempX) <- NULL

valX <- as.matrix(val)
valX <- valX[ , !colnames(valX) %in% c("price_sR","id","school")]
dimnames(valX) <- NULL
valY <- as.matrix(val)
valY <- valY[,3]
dimnames(valY) <- NULL

ntestX <- as.matrix(ntest)
ntestX <- ntestX[ , !colnames(ntestX) %in% c("id")]
dimnames(ntestX) <- NULL


## ----cnn_2Lay, cache = TRUE, echo = FALSE, messages = FALSE, warning = FALSE, fig.show='hold', result = 'hide'----------------------------
set.seed(83)
nn.first <- keras_model_sequential() %>%
  layer_dense(units = 100, activation = "relu", input_shape = c(13)) %>%
  #layer_batch_normalization() %>%
  #layer_dropout(rate = 0.4) %>%
  layer_dense(units = 50, activation = "relu") %>%
  #layer_dropout(rate = 0.3) %>%
  #layer_dense(units = 20, activation = "relu", kernel_regularizer = regularizer_l1(0.001)) %>%
  #layer_dropout(rate = 0.2) %>%
  layer_dense(units = 1, activation = 'sigmoid')

# Compile the model
nn.first %>% compile(
  loss = "binary_crossentropy",
  optimizer = optimizer_adam(learning_rate = 0.002),
  metrics = "accuracy"
)


hist <- nn.first %>% fit(temp, tempX, epochs = 100, batch_size = 32, validation_split = 0.3)

pred <- nn.first %>% predict(valX)
pred <- ifelse(pred > 0.5, 1, 0)
confusion1 <- table(pred,valY)

f1 <- nn.first %>% count_params()

## ----cnnplot_1, include = FALSE-----------------------------------------------------------------------------------------------------------
png("cnn_1.png", height = 300, width = 550, units = "px")
plot(hist)
dev.off()


## ----cnn_reg, cache = TRUE, echo = FALSE, messages = FALSE, warning = FALSE, fig.show='hold', result = 'hide'-----------------------------
set.seed(83)
nn.second <- keras_model_sequential() %>%
  layer_dense(units = 100, activation = "relu", input_shape = c(13)) %>%
  #layer_batch_normalization() %>%
  #layer_dropout(rate = 0.3) %>%
  layer_dense(units = 50, activation = "relu") %>%
  layer_dropout(rate = 0.1) %>%
  #layer_dense(units = 20, activation = "relu", kernel_regularizer = regularizer_l1(0.001)) %>%
  #layer_dropout(rate = 0.2) %>%
  layer_dense(units = 1, activation = 'sigmoid')

# Compile the model
nn.second %>% compile(
  loss = "binary_crossentropy",
  optimizer = optimizer_adam(learning_rate = 0.005),
  metrics = "accuracy"
)


hist <- nn.second %>% fit(temp, tempX, epochs = 100, batch_size = 32, validation_split = 0.2)

#pred <- nn.second %>% predict(ntestX)

pred <- nn.second %>% predict(valX)
pred <- ifelse(pred > 0.5, 1, 0)
confusion2 <- table(pred,valY)

f2 <- nn.second %>% count_params()

## ----cnnplot_2, include = FALSE-----------------------------------------------------------------------------------------------------------
png("cnn_2.png", height = 300, width = 550, units = "px")
plot(hist)
dev.off()


## ----cnn_noBNCB, cache = TRUE, echo = FALSE, messages = FALSE, warning = FALSE, fig.show='hold', result = 'hide'--------------------------
set.seed(83)
nn.third <- keras_model_sequential() %>%
  layer_dense(units = 100, activation = "relu", input_shape = c(13)) %>%
  #layer_batch_normalization() %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 50, activation = "relu") %>%
  layer_dropout(rate = 0.1) %>%
  layer_dense(units = 25, activation = "relu") %>%
  layer_dropout(rate = 0.05) %>%
  layer_dense(units = 1, activation = 'sigmoid')

# Compile the model
nn.third %>% compile(
  loss = "binary_crossentropy",
  optimizer = optimizer_adam(learning_rate = 0.003),
  metrics = "accuracy"
)


hist <- nn.third %>% fit(temp, tempX, epochs = 100, batch_size = 32, validation_split = 0.2)


pred <- nn.third %>% predict(valX)
pred <- ifelse(pred > 0.5, 1, 0)
confusion3 <- table(pred,valY)

f3 <- nn.third %>% count_params()

## ----cnnplot_3, include = FALSE-----------------------------------------------------------------------------------------------------------
png("cnn_3.png", height = 300, width = 550, units = "px")
plot(hist)
dev.off()


## ----cnn_fin, cache = TRUE, echo = FALSE, messages = FALSE, warning = FALSE, fig.show='hold', result = 'hide'-----------------------------
set.seed(83)
nn.dropout <- keras_model_sequential() %>%
  layer_dense(units = 100, activation = "relu", input_shape = c(13)) %>%
  layer_batch_normalization() %>%
  #layer_dropout(rate = 0.4) %>%
  layer_dense(units = 50, activation = "relu", kernel_regularizer = regularizer_l2(0.001)) %>%
  layer_dropout(rate = 0.1) %>%
  layer_dense(units = 25, activation = "relu") %>%
  layer_dropout(rate = 0.05) %>%
  layer_dense(units = 1, activation = 'sigmoid')

# Compile the model
nn.dropout %>% compile(
  loss = "binary_crossentropy",
  optimizer = optimizer_adam(learning_rate = 0.005),
  metrics = "accuracy"
)


hist <- nn.dropout %>% fit(temp, tempX, epochs = 100, batch_size = 32, validation_split = 0.3,
                           callbacks = list(callback_early_stopping(patience = 10), callback_reduce_lr_on_plateau()))


pred <- nn.dropout %>% predict(valX)
pred <- ifelse(pred > 0.5, 1, 0)
confusion4 <- table(pred,valY)

f4 <- nn.dropout %>%count_params()

## ----cnnplot_last, include = FALSE--------------------------------------------------------------------------------------------------------
png("cnn_last.png", height = 350, width = 550, units = "px")
plot(hist)
dev.off()


## ----knn_setup, include = FALSE-----------------------------------------------------------------------------------------------------------
k <- 1:20


n <- nrow(cl)
K <- 10
acc <- rep(NA,K)

fold <- sample(rep(1:K, each = n/K))
test.er <- rep(0, length(k))
for(c in 1:20){
  acc2 <- rep(NA,K)
  for(k in 1:K) {
    
    temp <- cl
    temp$school <- as.factor(temp$school)
    df.train <- temp[fold != k,]
    df.test <- temp[fold == k,]
    
    temp <- naive_bayes(school ~ ., data = df.train)
    pred <- predict(temp, newdata = df.test)
    
    acc[k] <- sum(pred == df.test$school) / nrow(df.test)
    
    df.train$school = as.numeric(df.train$school)
    df.test$school = as.numeric(df.test$school)
    df.trainX <- df.train[,-1]
    df.trainY <- df.train[,1]
    df.testX <- df.test[,-1]
    df.testY <- df.test[,1]
    
    knn.pred <- knn(train = df.trainX, test = df.testX, cl = df.trainY, k = c)
    
    
    pred <- knn(train = df.trainX, test = df.testX, cl = df.trainY, k = 15)
    acc2[k] <- sum(as.numeric(pred) == df.testY) / length(df.testY)
  }
  
  test.er[c] <- mean(acc2[k])
}


## ----knn_plot, echo = FALSE, out.height='60%', out.width='60%',fig.align='center'---------------------------------------------------------
plot(1:20,test.er,lwd=2,ylab = 'Test Acc',xlab = 'X Nearest Neighbours', type = 'l', main = 'Test Accuracy and K-Nearest-Neighbours')

## ----output_csv_retrieve (Not included in Rmd)

# change model
# change formula based on prediction or classification
pred <- predict(tre.bot,newdata = ntest)
output <- pred^2 * sign(pred)
f_output <- cbind(test$id,output)
write.csv(f_output, file = "submition.csv", row.names = FALSE)

# for trees classification
pred <- predict(tre.raf, newdata = ntest)
f_output <- cbind(test$id,as.numeric(pred)-1)
write.csv(f_output, file = "submition.csv", row.names = FALSE)

pred3 <- predict(tre.bot,newdata=ntest, type = 'response')
pred3 <- ifelse(pred3 > 0.5, 1, 0)
f_output <- cbind(test$id,pred3)
write.csv(f_output, file = "submition.csv", row.names = FALSE)

# for nn mods
ntestID <- ntest$id

ntestX <- as.matrix(ntest)
ntestX <- ntest[ , !colnames(ntest) %in% c("id")]
pred <- nn.second %>% predict(ntestX)
f_output <- cbind(test$id,pred2)

output <- cbind(test$id,as.numeric(pred2)-1)
write.csv(output, file = "submition.csv", row.names = FALSE)


pred2 <- predict(temp, ntest)
