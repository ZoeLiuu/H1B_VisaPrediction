## ----setup, include=TRUE, cache=FALSE, echo=FALSE----------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ---- echo = FALSE, warning=FALSE, message=FALSE-----------------------------------------------
library(readr)
library(readxl)
library(dplyr)
library(ggplot2)
library(caret)
library(DMwR)
library(knitr)
library(tibble)
library(tidyr)
library(lubridate)
library(rpart)
library(rpart.plot)
library(glmnet)
library(kernlab)
library(randomForest)
library(plotROC)



## ----load_data, message=FALSE, echo=FALSE------------------------------------------------------
#load("H1BPrediction.Rdata")
#load("all_years.Rdata")  ## dataset we created  and organized for the EDA
load("H1BVisaClassSuccessful-2.RData")
load("H1BFinalData.RData")



## ----variables, message=FALSE, echo=FALSE, eval=FALSE------------------------------------------
## names(all_years_2)


## ----general_plots, message=FALSE, echo=FALSE, fig.align="left", fig.width=8, fig.height=6-----
## case_statuses' percentage among all applications
all_years_2 %>%
  group_by(CASE_STATUS) %>%
  summarise(perc = n()/nrow(all_years_2)) %>%
  ggplot(mapping = aes(x = CASE_STATUS, y = perc, fill = CASE_STATUS,
                       label = scales::percent(perc))) +
  geom_bar(stat= "identity") +
  geom_text(position = position_dodge(width = .9),    # move to center of bars
            vjust = -0.5,    # nudge above top of bar
            size = 4) +
  scale_y_continuous(labels = scales::percent)


## ----clean_data, message=FALSE, echo=FALSE, eval=FALSE-----------------------------------------
## ## remove rows with over 0.2% missing values
FY_new <- all_years_2[-manyNAs(all_years),]
FY_new <- mutate(FY_new, SOC_CATEGORY = (substr(FY_new$SOC_CODE, start = 1, stop = 2)))
## 
## 
## ## Convert variables to numeric/integer/factor for tuning models
## #FY_new$PREVAILING_WAGE <- as.integer(gsub(",", "", FY_new$PREVAILING_WAGE))
## #FY_new$WAGE_RATE_OF_PAY <- as.numeric(gsub(",", "", FY_new$WAGE_RATE_OF_PAY))  # ADDED THIS LINE. NEED TO CONSIDER WAGE_RATE_OF_PAY
## FY_new$WAGE_RATE_OF_PAY_TO <- as.numeric(gsub(",", "", FY_new$WAGE_RATE_OF_PAY_TO))
## FY_new$WAGE_RATE_OF_PAY_FROM <- as.numeric(gsub(",", "", FY_new$WAGE_RATE_OF_PAY_FROM))
## #FY_new$SOC_CODE <- as.factor(gsub(".00", "", FY_new$SOC_CODE))
## FY_new$year <- as.integer(FY_new$year)
## FY_new$NAICS_CODE <- as.integer(FY_new$NAICS_CODE)
## 
## 
## FY_new <- mutate(FY_new, WAGE_MEAN = (WAGE_RATE_OF_PAY_FROM + WAGE_RATE_OF_PAY_TO)/2)
## 
## FY_new <- mutate(FY_new, WAGE_RATE_YEARLY = ifelse(WAGE_UNIT_OF_PAY == "Year", WAGE_MEAN,
##                                             ifelse(WAGE_UNIT_OF_PAY == "Month", WAGE_MEAN*12,
##                                             ifelse(WAGE_UNIT_OF_PAY == "Week", WAGE_MEAN*52,
##                                             ifelse(WAGE_UNIT_OF_PAY == "Bi-Weekly", WAGE_MEAN*52/2, WAGE_MEAN*40*52)))))
## 
## 


## ---- message=FALSE, echo=FALSE, eval=FALSE----------------------------------------------------
## names(FY_new)


## ---- message=FALSE, echo=FALSE, eval=FALSE----------------------------------------------------
## #can remove SOC_CODE, and "wage" variables except for WAGE_RATE_YEARLY
## 
## ## nearZeroVar(FY_new)
## ## romove variables with near zero variance
## 
## 
names(FY_new[c(3,4,5, 6, 7, 8, 10, 11, 13, 14, 15, 17, 19,20, 21, 24, 25, 26,27, 28, 29,30,32, 34, 35, 36, 37, 42, 49, 52)])
## 
FY_new2 <- FY_new[, -c(3,4,5, 6, 7, 8, 10, 11, 13, 14, 15, 17, 19,20, 21, 24, 25, 26,27, 28, 29,30,32, 34, 35, 36, 37, 42, 49, 52)]
nzv_FY_new2 <- nearZeroVar(FY_new2, saveMetrics = TRUE)
nzv_FY_new2 <- rownames_to_column(nzv_FY_new2)
nzv_FY_new2 <- nzv_FY_new2 %>%
filter(nzv == TRUE)
## 
FY_new2 <- FY_new2 %>%
  select(-c(nzv_FY_new2$rowname))
## 
FY_new2$CASE_SUBMITTED <- month(FY_new2$CASE_SUBMITTED)
## 
FY_new2$year <- as.integer(FY_new2$year)
FY_new2$PW_SOURCE_YEAR <- as.integer(FY_new2$PW_SOURCE_YEAR)
FY_new2$SOC_CATEGORY <- as.integer(FY_new2$SOC_CATEGORY)
## 
FY_new2 <- FY_new2 %>%
  filter(PW_SOURCE_YEAR >1000)


## ---- message=FALSE, echo=FALSE, eval=FALSE----------------------------------------------------
## OPTION 1
## 
## Change the reposnse to binary outcomes
## 
FY_new3 <- FY_new2 %>%
  filter(CASE_STATUS != "WITHDRAWN")
## 
FY_new3$CASE_STATUS <- gsub("CERTIFIED", "CERTIFIED", FY_new3$CASE_STATUS)
FY_new3$CASE_STATUS <- gsub("DENIED", "DENIED", FY_new3$CASE_STATUS)
FY_new3$CASE_STATUS <- gsub("CERTIFIED-WITHDRAWN", "CERTIFIED", FY_new3$CASE_STATUS)
FY_new2$CASE_STATUS <- gsub("WITHDRAWN", "DENIED", FY_new2$CASE_STATUS)
## 
FY_new3$CASE_STATUS <- as.factor(FY_new3$CASE_STATUS)
## 
FY_new3 <- FY_new3 %>%
  select(-SUPPORT_H1B)


## ---- message=FALSE, echo=FALSE, eval=FALSE----------------------------------------------------
## Option 2
## 
## Change the reposnse to binary outcomes
## 
## interesting find: because gsub is looking for patterns, it included "CERTIFIED-WITHDRAWN" in the initial "CERTIFIED" script. Then it also added the "WITHDRAWN" part so the result was a new category of "SUCCESSFUL-UNSUCCESSFUL"... so to fix this, I had to rearrange for "CERTIFIED-WITHDRAWN" to be evaluated first.
## 
FY_new3 <- FY_new2
## 
FY_new3$CASE_STATUS <- gsub("CERTIFIED-WITHDRAWN", "UNSUCCESSFUL", FY_new3$CASE_STATUS)
FY_new3$CASE_STATUS <- gsub("CERTIFIED", "SUCCESSFUL", FY_new3$CASE_STATUS)
FY_new3$CASE_STATUS <- gsub("DENIED", "UNSUCCESSFUL", FY_new3$CASE_STATUS)
FY_new3$CASE_STATUS <- gsub("WITHDRAWN", "UNSUCCESSFUL", FY_new3$CASE_STATUS)
## 
FY_new3$CASE_STATUS <- as.factor(FY_new3$CASE_STATUS)
## 
FY_new3 <- FY_new3 %>%
  select(-SUPPORT_H1B)



## ----revised_plots, message=FALSE, echo=FALSE, fig.align="left", fig.width=8, fig.height=6-----
## case_statuses' percentage among all applications (after cleaning)
FY_new3 %>%
  group_by(CASE_STATUS) %>%
  summarise(perc = n()/nrow(FY_new3)) %>%
  ggplot(mapping = aes(x = CASE_STATUS, y = perc, fill = CASE_STATUS,
                       label = scales::percent(perc))) +
  geom_bar(stat= "identity") +
  geom_text(position = position_dodge(width = .9),    # move to center of bars
            vjust = -0.5,    # nudge above top of bar
            size = 4) +
  scale_y_continuous(labels = scales::percent)


## ---- message=FALSE, echo=FALSE----------------------------------------------------------------

str(FY_new3)



## ----datasplit, message=FALSE, echo=FALSE, eval=FALSE------------------------------------------
index <- sample.int(n = nrow(FY_new3),
                    size = floor(0.008* nrow(FY_new3)),
                    replace = FALSE)
## 
train_FY <- FY_new3[index,]
test_FY <- FY_new3[-index,]
## 
library(janitor)
train_FY <- clean_names(train_FY)
test_FY <- clean_names(test_FY)


## ---- message=FALSE, echo=FALSE, fig.cap= "Summary of Training Data"---------------------------
dim(train_FY)

summary(train_FY)


## ----trControl, message=FALSE, echo=FALSE, eval=FALSE------------------------------------------
ctrl <- trainControl(method = "cv",
                     number = 10,
                     summaryFunction = twoClassSummary,
                     classProbs = TRUE,
                     savePredictions = TRUE)


## ----glmnet, warning=FALSE, eval=FALSE, echo=FALSE---------------------------------------------
## Linear: GLMnet
## 
enet.grid <- expand.grid(alpha = c(0.1, 0.3, 0.5, 0.7),
                         lambda = exp(seq(-6, 1, length.out = 21)))
## 
set.seed(4321)
fit.glmnet <- train(case_status ~ ., data = train_FY,
                    method = "glmnet",
                    metric = "ROC",
                    tuneGrid = enet.grid,
                    preProcess = c("center", "scale"),
                    trControl = ctrl, na.action = na.omit)

## ---- echo=FALSE, eval=FALSE-------------------------------------------------------------------
plot(fit.glmnet, xTrans = log)
## 
## confusion matrix
confusionMatrix.train(fit.glmnet)


## ----pls, warning=FALSE, eval=FALSE, echo=FALSE------------------------------------------------
## Linear: PLS
## 
library(pls)
pls_grid <- expand.grid(ncomp = seq(1, 13, by = 1))
## 
set.seed(4321)
fit.pls <- train(case_status ~ ., data = train_FY,
                 method = "pls",
                 metric = "ROC",
                 tuneGrid = pls_grid,
                 preProcess = c("center", "scale"),
                 trControl = ctrl, na.action = na.omit)

## ---- echo=FALSE, eval=FALSE-------------------------------------------------------------------
fit.pls
## 
## plot
plot(fit.pls)
## 
## confusion matrix
confusionMatrix.train(fit.pls)


## ----tree, echo=FALSE, eval=FALSE--------------------------------------------------------------
## Tree
## 
library(rpart)
library(rpart.plot)
## 
fit.tree <- train(x = train_FY[, 2:19],
                  y = train_FY$case_status,
                  method = "rpart",
                  metric = "ROC",
                  tuneLength = 30,
                  trControl = ctrl, na.action = na.omit)

## ---- echo=FALSE, eval=FALSE-------------------------------------------------------------------
fit.tree
## 
## plot
prp(fit.tree$finalModel,
    box.palette = "Reds",
    tweak = 1.5,
    varlen = 30)
## 
## confusion matrix
confusionMatrix.train(fit.tree)
## 


## ----nonlinear, echo=FALSE, eval=FALSE---------------------------------------------------------
## Nonlinear: SVM
## 
set.seed(4321)
fit.svm <- train(case_status ~ ., data = train_FY,
                 method = "svmRadial",
                 metric = "ROC",
                 preProcess = c("center", "scale"),
                 trControl = ctrl, na.action = na.omit)

## ---- echo=FALSE, eval=FALSE-------------------------------------------------------------------
fit.svm
## 
## the best model
fit.svm$bestTune
## 
## confusion matrix
confusionMatrix.train(fit.svm)


## ----ensemble, echo=FALSE, eval=FALSE----------------------------------------------------------
## Ensemble: Random Forest
## 
rf_gird <- expand.grid(mtry = c(2, 3, 6, 9, 12, 15))
## 
set.seed(4321)
fit.rf <- train(case_status ~ ., data = train_FY,
                method = "rf",
                metric = "ROC",
                trControl = ctrl,
                tuneGrid = rf_gird,
                importance = TRUE, na.action = na.omit)

## ---- echo=FALSE, eval=FALSE-------------------------------------------------------------------
fit.rf
## 
## confusion matrix
confusionMatrix.train(fit.rf)


## ----combineresults, echo=FALSE, fig.align="left", fig.width=8, fig.height=6, eval=FALSE-------
## **Figure 5:** Model Comparision (ROC, Sensitivity, Specificity)
## 
model_cv_result <- resamples(list(GLMNET = fit.glmnet,
                                  PLS = fit.pls,
                                  SVM = fit.svm,
                                  RF = fit.rf,
                                  Tree = fit.tree))
## visualize ROC, Sensitivity, and Specificity comparison across models
dotplot(model_cv_result)


## ----option2, echo=FALSE, eval=FALSE-----------------------------------------------------------
cv_pred_results <- fit.glmnet$pred %>% tbl_df() %>%
  filter(alpha %in%  fit.glmnet$bestTune$alpha,
         lambda %in% fit.glmnet$bestTune$lambda) %>%
  select(pred, obs, SUCCESSFUL, UNSUCCESSFUL, rowIndex, Resample) %>%
  mutate(model_name = "GLMNET") %>%
  bind_rows(fit.pls$pred %>%
              tbl_df() %>%
              filter(ncomp %in% fit.pls$bestTune$ncomp) %>%
              select(pred, obs, SUCCESSFUL, UNSUCCESSFUL, rowIndex, Resample) %>%
              mutate(model_name = "PLS")) %>%
  bind_rows(fit.svm$pred %>%
              tbl_df() %>%
              filter(sigma %in% fit.svm$bestTune$sigma,
                     C %in% fit.svm$bestTune$C) %>%
              select(pred, obs, SUCCESSFUL, UNSUCCESSFUL, rowIndex, Resample) %>%
              mutate(model_name = "SVM")) %>%
  bind_rows(fit.rf$pred %>%
              tbl_df() %>%
              filter(mtry == fit.rf$bestTune$mtry) %>%
              select(pred, obs, SUCCESSFUL, UNSUCCESSFUL, rowIndex, Resample) %>%
              mutate(model_name = "RF")) %>%
  bind_rows(fit.tree$pred %>%
              tbl_df() %>%
              filter(cp == fit.tree$bestTune$cp) %>%
              select(pred, obs, SUCCESSFUL, UNSUCCESSFUL, rowIndex, Resample) %>%
              mutate(model_name = "Tree"))


## ----foldscomparison, echo=FALSE, fig.align="left", fig.width=8, fig.height=6, eval=FALSE------
## **Figure 6:** Model Comparision (ROC for each 10 Fold)
## 
## plot the ROC for different folds
cv_pred_results %>%
  ggplot(mapping = aes(m = SUCCESSFUL,
                       d = ifelse(obs == "SUCCESSFUL", 1, 0))) +
  geom_roc(cutoffs.at = 0.5,
           mapping = aes(color = Resample)) +
  geom_roc(cutoffs.at = 0.5) +
  coord_equal() +
  facet_wrap(~ model_name) +
  style_roc()


## ----modelcomparison, echo=FALSE, fig.align="left", fig.width=8, fig.height=6, eval=FALSE------
## **Figure 7:** Model Comparision (Average ROC)
## 
cv_pred_results %>%
  ggplot(mapping = aes(m = SUCCESSFUL,
                       d = ifelse(obs == "SUCCESSFUL", 1, 0),
                       color = model_name)) +
  geom_roc(cutoffs.at = 0.5) +
  coord_equal() +
  style_roc() +
  ggthemes::scale_color_colorblind()


## ----plsvarimp, echo=FALSE, eval=FALSE---------------------------------------------------------
plot(varImp(fit.pls), top = 25)


## ----organize testset, echo=FALSE, eval=FALSE--------------------------------------------------
test_FY <- na.omit(test_FY) ## remove rows with NAs


## ----pred_rf, echo=FALSE, eval=FALSE-----------------------------------------------------------
pred <- predict(fit.rf, test_FY)
prediction <- as.data.frame(pred)
pred_cb <- cbind(prediction, test_FY)
## 
ggplot(pred_cb, mapping = aes(x = case_status)) +
  geom_density(color = "black", size = 1) +
  geom_density(aes(x = pred), color = "tomato", size = 1)


## ----glmnet_best, echo=FALSE, warning=FALSE, message = FALSE, eval=FALSE-----------------------
## **GLMnet**
## 
glmnet_num <- fit.glmnet$results %>%
  filter(ROC == max(ROC)) %>%
  summarize(n = ROC-ROCSD) %>%
  as.numeric(n)
## 
bestglmnet <- fit.glmnet$results %>%
  filter(ROC >= glmnet_num) %>%
  filter(alpha == min(alpha)) %>%
  filter(lambda == min(lambda))
## 
bestglmnet$model <- "GLMNET"
## 
glmnetvalues <- bestglmnet %>%
  elect(model, ROC, Sens, Spec, alpha, lambda)
## 


## ----pls_best, echo=FALSE, warning=FALSE, message = FALSE, eval=FALSE--------------------------
## **PLS**
## 
pls_num <- fit.pls$results %>%
  filter(ROC == max(ROC)) %>%
  summarize(n = ROC-ROCSD) %>%
  as.numeric(n)
## 
bestpls <- fit.pls$results %>%
  filter(ROC >= pls_num) %>%
  filter(ncomp == min(ncomp))
## 
bestpls$model <- "PLS"
## 
plsvalues <- bestpls %>%
  select(model, ROC, Sens, Spec, ncomp)


## ----rpart_best, echo=FALSE, warning=FALSE, message = FALSE, eval=FALSE------------------------
## **Regression Tree**
## 
rpart_num <- fit.tree$results %>%
  filter(ROC == max(ROC)) %>%
  filter(cp == max(cp)) %>%
  summarize(n = (ROC-ROCSD)) %>%
  as.numeric(n)
## 
besttree <- fit.tree$results %>%
  filter(ROC >= rpart_num) %>%
  filter(cp == max(cp))
## 
besttree$model <- "Tree"
## 
treevalues <- besttree %>%
  select(model, ROC, Sens, Spec, cp)


## ----mars_best, echo=FALSE, warning=FALSE, message = FALSE, eval=FALSE-------------------------
## **SVM**
## 
svm_num <- fit.svm$results %>%
  filter(ROC == max(ROC)) %>%
  summarize(n = ROC-ROCSD) %>%
  as.numeric(n)
## 
bestsvm <- fit.svm$results %>%
  filter(ROC >= svm_num) %>%
  filter(C == min(C))
## 
bestsvm$model <- "SVM"
## 
svmvalues <- bestsvm %>%
  select(model, ROC, Sens, Spec, sigma, C)



## ----rf_best, echo=FALSE, warning=FALSE, message = FALSE, eval=FALSE---------------------------
## **RF**
## 
rf_num <- fit.rf$results %>%
  filter(ROC == max(ROC)) %>%
  summarize(n = ROC-ROCSD) %>%
  as.numeric(n)
## 
bestrf <- fit.rf$results %>%
  filter(ROC >= rf_num) %>%
  filter(mtry == min(mtry))
## 
bestrf$model <- "RF"
## 
rfvalues <- bestrf %>%
  select(model, ROC, Sens, Spec, mtry)


## ----glmnet2, warning=FALSE, echo=FALSE, eval=FALSE--------------------------------------------
## Linear: GLMnet
## 
set.seed(4321)
final.glmnet <- train(case_status ~ ., data = train_FY,
                      method = "glmnet",
                      metric = "ROC",
                      tuneGrid = data.frame(alpha = glmnetvalues$alpha,
                                            lambda = glmnetvalues$lambda),
                      preProcess = c("center", "scale"),
                      trControl = ctrl, na.action = na.omit)
## 

## ---- echo=FALSE-------------------------------------------------------------------------------

final.glmnet

## confusion matrix
confusionMatrix.train(final.glmnet)


## ----pls2, warning=FALSE, echo=FALSE, eval=FALSE-----------------------------------------------
## Linear: PLS
## 
set.seed(4321)
final.pls <- train(case_status ~ ., data = train_FY,
                   method = "pls",
                   metric = "ROC",
                   tuneGrid = expand.grid(ncomp=plsvalues$ncomp),
                   preProcess = c("center", "scale"),
                   trControl = ctrl, na.action = na.omit)
## 

## ---- echo=FALSE-------------------------------------------------------------------------------
final.pls

## confusion matrix
confusionMatrix.train(final.pls)


## ----tree2, echo=FALSE, eval=FALSE-------------------------------------------------------------
## Tree
## 
final.tree <- train(case_status ~., data = train_FY,
                    method = "rpart",
                    metric = "ROC",
                    tuneGrid = expand.grid(cp=treevalues$cp),
                    trControl = ctrl, na.action = na.omit)

## ---- echo=FALSE,  fig.align="left", fig.width=8, fig.height=6---------------------------------
final.tree

## plot
prp(final.tree$finalModel,
    box.palette = "Reds",
    tweak = 1.5,
    varlen = 30)

## confusion matrix
confusionMatrix.train(final.tree)



## ----svm2, echo=FALSE, message=FALSE, eval=FALSE-----------------------------------------------
## Nonlinear: SVM
## 
## doesnt like no variance in each state, etc.
## 
set.seed(4321)
final.svm <- train(case_status ~ ., data = train_FY,
                   method = "svmRadial",
                   metric = "ROC",
                   tuneGrid = expand.grid(sigma = svmvalues$sigma, 
                                          C = svmvalues$C),
                   preProcess = c("center", "scale"),
                   trControl = ctrl, na.action = na.omit)


## ---- echo=FALSE-------------------------------------------------------------------------------
final.svm


## confusion matrix
confusionMatrix.train(final.svm)


## ----randforest2, echo=FALSE, eval=FALSE-------------------------------------------------------
## Ensemble: Random Forest
## 
set.seed(4321)
final.rf <- train(case_status ~ ., data = train_FY,
                  method = "rf",
                  metric = "ROC",
                  trControl = ctrl,
                  tuneGrid = expand.grid(mtry=rfvalues$mtry),
                  importance = TRUE, na.action = na.omit)


## ---- echo=FALSE-------------------------------------------------------------------------------
final.rf

## confusion matrix
confusionMatrix.train(final.rf)


## ---- echo=FALSE, eval=FALSE-------------------------------------------------------------------
## 
set.seed(123)
idxsmall <- sample.int(n = nrow(test_FY),
                       size = 36152,
                       replace = FALSE)
## 
test_FY_small <- test_FY[idxsmall,]
## 
predictions <- test_FY_small
pred_probs <- test_FY_small


## ----------------------------------------------------------------------------------------------
test_FY_small %>%
  group_by(case_status) %>%
  summarize(n=n())



## ----predict, warning=FALSE, echo=FALSE--------------------------------------------------------

predictions$glmnet <- predict(final.glmnet, test_FY_small)
predictions$pls <- predict(final.pls, test_FY_small)
predictions$tree <- predict(final.tree, test_FY_small)
predictions$svm <- predict(final.svm, test_FY_small)
predictions$rf <- predict(final.rf, test_FY_small)

predictions <- predictions %>%
  select(case_status, glmnet:rf)


## ----combineresults2, echo=FALSE---------------------------------------------------------------
final_model_results <- resamples(list(GLMNET = final.glmnet,
                                  PLS = final.pls,
                                  SVM = final.svm,
                                  RF = final.rf,
                                  Tree = final.tree))

## ---- echo=FALSE,  fig.align="left", fig.width=8, fig.height=6---------------------------------
## visualize ROC, Sensitivity, and Specificity comparison across models
dotplot(final_model_results)


## ---- echo=FALSE, eval=FALSE-------------------------------------------------------------------
## 
## changed to make for final.models
## 
final_cv_pred_results <- final.glmnet$pred %>% tbl_df() %>%
  filter(alpha %in%  final.glmnet$bestTune$alpha,
         lambda %in% final.glmnet$bestTune$lambda) %>%
  select(pred, obs, SUCCESSFUL, UNSUCCESSFUL, rowIndex, Resample) %>%
  mutate(model_name = "GLMNET") %>%
  bind_rows(final.pls$pred %>%
              tbl_df() %>%
              filter(ncomp %in% final.pls$bestTune$ncomp) %>%
              select(pred, obs, SUCCESSFUL, UNSUCCESSFUL, rowIndex, Resample) %>%
              mutate(model_name = "PLS")) %>%
  bind_rows(final.svm$pred %>%
              tbl_df() %>%
              filter(sigma %in% final.svm$bestTune$sigma,
                     C %in% final.svm$bestTune$C) %>%
              select(pred, obs, SUCCESSFUL, UNSUCCESSFUL, rowIndex, Resample) %>%
              mutate(model_name = "SVM")) %>%
  bind_rows(final.tree$pred %>%
              tbl_df() %>%
              filter(cp == final.tree$bestTune$cp) %>%
              select(pred, obs, SUCCESSFUL, UNSUCCESSFUL, rowIndex, Resample) %>%
              mutate(model_name = "Tree"))%>%
  bind_rows(final.rf$pred %>%
              bl_df() %>%
              filter(mtry == final.rf$bestTune$mtry) %>%
              select(pred, obs, SUCCESSFUL, UNSUCCESSFUL, rowIndex, Resample) %>%
              mutate(model_name = "RF"))


## ----foldscomparison2, echo=FALSE, fig.align="left", fig.width=8, fig.height=6-----------------
## plot the ROC for different folds
final_cv_pred_results %>%
  ggplot(mapping = aes(m = SUCCESSFUL,
                       d = ifelse(obs == "SUCCESSFUL", 1, 0))) +
  geom_roc(cutoffs.at = 0.5,
           mapping = aes(color = Resample)) +
  geom_roc(cutoffs.at = 0.5) +
  coord_equal() +
  facet_wrap(~ model_name) +
  style_roc()


## different cutoff
final_cv_pred_results %>%
  ggplot(mapping = aes(m = SUCCESSFUL,
                       d = ifelse(obs == "SUCCESSFUL", 1, 0))) +
  geom_roc(cutoffs.at = 0.7,
           mapping = aes(color = Resample)) +
  geom_roc(cutoffs.at = 0.7) +
  coord_equal() +
  facet_wrap(~ model_name) +
  style_roc()



## ----modelcomparison2, echo=FALSE, fig.align="left", fig.width=8, fig.height=6-----------------
final_cv_pred_results %>%
  ggplot(mapping = aes(m = UNSUCCESSFUL, 
                       d = ifelse(obs == "UNSUCCESSFUL", 1, 0),
                       color = model_name)) +
  geom_roc(cutoffs.at = 0.5) +
  coord_equal() +
  style_roc() +
  ggthemes::scale_color_colorblind()


## ----------------------------------------------------------------------------------------------
## this look like a step change generally, thus, this can be an ideal ROC curve
ggplot(fit.tree$results, mapping = aes(x = 1 - Spec, y = Sens)) +
  geom_line()

## ----------------------------------------------------------------------------------------------
## Get the ROC curve
roc.tree <- roc(test_FY$case_status, 
            predict(fit.tree, test_FY, type = "prob")[,1], 
            levels = rev(levels(test_FY$case_status)))
roc.tree ## AUC = 0.6747
plot(roc.tree, print.thres = c(.5), type = "S",
     print.thres.pattern = "%.3f (Spec = %.2f, Sens = %.2f)",
     print.thres.cex = .8, 
     legacy.axes = TRUE,
     main = "ROC of tree")

roc.rf <- roc(test_FY$case_status, 
            predict(fit.rf, test_FY, type = "prob")[,1], 
            levels = rev(levels(test_FY$case_status)))
roc.rf ## AUC = 0.7433  highest
plot(roc.rf, print.thres = c(.5), type = "S",
     print.thres.pattern = "%.3f (Spec = %.2f, Sens = %.2f)",
     print.thres.cex = .8, 
     legacy.axes = TRUE,
     main = "ROC of rf")

roc.glmnet <- roc(test_FY$case_status, 
            predict(fit.glmnet, test_FY, type = "prob")[,1], 
            levels = rev(levels(test_FY$case_status)))
roc.glmnet ## AUC = 0.7054
plot(roc.glmnet, print.thres = c(.5), type = "S",
     print.thres.pattern = "%.3f (Spec = %.2f, Sens = %.2f)",
     print.thres.cex = .8, 
     legacy.axes = TRUE,
     main = "ROC of glmnet")

roc.pls <- roc(test_FY$case_status, 
            predict(fit.pls, test_FY, type = "prob")[,1], 
            levels = rev(levels(test_FY$case_status)))
roc.pls ## AUC = 0.706
plot(roc.pls, print.thres = c(.5), type = "S",
     print.thres.pattern = "%.3f (Spec = %.2f, Sens = %.2f)",
     print.thres.cex = .8, 
     legacy.axes = TRUE,
     main = "ROC of pls")


roc.svm <- roc(test_FY$case_status, 
            predict(fit.svm, test_FY, type = "prob")[,1], 
            levels = rev(levels(test_FY$case_status)))
roc.svm ## AUC = 0.6937
plot(roc.svm, print.thres = c(.5), type = "S",
     print.thres.pattern = "%.3f (Spec = %.2f, Sens = %.2f)",
     print.thres.cex = .8, 
     legacy.axes = TRUE,
     main = "ROC of svm")




## ----------------------------------------------------------------------------------------------
## pick the median predicted value of the Successful cases for the test set
## this will be close to the threshold when we using ROC curve where TPR&FPR overlap
pred_rf <- predict(fit.rf, test_FY, type = "prob")
summary(pred_rf$SUCCESSFUL)


## ----------------------------------------------------------------------------------------------
matplot(data.frame(roc.rf$sensitivities, roc.rf$specificities),
        x = roc.rf$thresholds,
        xlab = "threshold", ylab = "TPR, TNR")
legend('bottomright', legend=c('TPR', 'TNR'), lty=1:2, col=1:2)

## ----------------------------------------------------------------------------------------------
threshold_5 <- 0.5
predicted_values5 <- ifelse(predict(final.rf, test_FY_small, type='prob') > threshold_5, 1, 0)
actual_values <- test_FY_small$case_status
conf_matrix_5 <- table(predicted_values5, actual_values)
conf_matrix_5


## ----------------------------------------------------------------------------------------------
threshold_8 <- 0.8
predicted_values8 <- ifelse(predict(final.rf, test_FY_small, type='prob') > threshold_8, 1, 0)
actual_values <- test_FY_small$case_status
conf_matrix_8 <- table(predicted_values8, actual_values)
conf_matrix_8

