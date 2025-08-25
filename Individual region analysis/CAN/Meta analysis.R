library(tidyverse)
library(lubridate)
library(dplyr)
library(purrr)
library(stringr)
library(data.table)
library(skimr)
library(survival)
library(dlnm)
library(gnm)
library(splines)
library(mixmeta)
library(sf)


# all_cause for CAN
dir_path <- "./Results/CAN/all_cause_model_balanced"
#dir_data_path <- "./Canada_data/canada_summer_clean_LocIDLarge/"
dir_CAN_all_cause_model <- dir(path = dir_path,pattern = ".rds$", full.names = FALSE)

PMparlist <- list()
for (i in 1:length(dir_CAN_all_cause_model)) {
        names <-
                str_extract(dir_CAN_all_cause_model[i], pattern = "((?<=all_cause_model_).*(?=\\.rds))|((?<=all_allcause_).*(?=\\.rds))")
        model_pred <-
                readRDS(file = paste0(dir_path, "/", dir_CAN_all_cause_model[i]))
        ncoef <- length(coef(model_pred$pred))
        par <- c(coef(model_pred$pred), vechMat(vcov(model_pred$pred)))
        names(par) <- c(paste0("coef", seq(ncoef)),
                        paste0("vcov", seq(ncoef * (ncoef + 1) / 2)))
        
        PMparlist[[i]] <-
                data.frame(location_predictors_2010_ED[location_predictors_2010_ED$LocIDLarge ==
                                                               names, c("LocIDLarge", "kgclzone1", "ISO3", "GDP_Category")],
                           t(par), row.names = names)
        #PMpar <- data.frame(t(par), row.names = i)
}

CAN_LocalDLarge_merge <- bind_rows(PMparlist)

CAN_LocalDLarge_merge <- data.frame(CAN_LocalDLarge_merge)

saveRDS(CAN_LocalDLarge_merge, file = "./Results/CAN/meta_CAN/CAN_LocalDLarge_merge.rds")

#cityind <- CAN_LocalDLarge_merge[,1:4]
coef <- as.matrix(CAN_LocalDLarge_merge[,grep("coef", names(CAN_LocalDLarge_merge))])
vcov <- as.matrix(CAN_LocalDLarge_merge[,grep("vcov", names(CAN_LocalDLarge_merge))])

model0 <- mixmeta(coef~1, vcov, method="reml")

# SUMMARY AND HETEROGENEITY TEST
summary(model0)

# average tmax distribution in CAN
Ave_tmax <- seq(from = 4, to = 30, by = 0.5)

# DEFINE SPLINE TRANSFORMATION ORIGINALLY USED IN FIRST-STAGE MODELS
knots <- quantile(Ave_tmax,probs = c(0.25,0.75),na.rm = TRUE)

bvar <- crossbasis(x = Ave_tmax, lag = c(0,5), argvar = list("bs", degree = 2, knots = knots), arglag = list(fun = "ns", knots =c(2,3)))

### POOLING COMPLEX MULTI-PARAMETER ASSOCIATIONS
# PREDICT THE ASSOCIATION
cp <- crosspred(bvar, coef=coef(model0), vcov=vcov(model0),
                model.link="log",
                at=Ave_tmax,cen = Ave_tmax[which.min(abs(bvar %*% coef(model0)))]) # cen= 6.5

saveRDS(object = model0,file = "./Results/CAN/meta_CAN/all_cause_meta_model_CAN.rds")

saveRDS(object = cp,file = "./Results/CAN/meta_CAN/all_cause_meta_pred_CAN.rds")

sink()
