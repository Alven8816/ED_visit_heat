
meta_regression_country_data <- function(dir_path = ".\\Results\\AUS\\"){
        
        dir_CAN_all_cause_model <- dir(path = dir_path,pattern = ".rds$", full.names = FALSE)
        
        PMparlist <- list()
        if (grepl("TWN", dir_CAN_all_cause_model[1])) {
                name_id <-
                        str_which(dir_CAN_all_cause_model, pattern = "((?<=all_cause_model_).*(?=\\.rds))|((?<=all_all_allcause_).*(?=\\.rds))")
                
                names <- str_extract(dir_CAN_all_cause_model[name_id], pattern = "((?<=all_cause_model_).*(?=\\.rds))|((?<=all_all_allcause_).*(?=\\.rds))")
                
                for (i in 1:length(name_id)) {
                        print(names[i])
                        model_pred <-
                                readRDS(file = paste0(dir_path, "/", dir_CAN_all_cause_model[name_id][i]))
                        ncoef <- length(coef(model_pred$pred))
                        par <- c(coef(model_pred$pred), vechMat(vcov(model_pred$pred)))
                        names(par) <- c(paste0("coef", seq(ncoef)),
                                        paste0("vcov", seq(ncoef * (ncoef + 1) / 2)))
                        
                        PMparlist[[i]] <-
                                data.frame(location_predictors_2010_ED[location_predictors_2010_ED$LocIDLarge ==
                                                                               names[i], c("LocIDLarge", "kgclzone1", "ISO3", "GDP_Category")],
                                           t(par), row.names = names[i])
                        #PMpar <- data.frame(t(par), row.names = i)
                }
                
                
        } else {
                for (i in 1:length(dir_CAN_all_cause_model)) {
                        names <-
                                str_extract(dir_CAN_all_cause_model, pattern = "((?<=all_cause_model_).*(?=\\.rds))|((?<=all_all_allcause_).*(?=\\.rds))")
                        print(names[i])
                        model_pred <-
                                readRDS(file = paste0(dir_path, "/", dir_CAN_all_cause_model[i]))
                        ncoef <- length(coef(model_pred$pred))
                        par <- c(coef(model_pred$pred), vechMat(vcov(model_pred$pred)))
                        names(par) <- c(paste0("coef", seq(ncoef)),
                                        paste0("vcov", seq(ncoef * (ncoef + 1) / 2)))
                        
                        PMparlist[[i]] <-
                                data.frame(location_predictors_2010_ED[location_predictors_2010_ED$LocIDLarge ==
                                                                               names[i], c("LocIDLarge", "kgclzone1", "ISO3", "GDP_Category")],
                                           t(par), row.names = names[i])
                        #PMpar <- data.frame(t(par), row.names = i)
                }
                
        }
        
        
        CAN_LocalDLarge_merge <- bind_rows(PMparlist)
        
        CAN_LocalDLarge_merge <- data.frame(CAN_LocalDLarge_merge)
        
        return(CAN_LocalDLarge_merge)
        
}


# TWN
TWN_LocalDLarge_merge <- meta_regression_country_data(dir_path = ".\\Results\\TWN\\balanced")

saveRDS(TWN_LocalDLarge_merge, file = "./Results/TWN/TWN_LocalDLarge_merge_all_cause.rds")

# AUS
AUS_LocalDLarge_merge <- meta_regression_country_data(dir_path = ".\\Results\\AUS\\all_cause_model_balanced")

saveRDS(AUS_LocalDLarge_merge, file = "./Results/AUS/AUS_LocalDLarge_merge_all_cause.rds")

# BRA
BRA_LocalDLarge_merge <- meta_regression_country_data(dir_path = ".\\Results\\BRA\\all_cause_model_balanced")

saveRDS(BRA_LocalDLarge_merge, file = "./Results/BRA/BRA_LocalDLarge_merge_all_cause.rds")

# NZL
NZL_LocalDLarge_merge <- meta_regression_country_data(dir_path = ".\\Results\\NZ\\all_cause_model_balanced")

saveRDS(NZL_LocalDLarge_merge, file = "./Results/NZ/NZL_LocalDLarge_merge_all_cause.rds")