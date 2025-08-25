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

source("./functions/create_case_crossover_subgroup.R")

# balanced control model calculation
#length(CAN_data_LocalDLarge_names)
CAN_data_LocalDLarge <-
        dir(path = "./data/CAN", pattern = ".rds$")

CAN_data_LocalDLarge_names <-
        sub("CAN_data_summer_", "", CAN_data_LocalDLarge) %>%
        sub(".rds", "", x = .)

sink(file = "./model_record.txt")
for (i in 1:length(CAN_data_LocalDLarge_names)) {
        print(i)
        tryCatch({
                name = CAN_data_LocalDLarge_names[i]
                print(name)
                CAN_data_summer <-
                        readRDS(
                                file = paste0(
                                        "./data/CAN/",
                                        name,".rds"
                                )
                        )
                # create age category
                CAN_data_summer[, age_Category := cut(age_con, 
                                                      breaks = c(-Inf, 19, 59, 70, Inf), 
                                                      labels = c("0-19", "20-59", "60-70", ">70"), 
                                                      include.lowest = FALSE,right = TRUE)]
                
                #certer <- ceiling(min(CAN_data_summer$tmax_lag0, na.rm = TRUE))
                # all_cause
                result_CAN <-
                        create_case_crossover_subgroup(CAN_data_summer)
                # Check the last warning
                last_warning <- tail(warnings(), n = 1)
                file_name <-
                        paste0("./Results/CAN/all_cause_model_balanced/all_cause_model_",
                               name,
                               ".rds")
                if (is.null(last_warning)== FALSE) {
                        if (grepl("Loglik converged before variable",
                                  last_warning)) {
                                message("Warning detected, switching to Model 2")
                                result_CAN <-
                                        create_case_crossover_subgroup(BRA_data_summer, holiday = FALSE)
                                saveRDS(result_CAN, file = file_name)
                        } else{
                                saveRDS(result_CAN, file = file_name) 
                        }
                } else {
                        saveRDS(result_CAN, file = file_name)
                }
        }, 
        warning = function(w) {
                # This block is executed if any warning is thrown
                if (grepl("Loglik converged before variable", w$message)) {
                        message("Warning detected in warning handler, switching to Model 2")
                        result_CAN <- create_case_crossover_subgroup(CAN_data_summer,holiday = FALSE)
                        
                }
                
        },
        error = function(e){
                cat("An error occurred in task:", name, "\nError message:", e$message, "\nContinuing with next task.\n")
        })
        next
        
}
# close the sink
sink(type = "message")

## Subgroup analysis

## male and female

model_subgroup <- function(subgroup = "sex", subgroup_value = "male"){
        CAN_data_LocalDLarge <- dir("./data/CAN",pattern = ".rds$")
        
        CAN_data_LocalDLarge_names <-
                sub("CAN_data_summer_", "", CAN_data_LocalDLarge) %>%
                sub(".rds", "", x = .)
        
        for (i in 1:length(CAN_data_LocalDLarge_names)) {
                print(i)
                tryCatch({
                        name = CAN_data_LocalDLarge_names[i]
                        print(name)
                        CAN_data_summer <-
                                readRDS(
                                        file = paste0(
                                                "./data/CAN/",
                                                name,".rds"
                                        )
                                )
                        # create age category
                        CAN_data_summer[, age_Category := cut(age_con, 
                                                              breaks = c(-Inf, 19, 59, 70, Inf), 
                                                              labels = c("0-19", "20-59", "60-70", "above70"), 
                                                              include.lowest = FALSE,right = TRUE)]
                        
                        #certer <- ceiling(min(CAN_data_summer$tmax_lag0, na.rm = TRUE))
                        # male
                        result_CAN <-
                                create_case_crossover_subgroup(CAN_data_summer, subgroup = subgroup, subgroup_value = subgroup_value)
                        # Check the last warning
                        last_warning <- tail(warnings(), n = 1)
                        if (grepl("Loglik converged before variable", last_warning)) {
                                message("Warning detected, switching to Model 2")
                                result_CAN <- create_case_crossover_subgroup(CAN_data_summer,subgroup = subgroup, subgroup_value = subgroup_value, holiday = FALSE)
                        }
                        
                        if (dir.exists(paste0("./Results/CAN/all_cause_",subgroup_value,"_balanced"))) {
                                # Directory exists
                                print(paste("Directory exists"))
                        } else {
                                # Directory does not exist, so create it
                                dir.create(paste0("./Results/CAN/all_cause_",subgroup_value,"_balanced"), recursive = TRUE)
                                print(paste("Directory created"))
                        }
                        file_name <- paste0("./Results/CAN/all_cause_",subgroup_value,"_balanced/all_cause_model_",subgroup_value,"_",name,".rds")  
                        saveRDS(result_CAN, file = file_name)
                }, 
                warning = function(w) {
                        # This block is executed if any warning is thrown
                        if (grepl("Loglik converged before variable", w$message)) {
                                message("Warning detected in warning handler, switching to Model 2")
                                result_CAN <- create_case_crossover_subgroup(CAN_data_summer,subgroup = subgroup, subgroup_value = subgroup_value, holiday = FALSE)
                        } else {
                                cat(name,"\nWarning message:",w$message)  
                        }
                },
                error = function(e){
                        cat("An error occurred in task:", name, "\nError message:", e$message, "\nContinuing with next task.\n")
                })
                next
        }
}

model_subgroup(subgroup_value = "female")
model_subgroup(subgroup_value = "male")

for (j in c("0-19", "20-59", "60-70", "above70")) {
        print(i)
        model_subgroup(subgroup = "age_Category",subgroup_value = j)}

### GDP
for (j in c("Low", "median", "High", "very high")) {
        print(i)
        model_subgroup(subgroup = "GDP_Category",subgroup_value = j)}

