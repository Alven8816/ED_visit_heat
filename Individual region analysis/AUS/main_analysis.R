AUS_data_summer_clean_short2 <- readRDS("./data/AUS/AUS_data_sample.rds") # Sample data with 35,000 records



create_case_crossover_model <- function(data, max_lags = 6, lag_window = c(0, 5), df_bs = 3, knots_ns = c(2, 3), centering = 12.5) {
        # Ensure data is a data.table
        if (!is.data.table(data)) {
                data <- as.data.table(data)
        }
        
        # Identify columns with tmax_lag
        columns_with_tmax_lag <- grep("tmax_lag", names(data), value = TRUE)[1:max_lags]
        
        # Subset the data.table by these column names
        tmax <- data[, ..columns_with_tmax_lag]
        
        # Create a case-crossover design model
        cb.tmax <- crossbasis(x = tmax, lag = lag_window, argvar = list("bs", df = df_bs), arglag = list(fun = "ns", knots = knots_ns))
        
        # Fit the model
        model <- coxph(Surv(time, case) ~ cb.tmax + RH + rainfall + holiday + strata(case_rn), 
                       method = "breslow", data = data)
        
        # Create predictions
        pred.tmax <- crosspred(cb.tmax, model = model, cen = centering)
        
        # Optionally plot the results
        plot(pred.tmax, xlab = "tmax", ylab = "Lag (Day)", zlab = "RR") ### 3D plot
        
        return(list(model = model, pred = pred.tmax))
}


## main result
result <- create_case_crossover_model(AUS_data_summer_clean_short2)

plot(result$pred,xlab="tmax",ylab="Lag (Day)",zlab="RR") ### 3D plot

plot(result$pred,"overall",exp=TRUE, ylab="Log (RR)", xlab="tmax") ### Plot cumulative effect

plot(result$pred,"slices",lag = 0, ylab="Log (RR)", xlab="tmax")

plot(result$pred,"slices",var = 35.4, ylab="Log (RR)", xlab="tmax")

#saveRDS(result,file = "./Results/AUS/all_cause_model.rds")

source("./functions/create_case_crossover_subgroup.R")

# for male subgroup:
e1 <- Sys.time()
result_male <- create_case_crossover_subgroup(AUS_data_summer_clean_short2, subgroup = "sex", subgroup_value = "male")
e2 <- Sys.time()
e2-e1

# for female subgroup:
result_female <- create_case_crossover_subgroup(AUS_data_summer_clean_short2, subgroup = "sex", subgroup_value = "female")

saveRDS(result_female,file = "./Results/AUS/all_cause_model_female.rds")


# group by age 
# Iterate over each subgroup
for (subgroup_value in levels(AUS_data_summer_clean_short2[["age_Category"]])) {
        print(subgroup_value)
        subgroup_data <-
                AUS_data_summer_clean_short2[get("age_Category") == subgroup_value]
        
        # Perform the analysis for this subgroup
        sub_results <-
                create_case_crossover_subgroup(subgroup_data,
                                               subgroup = "age_Category",
                                               subgroup_value = subgroup_value)
        
        if (subgroup_value==">70") {
                saveRDS(sub_results,file = paste0("./results/AUS/all_cause_model_","above_70",".rds"))
                
        } else{
                saveRDS(sub_results,file = paste0("./results/AUS/all_cause_model_",subgroup_value,".rds"))
        }
        
        
}

## FOR GDP CATEGORY
for (subgroup_value in levels(AUS_data_summer_clean_short2[["GDP_Category"]])[2:4]) {
        print(subgroup_value)
        subgroup_data <-
                AUS_data_summer_clean_short2[get("GDP_Category") == subgroup_value]
        
        # Perform the analysis for this subgroup
        sub_results <-
                create_case_crossover_subgroup(subgroup_data,
                                               subgroup = "GDP_Category",
                                               subgroup_value = subgroup_value)
        
        
        saveRDS(sub_results,file = paste0("./results/AUS/all_cause_model_",subgroup_value,".rds"))
        
}
