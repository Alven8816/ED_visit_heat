


result <- create_case_crossover_subgroup(BRA_data_summer_clean_short2,centering = 31.5,lag_window = c(0,2),max_lags = 3,knots_lag = 1)

plot(result$pred,xlab="tmax",ylab="Lag (Day)",zlab="RR") ### 3D plot

plot(result$pred,"overall",exp=TRUE, ylab="Log (RR)", xlab="tmax") ### Plot cumulative effect

plot(result$pred,"slices",lag = 1, ylab="Log (RR)", xlab="tmax")

plot(result$pred,"slices",var = 34.5, ylab="Log (RR)", xlab="tmax")

saveRDS(result,file = "./Results/BRA/all_cause_model.rds")

# for male subgroup:
e1 <- Sys.time()
result_male <- create_case_crossover_subgroup(BRA_data_summer_clean_short2, subgroup = "sex", subgroup_value = "male")
e2 <- Sys.time()
e2-e1

saveRDS(result_male,file = "./Results/BRA/all_cause_model_male.rds")
e2 <- Sys.time()
e2-e1

# for female subgroup:
result_female <- create_case_crossover_subgroup(BRA_data_summer_clean_short2, subgroup = "sex", subgroup_value = "female")

saveRDS(result_female,file = "./Results/BRA/all_cause_model_female.rds")


# group by age 
# Iterate over each subgroup
for (subgroup_value in levels(BRA_data_summer_clean_short2[["age_Category"]])) {
        print(subgroup_value)
        subgroup_data <-
                BRA_data_summer_clean_short2[get("age_Category") == subgroup_value]
        
        # Perform the analysis for this subgroup
        sub_results <-
                create_case_crossover_subgroup(subgroup_data,
                                               subgroup = "age_Category",
                                               subgroup_value = subgroup_value)
        
        if (subgroup_value==">70") {
                saveRDS(sub_results,file = paste0("./Results/BRA/all_cause_model_","above_70",".rds"))
                
        } else{
                saveRDS(sub_results,file = paste0("./Results/BRA/all_cause_model_",subgroup_value,".rds"))
        }
        
        
}

## GDP
# Iterate over each subgroup
for (subgroup_value in levels(BRA_data_summer_clean_short2[["GDP_Category"]])) {
        print(subgroup_value)
        subgroup_data <-
                BRA_data_summer_clean_short2[get("GDP_Category") == subgroup_value]
        
        # Perform the analysis for this subgroup
        sub_results <-
                create_case_crossover_subgroup(subgroup_data,
                                               subgroup = "GDP_Category",
                                               subgroup_value = subgroup_value)
        
        saveRDS(sub_results,file = paste0("./Results/BRA/all_cause_model_",subgroup_value,".rds"))
        
}