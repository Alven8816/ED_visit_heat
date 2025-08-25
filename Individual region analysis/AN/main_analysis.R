NZ_data_summer_clean_short2 <- readRDS("./data/NZ/NZ_data_test.rds")
                                       
                                       
source("./functions/create_case_crossover_subgroup.R")

result_NZ <- create_case_crossover_subgroup(NZ_data_summer_clean_short2)

plot(result_NZ$pred,xlab="tmax",ylab="Lag (Day)",zlab="RR") ### 3D plot

plot(result_NZ$pred,"overall",exp=TRUE, ylab="Log (RR)", xlab="tmax") ### Plot cumulative effect

plot(result_NZ$pred,"slices",lag = 0, ylab="Log (RR)", xlab="tmax")

plot(result_NZ$pred,"slices",var = 25.5, ylab="Log (RR)", xlab="tmax")


result_NZ$pred$matRRfit["25.5","lag0"]  # 1.109998
result_NZ$pred$matRRlow["25.5","lag0"]  # 1.096195
result_NZ$pred$matRRhigh["25.5","lag0"] # 1.123975

#saveRDS(result_NZ,file = "./Results/NZ/all_cause_model.rds")

# for male subgroup:
e1 <- Sys.time()
result_male <- create_case_crossover_subgroup(NZ_data_summer_clean_short2, subgroup = "sex", subgroup_value = "male")
e2 <- Sys.time()
e2-e1

saveRDS(result_male,file = "./results/NZ/all_cause_model_male.rds")

# for female subgroup:
result_female <- create_case_crossover_subgroup(NZ_data_summer_clean_short2, subgroup = "sex", subgroup_value = "female")

saveRDS(result_female,file = "./results/NZ/all_cause_model_female.rds")


# group by age 
# Iterate over each subgroup
for (subgroup_value in levels(NZ_data_summer_clean_short2[["age_Category"]])) {
        print(subgroup_value)
        subgroup_data <-
                NZ_data_summer_clean_short2[get("age_Category") == subgroup_value]
        
        # Perform the analysis for this subgroup
        sub_results <-
                create_case_crossover_subgroup(subgroup_data,
                                               subgroup = "age_Category",
                                               subgroup_value = subgroup_value)
        if (subgroup_value==">70") {
                saveRDS(sub_results,file = paste0("./results/NZ/all_cause_model_","above_70",".rds"))
                
        } else{
                saveRDS(sub_results,file = paste0("./results/NZ/all_cause_model_",subgroup_value,".rds"))
        }
        
        
}

## GDP
# Iterate over each subgroup
for (subgroup_value in levels(NZ_data_summer_clean_short2[["GDP_Category"]])) {
        print(subgroup_value)
        subgroup_data <-
                NZ_data_summer_clean_short2[get("GDP_Category") == subgroup_value]
        
        # Perform the analysis for this subgroup
        sub_results <-
                create_case_crossover_subgroup(subgroup_data,
                                               subgroup = "GDP_Category",
                                               subgroup_value = subgroup_value)
        saveRDS(sub_results,file = paste0("./results/NZ/all_cause_model_",subgroup_value,".rds"))
        
        
}
                                       