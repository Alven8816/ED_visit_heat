create_case_crossover_subgroup <- function(data, subgroup = NULL, subgroup_value = NULL, max_lags = 6, lag_window = c(0, 5), var_degree=2,knots_perc = c(25,75)/100, knots_lag = c(2, 3), centering = mean(tmax$tmax_lag0,na.rm=TRUE), holiday = TRUE) {
        # Ensure data is a data.table
        if (!is.data.table(data)) {
                data <- as.data.table(data)
                data[, time := 1]
        }
        
        # Filter data for the specified subgroup if provided
        if (!is.null(subgroup) && !is.null(subgroup_value)) {
                data <- data[get(subgroup) == subgroup_value]
                data[, time := 1]
        }
        
        # Identify columns with tmax_lag
        columns_with_tmax_lag <- grep("tmax_lag", names(data), value = TRUE)[1:max_lags]
        
        # Subset the data.table by these column names
        tmax <- data[, ..columns_with_tmax_lag]
        
        # Create a case-crossover design model
        cb.tmax <- crossbasis(x = tmax, lag = lag_window, argvar = list(fun="bs", degree=var_degree,
                                                                        knots=quantile(tmax$tmax_lag0,
                                                                                       knots_perc, na.rm=T)),
                              arglag = list(fun = "ns", knots = knots_lag))
        # Fit the model
        if (holiday ==TRUE) {
                model <- coxph(Surv(time, case) ~ cb.tmax + RH + rainfall + holiday + strata(case_rn), 
                               method = "breslow", data = data)
                
        } else {
                model <- coxph(Surv(time, case) ~ cb.tmax + RH + rainfall + strata(case_rn), 
                               method = "breslow", data = data)
        }
        
        # Create predictions
        pred.tmax <- crosspred(cb.tmax, model = model,  cen=centering)
        
        # Optionally plot the results
        #plot(pred.tmax, xlab = "tmax", ylab = "Lag (Day)", zlab = "RR") ### 3D plot
        
        
        return(list(model = model, pred = pred.tmax))
}

