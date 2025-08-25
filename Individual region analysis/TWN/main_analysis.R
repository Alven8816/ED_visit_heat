TWN_data_summer_clean <- readRDS("./data/TWN/TWN_data_summer_clean.rds")

TWN_data_summer_clean$pymdow <- as.factor(TWN_data_summer_clean$pymdow)
## Crossbasis for temperature and humidity

basistemp<-crossbasis(TWN_data_summer_clean$tmax, lag=6, argvar = list("bs", degree = 2, knots=quantile(TWN_data_summer_clean$tmax,c(25,75)/100, na.rm=T)), arglag = list(fun = "ns", knots = c(2, 3)), group=TWN_data_summer_clean$LocIDLarge)

basisrh<-crossbasis(TWN_data_summer_clean$RH,lag=6, argvar = list("bs", df = 3), arglag = list(fun = "ns", knots = c(2, 3)), group=TWN_data_summer_clean$LocIDLarge)

pred_specific <- NULL
for (i in 16:17) {
        name = names(TWN_data_summer_clean[,i])
        print(name)
        for (j in c("TWN_4")) {
                fit <- gnm(TWN_data_summer_clean[[i]]~basistemp+basisrh+rainfall + holiday,offset = log(Sum_pop),data=TWN_data_summer_clean,family=quasipoisson,eliminate = pymdow,subset = LocIDLarge == j)
                pred<-crosspred(basistemp,fit,cen=20)
                TWN_model_pred <- list(
                        model = fit,
                        pred = pred
                )
                saveRDS(TWN_model_pred,paste0("./Results/TWN/TWN_model_pred_",name,"_",j,".rds")) 
        }
        
        #pred_specific[[i]][[j]] <- data.frame(group = names(TWN_data_summer_clean[,i]),temp = pred$predvar,IR = pred$allRRfit,h = pred$allRRhigh,l = pred$allRRlow)
}

#pred_group_eff <- bind_rows(pred_specific)
#pred_group_eff$group <- factor(pred_group_eff$group)

#saveRDS(pred_group_eff, file = "./Results/TWN/TWN_model_pred_eff.rds")


## only for icd 75 for all dataset

fit <- gnm(TWN_data_summer_clean[[15]]~basistemp+basisrh+rainfall + holiday,offset = log(Sum_pop),data=TWN_data_summer_clean,family=quasipoisson,eliminate = pymdow)
pred<-crosspred(basistemp,fit,cen=20)
TWN_model_pred <- list(
        model = fit,
        pred = pred
)
saveRDS(TWN_model_pred,paste0("./Results/TWN/TWN_model_pred_",name,".rds")) 