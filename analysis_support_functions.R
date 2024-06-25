# Declare some functions to create survival results in batch.

# Create Kaplan-Meier Graphs
km_func <- function(data, formula, title, legend_title, legend_labs = c()){
  surv_formula <-  formula
  print(surv_formula)
  km_model <- survfit2(as.formula(surv_formula), data = data)
  km_model$call$formula <- surv_formula
  
  submit_df <- data.frame(km_model$time, km_model$surv, km_model$n.event, km_model$n.censor, km_model$n.risk
                          ,km_model$std.err, km_model$cumhaz, km_model$std.chaz)
  names(submit_df) <- c("time", "survprob", "event", "censor", "at_risk", "stderr", "cumhaz", "std.chaz")
  
  tag <-c()
  rownum <- c()
  for (i in 1:length(km_model$strata)){
    tag <- c(tag, rep(names(km_model$strata)[i], km_model$strata[i]))
    rownum <- c(rownum, 1:km_model$strata[i])
  }
  submit_df$strata <- tag
  submit_df$rn <- rownum
  submit_df$title <- title
  submit_df <- submit_df %>% 
    mutate(prob =  1.0 - (event/(at_risk))
           ,derived_survprob = zoo::rollapply(prob, width = rn, FUN = prod, fill = NA, align = "right")
           ,survprobcheck = ifelse(abs(survprob-derived_survprob) > 0.005, 1, 0)
           ,hazard = event/at_risk
           ,derived_cumhaz = zoo::rollapply(hazard, width = rn, FUN = sum, fill = NA, align = "right")
           ,event_rr3 = ifelse(event > 6, rrn(event), 0)
           ,censor_rr3 = ifelse(censor > 6, rrn(censor), 0)
           ,at_risk_rr3 = ifelse(at_risk > 6, rrn(at_risk), 0)
           ,prob_rr3 = 1.0 - (event_rr3/(at_risk_rr3))
           ,haz_rr3 = event_rr3/(at_risk_rr3)
           ,derived_survprob_rr3 = zoo::rollapply(prob_rr3, width = rn, FUN = prod, fill = NA, align = "right")
           ,derived_cumhaz_rr3 = zoo::rollapply(haz_rr3, width = rn, FUN = sum, fill = NA, align = "right")
           ,survprobcheck_rr3 = ifelse(abs(survprob-derived_survprob_rr3) > 0.05, 1, 0)
    )
  
  write.csv(x = submit_df %>% select("rn","strata","title", "time", "survprob", "event", "censor", "at_risk", "stderr", "cumhaz", "std.chaz"
                                     ,"derived_survprob","survprobcheck", "event_rr3", "censor_rr3", "at_risk_rr3","prob_rr3","haz_rr3","derived_survprob_rr3"
                                     ,"derived_cumhaz_rr3","survprobcheck_rr3")
            ,file = paste0("../output/csv/output_tables", "-",title, gsub("&|/|'|\\(|\\)", '_', legend_title), ".csv" )
            # ,sheetName = substr(paste0(title, "-",gsub("&|/|'|\\(|\\)", '_', legend_title)), 1, 31)
            ,row.names = FALSE
  )
  
  ggobj <- survminer::ggsurvplot(km_model, data = data, title = title, xlim = c(0, 120), break.x.by = 10, xlab = "Time (Months)"
                                 ,legend.title = legend_title
                                 ,legend.labs = legend_labs
                                 ,conf.int = TRUE, surv.median.line = "hv"
                                 # ,risk.table = TRUE, cumcensor = TRUE #pval = TRUE
                                 ,tables.height = 0.2, tables.theme = theme_cleantable(), ggtheme = theme_bw())
  ggsave(filename = paste0("../output/png/",title, "-",gsub("&|/|'|\\(|\\)", '_', legend_title), ".png"), plot = ggobj$plot)
}

apply_mutate_formulas <- function(df, formulas){
  quosures <- lapply(formulas, quo)
  return(mutate(df, !!!quosures))
}

# Create estimated survival curves from Cox Prop. Hazards model.
cox_model_survfit_batch <-  function(model, df, newdata=NA, editvalues, filelabel, title, legend_title, legend_labs = c()) {
  
  if(is.na(newdata))
    newdata <- with(df, data.frame(
      snz_sex_gender_code = c("M", "M")
      ,eth_maori = c(0,0)
      ,eth_european = c(1,1)
      ,eth_pasifika = c(0,0)
      ,eth_asian = c(0,0)
      ,eth_melaa = c(0,0)
      ,eth_others = c(0,0)
      ,dep_index_ind = factor(c("8_to_10", "8_to_10"))
      ,rural_ind = c(1,1)
      ,region_code_combined = factor(c("auckland_reg", "auckland_reg"))
      ,addr_transience = factor(c("0_1", "0_1"))
      ,snz_sex_gender_code_x_young_mother_ind = c(0,0)
      ,studied_or_study_ind = c(0,0)
      ,apc_employed_ind = c(0,0)
      ,time_spent_in_prevstage = mean(time_spent_in_prevstage))
    )
      
  newdata <- apply_mutate_formulas(newdata, formulas = list( eth_pasifika =  c(1, 1), eth_european =  c(0,0) ))
  print(newdata)
  
  km_model <- survfit(model, data = df, newdata = newdata)
  
  # Generate labels
  legend_labs <- gsub("&|/|=|'|\\(|\\)", '_', legend_labs)
  colnames(km_model$surv) <- paste0("surv", legend_labs)
  colnames(km_model$std.err) <- paste0("std.err", legend_labs)
  colnames(km_model$cumhaz) <- paste0("cumhaz", legend_labs)
  colnames(km_model$std.chaz) <- paste0("std.chaz", legend_labs)
  
  submit_df <- data.frame(km_model$time, km_model$surv,km_model$std.err, km_model$cumhaz, km_model$std.chaz)
  collist<- names(submit_df)
  
  tag <-c()
  rownum <- 1:nrow(submit_df)

  submit_df$strata <- tag
  submit_df$rn <- rownum
  submit_df$title <- title
  submit_df$var <- legend_title
  print(head(submit_df))
  
  write.csv(x = submit_df %>% select("rn","title", "var",all_of(collist))
            ,file = paste0("../output/csv/output_tables", title,"-", gsub("&|/|'|\\(|\\)", '_', legend_title), ".csv" )
            # ,sheetName = substr(paste0(title, "-",gsub("&|/|'|\\(|\\)", '_', legend_title)), 1, 31)
            ,row.names = FALSE
  )
  
  ggobj <- survminer::ggsurvplot(survfit(model, data = df, newdata = newdata)
                                 ,xlim = c(0, 120), break.x.by = 10, xlab = "Time (Months)"
                                 ,legend.title = legend_title
                                 ,legend.labs = legend_labs
                                 ,ggtheme = theme_bw()
                                 ,conf.int = TRUE
                                 ,surv.median.line = "hv")
  ggsave(filename = paste0(filelabel, ".png"), plot = ggobj$plot)
}

