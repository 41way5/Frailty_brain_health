#External validation of frailty, cardiometabolic frailty, and mental health in UKB---
library(dplyr)
library(ggplot2)
library(cowplot)

select <- dplyr::select
load("E:\\Tutor\\UK Biobank\\/Project/PAC and cancer/Data/UKB/ukb_aging_ca_Dec.RData")
ukb <- data.table::fread("E:/Tutor/Multi-region study/frailty and mental-wellbeing/Data/frailty.csv") %>% map_eid()


ukb_frailty <- ukb %>%
  left_join(ukb_olink %>% select(eid, bmi, sex), by = "eid") %>%
  mutate(
    ## 1) weight loss
    wt_loss = case_when(
      `2306-0.0` == "Yes - lost weight"     ~ 1L,
      `2306-0.0` == "Prefer not to answer"  ~ NA_integer_,
      is.na(`2306-0.0`)                     ~ NA_integer_,
      TRUE                                  ~ 0L
    ),
    
    ## 2) exhaustion
    exhaustion = case_when(
      `2080-0.0` %in% c("More than half the days", "Nearly every day") ~ 1L,
      `2080-0.0` %in% c("Do not know", "Prefer not to answer")         ~ NA_integer_,
      is.na(`2080-0.0`)                                                ~ NA_integer_,
      TRUE                                                             ~ 0L
    ),
    
    ## 3) slowness
    slowness = case_when(
      `924-0.0` == "Slow pace"            ~ 1L,
      `924-0.0` == "Prefer not to answer" ~ NA_integer_,
      is.na(`924-0.0`)                    ~ NA_integer_,
      TRUE                                ~ 0L
    ),
    
    ## grip max
    grip_max = {
      g <- pmax(as.numeric(`46-0.0`), as.numeric(`47-0.0`), na.rm = TRUE)
      ifelse(is.infinite(g), NA_real_, g)
    }
  ) %>%
  mutate(
    bmi_q = ifelse(is.na(bmi) | is.na(sex), NA_integer_, ntile(bmi, 5))
  ) %>%
  group_by(sex, bmi_q) %>%
  mutate(
    weak_cut = quantile(grip_max, probs = 0.20, na.rm = TRUE, type = 2),
    weakness = case_when(
      is.na(grip_max) ~ NA_integer_,
      grip_max <= weak_cut ~ 1L,
      TRUE ~ 0L
    )
  ) %>%
  ungroup() %>%
  mutate(
    low_pa = case_when(
      `6164-0.0` == "None of the above" ~ 1L,
      `6164-0.0` == "Prefer not to answer" ~ NA_integer_,
      `6164-0.0` == "Light DIY (eg: pruning, watering the lawn)" &
        `1011-0.0` %in% c("Once a week", "Once in the last 4 weeks") ~ 1L,
      TRUE ~ 0L
    ),
    
    frailty_score = wt_loss + exhaustion + slowness + weakness + low_pa,
    frailty= case_when(
      is.na(frailty_score) ~ NA_character_,
      frailty_score >= 3 ~ "3",
      frailty_score %in% 1:2 ~ "2",
      frailty_score == 0 ~ "1"
    )
  ) %>%
  select(eid, frailty_score, frailty,
         wt_loss, exhaustion, slowness, weakness, low_pa)
  
  Hmisc::describe(ukb_frailty$frailty)
  # n  missing distinct 
  # 482569    19367        3 
  # 
  # Value           0      1      3
  # Frequency  257765 205918  18886
  # Proportion  0.534  0.427  0.039

  #Define comorbidity
  
  
  ukb_comorbid <- read.csv(file = "E:\\Tutor\\UK Biobank\\Data/extra_outcome_NMR.csv",check.names = FALSE) %>%  
    select(eid,`42016-0.0`,`42017-0.0`,`42000-0.0`,`42001-0.0`,`42006-0.0`, `42007-0.0`,
                        `132032-0.0`,`132033-0.0`,`131670-0.0`,`131671-0.0`,`130708-0.0`,`130709-0.0`,
                        `131286-0.0`,`131287-0.0`
    ) %>% dplyr::rename(
      copd_date=`42016-0.0`,copd_s=`42017-0.0`,
      mi_date=`42000-0.0`,mi_s=`42001-0.0`,
      stroke_date=`42006-0.0`,stroke_s=`42007-0.0`,
      ckd_date=`132032-0.0`,ckd_s=`132033-0.0`,
      ld_date=`131670-0.0`,ld_s=`131671-0.0`,
      dm_date=`130708-0.0`,dm_s=`130709-0.0`,
      hyp_date=`131286-0.0`,hyp_s=`131287-0.0`
    ) %>% left_join(ukb_olink %>% select(eid,recruit_date)) %>% 
    mutate(
      mi_base = as.integer(mi_date!="" & mi_date <= recruit_date),
      stroke_base = as.integer(stroke_date!="" & stroke_date <= recruit_date),
      hyp_base = as.integer(hyp_date!="" & hyp_date <= recruit_date),
      t2d_base = as.integer(dm_date!="" & dm_date <= recruit_date)
    ) %>% 
    mutate(
      con_sum = rowSums(across(c(mi_base, stroke_base, hyp_base, t2d_base)), na.rm = TRUE),
      comorbid = case_when(
        con_sum >= 2 ~ 3L,
        con_sum == 0 ~ 1L,
        TRUE         ~ 2L
      )
    ) %>% select(-con_sum)


  
  ukb_outcome <- ukbb_full %>% dplyr::select(eid,`20023-0.0`,`4282-0.0`,`20016-0.0`,`399-0.1`,`20240-0.0`,`20191-0.0`,`20132-0.0`,#cognition
                                             `42018-0.0`,`42019-0.0`,`42020-0.0`,`42021-0.0`,`42022-0.0`,`42023-0.0`,`42032-0.0`,`42033-0.0`,`131042-0.0`,`131043-0.0`,
                                             `130874-0.0`,`130875-0.0`,`130894-0.0`,`130895-0.0`,`130906-0.0`,`130907-0.0`,`131060-0.0`,`131061-0.0`,
                                             `130868-0.0`,`130854-0.0`
                                             ) %>% 
    dplyr::rename(acd_date=`42018-0.0`,acd_s=`42019-0.0`,ad_date=`42020-0.0`,ad_s=`42021-0.0`,vd_date=`42022-0.0`,vd_s=`42023-0.0`,
                  pd_date=`42032-0.0`,pd_s=`42033-0.0`,ms_date=`131042-0.0`,ms_s=`131043-0.0`,scz_date=`130874-0.0`,scz_s=`130875-0.0`,
                  dep_date=`130894-0.0`,dep_s=`130895-0.0`,anx_date=`130906-0.0`,anx_s=`130907-0.0`,sd_date=`131060-0.0`,sd_s=`131061-0.0`
                  
    ) %>% 
    mutate(rt=as.numeric(scale(`20023-0.0`)),
           nm=as.numeric(scale(`4282-0.0`)),nm_fu=as.numeric(scale(`20240-0.0`)),
           fi=as.numeric(scale(`20016-0.0`)),fi_fu=as.numeric(scale(`20191-0.0`)),
           vm=-as.numeric(scale(`399-0.1`)),vm_fu=-as.numeric(scale(`20132-0.0`)),
           sud_date=pmax(`130868-0.0`,`130854-0.0`)
    ) %>% 
    left_join(ukb_olink %>% select(eid,death,date_of_death,date_of_birth,recruit_date,center,gender,36:43,life_cat,86:113))

  
  ukb_frailty_in <- ukb_frailty %>% left_join(ukb_comorbid %>% select(eid,comorbid)) %>% filter(!is.na(frailty)) %>% 
    mutate(frail_comorbid2=paste(frailty, comorbid, sep = "_")) %>% select(eid,frailty,comorbid,frail_comorbid2) %>% 
    left_join(ukb_outcome) %>% 
    mutate(date_of_death = ifelse(is.na(date_of_death), "", as.character(date_of_death)))
    
  ukb_frailty_summary <- ukb_frailty_in %>% 
    select(frailty, comorbid) %>% 
    group_by(frailty, comorbid) %>%
    summarise(count = n()) %>%
    group_by(frailty) %>%
    mutate(
      percent = count / sum(count) * 100,
      frailty = factor(frailty, levels = c(1, 2, 3), labels = c("Non-frail", "Pre-frail", "Frail")),
      comorbid = factor(comorbid, levels = c(1, 2, 3), labels = c("0", "1", "≥2"))
    )
  
  
  run_lm_frail <- function(outcome, data, covars = c("age","sex","ethnic","edu","tdi","center","pa","smok")) {
    
    fml <- as.formula(
      paste(outcome,
            "~ frail_comorbid2 +",
            paste(covars, collapse = " + "))
    )
    
    fit <- lm(fml, data = data)
    
    summary(fit)$coefficients %>%
      as.data.frame() %>%
      tibble::rownames_to_column("term") %>%
      dplyr::filter(grepl("^frail_comorbid2", term)) %>%
      dplyr::mutate(
        outcome = outcome,
        level = sub("frail_comorbid2", "", term)
      ) %>%
      dplyr::select(
        outcome,
        level,
        beta = Estimate,
        se   = `Std. Error`,
        p    = `Pr(>|t|)`
      )
  }
  
  
  res_cog <- purrr::map_dfr(
    c("rt", "nm", "fi", "vm"),
    run_lm_frail,
    data = ukb_frailty_in
  )
  ukb_frailty_in %>% filter(acd_date==""|acd_date>recruit_date) %>% nrow
  
  table(is.na(ukb_frailty_in$acd_date))
  
  # plotting
  {
    
    plot_df <- bind_rows(
      res_cog %>%
        mutate(
          Outcome = as.character(outcome),
          Exposure_level = as.character(level),
          Beta = beta,
          SE = se,
          CI_Lower = beta - 1.96 * se,
          CI_Upper = beta + 1.96 * se
        ) %>%
        select(Outcome, Exposure_level, Beta, SE, CI_Lower, CI_Upper),
      
      res_cog %>%
        distinct(outcome) %>%
        mutate(
          Outcome = as.character(outcome),
          Exposure_level = "1_1",
          Beta = 0,
          SE = NA_real_,
          CI_Lower = 0,
          CI_Upper = 0
        ) %>%
        select(Outcome, Exposure_level, Beta, SE, CI_Lower, CI_Upper)
    )
    
    plot_df <- plot_df %>%
      mutate(
        Exposure_level = factor(
          Exposure_level,
          levels = c("1_1","1_2","1_3","2_1","2_2","2_3","3_1","3_2","3_3")
        ),
        Exposure_label = case_when(
          Exposure_level == "1_1" ~ "Non-frail 0 (Ref)",
          Exposure_level == "1_2" ~ "Non-frail 1",
          Exposure_level == "1_3" ~ "Non-frail ≥2",
          Exposure_level == "2_1" ~ "Pre-frail 0",
          Exposure_level == "2_2" ~ "Pre-frail 1",
          Exposure_level == "2_3" ~ "Pre-frail ≥2",
          Exposure_level == "3_1" ~ "Frail 0",
          Exposure_level == "3_2" ~ "Frail 1",
          Exposure_level == "3_3" ~ "Frail ≥2"
        ),
        Exposure_label = factor(
          Exposure_label,
          levels = rev(c(
            "Non-frail 0 (Ref)",
            "Non-frail 1",
            "Non-frail ≥2",
            "Pre-frail 0",
            "Pre-frail 1",
            "Pre-frail ≥2",
            "Frail 0",
            "Frail 1",
            "Frail ≥2"
          ))
        )
      ) %>% 
      mutate(Outcome_label = case_when(
        Outcome=="rt"~"Reaction time",
        Outcome=="nm"~"Numerical memory",
        Outcome=="fi"~"Fluid intelligence",
        Outcome=="vm"~"Visual memory",TRUE~Outcome),
        Outcome_label=factor(Outcome_label,levels=c("Reaction time","Visual memory","Fluid intelligence","Numerical memory")))
      
  plot_cog <- ggplot(plot_df, aes(x = Beta, y = Exposure_label)) +
      geom_vline(
        xintercept = 0,
        linetype = "dashed",
        linewidth = 0.2,
        colour = "grey50"
      ) +
      geom_errorbarh(
        aes(xmin = CI_Lower, xmax = CI_Upper),
        height = 0,
        linewidth = 0.1,
        na.rm = TRUE
      ) +
      geom_point(size = 0.7, shape = 15) +
      facet_grid(. ~ Outcome_label, scales = "free_x") +
      theme_bw(base_size = 6, base_line_size = 0.2) +
      theme(
        panel.spacing.x = unit(0.4, "lines"),
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.2),
        strip.background = element_rect(fill = "grey90", colour = "white", linewidth = 0.2),
        strip.text = element_text(colour = "black"),
        axis.title.x = element_text(size = 6),
        panel.grid.major = element_line(linewidth = 0.2),
        panel.grid.minor = element_blank(),#element_line(linewidth = 0.2)
      ) +
      labs(
        x = "Beta (95% CI)",
        y = NULL
      )
  }
  
  

  
  cox <- function(data, exposures, outcomes, covar){
    
    result_list <- data.frame()
    p_values_all <- list()
    
    data <- data %>%
      mutate(across(where(is.character), ~ ifelse(. == "", NA, .))) %>%
      mutate(death = ifelse(is.na(death), 0, 1),
             death_date = date_of_death,
             death_s = 1)
    
    for (var in outcomes){
      data <- data %>%
        mutate(
          !!paste0(var, "_base") :=
            ifelse(as.Date(.data[[paste0(var, "_date")]]) <= as.Date(recruit_date), 1, NA),
          !!paste0(var, "_inci") :=
            ifelse(as.Date(.data[[paste0(var, "_date")]]) > as.Date(recruit_date) &
                     .data[[paste0(var, "_s")]]!= 0, 1, NA)
        ) %>%
        mutate(
          !!paste0(var, "_date") :=
            ifelse(!is.na(.data[[paste0(var, "_date")]]),
                   .data[[paste0(var, "_date")]],
                   ifelse(death == 1, date_of_death,
                          max(.data[[paste0(var, "_date")]], na.rm = TRUE))),
          !!paste0(var, "_fu") :=
            as.numeric(as.Date(.data[[paste0(var, "_date")]]) - as.Date(recruit_date)) / 365.25,
          !!paste0(var, "_inci") :=
            ifelse(is.na(.data[[paste0(var, "_inci")]]), 0, .data[[paste0(var, "_inci")]]),
          !!paste0(var, "_base2") :=
            ifelse(is.na(.data[[paste0(var, "_base")]]), 0, .data[[paste0(var, "_base")]]),
          !!var := ifelse(.data[[paste0(var, "_inci")]] == 1, 1, 0)
        )
    }
    
  
    for (outcome in outcomes) {
      for (exposure in exposures) {
        
        formula_input <- as.formula(
          paste0("survival::Surv(", outcome, "_fu,", outcome, "_inci) ~ ",
                 paste(c(exposure, covar), collapse = "+"))
        )
        
        data2 <- data %>% filter(is.na(.data[[paste0(outcome, "_base")]]))
        
        if (outcome %in% c("breast", "uterine", "ovarian")) data2 <- data2 %>% filter(sex == 0)
        if (outcome %in% c("prostate")) data2 <- data2 %>% filter(sex == 1)
        
        cox_model <- survival::coxph(formula_input, data = data2)
        coefmat <- summary(cox_model)$coefficients
        rn <- rownames(coefmat)
        
        idx <- which(rn == exposure)
        if (length(idx) == 0) idx <- grep(paste0("^", exposure), rn)
        
        for (i in idx) {
          key <- paste(outcome, rn[i], sep = "|")
          p_values_all[[key]] <- coefmat[i, "Pr(>|z|)"]
        }
      }
    }
    
    all_p_values <- unlist(p_values_all)
    fdr_p_values <- p.adjust(all_p_values, method = "fdr")
    
    for (outcome in outcomes) {
      for (exposure in exposures) {
        
        formula_input <- as.formula(
          paste0("survival::Surv(", outcome, "_fu,", outcome, "_inci) ~ ",
                 paste(c(exposure, covar), collapse = "+"))
        )
        
        data2 <- data %>% filter(is.na(.data[[paste0(outcome, "_base")]]))
        
        if (outcome %in% c("breast", "uterine", "ovarian",
                           "Breast Cancer","Uterine Cancer","Ovarian Cancer"))
          data2 <- data2 %>% filter(sex == 0)
        
        if (outcome %in% c("prostate","Prostate Cancer"))
          data2 <- data2 %>% filter(sex == 1)
        
        cox_model <- survival::coxph(formula_input, data = data2)
        cox_summary <- summary(cox_model)
        coefmat <- cox_summary$coefficients
        rn <- rownames(coefmat)
        
        idx <- which(rn == exposure)
        if (length(idx) == 0) idx <- grep(paste0("^", exposure), rn)
        
        hr_ci <- exp(confint(cox_model))
        
        n_cases_inci <- sum(data2[[paste0(outcome, "_inci")]] == 1)
        n_cases_base <- sum(data[[paste0(outcome, "_base2")]] == 1)
        median_follow_up <- median(as.numeric(data2[[paste0(outcome, "_fu")]]), na.rm = TRUE)
        
        median_age <- data2 %>%
          filter(.data[[paste0(outcome, "_inci")]] == 1) %>%
          summarise(median_fu = median(.data[[paste0(outcome, "_fu")]] + age, na.rm = TRUE)) %>%
          pull(median_fu)
        
        mean_diff <- if (is.numeric(data[[exposure]])) {
          data %>%
            summarise(
              mean_diff =
                mean(ifelse(.data[[outcome]] == 1, .data[[exposure]], NA), na.rm = TRUE) -
                mean(ifelse(.data[[outcome]] == 0, .data[[exposure]], NA), na.rm = TRUE)
            ) %>% pull(mean_diff)
        } else NA_real_
        
        for (i in idx) {
          key <- paste(outcome, rn[i], sep = "|")
          
          result <- data.frame(
            Outcome = outcome,
            Exposure = exposure,
            Exposure_level = sub(paste0("^", exposure), "", rn[i]),
            Term = rn[i],
            
            N_Cases_inci = n_cases_inci,
            N_Cases_base = n_cases_base,
            N = nrow(data2),
            Median_age = median_age,
            Mean_diff = mean_diff,
            Median_Follow_Up = median_follow_up,
            
            Beta = coefmat[i, "coef"],
            SE   = coefmat[i, "se(coef)"],
            HR   = exp(coefmat[i, "coef"]),
            CI_Lower = hr_ci[rn[i], "2.5 %"],
            CI_Upper = hr_ci[rn[i], "97.5 %"],
            z_score  = coefmat[i, "z"],
            
            P_Value = as.numeric(p_values_all[[key]]),
            FDR_P_Value = as.numeric(fdr_p_values[[key]]),
            stringsAsFactors = FALSE
          )
          
          result_list <- rbind(result_list, result)
        }
      }
    }
    
    return(list(result_list = result_list, data = data))
  }
  
  res_disease <- cox(data=ukb_frailty_in,exposures="frail_comorbid2",
                     outcomes=c("acd","ad","vd","pd","ms","scz","dep","anx","sd","death"),
                     covar=c("age","sex","ethnic","edu","tdi","center","pa","smok"))
  
  
  {
    res <- res_disease$result_list %>%
      mutate(
        Outcome = as.character(Outcome),
        Exposure = as.character(Exposure),
        Exposure_level = as.character(Exposure_level)
      )
    
    ref_df <- res %>%
      distinct(Outcome, Exposure) %>%
      mutate(
        Exposure_level = "1_1",
        Term = paste0(Exposure, "1_1"),
        N_Cases_inci = NA_integer_,
        N_Cases_base = NA_integer_,
        N = NA_integer_,
        Median_age = NA_real_,
        Mean_diff = NA_real_,
        Median_Follow_Up = NA_real_,
        Beta = 0,
        SE = NA_real_,
        HR = 1,
        CI_Lower = 1,
        CI_Upper = 1,
        z_score = NA_real_,
        P_Value = NA_real_,
        FDR_P_Value = NA_real_
      )
    
    plot_df <- bind_rows(res, ref_df) %>%
      mutate(
        ## order: put 1_1 first, others alphabetical (change levels if you want a custom order)
        Exposure_level = factor(
          Exposure_level,
          levels = c("1_1", setdiff(sort(unique(Exposure_level)), "1_1"))
        )
      ) %>% 
      mutate(Outcome_label = case_when(
        Outcome == "acd" ~ "All-cause dementia",
        Outcome == "ad" ~ "Alzheimer's disease",
        Outcome == "vd" ~ "Vascular dementia",
        Outcome == "pd" ~ "Parkinson's disease",
        Outcome == "ms" ~ "Multiple sclerosis",
        Outcome == "scz" ~ "Schizophrenia",
        Outcome == "dep" ~ "Depression",
        Outcome == "anx" ~ "Anxiety",
        Outcome == "sd" ~ "Sleep disorder",
        Outcome == "death" ~ "All-cause mortality",
        TRUE ~ as.character(Outcome)
      )) %>% 
      mutate(Exposure_label = case_when(
        Exposure_level == "1_1" ~ "Non-frail 0",
        Exposure_level == "1_2" ~ "Non-frail 1",
        Exposure_level == "1_3" ~ "Non-frail ≥2",
        Exposure_level == "2_1" ~ "Pre-frail 0",
        Exposure_level == "2_2" ~ "Pre-frail 1",
        Exposure_level == "2_3" ~ "Pre-frail ≥2",
        Exposure_level == "3_1" ~ "Frail 0",
        Exposure_level == "3_2" ~ "Frail 1",
        Exposure_level == "3_3" ~ "Frail ≥2"
      )) %>% 
      
      mutate(Outcome_label=factor(Outcome_label,levels=c("All-cause dementia","Alzheimer's disease","Vascular dementia","Parkinson's disease",
                                                         "Multiple sclerosis","Schizophrenia","Depression","Anxiety","Sleep disorder","All-cause mortality"
                                                         )),
             Exposure_label=factor(Exposure_label,levels=rev(c("Non-frail 0","Non-frail 1","Non-frail ≥2","Pre-frail 0","Pre-frail 1","Pre-frail ≥2","Frail 0","Frail 1","Frail ≥2")))
             )
    
    
      
    
    ## HR forest plot facet by outcome (log scale)
  plot_disease <- ggplot(plot_df, aes(x = HR, y = (Exposure_label))) +
      geom_vline(xintercept = 1,lwd=0.2, linetype = "dashed", colour = "grey50") +
      geom_errorbarh(aes(xmin = CI_Lower, xmax = CI_Upper), height = 0,lwd=0.1) +
      geom_point(size = 0.7,shape=15) +
      scale_x_log10() +
      facet_grid(. ~ Outcome_label, scales = "free_x",
                 labeller = labeller(Outcome_label = label_wrap_gen(width = 10))
                 ) +
      theme_bw(base_size = 6, base_line_size = 0.2) +
      theme(
        panel.spacing.x = unit(0.4, "lines"),
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.2),
        strip.background = element_rect(fill = "grey90", colour = "white", linewidth = 0.2),
        strip.text = element_text(colour = "black"),
        axis.title.x = element_text(size = 6),
        panel.grid.major = element_line(linewidth = 0.1),
        panel.grid.minor = element_blank(),#element_line(linewidth = 0.1)
      ) +
      labs(
        x = "HR (95% CI)",
        y = NULL
      )
    
    
  }

  # Prevalence of frailty
  {
    
    all_summary2 <- ukb_frailty_in %>% rename(frail="frailty") %>% select(eid,frail, comorbid) %>% 
      group_by(frail, comorbid) %>%
      summarise(count = n()) %>%
      ungroup() %>% 
      mutate(
        total_count = sum(count),
        percent = count / total_count * 100,
        frail = factor(frail, levels = c(1, 2, 3), labels = c("Non-frail", "Pre-frail", "Frail")),
        comorbid = factor(comorbid, levels = c(1, 2, 3), labels = c("0", "1", "≥2"))
      )
    
    all_summary <- ukb_frailty_in %>% rename(frail="frailty") %>% select(frail, comorbid) %>% 
      group_by(frail, comorbid) %>%
      summarise(count = n()) %>%
      mutate(total_count = sum(count),
             percent = count / total_count * 100) %>%
      mutate(frail = factor(frail, levels = c(1, 2, 3), labels = c("Non-frail", "Pre-frail", "Frail")),
             comorbid=factor(comorbid,levels = c(1, 2, 3),labels = c("0", "1", "≥2"))
      )
    
    pp1 <- ggplot(all_summary2, aes(x = factor(frail), y = percent, fill = factor(comorbid))) +
      geom_bar(stat = "identity", position = position_stack(), width = 1, color = "black", size = 0.2) +
      labs(title = "",
           x = "",
           y = "Prevalence (%)") +
      scale_fill_manual(values = c("lightgreen", "lightblue", "lightcoral"),
                        name = "No. of disease") +
      theme_minimal(base_size = 6,base_line_size = 0.2) + theme(panel.grid = element_blank(),
                              axis.line = element_line(color = "black", size = 0.2),
                              axis.ticks = element_line(color = "black", size = 0.2),
                              panel.spacing = unit(0, "lines"),
                              axis.text.x = element_text(angle = 45, hjust = 1),
                              strip.text = element_text(angle = 30, hjust = 0, vjust = 0, size = 8),
                              strip.background = element_blank(),
                              strip.placement = "outside",
                              legend.position = "none",
                              legend.text = element_text(size = 8),
                              legend.title = element_text(size = 8),
                              legend.key.size = unit(0.2, "cm"),
                              # axis.title.y = element_text(size = 6),
                              plot.margin = unit(c(0, 0, 0, 0.2), "cm")
                              ) +
      scale_x_discrete(expand = c(0, 0)) +
      coord_cartesian(xlim = c(0, 4), expand = FALSE) +
      geom_hline(yintercept = 0, color = "black", linetype = "solid", size = 0.2)
    
    
    pp2 <- ggplot(all_summary, aes(x = factor(frail), y = percent, fill = factor(comorbid))) +
      geom_bar(stat = "identity", position = position_stack(), width = 1, color = "black", size = 0.2) +
      labs(title = "",
           x = "",
           y = "Proportion (%)") +
      scale_fill_manual(values = c("lightgreen", "lightblue", "lightcoral"),
                        name = "No. of disease") +
      theme_minimal(base_size = 6,base_line_size = 0.2) + theme(panel.grid = element_blank(),
                              axis.line = element_line(color = "black", size = 0.2),
                              axis.ticks = element_line(color = "black", size = 0.2),
                              panel.spacing = unit(0, "lines"),
                              axis.text.x = element_text(angle = 45, hjust = 1),
                              strip.background = element_blank(),
                              strip.placement = "outside",
                              legend.position = "none",
                              strip.text = element_blank(),
                              # axis.title.y = element_text(size = 8),
                              plot.margin = unit(c(0, 0, 0, 0.1), "cm")
                              ) +
      scale_x_discrete(expand = c(0, 0)) +
      coord_cartesian(xlim = c(0, 4), expand = FALSE) +
      geom_hline(yintercept = 0, color = "black", linetype = "solid", size = 0.2)
    
    
    plot_frail <- cowplot::plot_grid(pp1,pp2,nrow=1,align = "h")
    
  }
  
  cowplot::plot_grid(plot_grid(plot_frail,plot_cog,rel_widths = c(1:2),nrow=1,align = "hv"), 
                     plot_disease,
                     ncol=1,align = "hv")
  
  
  ukb_frailty_aging <- ukb_frailty_in 
  {
  organ_list <- c(
    "Organismal","Immune","Artery","Adipose","Muscle","Skin",
    "Brain","Pituitary","Salivary","Esophagus",
    "Lung","Heart",
    "Liver","Stomach","Pancreas","Intestine","Kidney","Male"
  )
  organ_pac <- paste0(organ_list, "_res")
  
  plot_df <- ukb_frailty_in %>% filter(t1==1) %>% 
    select(frail_comorbid2, all_of(organ_pac)) %>%
    tidyr::pivot_longer(cols = all_of(organ_pac), names_to = "Organ", values_to = "age_gap") %>%
    mutate(Organ = sub("_res$", "", Organ),
           Organ = factor(Organ, levels = organ_list),
           frailty_comorbid2 = factor(frail_comorbid2))
  
  ggplot(plot_df, aes(x = frail_comorbid2, y = age_gap)) +
    geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.2, colour = "grey50") +
    geom_boxplot(width = 0.6, outlier.size = 0.4, linewidth = 0.2) +
    facet_wrap(~ Organ, scales = "free_y") +
    theme_bw(base_size = 8, base_line_size = 0.2) +
    theme(
      panel.grid = element_blank(),
      panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.2),
      strip.background = element_rect(fill = "grey90", colour = "grey60", linewidth = 0.2),
      axis.title.x = element_blank()
    ) +
    labs(y = "Organ age gap (residual)")
  
  ukb_frailty_in %>% filter(!is.na(Organismal_res)&t1==1) %>% nrow()
  table(ukb_frailty_in %>% filter(!is.na(Organismal_res)) %>% select(frail_comorbid2))
  
  lm(Brain_res ~ frail_comorbid2+age+sex+bmi, data = ukb_frailty_in) %>% summary
  lm(Organismal_res ~ frail_comorbid2+age+sex+bmi, data = ukb_frailty_in) %>% summary
  lm(Artery_res ~ frail_comorbid2+age+sex+bmi, data = ukb_frailty_in) %>% summary
  
  vars <- paste0(organ_list, "_res")
  covar <- c("age","sex","ethnic","edu","tdi","center","pa","smok")
  res_coef <- do.call(
    rbind,
    lapply(vars, function(v) {
      fit <- lm(
        as.formula(  paste(v, "~ frail_comorbid2 +", paste(covar, collapse = " + "))),
        data = ukb_frailty_in
      )
      s <- summary(fit)$coefficients
      out <- s[grep("^frail_comorbid2", rownames(s)), , drop = FALSE]
      data.frame(
        organ = v,
        level = rownames(out),
        beta = out[, "Estimate"],
        se   = out[, "Std. Error"],
        p    = out[, "Pr(>|t|)"],
        row.names = NULL
      )
    })
  )
  res_coef$FDR <- p.adjust(res_coef$p, method = "BH")
  
  table(res_coef$level)
  
  res_coef <- res_coef %>% 
    mutate(organ=gsub("_res","",organ),
           organ=factor(organ,levels=c("Organismal","Immune","Artery","Adipose","Muscle","Skin",
                                       "Brain","Esophagus","Lung","Heart",
                                       "Liver","Stomach","Pancreas","Intestine","Kidney","Male","Pituitary","Salivary"
                                       ))
           ) %>% 
    mutate(
      level = sub("^frail_comorbid2", "", level),
      level = case_when(
        level == "1_1" ~ "Non-frail 0 (Ref)",
        level == "1_2" ~ "Non-frail 1",
        level == "1_3" ~ "Non-frail ≥2",
        level == "2_1" ~ "Pre-frail 0",
        level == "2_2" ~ "Pre-frail 1",
        level == "2_3" ~ "Pre-frail ≥2",
        level == "3_1" ~ "Frail 0",
        level == "3_2" ~ "Frail 1",
        level == "3_3" ~ "Frail ≥2"
      ),
      level = factor(
        level,
        levels = rev(c(
          "Non-frail 0 (Ref)",
          "Non-frail 1",
          "Non-frail ≥2",
          "Pre-frail 0",
          "Pre-frail 1",
          "Pre-frail ≥2",
          "Frail 0",
          "Frail 1",
          "Frail ≥2"
        ))
      )
      )
    
    
  
  
  plot_aging <- ggplot(res_coef, aes(x = beta, y = level)) +
    geom_vline(
      xintercept = 0,
      linetype = "dashed",
      linewidth = 0.2,
      colour = "grey50"
    ) +
    geom_errorbarh(
      aes(xmin = beta-1.96*se, xmax = beta+1.96*se),
      height = 0,
      linewidth = 0.1,
      na.rm = TRUE
    ) +
    geom_point(size = 0.7, shape = 15) +
    facet_grid(. ~ organ, scales = "fixed") +
    coord_cartesian(xlim = c(-0.2,0.9)) +              
    scale_x_continuous(breaks = seq(-0.2, 0.9, by = 0.3)) +  
    theme_bw(base_size = 6, base_line_size = 0.2) +
    theme(
      panel.spacing.x = unit(0.4, "lines"),
      panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.2),
      strip.background = element_rect(fill = "grey90", colour = "white", linewidth = 0.2),
      strip.text = element_text(colour = "black"),
      axis.title.x = element_text(size = 6),
      panel.grid.major = element_line(linewidth = 0.2),
      panel.grid.minor = element_blank(),#element_line(linewidth = 0.2)
    ) +
    labs(
      x = "Beta (95% CI)",
      y = NULL
    )
  
  res_hm <- res_coef %>%
    mutate(
      organ = factor(organ, levels = unique(organ)),
      level = factor(level, levels = rev(unique(level))),
      sig = case_when(
        FDR < 0.001 ~ "***",
        FDR < 0.01  ~ "**",
        FDR < 0.05  ~ "*",
        TRUE ~ ""
      )
    )
  
  library("scales")
  library("ggsci")
  show_col(pal_npg("nrc")(10))
  pal_npg("nrc")(10)
  
  plot_aging <- ggplot(res_hm, aes(x = organ, y = level, fill = beta)) +
    geom_tile(color = "white", linewidth = 0.2) +
    geom_text(aes(label = sig), size = 2) +
    scale_fill_gradient2(
      low = "#8491B4FF",
      mid = "white",
      high = "#DC0000FF",
      midpoint = 0,
      limits = c(-0.2, 0.7),
      oob = scales::squish
    ) +
    guides(fill = guide_colorbar(
      ticks = TRUE,
      breaks = c(-0.2, 0, 0.2, 0.4, 0.6),
      barwidth  = unit(0.1, "cm"), 
      barheight = unit(1, "cm")     
    )) +
    theme_bw(base_size = 6, base_line_size = 0.2) +
    theme(
      panel.grid = element_blank(),
      axis.title = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.2),
      strip.background = element_rect(fill = "grey90", colour = "white", linewidth = 0.2),
      plot.margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm")
    ) +
    labs(fill = "Beta")
  
  }
  
  plot_aging
  
  
  
  
  library(survival)
  
  covar <- c("age","sex","ethnic","edu","tdi","center","pa","smok")
  
  # 0) total effect: Y ~ X + C
  fit_total <- coxph(
    as.formula(paste("Surv(acd_fu, acd) ~ frail_comorbid2 +", paste(covar, collapse=" + "))),
    data = res_disease$data
  )
  
  # 1) a-path: M ~ X + C
  fit_a <- lm(
    as.formula(paste("Brain_res ~ frail_comorbid2 +", paste(covar, collapse=" + "))),
    data = res_disease$data
  )
  
  # 2) direct effect model: Y ~ X + M + C
  fit_direct <- coxph(
    as.formula(paste("Surv(acd_fu, acd) ~ frail_comorbid2 + Brain_res +", paste(covar, collapse=" + "))),
    data = res_disease$data
  )
  fit_a %>% summary()
  fit_direct %>% summary()
  
  b_total  <- coef(fit_total)[grep("^frail_comorbid2", names(coef(fit_total)))]
  b_direct <- coef(fit_direct)[grep("^frail_comorbid2", names(coef(fit_direct)))]
  indirect <- b_total - b_direct
  prop_med <- indirect / b_total
  
  data.frame(
    level = names(b_total),
    total_logHR = as.numeric(b_total),
    direct_logHR = as.numeric(b_direct),
    indirect_logHR = as.numeric(indirect),
    prop_mediated = as.numeric(prop_med)
  )
  
  
  #Mediation
  
  run_mediation_grid <- function(data, outcomes, mediators, covar, x = "frail_comorbid2") {
    out_list <- list()
    k <- 0L
    
    for (y in outcomes) {
      time_var  <- paste0(y, "_fu")
      event_var <- y
      base_var  <- paste0(y, "_base")
      
      if (!all(c(time_var, event_var, x) %in% names(data))) next
      
      d_sub <- data
      d_sub <- d_sub %>% filter(is.na(.data[[base_var]]))
      
      f_total <- as.formula(paste0("Surv(", time_var, ", ", event_var, ") ~ ", x, " + ", paste(covar, collapse = " + ")))
      fit_total <- tryCatch(coxph(f_total, data = d_sub), error = function(e) NULL)
      if (is.null(fit_total)) next
      
      b_total <- coef(fit_total)
      b_total <- b_total[grep(paste0("^", x), names(b_total))]
      if (length(b_total) == 0) next
      
      for (m in mediators) {
        if (!all(c(m, x) %in% names(d_sub))) next
        
        f_a <- as.formula(paste0(m, " ~ ", x, " + ", paste(covar, collapse = " + ")))
        f_direct <- as.formula(paste0("Surv(", time_var, ", ", event_var, ") ~ ", x, " + ", m, " + ", paste(covar, collapse = " + ")))
        
        fit_a <- tryCatch(lm(f_a, data = d_sub), error = function(e) NULL)
        fit_direct <- tryCatch(coxph(f_direct, data = d_sub), error = function(e) NULL)
        if (is.null(fit_a) || is.null(fit_direct)) next
        
        a_hat <- coef(fit_a)
        a_hat <- a_hat[grep(paste0("^", x), names(a_hat))]
        
        b_direct <- coef(fit_direct)
        b_direct <- b_direct[grep(paste0("^", x), names(b_direct))]
        
        lev <- intersect(names(b_total), names(b_direct))
        if (length(lev) == 0) next
        
        indirect <- b_total[lev] - b_direct[lev]
        prop_med <- indirect / b_total[lev]
        
        k <- k + 1L
        out_list[[k]] <- data.frame(
          outcome = y,
          mediator = m,
          level = sub(paste0("^", x), "", lev),
          n = nrow(d_sub),
          a_X_to_M = ifelse(lev %in% names(a_hat), as.numeric(a_hat[lev]), NA_real_),
          total_logHR = as.numeric(b_total[lev]),
          direct_logHR = as.numeric(b_direct[lev]),
          indirect_logHR = as.numeric(indirect),
          prop_mediated = as.numeric(prop_med),
          row.names = NULL
        )
      }
    }
    
    res <- do.call(rbind, out_list)
    if (is.null(res)) return(data.frame())
    res
  }
  
  covar <- c("age","sex","ethnic","edu","tdi","center","pa","smok")
  organ_list <- c("Organismal","Immune","Artery","Adipose","Muscle","Skin","Brain","Pituitary","Salivary","Esophagus","Lung","Heart","Liver","Stomach","Pancreas","Intestine","Kidney","Male")
  organ_pac <- paste0(organ_list, "_res")
  diseases <- c("acd","ad","vd","pd","ms","scz","dep","anx","sd","death")
  
  med_res <- run_mediation_grid(
    data = res_disease$data,
    outcomes = diseases,
    mediators = organ_pac,
    covar = covar,
    x = "frail_comorbid2"
  )
  
  
  d_xy <- res_disease$data %>%
    filter(is.na(acd_base)) %>%
    mutate(frail_comorbid2 = factor(gsub("[[:space:]]+", "", as.character(frail_comorbid2)))) %>%
    mutate(frail_comorbid2 = relevel(frail_comorbid2, ref = "1_1"))
  
  d_xy <- d_xy[complete.cases(d_xy[, c("acd_fu","acd","frail_comorbid2","Brain_res", covar)]), ]
  
  fit_m <- lm(
    Brain_res ~ frail_comorbid2 + age + sex + ethnic + edu + tdi + center + pa + smok,
    data = d_xy
  )
  
  fit_y <- coxph(
    Surv(acd_fu, acd) ~ frail_comorbid2 + Brain_res + age + sex + ethnic + edu + tdi + center + pa + smok,
    data = d_xy
  )
  
  fit_m$call$data <- quote(d_xy)
  fit_y$call$data <- quote(d_xy)
  
  med <- mediate(
    model.m = fit_m,
    model.y = fit_y,
    treat = "frail_comorbid2",
    mediator = "Brain_res",
    control.value = "1_1",
    treat.value = "3_3",
    boot = TRUE,
    sims = 500
  )
  
  summary(med)
  
  library(dplyr)
  library(survival)
  library(mediation)
  
  covar <- c("age","sex","ethnic","edu","tdi","center","pa","smok")
  x <- "frail_comorbid2"
  
  ys <- c("acd","ad","vd","dep","anx","sd","death")
  # organ_list <- c("Organismal","Immune","Artery","Adipose","Muscle","Skin","Brain","Esophagus","Lung","Heart","Liver","Stomach","Pancreas","Intestine","Kidney")
  
  organ_list <- c("Organismal","Immune","Artery","Brain","Heart","Stomach","Pancreas","Intestine")
  ms <-  paste0(organ_list, "_res")
  
  # treat_levels_target <- c("2_1","2_2","2_3","3_1","3_2","3_3")
  treat_levels_target <- c("3_3")
  control_level <- "1_1"
  
  get_lm_term <- function(fit, term) {
    sm <- summary(fit)$coefficients
    if (!term %in% rownames(sm)) return(c(est=NA, se=NA, p=NA))
    c(est = unname(sm[term, "Estimate"]),
      se  = unname(sm[term, "Std. Error"]),
      p   = unname(sm[term, "Pr(>|t|)"]))
  }
  
  get_survreg_term <- function(fit, term) {
    sm <- summary(fit)$table
    if (!term %in% rownames(sm)) return(c(est=NA, se=NA, p=NA))
    c(est = unname(sm[term, "Value"]),
      se  = unname(sm[term, "Std. Error"]),
      p   = unname(sm[term, "p"]))
  }
  
  run_one <- function(res_disease_data, y, mediator, treat_level,
                      sims = 500, boot = TRUE) {
    
    time_var  <- paste0(y, "_fu")
    event_var <- y
    base_var  <- paste0(y, "_base")
    
    d_xy <- res_disease_data %>%
      filter(is.na(.data[[base_var]])) %>%
      mutate(
        frail_comorbid2 = factor(gsub("[[:space:]]+", "", as.character(.data[[x]]))),
        frail_comorbid2 = relevel(frail_comorbid2, ref = control_level)
      ) %>%
      mutate(ethnic = ifelse(ethnic %in% c("1", "2", "4", "5"), "Other", ethnic),
             edu = dplyr::recode(
               edu,
               "1" = "I",
               "2" = "II",
               "5" = "V",
               .default = edu
             ),
      ) %>% 
      filter(complete.cases(across(all_of(c(time_var, event_var, x, mediator, covar)))))
    
    lv <- levels(d_xy[[x]])
    
    
    fit_m <- lm(
      formula = as.formula(
        paste(mediator, "~", x, "+", paste(covar, collapse = " + "))
      ),
      data = d_xy
    )
    
    fit_y <- survreg(
      formula = as.formula(
        paste0(
          "Surv(", time_var, ",", event_var, ") ~ ",
          x, " + ", mediator, " + ", paste(covar, collapse = " + ")
        )
      ),
      data = d_xy,
      dist = "weibull"
    )
    
    
    
    
    term_a <- paste0(x, treat_level)
    a_info <- get_lm_term(fit_m, term_a)
    b_info <- get_survreg_term(fit_y, mediator)
    cprime_info <- get_survreg_term(fit_y, term_a)
    
    med_res <- tryCatch({
      
      med <- mediate(
        model.m = fit_m,
        model.y = fit_y,
        treat = x,
        mediator = mediator,
        outcome = time_var,
        control.value = control_level,
        treat.value = treat_level,
        boot = boot,
        sims = sims
      )
      sm <- summary(med)
      
      
      
      data.frame(
        acme_avg = unname(sm$d.avg),
        acme_ci_low = unname(sm$d.avg.ci[1]),
        acme_ci_high = unname(sm$d.avg.ci[2]),
        acme_p = unname(sm$d.avg.p),
        
        ade_avg = unname(sm$z.avg),
        ade_ci_low = unname(sm$z.avg.ci[1]),
        ade_ci_high = unname(sm$z.avg.ci[2]),
        ade_p = unname(sm$z.avg.p),
        
        te = unname(sm$tau.coef),
        te_ci_low = unname(sm$tau.ci[1]),
        te_ci_high = unname(sm$tau.ci[2]),
        te_p = unname(sm$tau.p),
        
        prop_med = unname(sm$n.avg),
        prop_ci_low = unname(sm$n.avg.ci[1]),
        prop_ci_high = unname(sm$n.avg.ci[2]),
        prop_p = unname(sm$n.avg.p),
        
        error = NA_character_,
        stringsAsFactors = FALSE
      )
    })
    
    out <- data.frame(
      outcome = y,
      time_var = time_var,
      event_var = event_var,
      mediator = mediator,
      control = control_level,
      treat = treat_level,
      n = nrow(d_xy),
      
      a_est = unname(a_info["est"]), a_se = unname(a_info["se"]), a_p = unname(a_info["p"]),
      b_est = unname(b_info["est"]), b_se = unname(b_info["se"]), b_p = unname(b_info["p"]),
      cprime_est = unname(cprime_info["est"]), cprime_se = unname(cprime_info["se"]), cprime_p = unname(cprime_info["p"]),
      
      stringsAsFactors = FALSE
    )
    
    cbind(out, med_res)
  }
  
  
  
  run_all_mediation <- function(res_disease_data, ys, ms,
                                sims = 500, boot = TRUE,
                                treat_levels = treat_levels_target,
                                dist = "weibull") {
    
    res_list <- vector("list", length(ys) * length(ms) * length(treat_levels))
    k <- 1
    
    for (yy in ys) {
      for (mm in ms) {
        for (tr in treat_levels) {
          res_list[[k]] <- run_one(
            res_disease_data = res_disease_data,
            y = yy,
            mediator = mm,
            treat_level = tr,
            sims = sims,
            boot = boot
          )
          k <- k + 1
        }
      }
    }
    
    bind_rows(res_list)
  }
  
  summary_df <- run_all_mediation(
    res_disease_data = res_disease$data,
    ys = ys,
    ms = ms,
    sims = 50,
    boot = F,
  )
  
  #Plotting
  library(dplyr)
  library(ComplexHeatmap)
  library(circlize)
  
  
  organ_order <- summary_df %>%
    filter(outcome == "death", !is.na(prop_med)) %>%
    mutate(organ = gsub("_res$", "", mediator)) %>%
    arrange(desc(prop_med)) %>%
    pull(organ) %>%
    unique()
  
  mat <- summary_df %>%
    mutate(
      outcome_label = recode(
        outcome,
        death      = "All-cause mortality",
        acd        = "All-cause dementia",
        ad     = "Alzheimer’s disease",
        vd         = "Vascular dementia",
        dep        = "Depression",
        anx   = "Anxiety",
        sd = "Sleep disorders"
      ),
      organ = gsub("_res$", "", mediator),
      organ = factor(organ, levels = organ_order)
    ) %>% 
    filter(prop_p < 0.05) %>%
    mutate(value = prop_med * 100) %>%
    select(outcome_label, organ, value) %>%
    tidyr::pivot_wider(names_from = organ, values_from = value) %>%
    tibble::column_to_rownames("outcome_label") %>%
    as.matrix()
  
  mat <- mat[, intersect(organ_order, colnames(mat)), drop = FALSE]
  
  
  
  df <- as.data.frame(as.table(mat))
  colnames(df) <- c("row", "col", "value")
  
  plot_interaction <- ggplot(df, aes(x = col, y = row, fill = value)) +
    geom_tile(color = "white", linewidth = 0.5) +
    geom_text(
      aes(label = ifelse(is.na(value), "", sprintf("%.1f", value))),
      size = 2, color = "black"
    ) +
    scale_fill_gradient2(
      low = "white",
      high = "#DC0000FF",
      midpoint = 0,
      na.value = "white"
    ) +
    guides(
      fill = guide_colorbar(
        title = "Proportion\nmediated (%)",
        title.theme = element_text(size = 6),
        label.theme = element_text(size = 6),
        barwidth  = unit(0.1, "cm"), 
        barheight = unit(1, "cm")    
      )
    ) +
    theme_bw(base_size = 6, base_line_size = 0.2) +
    theme(
      panel.grid = element_blank(),
      axis.title = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
      axis.text.y = element_text(size = 6),
      panel.border = element_rect(colour = "black", linewidth = 0.2),
      strip.background = element_rect(fill = "grey90", colour = "white", linewidth = 0.2),
      plot.margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm")
    )
  
  
  
  
  #Assess individual protein associated with frail_comorbidity and chronic diseases----
  
  extract_top_proteins <- function(organ, num_proteins) {
    file_name <- paste("E:\\Tutor\\UK Biobank\\Data/Proteomic/Data/organ_PAC_50%_missing/shap/shap_", organ, ".csv", sep = "")
    data <- read.csv(file = file_name) %>% 
      dplyr::select(-eid) %>% 
      mutate_all(abs) %>%  
      summarise_all(mean, na.rm = TRUE) %>% 
      tidyr::gather(key = "protein", value = "mean_value") %>%
      arrange(desc(mean_value))
    
    if (nrow(data) < num_proteins) {
      num_proteins <- nrow(data)
    }
    
    top_proteins <- data$protein[1:num_proteins]
    return(top_proteins)
  }
  
  #Extract the top n prot for each organ
  top_proteins_list <- list()
  for (organ in organ_list) {
    top_proteins <- extract_top_proteins(organ, 300)
    top_proteins_list[[organ]] <- top_proteins
  }
  top_proteins_list
  
  organ_prot <- unname(unlist(top_proteins_list[c("Brain","Stomach","Organismal","Artery","Heart")], use.names = FALSE))
  
  olink_subset <- ukb_frailty_in %>%   
    left_join(
      olink_full %>% select(eid, all_of(organ_prot)),
      by = "eid"
    ) %>%
    mutate(across(all_of(organ_prot), ~ as.numeric(scale(.)))) 
  
  table(olink_subset$t1) #random sample
  library(dplyr)
  library(tidyr)
  library(broom)
  
  protein_organ_map <- bind_rows(
    lapply(c("Brain","Stomach","Organismal","Artery","Heart"), function(org) {
      tibble(
        protein = top_proteins_list[[org]],
        organ = org
      )
    })
  )
  
  
  
  res_plot <- olink_subset %>%
    filter(frail_comorbid2 %in% c("1_1","3_3")) %>%
    mutate(frail_comorbid2 = relevel(factor(frail_comorbid2), ref = "1_1")) %>%
    select(frail_comorbid2, age, sex, ethnic, edu, tdi, center, pa, smok,
           all_of(organ_prot)) %>%
    pivot_longer(
      cols = all_of(organ_prot),
      names_to = "protein",
      values_to = "value"
    ) %>%
    group_by(protein) %>%
    group_modify(~ {
      tidy(
        lm(value ~ frail_comorbid2 + age + sex + ethnic + edu + tdi + center + pa + smok,
           data = .x)
      )
    }) %>%
    filter(term == "frail_comorbid23_3") %>%
    ungroup() %>%
    transmute(
      protein,
      logFC = estimate,
      p = p.value
    ) %>%
    left_join(protein_organ_map, by = "protein")
  
  res_plot <- res_plot %>%
    group_by(protein) %>%
    filter(
      !(organ == "Organismal" & any(organ != "Organismal"))
    ) %>%
    ungroup()
  
  res_plot$fdr <- p.adjust(res_plot$p, method = "fdr")
  res_plot$neglog10p <- -log10(pmax(res_plot$fdr, .Machine$double.xmin))
  
  
  label_df <- res_plot %>%
    filter(fdr < 0.05) %>%
    mutate(direction = if_else(logFC > 0, "+", "-")) %>%
    group_by(organ, direction) %>%
    slice_max(abs(logFC), n = 5, with_ties = FALSE) %>%
    ungroup()
  
  library(ggrepel)
  
  organ_cols <- c(
    Brain        = "#4B0082",
    Heart        = "#A52A2A",
    Artery       = "#FF7F50",
    Organismal  = "grey",
    Stomach     = "#4E79A7"
  )
  
  plot_voc <-   ggplot(res_plot, aes(logFC, neglog10p, color = organ)) +
    geom_point(alpha = 0.6,size=1.5,stroke=0,shape=20) +
    geom_text_repel(
      data = label_df,
      aes(label = protein),
      size = 2,
      box.padding = 0.4,      
      point.padding = 0.2,      
      min.segment.length = 0.1, 
      segment.alpha = 0.3,      
      segment.size = 0.2,       
      max.overlaps = Inf,      
      show.legend = FALSE
    )+
    geom_vline(xintercept = 0, linetype = "dashed",lwd=0.2) +
    geom_hline(yintercept = -log10(0.05), linetype = "dashed",lwd=0.2) +
    scale_color_manual(values = organ_cols) +
    theme_classic(base_line_size = 0.2,base_size = 6)+
    labs(title = NULL,
         x = "Effect of frailty and cardiometabolic multimorbidity",
         y = "-log10(FDR-corrected p-value)")+
    theme(
      legend.position = c(0.02, 0.98),
      legend.justification = c(0, 1),
      legend.spacing.y = unit(0.01, "cm"),
      legend.key.height = unit(0.1, "cm"),
      legend.text = element_text(lineheight = 0.2),
      legend.background = element_rect(fill = "white", colour = NA),
      legend.key = element_blank(),
      legend.title = element_blank()
    )
  
  
  # Functional enrichment of significant proteins
  sig_prot <- res_plot %>% filter(fdr<0.05)
  
  library(clusterProfiler)
  library(org.Hs.eg.db)
  
  go_enrichment <- enrichGO(gene = sig_prot$protein %>% unique(), 
                            OrgDb = org.Hs.eg.db,
                            keyType = "SYMBOL",   
                            ont = "BP",
                            # pvalueCutoff = 0.05
  )
  
  head(go_enrichment,n=20)
  
  plot_enrich <-  dotplot(go_enrichment, showCategory =6)+
    guides(size = FALSE,color = guide_colorbar(barwidth = 0.5,barheight = 2))+
    scale_size_continuous(range = c(1, 2.5))+ 
    scale_color_gradient(high = "#d0d1e6", low ="#023858")+
    labs(color = "q value",x="Gene ratio")+
    theme_bw(base_size = 6,base_line_size = 0.1)+
    theme(panel.grid = element_blank(),  
          axis.text.x = element_text(size = 6,colour ="black"),
          axis.text.y = element_text(size = 6,colour ="black"),
    )+
    scale_y_discrete(labels = function(x) (stringr::str_wrap(stringr::str_to_sentence(x), width = 30)))+
    scale_x_continuous(limits = c(0.03,0.13), breaks = c(0.04, 0.08,0.12))+
    theme(legend.text = element_text(size = 5))  
  
  
  
  sel_go <- c(
    "extracellular matrix organization",
    "regulation of vasculature development",
    "extracellular matrix disassembly",
    "regulation of angiogenesis"
  )
  table(df_go_gene$GO)
  
  df_go_gene <- as.data.frame(go_enrichment@result) %>%
    filter(Description %in% sel_go) %>%
    select(Description, geneID) %>%
    separate_rows(geneID, sep = "/") %>%
    dplyr::rename(
      GO = Description,
      gene = geneID
    ) %>% 
    mutate(
      GO = recode(
        GO,
        "extracellular matrix organization"  = "ECM organization",
        "extracellular matrix disassembly"   = "ECM disassembly"
      )
    ) %>% mutate(GO = stringr::str_to_sentence(GO),
                 GO = stringr::str_replace_all(GO, "\\bEcm\\b", "ECM")
    ) %>% 
    left_join(protein_organ_map, by = c("gene"="protein"))
  
  df_go_gene <- df_go_gene %>%
    group_by(gene) %>%
    filter(
      !(organ == "Organismal" & any(organ != "Organismal"))
    ) %>%
    ungroup()
  
  
  library(ggplot2)
  library(ggalluvial)
  library(stringr)
  
  
  organ_cols <- c(
    Brain        = "#4B0082",
    Heart        = "#A52A2A",
    Artery       = "#FF7F50",
    Organismal  = "lightgrey",
    Stomach     = "#4E79A7"
  )
  
  plot_go <- ggplot(
    df_go_gene,
    aes(axis1 = GO, axis2 = gene)
  ) +
    geom_alluvium(
      aes(fill = organ),
      alpha = 0.7,
      width = 0.2
    ) +
    geom_stratum(
      width = 0.25,
      color = "black",
      size = 0.2
    ) +
    geom_text(
      stat = "stratum",
      aes(
        label = after_stat(stratum),
        angle = ifelse(
          after_stat(stratum) %in% df_go_gene$gene,
          90, 0
        )
      ),
      size = 1.5
    ) +
    scale_fill_manual(values = organ_cols) +
    scale_x_discrete(
      limits = c("GO", "Gene"),
      expand = c(0.1, 0.1)
    ) +
    coord_flip() +
    theme_classic(base_size = 5, base_line_size = 0.2) +
    theme(
      legend.position = "none",
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.line = element_blank(),
      panel.border = element_blank()
    )
