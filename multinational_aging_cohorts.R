#Association of Fraily and cardiometabolic multimorbidity with cognition and mental well-being#

rm(list=ls())
library(dplyr)
library(cowplot)
library(Hmisc)
library(readxl)
library(tidyr)
library(plm)
library(ckbplotr)
library(haven)
library(survminer)
library(grid)
library(gridExtra)

#Multicohort Data Harmonization and Curation-----------------------------------------------------------------------------------
### ELSA w6-w9, 2012-2018####
elsa <- read.delim("E:/Tutor/Open aging cohort/ELSA/UKDA-5050-tab/tab/h_elsa_g2.tab")
elsa_w9 <- read.delim("E:/Tutor/Open aging cohort/ELSA/UKDA-5050-tab/tab/wave_9_elsa_data_eul_v1.tab") %>% select(idauniq,scovha) %>% rename(ha_09=scovha)
elsa_w8 <- read.delim("E:/Tutor/Open aging cohort/ELSA/UKDA-5050-tab/tab/wave_8_elsa_data_eul_v2.tab") %>% select(idauniq,scovha)%>% rename(ha_08=scovha)
elsa_w7 <- read.delim("E:/Tutor/Open aging cohort/ELSA/UKDA-5050-tab/tab/wave_7_elsa_data.tab") %>% select(idauniq,scovha)%>% rename(ha_07=scovha)
elsa_w6 <- read.delim("E:/Tutor/Open aging cohort/ELSA/UKDA-5050-tab/tab/wave_6_elsa_data_v2.tab") %>% select(idauniq,scovha)%>% rename(ha_06=scovha)
elsa_w5 <- read.delim("E:/Tutor/Open aging cohort/ELSA/UKDA-5050-tab/tab/wave_5_elsa_data_v4.tab") %>% select(idauniq,scfeha)%>%mutate(scfeha=ifelse(scfeha>0,6-scfeha, NA)) %>% rename(ha_05=scfeha)

w6 <- elsa %>% filter(r6iwstat==1) %>% mutate(bmi_4_6=ifelse(!is.na(r4mweight)&!is.na(r6mweight),(r6mweight-r4mweight)/r4mweight,NA),
                                              bmi_c_6=ifelse(bmi_4_6<=-0.1,1,0),
                                              tired=ifelse(r6effort==1|r6going==1,1,0),
                                              lowpa=ifelse(r6mdactx_e==5&r6vgactx_e==5,1,0)) %>%
  group_by(ragender, r6mheight) %>%
  mutate(slow = ifelse(r6wspeed<=quantile(r6wspeed, 0.2, na.rm = TRUE)|r6walkra==1,1,0)) %>% ungroup() %>% mutate(r6mgrip=pmax(r6rgrip,r6lgrip)) %>%
  group_by(ragender, r6mbmicat) %>%
  mutate(weak = ifelse(r6mgrip<=quantile(r6mgrip, 0.2, na.rm = TRUE),1,0)) %>% ungroup() %>%
  mutate(sum=rowSums(.[c("bmi_c_6", "tired", "lowpa","slow","weak")], na.rm = TRUE),
         frail=case_when(sum>=3~3,
                         sum==0~1,
                         T~2)) %>%
  mutate(new_dem=pmax(r7alzhs,r8alzhs,r9alzhs,r7demens,r8demens,r9demens,na.rm = TRUE),
         new_dem=ifelse(new_dem<0, 0, new_dem),
         t6=ifelse(r6iwstat==1,0,NA),
         t7=ifelse(r7iwstat==1,2,NA),
         t8=ifelse(r8iwstat==1,4,NA),
         t9=ifelse(r9iwstat==1,6,NA),
         time_dem=case_when(r7alzhs==1|r7demens==1~2,
                            r8alzhs==1|r8demens==1~4,
                            r9alzhs==1|r9demens==1~6,
                            T~NA),
         dem_time=ifelse(new_dem==1,time_dem,pmax(t6,t7,t8,t9,na.rm = TRUE)),
         dep_09=ifelse(r9cesd>=4,1,0),
         dep_08=ifelse(r8cesd>=4,1,0),
         dep_07=ifelse(r7cesd>=4,1,0),
         dep_06=ifelse(r6cesd>=4,1,0),
         new_dep=pmax(dep_07,dep_08,dep_09,na.rm = TRUE),
         time_dep=case_when(dep_07==1~2,
                            dep_08==1~4,
                            dep_09==1~6,
                            T~NA),
         dep_time=ifelse(new_dep==1,time_dep,pmax(t6,t7,t8,t9,na.rm = TRUE)),
         con_sum=rowSums(.[c("r6hibpe", "r6diabe", "r6hearte", "r6stroke")] * (.[c("r6hibpe", "r6diabe", "r6hearte", "r6stroke")] >= 0), na.rm = TRUE),
         comorbid=case_when(con_sum>=2~3,
                            con_sum==0~1,
                            T~2),
         r9health=ifelse(r9shlt>=4,1,0)) %>%
  mutate(frail_comorbid=ifelse(frail==3,paste(frail, comorbid, sep = "_"),frail),
         frail_comorbid2=paste(frail, comorbid, sep = "_")) %>%
  left_join(elsa_w5,by="idauniq") %>%
  left_join(elsa_w6,by="idauniq") %>%
  left_join(elsa_w7,by="idauniq") %>%
  left_join(elsa_w8,by="idauniq") %>%
  left_join(elsa_w9,by="idauniq") %>%
  mutate(
    tt1=ifelse(r1iwstat==1,2002,NA),
    tt2=ifelse(r2iwstat==1,2004,NA),
    tt3=ifelse(r3iwstat==1,2006,NA),
    tt4=ifelse(r4iwstat==1,2008,NA),
    tt5=ifelse(r5iwstat==1,2010,NA),
    tt6=ifelse(r6iwstat==1,2012,NA),
    tt7=ifelse(r7iwstat==1,2014,NA),
    tt8=ifelse(r8iwstat==1,2016,NA),
    tt9=ifelse(r9iwstat==1,2018,NA),
    
    
    t_1=ifelse(r6iwstat==1,2012,NA),
    t_2=ifelse(r7iwstat==1,2014,NA),
    t_3=ifelse(r8iwstat==1,2016,NA),
    t_4=ifelse(r9iwstat==1,2018,NA)
  ) %>%
  mutate(
    r6shlt=ifelse(r6shlt>=0,6-r6shlt,NA),
    r7shlt=ifelse(r7shlt>=0,6-r7shlt,NA),
    r8shlt=ifelse(r8shlt>=0,6-r8shlt,NA),
    r9shlt=ifelse(r9shlt>=0,6-r9shlt,NA),
    ha_06=ifelse(ha_06>=0&ha_06<=10,ha_06,NA),
    ha_07=ifelse(ha_07>=0&ha_07<=10,ha_07,NA),
    ha_08=ifelse(ha_08>=0&ha_08<=10,ha_08,NA),
    ha_09=ifelse(ha_09>=0&ha_09<=10,ha_09,NA)
  ) %>%
  mutate(r6ser7=NA,
         wealth5= ntile(h6atotb, 5)
         ) %>%
  mutate(
    r6all=rowSums(.[c("r6tr20", "r6orient", "r6ser7")] * (.[c("r6tr20", "r6orient", "r6ser7")] >= 0), na.rm = TRUE),
    r7all=rowSums(.[c("r7tr20", "r7orient", "r7ser7")] * (.[c("r7tr20", "r7orient", "r7ser7")] >= 0), na.rm = TRUE),
    r8all=rowSums(.[c("r8tr20", "r8orient", "r8ser7")] * (.[c("r8tr20", "r8orient", "r8ser7")] >= 0), na.rm = TRUE),
    r9all=rowSums(.[c("r9tr20", "r9orient", "r9ser7")] * (.[c("r9tr20", "r9orient", "r9ser7")] >= 0), na.rm = TRUE),
    r6all=ifelse(r6all==0,NA,r6all),
    r7all=ifelse(r7all==0,NA,r7all),
    r8all=ifelse(r8all==0,NA,r8all),
    r9all=ifelse(r9all==0,NA,r9all)
 
    
    ) %>%
  mutate_at(vars(c("ha_06","ha_07","ha_08","ha_09",
                                 "r6shlt", "r7shlt", "r8shlt", "r9shlt",
                                 "r6lstsf","r7lstsf","r8lstsf","r9lstsf",
                   "r1cesd","r2cesd", "r3cesd", "r4cesd", "r5cesd", "r6cesd", "r7cesd", "r8cesd", "r9cesd",
                                 "r6tr20","r7tr20","r8tr20","r9tr20",
                                 "r6orient","r7orient","r8orient","r9orient",
                                 "r6ser7","r7ser7","r8ser7","r9ser7",
                                 "r6all", "r7all", "r8all", "r9all"
                                 )), scale)



    w6_long <- w6 %>% select(c(idauniq, frail_comorbid2, frail,comorbid,
                               ha_06, ha_07, ha_08, ha_09,
                               r6lstsf, r7lstsf, r8lstsf, r9lstsf,
                               r6cesd, r7cesd, r8cesd, r9cesd,
                               r6shlt, r7shlt, r8shlt, r9shlt,
                               "r6tr20", "r7tr20", "r8tr20", "r9tr20",
                               "r6orient", "r7orient", "r8orient", "r9orient",
                               "r6ser7", "r7ser7", "r8ser7", "r9ser7",
                               "r6all", "r7all", "r8all", "r9all",
                               t_1, t_2, t_3, t_4))

  # #Associating using lagged fixed effects models with an Arellano–Bond estimator  
  #    w6_long2 <- w6 %>% select(c(idauniq, frail_comorbid2,
  #                                "r1cesd","r2cesd", "r3cesd", "r4cesd", "r5cesd", "r6cesd", "r7cesd", "r8cesd", "r9cesd",
  #                                tt1, tt2, tt3, tt4, tt5, tt6, tt7, tt8, tt9)) %>% 
  #       filter(!is.na(tt5)&!is.na(tt6)&!is.na(tt7)&!is.na(tt8)&!is.na(tt9))
  # 
  #    df1 <- pivot_longer(w6_long2, cols = starts_with("tt"), names_to = "time", values_to = "year")
  #    df4 <- pivot_longer(w6_long2, cols = ends_with("cesd"), names_to = "variable", values_to = "cesd") %>% mutate(cesd=as.numeric(cesd))
  #    
  #    df_long2 <- bind_cols(select(df1, c(idauniq, frail_comorbid2, year)),
  #                          select(df4, c(cesd))
  #                          ) %>% 
  #      left_join(select(w6, c(idauniq, ragender, r6agey, raedyrs_e, wealth5, r6smokev, r6drink, r6psyche, r6demens)), by = "idauniq")  %>%
  #         mutate(frail_comorbid2=ifelse(year<2012,NA,frail_comorbid2)) %>% 
  #           filter(!is.na(year)) %>% mutate(random = sample(1:100, n(), replace = TRUE)) %>% 
  #             filter(year>=2010)
  #      
  #    
  #    panel_data <- pdata.frame(df_long2, index = c("idauniq","year"))
  #    # Lagged fixed effect model using Arellano-Bond approach with twpstep system generalized method of moments (GMM)
  #    
  #    pgmm(log(cesd)~plm::lag(log(cesd), 1:2)+frail_comorbid2 |plm::lag(log(cesd), 2:5),
  #         data = filter(panel_data), effect = "individual", model = "twosteps", transformation=c("ld"),collapse = T)


  #Association using fixed effect model
    
  else_fig <- function(){
    
    {
      #OLS regression
      w6$frail_comorbid2 <- factor(w6$frail_comorbid2, levels = c("1_1", "1_2", "1_3", "2_1", "2_2", "2_3", "3_1", "3_2", "3_3"))
      dummy_variables <- model.matrix(~ frail_comorbid2 - 1, data = w6)
      w6_ols <- cbind(w6, dummy_variables) %>% select(-frail_comorbid2)
      out_cesd <- lm(r9cesd ~ frail_comorbid21_2 + frail_comorbid21_3 + frail_comorbid22_1 +frail_comorbid22_2 + frail_comorbid22_3 + frail_comorbid23_1 +frail_comorbid23_2 + frail_comorbid23_3 + ragender + r6agey + raedyrs_e + r6cesd, data = w6_ols) %>% summary()
      out_sf <- lm(r9lstsf ~ frail_comorbid21_2 + frail_comorbid21_3 + frail_comorbid22_1 +frail_comorbid22_2 + frail_comorbid22_3 + frail_comorbid23_1 +frail_comorbid23_2 + frail_comorbid23_3 + ragender + r6agey + raedyrs_e +r6lstsf, data = w6_ols) %>% summary()
      out_ha <- lm(ha_09 ~ frail_comorbid21_2 + frail_comorbid21_3 + frail_comorbid22_1 +frail_comorbid22_2 + frail_comorbid22_3 + frail_comorbid23_1 +frail_comorbid23_2 + frail_comorbid23_3 + ragender + r6agey + raedyrs_e +ha_06, data = w6_ols) %>% summary()
      out_sh <- lm(r9shlt ~ frail_comorbid21_2 + frail_comorbid21_3 + frail_comorbid22_1 +frail_comorbid22_2 + frail_comorbid22_3 + frail_comorbid23_1 +frail_comorbid23_2 + frail_comorbid23_3 + ragender + r6agey + raedyrs_e +r6shlt, data = w6_ols) %>% summary()
      
      out_tr <- lm(r9tr20 ~ r6tr20+frail_comorbid21_2 + frail_comorbid21_3 + frail_comorbid22_1 +frail_comorbid22_2 + frail_comorbid22_3 + frail_comorbid23_1 +frail_comorbid23_2 + frail_comorbid23_3 + ragender + r6agey + raedyrs_e , data = filter(w6_ols,r6demens != 1)) %>% summary()
      out_or <- lm(r9orient ~ r6orient+frail_comorbid21_2 + frail_comorbid21_3 + frail_comorbid22_1 +frail_comorbid22_2 + frail_comorbid22_3 + frail_comorbid23_1 +frail_comorbid23_2 + frail_comorbid23_3 + ragender + r6agey + raedyrs_e, data = filter(w6_ols,r6demens != 1)) %>% summary()
      out_se <- lm(r9ser7 ~ r7ser7+frail_comorbid21_2 + frail_comorbid21_3 + frail_comorbid22_1 +frail_comorbid22_2 + frail_comorbid22_3 + frail_comorbid23_1 +frail_comorbid23_2 + frail_comorbid23_3 + ragender + r6agey + raedyrs_e, data = filter(w6_ols,r6demens != 1)) %>% summary()
      out_all <- lm(r9all ~ r6all+frail_comorbid21_2 + frail_comorbid21_3 + frail_comorbid22_1 +frail_comorbid22_2 + frail_comorbid22_3 + frail_comorbid23_1 +frail_comorbid23_2 + frail_comorbid23_3 + ragender + r6agey + raedyrs_e, data = filter(w6_ols,r6demens != 1)) %>% summary()
      
      # Mental wellbeing
      f1 <- as.data.frame(out_cesd$coefficients)[2:9,] %>% mutate(analysis = "Depressive symptom") %>% add_row(analysis = "Depressive symptom") %>% add_row(analysis = "Depressive symptom") %>% add_row(analysis = "Depressive symptom") %>% add_row(analysis = "Depressive symptom")
      f2 <- as.data.frame(out_sf$coefficients)[2:9,] %>% mutate(analysis = "Life satisfaction") %>% add_row(analysis = "Life satisfaction") %>% add_row(analysis = "Life satisfaction") %>% add_row(analysis = "Life satisfaction") %>% add_row(analysis = "Life satisfaction")
      f3 <- as.data.frame(out_ha$coefficients)[2:9,] %>% mutate(analysis = "Happiness") %>% add_row(analysis = "Happiness") %>% add_row(analysis = "Happiness") %>% add_row(analysis = "Happiness") %>% add_row(analysis = "Happiness")
      f4 <- as.data.frame(out_sh$coefficients)[2:9,] %>% mutate(analysis = "Self-reported health") %>% add_row(analysis = "Self-reported health") %>% add_row(analysis = "Self-reported health") %>% add_row(analysis = "Self-reported health") %>% add_row(analysis = "Self-reported health")
      
      forest_data <- rbind(f1, f2, f3, f4)
      forest_data$Label <- rep(c("1", "≥2", "0", "1", "≥2", "0", "1", "≥2", "0 (Ref)", "Non-frail", "Pre-frail", "Frail"), times = 4)
      forest_data$seq <- rep(c(3, 4, 6, 7, 8, 10, 11, 12, 2, 1, 5, 9), times = 4)
      forest_data <- forest_data %>% arrange(seq) %>% mutate(seq = as.factor(seq),
                                                             est = Estimate,
                                                             se = `Std. Error`,
                                                             est = ifelse(seq == 2, 0, est),
                                                             subgroup = ifelse(is.na(est) == FALSE, paste0(seq, Label), est),
                                                             lci = est - 1.96 * se,
                                                             uci = est + 1.96 * se)
      
      row_labels <- forest_data %>% filter(analysis == "Depressive symptom") %>% select(subgroup, Label)
      
      p1_ols <- forest_plot(split(forest_data, ~analysis), col.key = "subgroup", row.labels = row_labels, exponentiate = F,
                            scalepoints = T, nullval = 0, xlab = "",
                            plot.margin = margin(1, 4, 1, 4, "mm"),
                            xlim = c(-1.7, 1.7), row.labels.heading="ELSA", base_size = 7,pointsize = 2,quiet = TRUE,panel.headings = rep("", 4),
                            xticks = c(-1.5, -1, -0.5, 0, 0.5, 1.0, 1.5),
                            base_line_size = 0.2)
      p1_ols$plot <- p1_ols$plot + theme(panel.spacing.x  = unit(18, "mm"))
      
      
      
      
      # Cognition
      f5 <- as.data.frame(out_tr$coefficients)[3:10,] %>% mutate(analysis = "Memory") %>% add_row(analysis = "Memory") %>% add_row(analysis = "Memory") %>% add_row(analysis = "Memory") %>% add_row(analysis = "Memory")
      f6 <- as.data.frame(out_or$coefficients)[3:10,] %>% mutate(analysis = "Orientation") %>% add_row(analysis = "Orientation") %>% add_row(analysis = "Orientation") %>% add_row(analysis = "Orientation") %>% add_row(analysis = "Orientation")
      f7 <- as.data.frame(out_se$coefficients)[3:10,] %>% mutate(analysis = "Numeracy") %>% add_row(analysis = "Numeracy") %>% add_row(analysis = "Numeracy") %>% add_row(analysis = "Numeracy") %>% add_row(analysis = "Numeracy")
      f8 <- as.data.frame(out_all$coefficients)[3:10,] %>% mutate(analysis = "Global cognitive function") %>% add_row(analysis = "Global cognitive function") %>% add_row(analysis = "Global cognitive function") %>% add_row(analysis = "Global cognitive function") %>% add_row(analysis = "Global cognitive function")
      forest_data2 <- rbind(f5, f6, f7, f8)
      
      forest_data2$Label <- rep(c("1", "≥2", "0", "1", "≥2", "0", "1", "≥2", "0 (Ref)", "Non-frail", "Pre-frail", "Frail"), times = 4)
      forest_data2$seq <- rep(c(3, 4, 6, 7, 8, 10, 11, 12, 2, 1, 5, 9), times = 4)
      forest_data2 <- forest_data2 %>% arrange(seq) %>% mutate(seq = as.factor(seq),
                                                               est = Estimate,
                                                               se = `Std. Error`,
                                                               est = ifelse(seq == 2, 0, est),
                                                               subgroup = ifelse(is.na(est) == FALSE, paste0(seq, Label), est),
                                                               lci = est - 1.96 * se,
                                                               uci = est + 1.96 * se)
      
      row_labels <- forest_data2 %>% filter(analysis == "Memory") %>% select(subgroup, Label)
      
      p1b_ols <- forest_plot(split(forest_data2, ~analysis), col.key = "subgroup", row.labels = row_labels, exponentiate = FALSE,
                             scalepoints = TRUE, nullval = 0, xlab = "",
                             plot.margin = margin(1, 4, 1, 4, "mm"),
                             row.labels.heading="ELSA", base_size = 7,pointsize = 2,quiet = TRUE,panel.headings = rep("", 4),
                             xlim        = c(-0.75, 0.75),
                             xticks      = c(-0.6, -0.4,-0.2,0,0.2,0.4,0.6),
                             base_line_size = 0.2)
      
      p1b_ols$plot <- p1b_ols$plot + theme(panel.spacing.x  = unit(18, "mm"))
    }
    
    
    df1 <- pivot_longer(w6_long, cols = starts_with("t_"), names_to = "time", values_to = "year")
    df2 <- pivot_longer(w6_long, cols = starts_with("ha_"), names_to = "variable", values_to = "ha")
    df3 <- pivot_longer(w6_long, cols = ends_with("stsf"), names_to = "variable", values_to = "sf")
    df4 <- pivot_longer(w6_long, cols = ends_with("cesd"), names_to = "variable", values_to = "cesd")
    df5 <- pivot_longer(w6_long, cols = ends_with("shlt"), names_to = "variable", values_to = "sh")
    df6 <- pivot_longer(w6_long, cols = ends_with("tr20"), names_to = "variable", values_to = "tr")
    df7 <- pivot_longer(w6_long, cols = ends_with("orient"), names_to = "variable", values_to = "or")
    df8 <- pivot_longer(w6_long, cols = ends_with("ser7"), names_to = "variable", values_to = "se")
    df9 <- pivot_longer(w6_long, cols = ends_with("all"), names_to = "variable", values_to = "all")
    
    df_long <- bind_cols(select(df1, c(idauniq, frail_comorbid2, frail,comorbid,year)),
                         select(df2, c(ha)),
                         select(df3, c(sf)),
                         select(df4, c(cesd)),
                         select(df5, c(sh)),
                         select(df6, c(tr)),
                         select(df7, c(or)),
                         select(df8, c(se)),
                         select(df9, c(all))) %>%
      filter(!is.na(year)) %>%
      left_join(select(w6, c(idauniq, ragender, r6agey, raedyrs_e, wealth5, r6smokev, r6drink, r6psyche, r6demens)), by = "idauniq")
    
    panel_data <- pdata.frame(df_long, index = c("idauniq","year"))

    # panel_data <- make.pbalanced(panel_data)
    
    # pvar(panel_data, index = c("idauniq","year"))
    #Test for fixed or random model
      # fx_cesd <- plm(cesd ~ frail_comorbid2 + ragender + r6agey + raedyrs_e+ wealth5 + r6smokev + r6drink + r6psyche, data = filter(panel_data, !is.na(cesd)), model = "within")
      # ra_cesd <- plm(cesd ~ frail_comorbid2 + ragender + r6agey + raedyrs_e+ wealth5 + r6smokev + r6drink + r6psyche, data = filter(panel_data, !is.na(cesd)), model = "random")
      # phtest(fx_cesd,ra_cesd)

    #Using OLS that would violate the independence assumption 
    #lm(sf ~ frail_comorbid2 + ragender + r6agey + raedyrs_e+ wealth5 + r6smokev + r6drink + r6psyche, data = filter(panel_data, !is.na(sf)))


    #fx_cesd <- plm(cesd ~ as.factor(frail)+as.factor(comorbid)+(frail)*(comorbid) + ragender + r6agey + raedyrs_e+ wealth5 + r6smokev + r6drink + r6psyche, data = filter(panel_data, !is.na(cesd)), model = "within")
    fx_cesd <- plm(cesd ~ frail_comorbid2+ ragender + r6agey + raedyrs_e+ wealth5 + r6smokev + r6drink + r6psyche, data = filter(panel_data,!is.na(cesd)), model = "random")
    fx_ha <- plm(ha ~ frail_comorbid2 + ragender + r6agey + raedyrs_e+wealth5 + r6smokev + r6drink + r6psyche, data = filter(panel_data, !is.na(ha)), model = "random")
    fx_sh <- plm(sh ~ frail_comorbid2 + ragender + r6agey + raedyrs_e+wealth5 + r6smokev + r6drink + r6psyche, data = filter(panel_data, !is.na(sh)), model = "random")
    fx_tr <- plm(tr ~ frail_comorbid2 + ragender + r6agey + raedyrs_e + wealth5 + r6smokev + r6drink + r6psyche, data = filter(panel_data, !is.na(tr) & r6demens != 1), model = "random")
    fx_or <- plm(or ~ frail_comorbid2 + ragender + r6agey + raedyrs_e + wealth5 + r6smokev + r6drink + r6psyche, data = filter(panel_data, !is.na(or) & r6demens != 1), model = "random")
    fx_se <- plm(se ~ frail_comorbid2 + ragender + r6agey + raedyrs_e + wealth5 + r6smokev + r6drink + r6psyche, data = filter(panel_data, !is.na(se) & r6demens != 1), model = "random")
    fx_all <- plm(all ~ frail_comorbid2 + ragender + r6agey + raedyrs_e + wealth5 + r6smokev + r6drink + r6psyche, data = filter(panel_data, !is.na(all) & r6demens != 1), model = "random")
    
    # panel_data <- pdata.frame(df_long, index = c("year"))
    fx_sf <- plm(sf ~ frail_comorbid2 + ragender + r6agey + raedyrs_e+ wealth5 + r6smokev + r6drink + r6psyche, data = filter(panel_data, !is.na(sf)), model = "between")
    
    out_cesd <- summary(fx_cesd)
    out_sf <- summary(fx_sf)
    out_ha <- summary(fx_ha)
    out_sh <- summary(fx_sh)
    out_tr <- summary(fx_tr)
    out_or <- summary(fx_or)
    out_se <- summary(fx_se)
    out_all <- summary(fx_all)

      # Mental wellbeing
    fix_coef <- function(out, label) {
      df <- as.data.frame(out$coefficients)[2:9, ]
      
      colnames(df) <- gsub("t-value", "z-value", colnames(df))
      colnames(df) <- gsub("Pr\\(>\\|t\\|\\)", "Pr(>|z|)", colnames(df))
      
      df$analysis <- label
      
      blank <- df[1:4, ]
      blank[,] <- NA
      blank$analysis <- label
      
      bind_rows(df, blank)
    }
    
    f1 <- fix_coef(out_cesd, "Depressive symptom")
    f2 <- fix_coef(out_sf, "Life satisfaction")
    f3 <- fix_coef(out_ha, "Happiness")
    f4 <- fix_coef(out_sh, "Self-reported health")
    
    forest_data <- bind_rows(f1, f2, f3, f4)
      forest_data_elsa <- rbind(f1, f2, f3, f4) %>% mutate(study="ELSA",level=rep(c("1_2", "1_3", "2_1", "2_2", "2_3", "3_1", "3_2", "3_3", "", "", "",""),times=4))
      forest_data$Label <- rep(c("1", "≥2", "0", "1", "≥2", "0", "1", "≥2", "0 (Ref)", "Non-frail", "Pre-frail", "Frail"), times = 4)
      forest_data$seq <- rep(c(3, 4, 6, 7, 8, 10, 11, 12, 2, 1, 5, 9), times = 4)
      forest_data <- forest_data %>% arrange(seq) %>% mutate(seq = as.factor(seq),
                                                             est = Estimate,
                                                             se = `Std. Error`,
                                                             est = ifelse(seq == 2, 0, est),
                                                             subgroup = ifelse(is.na(est) == FALSE, paste0(seq, Label), est),
                                                             lci = est - 1.96 * se,
                                                             uci = est + 1.96 * se)
      
      row_labels <- forest_data %>% filter(analysis == "Depressive symptom") %>% select(subgroup, Label)
      
      p1 <- forest_plot(split(forest_data, ~analysis), col.key = "subgroup", row.labels = row_labels, exponentiate = F,
                        scalepoints = T, nullval = 0, xlab = "",
                        plot.margin = margin(1, 4, 1, 4, "mm"),
                        xlim = c(-1.7, 1.7), row.labels.heading="ELSA", base_size = 7,pointsize = 2,quiet = TRUE,panel.headings = rep("", 4),
                        xticks = c(-1.5, -1, -0.5, 0, 0.5, 1.0, 1.5),
                        base_line_size = 0.2)
      p1$plot <- p1$plot + theme(panel.spacing.x  = unit(18, "mm"))
      

      # Cognition
      f5 <- as.data.frame(out_tr$coefficients)[2:9,] %>% mutate(analysis = "Memory") %>% add_row(analysis = "Memory") %>% add_row(analysis = "Memory") %>% add_row(analysis = "Memory") %>% add_row(analysis = "Memory")
      f6 <- as.data.frame(out_or$coefficients)[2:9,] %>% mutate(analysis = "Orientation") %>% add_row(analysis = "Orientation") %>% add_row(analysis = "Orientation") %>% add_row(analysis = "Orientation") %>% add_row(analysis = "Orientation")
      f7 <- as.data.frame(out_se$coefficients)[2:9,] %>% mutate(analysis = "Numeracy") %>% add_row(analysis = "Numeracy") %>% add_row(analysis = "Numeracy") %>% add_row(analysis = "Numeracy") %>% add_row(analysis = "Numeracy")
      f8 <- as.data.frame(out_all$coefficients)[2:9,] %>% mutate(analysis = "Global cognitive function") %>% add_row(analysis = "Global cognitive function") %>% add_row(analysis = "Global cognitive function") %>% add_row(analysis = "Global cognitive function") %>% add_row(analysis = "Global cognitive function")
      forest_data2 <- rbind(f5, f6, f7, f8)
      forest_data2_elsa <- rbind(f5, f6, f7, f8) %>% mutate(study="ELSA",level=rep(c("1_2", "1_3", "2_1", "2_2", "2_3", "3_1", "3_2", "3_3", "", "", "",""),times=4))
      
      forest_data2$Label <- rep(c("1", "≥2", "0", "1", "≥2", "0", "1", "≥2", "0 (Ref)", "Non-frail", "Pre-frail", "Frail"), times = 4)
      forest_data2$seq <- rep(c(3, 4, 6, 7, 8, 10, 11, 12, 2, 1, 5, 9), times = 4)
      forest_data2 <- forest_data2 %>% arrange(seq) %>% mutate(seq = as.factor(seq),
                                                               est = Estimate,
                                                               se = `Std. Error`,
                                                               est = ifelse(seq == 2, 0, est),
                                                               subgroup = ifelse(is.na(est) == FALSE, paste0(seq, Label), est),
                                                               lci = est - 1.96 * se,
                                                               uci = est + 1.96 * se)
      
      row_labels <- forest_data2 %>% filter(analysis == "Memory") %>% select(subgroup, Label)
      
      p1_b <- forest_plot(split(forest_data2, ~analysis), col.key = "subgroup", row.labels = row_labels, exponentiate = FALSE,
                          scalepoints = TRUE, nullval = 0, xlab = "",
                          plot.margin = margin(1, 4, 1, 4, "mm"),
                          row.labels.heading="ELSA", base_size = 7,pointsize = 2,quiet = TRUE,panel.headings = rep("", 4),
                          xlim        = c(-0.6, 0.6),
                          xticks      = c(-0.6, -0.4,-0.2,0,0.2,0.4,0.6),
                          base_line_size = 0.2)
      p1_b$plot <- p1_b$plot + theme(plot.title = element_text(size = 6, face = "bold"))
      
      return(list(p1 = p1$plot, p1_b = p1_b$plot, p1_ols=p1_ols$plot, p1b_ols=p1b_ols$plot,df_long=df_long,forest=forest_data_elsa,forest2=forest_data2_elsa))
    }
  res_else <- else_fig()
  p1 <- res_else$p1
  p1_b <- res_else$p1_b 
  p1_ols <- res_else$p1_ols
  p1b_ols <- res_else$p1b_ols
  else_long <- res_else$df_long
  forest_data_elsa <- res_else$forest
  forest_data2_elsa <- res_else$forest2
  
  #Dementia
  out_dem <- coxph(Surv(dem_time, new_dem) ~ relevel(as.factor(frail_comorbid2),ref="1_1")+ragender+r6agey+raedyrs_e+wealth5 + r6smokev + r6drink, data = filter(w6,r6demene!=1)) %>% summary
  #out_dem <- coxph(Surv(dem_time, new_dem) ~ relevel(as.factor(frail_comorbid2),ref="1_1")+ragender+r6agey+raedyrs_e+wealth5 + r6smokev + r6drink, data = filter(w6,r6demene!=1)) %>% summary
  
  f9 <- as.data.frame(out_dem$coefficients)[1:8,] %>% mutate(analysis="Dementia") %>%  add_row(analysis = "Dementia")%>%  add_row(analysis = "Dementia")%>%  add_row(analysis = "Dementia")%>%  add_row(analysis = "Dementia")
  forest_data3_elsa <- f9
  forest_data3 <-  rbind(f9)
  forest_data3$Label <- rep(c("1","≥2","0","1","≥2","0","1","≥2","0 (Ref)","Non-frail","Pre-frail","Frail"),times=1)
  forest_data3$seq <- rep(c(3,4,6,7,8,10,11,12,2,1,5,9),times=1)
  forest_data3<- forest_data3 %>% arrange(seq) %>% mutate(seq=as.factor(seq),
                                                                        est=coef,
                                                                        se=`se(coef)`,
                                                                        est=ifelse(seq==2,0,est),
                                                                        subgroup=ifelse(is.na(est)==F,paste0(seq,Label),est),
                                                                        lci=est-1.96*se,
                                                                        uci=est+1.96*se,
                                                                       )
  row_labels <- forest_data3 %>% filter(analysis=="Dementia") %>% select(subgroup,Label)
  p1_a <- forest_plot(split(forest_data3,~analysis), col.key = "subgroup",row.labels = row_labels, exponentiate = T,logscale=T,
                                   scalepoints=T,nullval=1,xlab="HR (95% CI)",row.labels.heading="ELSA",base_size = 7,pointsize = 2,quiet = TRUE,
                                  title="", plot.margin = margin(1, 4, 1, 4, "mm"),
                                  #xlim        = c(-1, 1),
                                    #xticks      = c(-1,-0.5,0,0.5,1.0),
                                  base_line_size = 0.2)
  
  #Interaction of frailty and multimorbidity
  interact_else <- function(){
  outcomes <- c("all","cesd","ha","sf","sh")
  interaction_else <- data.frame()
  df_long <- res_else$df_long
    for (outcome in outcomes) {
            if(outcome=="sf"){
              panel_data <- pdata.frame(df_long, index = c("year"))
              model = "within"
              m1 <- plm(as.formula(paste0(outcome,"~as.factor(frail)+as.factor(comorbid)","+ ragender + r6agey + raedyrs_e+ wealth5 + r6smokev + r6drink + r6psyche")),data = filter(panel_data,!is.na(get(outcome))),model = model)
              m2 <- plm(as.formula(paste0(outcome,"~as.factor(frail)*as.factor(comorbid)","+ ragender + r6agey + raedyrs_e+ wealth5 + r6smokev + r6drink + r6psyche")),data = filter(panel_data,!is.na(get(outcome))),model = model)
              outcome1 <- plm(as.formula(paste0(outcome,"~as.factor(frail)+as.factor(comorbid)","+ ragender + r6agey + raedyrs_e+ wealth5 + r6smokev + r6drink + r6psyche")),data = filter(panel_data,!is.na(get(outcome))),model = model) %>% summary()
              outcome2 <- plm(as.formula(paste0(outcome,"~as.factor(frail)*as.factor(comorbid)","+ ragender + r6agey + raedyrs_e+ wealth5 + r6smokev + r6drink + r6psyche")),data = filter(panel_data,!is.na(get(outcome))),model = model) %>% summary()
              
              outcome3 <- as.data.frame(outcome1$coefficients)[1:4,] %>% mutate(outcome=outcome,p=pFtest(m2, m1)$p.value) %>% 
                mutate(est=Estimate, stderr=`Std. Error`,lci=est-1.96*stderr, hci=est+1.96*stderr, beta=paste0(round(est,digits=2), " (",round(lci,digits=2)," to ",round(hci,digits=2),")")) %>% select(est,lci,hci,beta,outcome,p)
              
        }
            else{    
              panel_data <- pdata.frame(df_long, index = c("idauniq","year"))
              model = "random"
              m1 <- plm(as.formula(paste0(outcome,"~as.factor(frail)+as.factor(comorbid)","+ ragender + r6agey + raedyrs_e+ wealth5 + r6smokev + r6drink + r6psyche")),data = filter(panel_data,!is.na(get(outcome))),model = model)
              m2 <- plm(as.formula(paste0(outcome,"~as.factor(frail)*as.factor(comorbid)","+ ragender + r6agey + raedyrs_e+ wealth5 + r6smokev + r6drink + r6psyche")),data = filter(panel_data,!is.na(get(outcome))),model = model)
              outcome1 <- plm(as.formula(paste0(outcome,"~as.factor(frail)+as.factor(comorbid)","+ ragender + r6agey + raedyrs_e+ wealth5 + r6smokev + r6drink + r6psyche")),data = filter(panel_data,!is.na(get(outcome))),model = model) %>% summary()
              outcome2 <- plm(as.formula(paste0(outcome,"~as.factor(frail)*as.factor(comorbid)","+ ragender + r6agey + raedyrs_e+ wealth5 + r6smokev + r6drink + r6psyche")),data = filter(panel_data,!is.na(get(outcome))),model = model) %>% summary()
              
              outcome3 <- as.data.frame(outcome1$coefficients)[2:5,] %>% mutate(outcome=outcome,p=pFtest(m2, m1)$p.value) %>% 
                mutate(est=Estimate, stderr=`Std. Error`,lci=est-1.96*stderr, hci=est+1.96*stderr, beta=paste0(round(est,digits=2), " (",round(lci,digits=2)," to ",round(hci,digits=2),")")) %>% select(est,lci,hci,beta,outcome,p)
              
            }
      
    interaction_else <- rbind(interaction_else,outcome3)
    }
  return(interaction_else)
  }
  
  interaction_else <-  interact_else()
  ##Dementia
  outcome1 <- coxph(Surv(dem_time, new_dem) ~ (as.factor(frail))+(as.factor(comorbid))+ragender+r6agey+raedyrs_e+wealth5 + r6smokev + r6drink, data = filter(w6,r6demene!=1))
  outcome2 <- coxph(Surv(dem_time, new_dem) ~ as.factor(frail)*as.factor(comorbid)+ragender+r6agey+raedyrs_e+wealth5 + r6smokev + r6drink, data = filter(w6,r6demene!=1))
  outcome3 <- as.data.frame(summary(outcome1)$coefficients)[1:4, ] %>%
    mutate(
      outcome = "dementia",
      p = anova(outcome1, outcome2, test = "LRT")$`Pr(>|Chi|)`[2],  
      HR = exp(coef),                                 
      lci = exp(coef - 1.96 * `se(coef)`),
      hci = exp(coef + 1.96 * `se(coef)`),
      beta = paste0(round(HR, 2), " (", round(lci, 2), "–", round(hci, 2), ")")
    ) %>%
    select(HR, lci, hci, beta, outcome, p)
  outcome3
  
  inter_else <- bind_rows(outcome3 %>% rename(est=HR), interaction_else)

  ### CHARLS w2-w4, 2012-2018####
  charls <- read_dta("E:/Tutor/Open aging cohort/CHARLS/Harmonized Dataset/H_CHARLS_D_Data.dta")
  w2 <- charls %>% filter(r2iwstat==1) %>% 
    mutate(bmi_1_2=ifelse(!is.na(r2mweight)&!is.na(r1mweight),(r2mweight-r1mweight)/r1mweight,NA),
           bmi_c_2=ifelse(bmi_1_2<=-0.05,1,0),
           tired=ifelse(r2effortl>=3|r2goingl>=3,1,0),
           lowpa=ifelse(r2vgact_c==0&r2mdact_c==0,1,0)) %>% 
    group_by(ragender, r2mheight) %>%
    mutate(slow = ifelse(r2wspeed<=quantile(r2wspeed, 0.2, na.rm = TRUE)|r2walkcomp>=2,1,0)) %>% 
    ungroup() %>% 
    mutate(r2mgrip=pmax(r2lgrip,r2rgrip)) %>%
    group_by(ragender, r2mbmicata) %>%
    mutate(weak = ifelse(r2mgrip<=quantile(r2mgrip, 0.2, na.rm = TRUE),1,0)) %>% 
    ungroup() %>% 
    mutate(sum=rowSums(.[c("bmi_c_2", "tired", "lowpa","slow","weak")], na.rm = TRUE),
           frail=case_when(sum>=3~3,
                           sum==0~1,
                           T~2)) %>% 
    filter(rowSums(!is.na(select(., bmi_c_2, tired, lowpa, slow, weak))) >= 4) %>%
    mutate( t2=ifelse(r2iwstat==1,0,NA),
            t3=ifelse(r3iwstat==1,2,NA),
            t4=ifelse(r4iwstat==1,6,NA),
            dep_04=ifelse(r4cesd10>=12,1,0),
            dep_03=ifelse(r3cesd10>=12,1,0),
            dep_02=ifelse(r2cesd10>=12,1,0),
            new_dep=pmax(dep_03,dep_04,na.rm = TRUE),
            time_dep=case_when(
              dep_03==1~2,
              dep_04==1~6,
              T~NA),
            dep_time=ifelse(new_dep==1,time_dep,pmax(t2,t3,t4,na.rm = TRUE)),
            dem_04=ifelse(r4memrye==1,1,0),
            dem_03=ifelse(r3memrye==1,1,0),
            dem_02=ifelse(r2memrye==1,1,0),
            new_dem=pmax(dem_03,dem_04,na.rm = TRUE),
            time_dem=case_when(
              dem_03==1~2,
              dem_04==1~6,
              T~NA),
            dem_time=ifelse(new_dem==1,time_dem,pmax(t2,t3,t4,na.rm = TRUE))
    ) %>%
    mutate(con_sum= rowSums(across(c("r2hibpe", "r2diabe", "r2hearte", "r2stroke"), ~ pmax(as.numeric(.), 0)), na.rm = TRUE),
           comorbid=case_when(con_sum>=2~3,
                              con_sum==0~1,
                              T~2),
           r4health=ifelse(r4shlta>=4,1,0)
    ) %>%
    mutate(frail_comorbid=ifelse(frail==3,paste(frail, comorbid, sep = "_"),frail),
           frail_comorbid2=paste(frail, comorbid, sep = "_")
    ) %>%  
    mutate(
      t_1=ifelse(r2iwstat==1,2012,NA),
      t_2=ifelse(r3iwstat==1,2014,NA),
      t_3=ifelse(r4iwstat==1,2018,NA)
    ) %>%
    mutate(
      r2shlta=ifelse(r2shlta>=0,6-r2shlta,NA),
      r3shlta=ifelse(r3shlta>=0,6-r3shlta,NA),
      r4shlta=ifelse(r4shlta>=0,6-r4shlta,NA)
    ) %>%
    mutate(
      r2all=rowSums(.[c("r2tr20", "r2orient", "r2ser7")] * (.[c("r2tr20", "r2orient", "r2ser7")] >= 0), na.rm = TRUE),
      r3all=rowSums(.[c("r3tr20", "r3orient", "r3ser7")] * (.[c("r3tr20", "r3orient", "r3ser7")] >= 0), na.rm = TRUE),
      r4all=rowSums(.[c("r4tr20", "r4orient", "r4ser7")] * (.[c("r4tr20", "r4orient", "r4ser7")] >= 0), na.rm = TRUE)
    ) %>%
    mutate(wealth5= ntile(hh2atoth, 5)) %>%
    mutate_at(vars(c("r2whappyl","r3whappyl","r4whappyl",
                     "r2shlta", "r3shlta","r4shlta",
                     "r2satlife","r3satlife","r4satlife",
                     "r2cesd10", "r3cesd10", "r4cesd10",
                     "r2tr20", "r2orient", "r2ser7","r2all",
                     "r3tr20", "r3orient", "r3ser7","r3all",
                     "r4tr20", "r4orient", "r4ser7","r4all"
    )), scale)
  
  charls_fig <- function(){
    w2_long <- w2 %>% select(c(ID,frail_comorbid2,frail, comorbid,
                               "r2whappyl","r3whappyl","r4whappyl",
                               "r2shlta", "r3shlta","r4shlta",
                               "r2satlife","r3satlife","r4satlife",
                               "r2cesd10", "r3cesd10", "r4cesd10",
                               "r2tr20", "r2orient", "r2ser7","r2all",
                               "r3tr20", "r3orient", "r3ser7","r3all",
                               "r4tr20", "r4orient", "r4ser7","r4all",
                               t_1,t_2,t_3))
    
    
    {#OLS regression
      w2$frail_comorbid2 <- factor(w2$frail_comorbid2, levels = c("1_1", "1_2", "1_3", "2_1", "2_2", "2_3", "3_1", "3_2", "3_3"))
      dummy_variables <- model.matrix(~ frail_comorbid2 - 1, data = w2)
      w2_ols <- cbind(w2, dummy_variables) %>% select(-frail_comorbid2)
      
      out_cesd <- lm(r4cesd10 ~ frail_comorbid21_2 + frail_comorbid21_3 + frail_comorbid22_1 +frail_comorbid22_2 + frail_comorbid22_3 + frail_comorbid23_1 +frail_comorbid23_2 + frail_comorbid23_3 + ragender + r2agey + raeduc_c+ r2cesd10, data = w2_ols) %>% summary()
      out_sf <- lm(r4satlife ~ frail_comorbid21_2 + frail_comorbid21_3 + frail_comorbid22_1 +frail_comorbid22_2 + frail_comorbid22_3 + frail_comorbid23_1 +frail_comorbid23_2 + frail_comorbid23_3 + ragender + r2agey + raeduc_c +r2satlife, data = w2_ols) %>% summary()
      out_ha <- lm(r4whappyl ~ frail_comorbid21_2 + frail_comorbid21_3 + frail_comorbid22_1 +frail_comorbid22_2 + frail_comorbid22_3 + frail_comorbid23_1 +frail_comorbid23_2 + frail_comorbid23_3 + ragender + r2agey + raeduc_c +r2whappyl, data = w2_ols) %>% summary()
      out_sh <- lm(r4shlta ~ frail_comorbid21_2 + frail_comorbid21_3 + frail_comorbid22_1 +frail_comorbid22_2 + frail_comorbid22_3 + frail_comorbid23_1 +frail_comorbid23_2 + frail_comorbid23_3 + ragender + r2agey + raeduc_c +r2shlta, data = w2_ols) %>% summary()
      
      out_tr <- lm(r4tr20 ~ r2tr20+frail_comorbid21_2 + frail_comorbid21_3 + frail_comorbid22_1 +frail_comorbid22_2 + frail_comorbid22_3 + frail_comorbid23_1 +frail_comorbid23_2 + frail_comorbid23_3 + ragender + r2agey + raeduc_c, data = filter(w2_ols,dem_02 != 1)) %>% summary()
      out_or <- lm(r4orient ~ r2orient+frail_comorbid21_2 + frail_comorbid21_3 + frail_comorbid22_1 +frail_comorbid22_2 + frail_comorbid22_3 + frail_comorbid23_1 +frail_comorbid23_2 + frail_comorbid23_3 + ragender + r2agey + raeduc_c, data = filter(w2_ols,dem_02 != 1)) %>% summary()
      out_se <- lm(r4ser7 ~ r2ser7+frail_comorbid21_2 + frail_comorbid21_3 + frail_comorbid22_1 +frail_comorbid22_2 + frail_comorbid22_3 + frail_comorbid23_1 +frail_comorbid23_2 + frail_comorbid23_3 + ragender + r2agey + raeduc_c, data = filter(w2_ols,dem_02 != 1)) %>% summary()
      out_all <- lm(r4all ~ r2all+frail_comorbid21_2 + frail_comorbid21_3 + frail_comorbid22_1 +frail_comorbid22_2 + frail_comorbid22_3 + frail_comorbid23_1 +frail_comorbid23_2 + frail_comorbid23_3 + ragender + r2agey + raeduc_c, data = filter(w2_ols,dem_02 != 1)) %>% summary()
      
      # Mental wellbeing
      f1 <- as.data.frame(out_cesd$coefficients)[2:9,] %>% mutate(analysis = "Depressive symptom") %>% add_row(analysis = "Depressive symptom") %>% add_row(analysis = "Depressive symptom") %>% add_row(analysis = "Depressive symptom") %>% add_row(analysis = "Depressive symptom")
      f2 <- as.data.frame(out_sf$coefficients)[2:9,] %>% mutate(analysis = "Life satisfaction") %>% add_row(analysis = "Life satisfaction") %>% add_row(analysis = "Life satisfaction") %>% add_row(analysis = "Life satisfaction") %>% add_row(analysis = "Life satisfaction")
      f3 <- as.data.frame(out_ha$coefficients)[2:9,] %>% mutate(analysis = "Happiness") %>% add_row(analysis = "Happiness") %>% add_row(analysis = "Happiness") %>% add_row(analysis = "Happiness") %>% add_row(analysis = "Happiness")
      f4 <- as.data.frame(out_sh$coefficients)[2:9,] %>% mutate(analysis = "Self-reported health") %>% add_row(analysis = "Self-reported health") %>% add_row(analysis = "Self-reported health") %>% add_row(analysis = "Self-reported health") %>% add_row(analysis = "Self-reported health")
      
      forest_data <- rbind(f1, f2, f3, f4)
      forest_data$Label <- rep(c("1", "≥2", "0", "1", "≥2", "0", "1", "≥2", "0 (Ref)", "Non-frail", "Pre-frail", "Frail"), times = 4)
      forest_data$seq <- rep(c(3, 4, 6, 7, 8, 10, 11, 12, 2, 1, 5, 9), times = 4)
      forest_data <- forest_data %>% arrange(seq) %>% mutate(seq = as.factor(seq),
                                                             est = Estimate,
                                                             se = `Std. Error`,
                                                             est = ifelse(seq == 2, 0, est),
                                                             subgroup = ifelse(is.na(est) == FALSE, paste0(seq, Label), est),
                                                             lci = est - 1.96 * se,
                                                             uci = est + 1.96 * se)
      
      row_labels <- forest_data %>% filter(analysis == "Depressive symptom") %>% select(subgroup, Label)
      
      p2_ols <- forest_plot(split(forest_data, ~analysis), col.key = "subgroup", row.labels = row_labels, exponentiate = F,
                            scalepoints = T, nullval = 0, xlab = "",
                            plot.margin = margin(1, 4, 1, 4, "mm"),
                            xlim = c(-1.7, 1.7), row.labels.heading="CHARLS", base_size = 7,pointsize = 2,quiet = TRUE,panel.headings = rep("", 4),
                            xticks = c(-1.5, -1, -0.5, 0, 0.5, 1.0, 1.5),
                            base_line_size = 0.2)
      p2_ols$plot <- p2_ols$plot + theme(panel.spacing.x  = unit(18, "mm"))
      
      
      # Cognition
      f5 <- as.data.frame(out_tr$coefficients)[3:10,] %>% mutate(analysis = "Memory") %>% add_row(analysis = "Memory") %>% add_row(analysis = "Memory") %>% add_row(analysis = "Memory") %>% add_row(analysis = "Memory")
      f6 <- as.data.frame(out_or$coefficients)[3:10,] %>% mutate(analysis = "Orientation") %>% add_row(analysis = "Orientation") %>% add_row(analysis = "Orientation") %>% add_row(analysis = "Orientation") %>% add_row(analysis = "Orientation")
      f7 <- as.data.frame(out_se$coefficients)[3:10,] %>% mutate(analysis = "Numeracy") %>% add_row(analysis = "Numeracy") %>% add_row(analysis = "Numeracy") %>% add_row(analysis = "Numeracy") %>% add_row(analysis = "Numeracy")
      f8 <- as.data.frame(out_all$coefficients)[3:10,] %>% mutate(analysis = "Global cognitive function") %>% add_row(analysis = "Global cognitive function") %>% add_row(analysis = "Global cognitive function") %>% add_row(analysis = "Global cognitive function") %>% add_row(analysis = "Global cognitive function")
      forest_data2 <- rbind(f5, f6, f7, f8)
      
      forest_data2$Label <- rep(c("1", "≥2", "0", "1", "≥2", "0", "1", "≥2", "0 (Ref)", "Non-frail", "Pre-frail", "Frail"), times = 4)
      forest_data2$seq <- rep(c(3, 4, 6, 7, 8, 10, 11, 12, 2, 1, 5, 9), times = 4)
      forest_data2 <- forest_data2 %>% arrange(seq) %>% mutate(seq = as.factor(seq),
                                                               est = Estimate,
                                                               se = `Std. Error`,
                                                               est = ifelse(seq == 2, 0, est),
                                                               subgroup = ifelse(is.na(est) == FALSE, paste0(seq, Label), est),
                                                               lci = est - 1.96 * se,
                                                               uci = est + 1.96 * se)
      
      row_labels <- forest_data2 %>% filter(analysis == "Memory") %>% select(subgroup, Label)
      
      p2b_ols <- forest_plot(split(forest_data2, ~analysis), col.key = "subgroup", row.labels = row_labels, exponentiate = FALSE,
                             scalepoints = TRUE, nullval = 0, xlab = "",
                             plot.margin = margin(1, 4, 1, 4, "mm"),
                             row.labels.heading="CHARLS", base_size = 7,quiet = TRUE,panel.headings = rep("", 4),
                             xlim        = c(-0.75, 0.75),
                             xticks      = c(-0.6, -0.4,-0.2,0,0.2,0.4,0.6),
                             base_line_size = 0.2)
      p2b_ols$plot <- p2b_ols$plot + theme(panel.spacing.x  = unit(18, "mm"))
      
      
    }
    
    df1 <- pivot_longer(w2_long, cols = starts_with("t_"), names_to = "time", values_to = "year")
    df2 <- pivot_longer(w2_long, cols = ends_with("whappyl"), names_to = "variable", values_to = "ha")
    df3 <- pivot_longer(w2_long, cols = ends_with("satlife"), names_to = "variable", values_to = "sf")
    df4 <- pivot_longer(w2_long, cols = ends_with("cesd10"), names_to = "variable", values_to = "cesd")
    df5 <- pivot_longer(w2_long, cols = ends_with("shlta"), names_to = "variable", values_to = "sh")
    
    df6 <- pivot_longer(w2_long, cols = ends_with("tr20"), names_to = "variable", values_to = "tr")
    df7 <- pivot_longer(w2_long, cols = ends_with("orient"), names_to = "variable", values_to = "or")
    df8 <- pivot_longer(w2_long, cols = ends_with("ser7"), names_to = "variable", values_to = "se")
    df9 <- pivot_longer(w2_long, cols = ends_with("all"), names_to = "variable", values_to = "all")
    
    df_long <- bind_cols(select(df1, c(ID, frail_comorbid2, frail, comorbid, year)), select(df2, c(ha)), select(df3, c(sf)), select(df4, c(cesd)), select(df5, c(sh)),
                         select(df6, c(tr)), select(df7, c(or)), select(df8, c(se)), select(df9, c(all))) %>%
      filter(!is.na(year)) %>% left_join(select(w2, c(ID, ragender, r2agey, raeduc_c, wealth5, r2smokev, r2drinkev, r2psyche, r2memrye)), by = "ID")
    
    panel_data <- pdata.frame(df_long, index = c("ID","year"))
    
    fx_sf <- plm(sf ~ frail_comorbid2 + ragender + r2agey + raeduc_c+wealth5 + r2smokev + r2drinkev + r2psyche, data = filter(panel_data, !is.na(sf)), model = "random")
    fx_ha <- plm(ha ~ frail_comorbid2 + ragender + r2agey + raeduc_c+wealth5 + r2smokev + r2drinkev + r2psyche, data = filter(panel_data, !is.na(ha)), model = "random")
    fx_sh <- plm(sh ~ frail_comorbid2 + ragender + r2agey + raeduc_c+wealth5 + r2smokev + r2drinkev + r2psyche, data = filter(panel_data, !is.na(sh)), model = "random")
    fx_cesd <- plm(cesd ~ frail_comorbid2 + ragender + r2agey+raeduc_c+wealth5 + r2smokev + r2drinkev + r2psyche, data = filter(panel_data,!is.na(cesd)), model = "random")
    
    fx_tr <- plm(tr ~ frail_comorbid2 + ragender + r2agey + raeduc_c + wealth5 + r2smokev + r2drinkev + r2psyche, data = filter(panel_data, !is.na(tr) & r2memrye != 1), model = "random")
    fx_or <- plm(or ~ frail_comorbid2 + ragender + r2agey + raeduc_c + wealth5 + r2smokev + r2drinkev + r2psyche, data = filter(panel_data, !is.na(or) & r2memrye != 1), model = "random")
    fx_se <- plm(se ~ frail_comorbid2 + ragender + r2agey + raeduc_c + wealth5 + r2smokev + r2drinkev + r2psyche, data = filter(panel_data, !is.na(se) & r2memrye != 1), model = "random")
    fx_all <- plm(all ~ frail_comorbid2 + ragender + r2agey + raeduc_c + wealth5 + r2smokev + r2drinkev + r2psyche, data = filter(panel_data, !is.na(all) & r2memrye != 1), model = "random")
    
    out_cesd <- summary(fx_cesd)
    out_sf <- summary(fx_sf)
    out_ha <- summary(fx_ha)
    out_sh <- summary(fx_sh)
    out_tr <- summary(fx_tr)
    out_or <- summary(fx_or)
    out_se <- summary(fx_se)
    out_all <- summary(fx_all)
    
    f1 <- as.data.frame(out_cesd$coefficients)[2:9,] %>% mutate(analysis = "Depressive symptom") %>% add_row(analysis = "Depressive symptom") %>% add_row(analysis = "Depressive symptom") %>% add_row(analysis = "Depressive symptom") %>% add_row(analysis = "Depressive symptom")
    f2 <- as.data.frame(out_sf$coefficients)[2:9,] %>% mutate(analysis = "Life satisfaction") %>% add_row(analysis = "Life satisfaction") %>% add_row(analysis = "Life satisfaction") %>% add_row(analysis = "Life satisfaction") %>% add_row(analysis = "Life satisfaction")
    f3 <- as.data.frame(out_ha$coefficients)[2:9,] %>% mutate(analysis = "Happiness") %>% add_row(analysis = "Happiness") %>% add_row(analysis = "Happiness") %>% add_row(analysis = "Happiness") %>% add_row(analysis = "Happiness")
    f4 <- as.data.frame(out_sh$coefficients)[2:9,] %>% mutate(analysis = "Self-reported health") %>% add_row(analysis = "Self-reported health") %>% add_row(analysis = "Self-reported health") %>% add_row(analysis = "Self-reported health") %>% add_row(analysis = "Self-reported health")
    
    forest_data <- rbind(f1, f2, f3, f4)
    forest_data_charls <- rbind(f1, f2, f3, f4) %>% mutate(study="CHARLS",level=rep(c("1_2", "1_3", "2_1", "2_2", "2_3", "3_1", "3_2", "3_3", "", "", "",""),times=4))
    forest_data$Label <- rep(c("1", "≥2", "0", "1", "≥2", "0", "1", "≥2", "0 (Ref)", "Non-frail", "Pre-frail", "Frail"), times = 4)
    forest_data$seq <- rep(c(3, 4, 6, 7, 8, 10, 11, 12, 2, 1, 5, 9), times = 4)
    forest_data <- forest_data %>% arrange(seq) %>% mutate(seq = as.factor(seq),
                                                           est = Estimate,
                                                           se = `Std. Error`,
                                                           est = ifelse(seq == 2, 0, est),
                                                           subgroup = ifelse(is.na(est) == F, paste0(seq, Label), est),
                                                           lci = est - 1.96 * se,
                                                           uci = est + 1.96 * se)
    
    row_labels <- forest_data %>% filter(analysis == "Depressive symptom") %>% select(subgroup, Label)
    
    p2 <- forest_plot(split(forest_data, ~analysis), col.key = "subgroup", row.labels = row_labels, exponentiate = F,
                      scalepoints = T, nullval = 0, xlab = "",
                      # title = "CHARLS", 
                      plot.margin = margin(1, 4, 1, 4, "mm"),
                      xlim = c(-1.7, 1.7), row.labels.heading="CHARLS", base_size = 7,pointsize = 2,quiet = TRUE,panel.headings = rep("", 4),
                      xticks = c(-1.5, -1, -0.5, 0, 0.5, 1.0, 1.5),
                      base_line_size = 0.2)
    p2$plot <- p2$plot + theme(panel.spacing.x  = unit(18, "mm"))
    
    # Cognition
    f5 <- as.data.frame(out_tr$coefficients)[2:9,] %>% mutate(analysis = "Memory") %>% add_row(analysis = "Memory") %>% add_row(analysis = "Memory") %>% add_row(analysis = "Memory") %>% add_row(analysis = "Memory")
    f6 <- as.data.frame(out_or$coefficients)[2:9,] %>% mutate(analysis = "Orientation") %>% add_row(analysis = "Orientation") %>% add_row(analysis = "Orientation") %>% add_row(analysis = "Orientation") %>% add_row(analysis = "Orientation")
    f7 <- as.data.frame(out_se$coefficients)[2:9,] %>% mutate(analysis = "Numeracy") %>% add_row(analysis = "Numeracy") %>% add_row(analysis = "Numeracy") %>% add_row(analysis = "Numeracy") %>% add_row(analysis = "Numeracy")
    f8 <- as.data.frame(out_all$coefficients)[2:9,] %>% mutate(analysis = "Global cognitive function") %>% add_row(analysis = "Global cognitive function") %>% add_row(analysis = "Global cognitive function") %>% add_row(analysis = "Global cognitive function") %>% add_row(analysis = "Global cognitive function")
    
    forest_data2 <- rbind(f5, f6, f7, f8)
    forest_data2_charls <- rbind(f5, f6, f7, f8) %>% mutate(study="CHARLS",level=rep(c("1_2", "1_3", "2_1", "2_2", "2_3", "3_1", "3_2", "3_3", "", "", "",""),times=4))
    forest_data2$Label <- rep(c("1", "≥2", "0", "1", "≥2", "0", "1", "≥2", "0 (Ref)", "Non-frail", "Pre-frail", "Frail"), times = 4)
    forest_data2$seq <- rep(c(3, 4, 6, 7, 8, 10, 11, 12, 2, 1, 5, 9), times = 4)
    forest_data2 <- forest_data2 %>% arrange(seq) %>% mutate(seq = as.factor(seq),
                                                             est = Estimate,
                                                             se = `Std. Error`,
                                                             est = ifelse(seq == 2, 0, est),
                                                             subgroup = ifelse(is.na(est) == F, paste0(seq, Label), est),
                                                             lci = est - 1.96 * se,
                                                             uci = est + 1.96 * se)
    
    row_labels <- forest_data2 %>% filter(analysis == "Memory") %>% select(subgroup, Label)
    
    
    p2_b <- forest_plot(split(forest_data2, ~analysis), col.key = "subgroup", row.labels = row_labels, exponentiate = FALSE,
                        scalepoints = TRUE, nullval = 0, xlab = "β (95% CI)",
                        plot.margin = margin(1, 4, 1, 4, "mm"),
                        row.labels.heading="CHARLS", base_size = 7,quiet = TRUE,panel.headings = rep("", 4),
                        xlim        = c(-0.6, 0.6),
                        xticks      = c(-0.6, -0.4,-0.2,0,0.2,0.4,0.6),
                        base_line_size = 0.2)
    
    p2_b$plot <- p2_b$plot + theme(panel.spacing.x  = unit(18, "mm"))
    
    return(list(p2 = p2$plot, p2_b = p2_b$plot, p2_ols=p2_ols$plot, p2b_ols=p2b_ols$plot,df_long=df_long,forest=forest_data_charls,forest2=forest_data2_charls))
  }
  res_charls <- charls_fig()
  p2 <- res_charls$p2
  p2_b <- res_charls$p2_b
  p2_ols <- res_charls$p2_ols
  p2b_ols <- res_charls$p2b_ols
  charls_long <- res_charls$df_long
  forest_data_charls <- res_charls$forest
  forest_data2_charls <- res_charls$forest2
  
  
  #Dementia
  out_dem <- coxph(Surv(dem_time, new_dem) ~ relevel(as.factor(frail_comorbid2),ref="1_1")+ragender + r2agey + raeduc_c+wealth5 + r2smokev + r2drinkev + r2psyche, data = filter(w2,dem_02!=1)) %>% summary
  
  f9 <- as.data.frame(out_dem$coefficients)[1:8,] %>% mutate(analysis="Dementia") %>%  add_row(analysis = "Dementia")%>%  add_row(analysis = "Dementia")%>%  add_row(analysis = "Dementia")%>%  add_row(analysis = "Dementia")
  forest_data3_charls <- f9
  forest_data3 <-  rbind(f9)
  forest_data3$Label <- rep(c("1","≥2","0","1","≥2","0","1","≥2","0 (Ref)","Non-frail","Pre-frail","Frail"),times=1)
  forest_data3$seq <- rep(c(3,4,6,7,8,10,11,12,2,1,5,9),times=1)
  forest_data3<- forest_data3 %>% arrange(seq) %>% mutate(seq=as.factor(seq),
                                                          est=coef,
                                                          se=`se(coef)`,
                                                          est=ifelse(seq==2,0,est),
                                                          subgroup=ifelse(is.na(est)==F,paste0(seq,Label),est),
                                                          lci=est-1.96*se,
                                                          uci=est+1.96*se,
  )
  row_labels <- forest_data3 %>% filter(analysis=="Dementia") %>% select(subgroup,Label)
  p2_a <- forest_plot(split(forest_data3,~analysis), col.key = "subgroup",row.labels = row_labels, exponentiate = T,logscale=T,
                      scalepoints=T,nullval=1,xlab="HR (95% CI)",
                      row.labels.heading="CHARLS", base_size = 7,quiet = TRUE,
                      title="", plot.margin = margin(1, 4, 1, 4, "mm"),
                      #xlim        = c(-1, 1),
                      #xticks      = c(-1,-0.5,0,0.5,1.0),
                      base_line_size = 0.2)
  
  #Interaction of frailty and multimorbidity
  interact_charls <- function(){
    outcomes <- c("all","cesd","ha","sf","sh")
    interaction_charls <- data.frame()
    panel_data <- pdata.frame(charls_long, index = c("ID","year"))
    covar <- "+ragender + r2agey + raeduc_c + wealth5 + r2smokev + r2drinkev + r2psyche"
    for (outcome in outcomes) {
      
      if(outcome=="all"){
        #panel_data <- pdata.frame(df_long, index = c("year"))
        outcome1 <- plm(as.formula(paste0(outcome,"~as.factor(frail)+as.factor(comorbid)",covar)),data = filter(panel_data,!is.na(get(outcome)& r2memrye != 1)),model = "random") %>% summary()
        outcome2 <- plm(as.formula(paste0(outcome,"~as.factor(frail)*as.factor(comorbid)",covar)),data = filter(panel_data,!is.na(get(outcome)& r2memrye != 1)),model = "random") %>% summary()
        m1 <- plm(as.formula(paste0(outcome,"~as.factor(frail)+as.factor(comorbid)",covar)),data = filter(panel_data,!is.na(get(outcome)& r2memrye != 1)),model = "random")
        m2 <- plm(as.formula(paste0(outcome,"~as.factor(frail)*as.factor(comorbid)",covar)),data = filter(panel_data,!is.na(get(outcome)& r2memrye != 1)),model = "random")
      
         outcome3 <- as.data.frame(outcome1$coefficients)[2:5,] %>% mutate(outcome=outcome,p=pFtest(m2, m1)$p.value) %>% 
          mutate(est=Estimate, stderr=`Std. Error`,lci=est-1.96*stderr, hci=est+1.96*stderr, beta=paste0(round(est,digits=2), " (",round(lci,digits=2)," to ",round(hci,digits=2),")")) %>% select(est,lci,hci,beta,outcome,p)
      }
      else{
        #panel_data <- pdata.frame(df_long, index = c("idauniq","year"))
        outcome1 <- plm(as.formula(paste0(outcome,"~as.factor(frail)+as.factor(comorbid)",covar)),data = filter(panel_data,!is.na(get(outcome))),model = "random") %>% summary()
        outcome2 <- plm(as.formula(paste0(outcome,"~as.factor(frail)*as.factor(comorbid)",covar)),data = filter(panel_data,!is.na(get(outcome))),model = "random") %>% summary()
        m1 <- plm(as.formula(paste0(outcome,"~as.factor(frail)+as.factor(comorbid)",covar)),data = filter(panel_data,!is.na(get(outcome))),model = "random")
        m2 <- plm(as.formula(paste0(outcome,"~as.factor(frail)*as.factor(comorbid)",covar)),data = filter(panel_data,!is.na(get(outcome))),model = "random") 
      
        outcome3 <- as.data.frame(outcome1$coefficients)[2:5,] %>% mutate(outcome=outcome,p=pFtest(m2, m1)$p.value) %>% 
          mutate(est=Estimate, stderr=`Std. Error`,lci=est-1.96*stderr, hci=est+1.96*stderr, beta=paste0(round(est,digits=2), " (",round(lci,digits=2)," to ",round(hci,digits=2),")")) %>% select(est,lci,hci,beta,outcome,p)
      }
      interaction_charls <- rbind(interaction_charls,outcome3)
    }
    return(interaction_charls)
  }
  interaction_charls <- interact_charls()
  interaction_charls
  
  ##Dementia
  outcome1 <- coxph(Surv(dem_time, new_dem) ~ (as.factor(frail))+(as.factor(comorbid))+ragender + r2agey + raeduc_c+wealth5 + r2smokev + r2drinkev + r2psyche, data = filter(w2,dem_02!=1))
  outcome2 <- coxph(Surv(dem_time, new_dem) ~ as.factor(frail)*as.factor(comorbid)+ragender + r2agey + raeduc_c+wealth5 + r2smokev + r2drinkev + r2psyche, data = filter(w2,dem_02!=1)) 
  
  outcome3 <- as.data.frame(summary(outcome1)$coefficients)[1:4, ] %>%
    mutate(
      outcome = "dementia",
      p =anova(outcome1, outcome2, test = "LRT")$`Pr(>|Chi|)`[2],  
      HR = exp(coef),                                 
      lci = exp(coef - 1.96 * `se(coef)`),
      hci = exp(coef + 1.96 * `se(coef)`),
      beta = paste0(round(HR, 2), " (", round(lci, 2), "–", round(hci, 2), ")")
    ) %>%
    select(HR, lci, hci, beta, outcome, p)
  
  inter_charls <- bind_rows(outcome3 %>% rename(est=HR), interaction_charls)
  
### HRS w11-w14, 2012-2018####
               hrs_rand <- read_dta("E:/Tutor/Open aging cohort/HRS/randhrs1992_2020v1.dta")
               hrs_rand_subset <- hrs_rand %>%
                 select(c(ragender, r11agey_b, hhidpn, r11effort, r11going, r11cesd, r12cesd, r13cesd, r14cesd, r11vgactx, r11mdactx,
                          r11hearte, r11diabe, r11hibpe, r11stroke, r11shlt, r12shlt, r13shlt, r14shlt,
                          r11demene, r11alzhee, r12demen, r13demen, r14demen, r12alzhe, r13alzhe, r14alzhe,
                          r11imrc, r12imrc, r13imrc,
                          r11dlrc, r12dlrc, r13dlrc,
                          r11ser7, r12ser7, r13ser7,
                          r11drink, r11psych, h11atoth))
               
               hrs_all <- read_dta("E:/Tutor/Open aging cohort/HRS/H_HRS_d.dta")
               hrs <- hrs_all %>%left_join(hrs_rand_subset, by = "hhidpn")
               
               
               w11 <- hrs %>%
                 filter(r11iwmode >= 1) %>%
                 mutate(
                   bmi_9_11 = ifelse(!is.na(r11mweight) & !is.na(r9mweight), (r11mweight - r9mweight) / r9mweight, NA),
                   bmi_c_11 = ifelse(bmi_9_11 <= -0.1 | r11mbmicat == 1, 1, 0),
                   tired = ifelse(r11effort == 1 | r11going == 1, 1, 0),
                   lowpa = ifelse(r11vgactx == 5 & r11mdactx == 5, 1, 0)
                 ) %>%
                 group_by(ragender, r11mheight) %>%
                 mutate(slow = ifelse(r11wspeed <= quantile(r11wspeed, 0.2, na.rm = TRUE) | r11walkcomp == 0, 1, 0)) %>%
                 ungroup() %>%
                 mutate(r11mgrip = pmax(r11lgrip, r11rgrip)) %>%
                 group_by(ragender, r11mbmicat) %>%
                 mutate(weak = ifelse(r11mgrip <= quantile(r11mgrip, 0.2, na.rm = TRUE), 1, 0)) %>%
                 ungroup() %>%
                 mutate(
                   sum = rowSums(.[c("bmi_c_11", "tired", "lowpa", "slow", "weak")], na.rm = TRUE),
                   frail = case_when(
                     sum >= 3 ~ 3,
                     sum == 0 ~ 1,
                     T ~ 2
                   )
                 ) %>%
                 filter(rowSums(!is.na(select(., bmi_c_11, tired, lowpa, slow, weak))) >= 4) %>%
                 mutate(
                   t11 = ifelse(r11iwmode >= 1, 0, NA),
                   t12 = ifelse(r12iwmode >= 1, 2, NA),
                   t13 = ifelse(r13iwmode >= 1, 4, NA),
                   t14 = ifelse(r14iwmode >= 1, 6, NA),
                   dep_14 = ifelse(r14cesd >= 4, 1, 0),
                   dep_13 = ifelse(r13cesd >= 4, 1, 0),
                   dep_12 = ifelse(r12cesd >= 4, 1, 0),
                   dep_11 = ifelse(r11cesd >= 4, 1, 0),
                   new_dep = pmax(dep_12, dep_13, dep_14, na.rm = TRUE),
                   time_dep = case_when(
                     dep_12 == 1 ~ 2,
                     dep_13 == 1 ~ 4,
                     dep_14 == 1 ~ 6,
                     T ~ NA
                   ),
                   dep_time = ifelse(new_dep == 1, time_dep, pmax(t11, t12, t13, t14, na.rm = TRUE)),
                   dem_11 = ifelse(r11demene == 1 | r11alzhee == 1, 1, 0),
                   dem_12 = ifelse(r12demen == 1 | r12alzhe == 1, 1, 0),
                   dem_13 = ifelse(r13demen == 1 | r13alzhe == 1, 1, 0),
                   dem_14 = ifelse(r14demen == 1 | r14alzhe == 1, 1, 0),
                   new_dem = pmax(dem_11, dem_12, dem_13, dem_14, na.rm = TRUE),
                   time_dem = case_when(
                     dem_12 == 1 ~ 2,
                     dem_13 == 1 ~ 4,
                     dem_14 == 1 ~ 6,
                     T ~ NA
                   ),
                   dem_time = ifelse(new_dem == 1, time_dem, pmax(t11, t12, t13, t14, na.rm = TRUE))
                 ) %>%
                 mutate(
                   con_sum = rowSums(across(c("r11hibpe", "r11diabe", "r11hearte", "r11stroke"), ~pmax(as.numeric(.), 0)), na.rm = TRUE),
                   comorbid = case_when(
                     con_sum >= 2 ~ 3,
                     con_sum == 0 ~ 1,
                     T ~ 2
                   ),
                   r4health = ifelse(r11shlt >= 4, 1, 0)
                 ) %>%
                 mutate(
                   frail_comorbid = ifelse(frail == 3, paste(frail, comorbid, sep = "_"), frail),
                   frail_comorbid2 = paste(frail, comorbid, sep = "_")
                 ) %>%
                 mutate(
                   t_1 = ifelse(r11iwmode >= 1, 2012, NA),
                   t_2 = ifelse(r12iwmode >= 1, 2014, NA),
                   t_3 = ifelse(r13iwmode >= 1, 2016, NA),
                   t_4 = ifelse(r14iwmode >= 1, 2018, NA)
                 ) %>%
                 mutate(
                   r11shlt = ifelse(r11shlt >= 0, 6 - r11shlt, NA),
                   r12shlt = ifelse(r12shlt >= 0, 6 - r12shlt, NA),
                   r13shlt = ifelse(r13shlt >= 0, 6 - r13shlt, NA),
                   r14shlt = ifelse(r14shlt >= 0, 6 - r14shlt, NA)
                 ) %>%
                 mutate(r11tr20 = r11imrc + r11dlrc, r12tr20 = r12imrc + r12dlrc, r13tr20 = r13imrc + r13dlrc) %>%
                 mutate(
                   r14tr20 = NA, r14ser7 = NA, r14orient = NA,
                   r11all = rowSums(.[c("r11tr20", "r11orient", "r11ser7")] * (.[c("r11tr20", "r11orient", "r11ser7")] >= 0), na.rm = TRUE),
                   r12all = rowSums(.[c("r12tr20", "r12orient", "r12ser7")] * (.[c("r12tr20", "r12orient", "r12ser7")] >= 0), na.rm = TRUE),
                   r13all = rowSums(.[c("r13tr20", "r13orient", "r13ser7")] * (.[c("r13tr20", "r13orient", "r13ser7")] >= 0), na.rm = TRUE),
                   r14all = NA
                 ) %>%
                 mutate(wealth5 = ntile(h11atoth, 5)) %>%
                 mutate_at(vars(c("r11fhappy", "r12fhappy", "r13fhappy", "r14fhappy",
                                  "r11shlt", "r12shlt", "r13shlt", "r14shlt",
                                  "r11lstsf", "r12lstsf", "r13lstsf", "r14lstsf",
                                  "r11cesd", "r12cesd", "r13cesd", "r14cesd",
                                  "r11tr20", "r11orient", "r11ser7", "r11all",
                                  "r12tr20", "r12orient", "r12ser7", "r12all",
                                  "r13tr20", "r13orient", "r13ser7", "r13all",
                                  "r14tr20", "r14orient", "r14ser7", "r14all",
                                  "r4health")), scale)
            hrs_fig <- function(){
              
               w11_long <- w11 %>%
                 select(c(hhidpn, frail_comorbid2,frail,comorbid,
                          "r11fhappy", "r12fhappy", "r13fhappy", "r14fhappy",
                          "r11shlt", "r12shlt", "r13shlt", "r14shlt",
                          "r11lstsf", "r12lstsf", "r13lstsf", "r14lstsf",
                          "r11cesd", "r12cesd", "r13cesd", "r14cesd",
                          "r11tr20", "r11orient", "r11ser7", "r11all",
                          "r12tr20", "r12orient", "r12ser7", "r12all",
                          "r13tr20", "r13orient", "r13ser7", "r13all",
                          "r14tr20", "r14orient", "r14ser7", "r14all",
                          t_1, t_2, t_3, t_4))

               
               {#OLS regression
                 w11$frail_comorbid2 <- factor(w11$frail_comorbid2, levels = c("1_1", "1_2", "1_3", "2_1", "2_2", "2_3", "3_1", "3_2", "3_3"))
                 dummy_variables <- model.matrix(~ frail_comorbid2 - 1, data = w11)
                 w11_ols <- cbind(w11, dummy_variables) %>% select(-frail_comorbid2)
                 
                 out_cesd <- lm(r14cesd ~ frail_comorbid21_2 + frail_comorbid21_3 + frail_comorbid22_1 +frail_comorbid22_2 + frail_comorbid22_3 + frail_comorbid23_1 +frail_comorbid23_2 + frail_comorbid23_3 +ragender + r11agey_b+raeducl+ r11cesd, data = w11_ols) %>% summary()
                 out_sf <- lm(r13lstsf ~ frail_comorbid21_2 + frail_comorbid21_3 + frail_comorbid22_1 +frail_comorbid22_2 + frail_comorbid22_3 + frail_comorbid23_1 +frail_comorbid23_2 + frail_comorbid23_3 + ragender + r11agey_b+raeducl +r11lstsf, data = w11_ols) %>% summary()
                 out_ha <- lm(r13fhappy ~ frail_comorbid21_2 + frail_comorbid21_3 + frail_comorbid22_1 +frail_comorbid22_2 + frail_comorbid22_3 + frail_comorbid23_1 +frail_comorbid23_2 + frail_comorbid23_3 + ragender + r11agey_b+raeducl +r11fhappy, data = w11_ols) %>% summary()
                 out_sh <- lm(r14shlt ~ frail_comorbid21_2 + frail_comorbid21_3 + frail_comorbid22_1 +frail_comorbid22_2 + frail_comorbid22_3 + frail_comorbid23_1 +frail_comorbid23_2 + frail_comorbid23_3 +ragender + r11agey_b+raeducl +r11shlt, data = w11_ols) %>% summary()
                 
                 out_tr <- lm(r13tr20 ~ r11tr20+frail_comorbid21_2 + frail_comorbid21_3 + frail_comorbid22_1 +frail_comorbid22_2 + frail_comorbid22_3 + frail_comorbid23_1 +frail_comorbid23_2 + frail_comorbid23_3 + ragender + r11agey_b+raeducl, data = filter(w11_ols,dem_11 != 1)) %>% summary()
                 out_or <- lm(r13orient ~ r11orient+frail_comorbid21_2 + frail_comorbid21_3 + frail_comorbid22_1 +frail_comorbid22_2 + frail_comorbid22_3 + frail_comorbid23_1 +frail_comorbid23_2 + frail_comorbid23_3 + ragender + r11agey_b+raeducl, data = filter(w11_ols,dem_11 != 1)) %>% summary()
                 out_se <- lm(r13ser7 ~ r11ser7+frail_comorbid21_2 + frail_comorbid21_3 + frail_comorbid22_1 +frail_comorbid22_2 + frail_comorbid22_3 + frail_comorbid23_1 +frail_comorbid23_2 + frail_comorbid23_3 + ragender + r11agey_b+raeducl, data = filter(w11_ols,dem_11 != 1)) %>% summary()
                 out_all <- lm(r13all ~ r11all+frail_comorbid21_2 + frail_comorbid21_3 + frail_comorbid22_1 +frail_comorbid22_2 + frail_comorbid22_3 + frail_comorbid23_1 +frail_comorbid23_2 + frail_comorbid23_3 + ragender + r11agey_b+raeducl, data = filter(w11_ols,dem_11 != 1)) %>% summary()
                 
                 # Mental wellbeing
                 f1 <- as.data.frame(out_cesd$coefficients)[2:9,] %>% mutate(analysis = "Depressive symptom") %>% add_row(analysis = "Depressive symptom") %>% add_row(analysis = "Depressive symptom") %>% add_row(analysis = "Depressive symptom") %>% add_row(analysis = "Depressive symptom")
                 f2 <- as.data.frame(out_sf$coefficients)[2:9,] %>% mutate(analysis = "Life satisfaction") %>% add_row(analysis = "Life satisfaction") %>% add_row(analysis = "Life satisfaction") %>% add_row(analysis = "Life satisfaction") %>% add_row(analysis = "Life satisfaction")
                 f3 <- as.data.frame(out_ha$coefficients)[2:9,] %>% mutate(analysis = "Happiness") %>% add_row(analysis = "Happiness") %>% add_row(analysis = "Happiness") %>% add_row(analysis = "Happiness") %>% add_row(analysis = "Happiness")
                 f4 <- as.data.frame(out_sh$coefficients)[2:9,] %>% mutate(analysis = "Self-reported health") %>% add_row(analysis = "Self-reported health") %>% add_row(analysis = "Self-reported health") %>% add_row(analysis = "Self-reported health") %>% add_row(analysis = "Self-reported health")
                 
                 forest_data <- rbind(f1, f2, f3, f4)
                 forest_data$Label <- rep(c("1", "≥2", "0", "1", "≥2", "0", "1", "≥2", "0 (Ref)", "Non-frail", "Pre-frail", "Frail"), times = 4)
                 forest_data$seq <- rep(c(3, 4, 6, 7, 8, 10, 11, 12, 2, 1, 5, 9), times = 4)
                 forest_data <- forest_data %>% arrange(seq) %>% mutate(seq = as.factor(seq),
                                                                        est = Estimate,
                                                                        se = `Std. Error`,
                                                                        est = ifelse(seq == 2, 0, est),
                                                                        subgroup = ifelse(is.na(est) == FALSE, paste0(seq, Label), est),
                                                                        lci = est - 1.96 * se,
                                                                        uci = est + 1.96 * se)
                 
                 row_labels <- forest_data %>% filter(analysis == "Depressive symptom") %>% select(subgroup, Label)
                 
                 p3_ols <- forest_plot(split(forest_data, ~analysis), col.key = "subgroup", row.labels = row_labels, exponentiate = FALSE,
                                       scalepoints = TRUE, nullval = 0, xlab = "β (95% CI)",
                                       row.labels.heading="HRS", plot.margin = margin(1, 4, 1, 4, "mm"),
                                       xlim = c(-1.7, 1.7),base_size = 7,pointsize = 2,quiet = TRUE,
                                       xticks = c(-1.5, -1, -0.5, 0, 0.5, 1.0, 1.5),
                                       base_line_size = 0.2)
                 p3_ols$plot <- p3_ols$plot + theme(panel.spacing.x  = unit(18, "mm"))
                 
                 # Cognition
                 f5 <- as.data.frame(out_tr$coefficients)[3:10,] %>% mutate(analysis = "Memory") %>% add_row(analysis = "Memory") %>% add_row(analysis = "Memory") %>% add_row(analysis = "Memory") %>% add_row(analysis = "Memory")
                 f6 <- as.data.frame(out_or$coefficients)[3:10,] %>% mutate(analysis = "Orientation") %>% add_row(analysis = "Orientation") %>% add_row(analysis = "Orientation") %>% add_row(analysis = "Orientation") %>% add_row(analysis = "Orientation")
                 f7 <- as.data.frame(out_se$coefficients)[3:10,] %>% mutate(analysis = "Numeracy") %>% add_row(analysis = "Numeracy") %>% add_row(analysis = "Numeracy") %>% add_row(analysis = "Numeracy") %>% add_row(analysis = "Numeracy")
                 f8 <- as.data.frame(out_all$coefficients)[3:10,] %>% mutate(analysis = "Global cognitive function") %>% add_row(analysis = "Global cognitive function") %>% add_row(analysis = "Global cognitive function") %>% add_row(analysis = "Global cognitive function") %>% add_row(analysis = "Global cognitive function")
                 forest_data2 <- rbind(f5, f6, f7, f8)
                 
                 forest_data2$Label <- rep(c("1", "≥2", "0", "1", "≥2", "0", "1", "≥2", "0 (Ref)", "Non-frail", "Pre-frail", "Frail"), times = 4)
                 forest_data2$seq <- rep(c(3, 4, 6, 7, 8, 10, 11, 12, 2, 1, 5, 9), times = 4)
                 forest_data2 <- forest_data2 %>% arrange(seq) %>% mutate(seq = as.factor(seq),
                                                                          est = Estimate,
                                                                          se = `Std. Error`,
                                                                          est = ifelse(seq == 2, 0, est),
                                                                          subgroup = ifelse(is.na(est) == FALSE, paste0(seq, Label), est),
                                                                          lci = est - 1.96 * se,
                                                                          uci = est + 1.96 * se)
                 
                 row_labels <- forest_data2 %>% filter(analysis == "Memory") %>% select(subgroup, Label)
                 
                 p3b_ols <- forest_plot(split(forest_data2, ~analysis), col.key = "subgroup", row.labels = row_labels, exponentiate = FALSE,
                                        scalepoints = TRUE, nullval = 0, xlab = "β (95% CI)",
                                        row.labels.heading="HRS",
                                        plot.margin = margin(1, 4, 1, 4, "mm"), base_size = 7,pointsize = 2,quiet = TRUE,
                                        xlim        = c(-0.75, 0.75),
                                        xticks      = c(-0.6, -0.4,-0.2,0,0.2,0.4,0.6),
                                        base_line_size = 0.2)
                 p3b_ols$plot <- p3b_ols$plot + theme(panel.spacing.x  = unit(18, "mm"))
                 

                 }

               df1 <- pivot_longer(w11_long, cols = starts_with("t_"), names_to = "time", values_to = "year")
               df2 <- pivot_longer(w11_long, cols = ends_with("happy"), names_to = "variable", values_to = "ha")
               df3 <- pivot_longer(w11_long, cols = ends_with("stsf"), names_to = "variable", values_to = "sf")
               df4 <- pivot_longer(w11_long, cols = ends_with("cesd"), names_to = "variable", values_to = "cesd")
               df5 <- pivot_longer(w11_long, cols = ends_with("shlt"), names_to = "variable", values_to = "sh")
               df6 <- pivot_longer(w11_long, cols = ends_with("tr20"), names_to = "variable", values_to = "tr")
               df7 <- pivot_longer(w11_long, cols = ends_with("orient"), names_to = "variable", values_to = "or")
               df8 <- pivot_longer(w11_long, cols = ends_with("ser7"), names_to = "variable", values_to = "se")
               df9 <- pivot_longer(w11_long, cols = ends_with("all"), names_to = "variable", values_to = "all")
               


               df_long <- bind_cols(select(df1, c(hhidpn, frail_comorbid2, frail,comorbid, year)), select(df2, c(ha)), select(df3, c(sf)), select(df4, c(cesd)), select(df5, c(sh)),
                                    select(df6, c(tr)), select(df7, c(or)), select(df8, c(se)), select(df9, c(all))) %>%
                 filter(!is.na(year)) %>% left_join(select(w11, c(hhidpn, ragender, r11agey_b, raeducl, wealth5, r11smokef, r11drink, r11psych, r11demene)), by = "hhidpn")
               
               panel_data <- pdata.frame(df_long, index = c("hhidpn","year"))

               
               fx_cesd <- plm(cesd ~ frail_comorbid2 + ragender + r11agey_b+raeducl+ wealth5 + r11smokef + r11drink + r11psych, data = filter(panel_data,!is.na(cesd)), model = "random")
               fx_sf <- plm(sf ~ frail_comorbid2 + ragender + r11agey_b+raeducl+ wealth5 + r11smokef + r11drink + r11psych, data = filter(panel_data,!is.na(sf)), model = "random")
               fx_ha <- plm(ha ~ frail_comorbid2 + ragender + r11agey_b+raeducl+ wealth5 + r11smokef + r11drink + r11psych, data = filter(panel_data,!is.na(ha)), model = "random")
               fx_sh <- plm(sh ~ frail_comorbid2 + ragender + r11agey_b+raeducl+ wealth5 + r11smokef + r11drink + r11psych, data = filter(panel_data,!is.na(sh)), model = "random")
               
               fx_tr <- plm(tr ~ frail_comorbid2 + ragender + r11agey_b + raeducl + wealth5 + r11smokef + r11drink + r11psych, data = filter(panel_data, !is.na(tr) & r11demene != 1), model = "random")
               fx_or <- plm(or ~ frail_comorbid2 + ragender + r11agey_b + raeducl + wealth5 + r11smokef + r11drink + r11psych, data = filter(panel_data, !is.na(or) & r11demene != 1), model = "random")
               fx_se <- plm(se ~ frail_comorbid2 + ragender + r11agey_b + raeducl + wealth5 + r11smokef + r11drink + r11psych, data = filter(panel_data, !is.na(se) & r11demene != 1), model = "random")
               fx_all <- plm(all ~ frail_comorbid2 + ragender + r11agey_b + raeducl + wealth5 + r11smokef + r11drink + r11psych, data = filter(panel_data, !is.na(all) & r11demene != 1), model = "random")
               
               out_cesd <- summary(fx_cesd)
               out_sf <- summary(fx_sf)
               out_ha <- summary(fx_ha)
               out_sh <- summary(fx_sh)
               out_tr <- summary(fx_tr)
               out_or <- summary(fx_or)
               out_se <- summary(fx_se)
               out_all <- summary(fx_all)
               
               
               f1 <- as.data.frame(out_cesd$coefficients)[2:9,] %>% mutate(analysis="Depressive symptom") %>%  add_row(analysis = "Depressive symptom")%>%  add_row(analysis = "Depressive symptom")%>%  add_row(analysis = "Depressive symptom")%>%  add_row(analysis = "Depressive symptom")
               f2 <- as.data.frame(out_sf$coefficients)[2:9,] %>% mutate(analysis="Life satisfaction") %>%  add_row(analysis = "Life satisfaction")%>%  add_row(analysis = "Life satisfaction")%>%  add_row(analysis = "Life satisfaction")%>%  add_row(analysis = "Life satisfaction")
               f3 <- as.data.frame(out_ha$coefficients)[2:9,] %>% mutate(analysis="Happiness") %>%  add_row(analysis = "Happiness")%>%  add_row(analysis = "Happiness")%>%  add_row(analysis = "Happiness")%>%  add_row(analysis = "Happiness")
               f4 <- as.data.frame(out_sh$coefficients)[2:9,] %>% mutate(analysis="Self-reported health") %>%  add_row(analysis = "Self-reported health")%>%  add_row(analysis = "Self-reported health")%>%  add_row(analysis = "Self-reported health")%>%  add_row(analysis = "Self-reported health")
               forest_data <-  rbind(f1,f2,f3,f4)
               forest_data_hrs <- rbind(f1, f2, f3, f4) %>% mutate(study="HRS",level=rep(c("1_2", "1_3", "2_1", "2_2", "2_3", "3_1", "3_2", "3_3", "", "", "",""),times=4))
               
               forest_data$Label <- rep(c("1","≥2","0","1","≥2","0","1","≥2","0 (Ref)","Non-frail","Pre-frail","Frail"),times=4)
               forest_data$seq <- rep(c(3,4,6,7,8,10,11,12,2,1,5,9),times=4)
               forest_data<- forest_data %>% arrange(seq) %>% mutate(seq=as.factor(seq),
                                                                                   est=Estimate,
                                                                                   se=`Std. Error`,
                                                                                   est=ifelse(seq==2,0,est),
                                                                                   subgroup=ifelse(is.na(est)==F,paste0(seq,Label),est),
                                                                                   lci=est-1.96*se,
                                                                                   uci=est+1.96*se,
                                                                                   )
               row_labels <- forest_data %>% filter(analysis=="Depressive symptom") %>% select(subgroup,Label)
               
               p3 <- forest_plot(split(forest_data,~analysis), col.key = "subgroup",row.labels = row_labels, exponentiate = F,
                                               scalepoints=T,nullval=0, xlab="",
                                               xticks = c(-1.5, -1, -0.5, 0, 0.5, 1.0, 1.5), xlim = c(-1.7, 1.7),row.labels.heading="HRS",
                                               plot.margin = margin(1, 4, 1, 4, "mm"),base_size = 7,pointsize = 2,
                                               base_line_size = 0.2)
               p3$plot <- p3$plot + theme(panel.spacing.x  = unit(18, "mm"))
               
   
               f5 <- as.data.frame(out_tr$coefficients)[2:9,] %>% mutate(analysis = "Memory") %>% add_row(analysis = "Memory") %>% add_row(analysis = "Memory") %>% add_row(analysis = "Memory") %>% add_row(analysis = "Memory")
               f6 <- as.data.frame(out_or$coefficients)[2:9,] %>% mutate(analysis = "Orientation") %>% add_row(analysis = "Orientation") %>% add_row(analysis = "Orientation") %>% add_row(analysis = "Orientation") %>% add_row(analysis = "Orientation")
               f7 <- as.data.frame(out_se$coefficients)[2:9,] %>% mutate(analysis = "Numeracy") %>% add_row(analysis = "Numeracy") %>% add_row(analysis = "Numeracy") %>% add_row(analysis = "Numeracy") %>% add_row(analysis = "Numeracy")
               f8 <- as.data.frame(out_all$coefficients)[2:9,] %>% mutate(analysis = "Global cognitive function") %>% add_row(analysis = "Global cognitive function") %>% add_row(analysis = "Global cognitive function") %>% add_row(analysis = "Global cognitive function") %>% add_row(analysis = "Global cognitive function")
               
               forest_data2 <- rbind(f5, f6, f7, f8)
               forest_data2_hrs <- rbind(f5, f6, f7, f8) %>% mutate(study="HRS",level=rep(c("1_2", "1_3", "2_1", "2_2", "2_3", "3_1", "3_2", "3_3", "", "", "",""),times=4))
               
               forest_data2$Label <- rep(c("1", "≥2", "0", "1", "≥2", "0", "1", "≥2", "0 (Ref)", "Non-frail", "Pre-frail", "Frail"), times = 4)
               forest_data2$seq <- rep(c(3, 4, 6, 7, 8, 10, 11, 12, 2, 1, 5, 9), times = 4)
               
               forest_data2 <- forest_data2 %>%
                 arrange(seq) %>%
                 mutate(seq = as.factor(seq),
                        est = Estimate,
                        se = `Std. Error`,
                        est = ifelse(seq == 2, 0, est),
                        subgroup = ifelse(!is.na(est) == TRUE, paste0(seq, Label), est),
                        lci = est - 1.96 * se,
                        uci = est + 1.96 * se)
               
               row_labels <- forest_data2 %>% filter(analysis == "Memory") %>% select(subgroup, Label)
               
               p3_b <- forest_plot(split(forest_data2, ~analysis), col.key = "subgroup", row.labels = row_labels, exponentiate = F,
                                   scalepoints = T, nullval = 0, xlab = "",
                                   row.labels.heading="HRS", plot.margin = margin(1, 4, 1, 4, "mm"), base_size = 7,pointsize = 2,quiet = TRUE,
                                   xlim        = c(-0.6, 0.6),
                                   xticks      = c(-0.6, -0.4,-0.2,0,0.2,0.4,0.6),
                                   base_line_size = 0.2)
               
               p3_b$plot <- p3_b$plot + theme(panel.spacing.x  = unit(18, "mm"))
               
               return(list(p3 = p3$plot, p3_b = p3_b$plot, p3_ols=p3_ols$plot, p3b_ols=p3b_ols$plot,df_long=df_long,forest=forest_data_hrs,forest2=forest_data2_hrs))
               
              }
            res_hrs<- hrs_fig()
            p3 <- res_hrs$p3
            p3_b <- res_hrs$p3_b
            p3_ols <- res_hrs$p3_ols
            p3b_ols <- res_hrs$p3b_ols
            hrs_long <- res_hrs$df_long
            forest_data_hrs <- res_hrs$forest
            forest_data2_hrs <- res_hrs$forest2
              
#Dementia
              out_dem <- coxph(Surv(dem_time, new_dem) ~ relevel(as.factor(frail_comorbid2),ref="1_1")+ragender + r11agey_b+raeducl+ wealth5 + r11smokef + r11drink, data = filter(w11,dem_11!=1)) %>% summary
              # out_dem <- glm(new_dem ~ relevel(as.factor(frail_comorbid2), ref = "1_1") + ragender + r11agey_b + raeducl + wealth5 + r11smokef + r11drink, 
              #     data = filter(w11, dem_11 != 1), family = binomial)%>% summary
              
              
              f9 <- as.data.frame(out_dem$coefficients)[1:8,] %>% mutate(analysis="Dementia") %>%  add_row(analysis = "Dementia")%>%  add_row(analysis = "Dementia")%>%  add_row(analysis = "Dementia")%>%  add_row(analysis = "Dementia")
              forest_data3 <-  rbind(f9)
              forest_data3_hrs <-  f9
              forest_data3$Label <- rep(c("1","≥2","0","1","≥2","0","1","≥2","0 (Ref)","Non-frail","Pre-frail","Frail"),times=1)
              forest_data3$seq <- rep(c(3,4,6,7,8,10,11,12,2,1,5,9),times=1)
              forest_data3<- forest_data3 %>% arrange(seq) %>% mutate(seq=as.factor(seq),
                                                                      est=coef,
                                                                      se=`se(coef)`,
                                                                      est=ifelse(seq==2,0,est),
                                                                      subgroup=ifelse(is.na(est)==F,paste0(seq,Label),est),
                                                                      lci=est-1.96*se,
                                                                      uci=est+1.96*se,
              )
              row_labels <- forest_data3 %>% filter(analysis=="Dementia") %>% select(subgroup,Label)
              p3_a <- forest_plot(split(forest_data3,~analysis), col.key = "subgroup",row.labels = row_labels, exponentiate = T,logscale=T,
                                  scalepoints=T,nullval=1,xlab="HR (95% CI)",
                                  row.labels.heading="HRS", base_size = 7,pointsize = 2,quiet = TRUE,
                                  plot.margin = margin(1, 4, 1, 4, "mm"),
                                  #xlim        = c(-1, 1),
                                  #xticks      = c(-1,-0.5,0,0.5,1.0),
                                  base_line_size = 0.2)               
             
#Interaction of frailty and multimorbidity
            interact_hrs <- function(){
                outcomes <- c("all","cesd","ha","sf","sh")
                interaction_hrs <- data.frame()
                panel_data <- pdata.frame(hrs_long, index = c("hhidpn","year"))
                covar <- "+ragender + r11agey_b+raeducl+ wealth5 + r11smokef + r11drink + r11psych"
                for (outcome in outcomes) {
                  # if(outcome=="sf"){
                  #   panel_data <- pdata.frame(df_long, index = c("year"))
                  # }
                  # else{    
                  #   panel_data <- pdata.frame(df_long, index = c("idauniq","year"))
                  # }
                  
                  if(outcome=="all"){
                    #panel_data <- pdata.frame(df_long, index = c("year"))
                    outcome1 <- plm(as.formula(paste0(outcome,"~as.factor(frail)+as.factor(comorbid)",covar)),data = filter(panel_data,!is.na(get(outcome)& r11demene != 1)),model = "random") %>% summary()
                    outcome2 <- plm(as.formula(paste0(outcome,"~as.factor(frail)*as.factor(comorbid)",covar)),data = filter(panel_data,!is.na(get(outcome)& r11demene != 1)),model = "random") %>% summary()
                    
                    m1 <- plm(as.formula(paste0(outcome,"~as.factor(frail)+as.factor(comorbid)",covar)),data = filter(panel_data,!is.na(get(outcome)& r11demene != 1)),model = "random")
                    m2 <- plm(as.formula(paste0(outcome,"~as.factor(frail)*as.factor(comorbid)",covar)),data = filter(panel_data,!is.na(get(outcome)& r11demene != 1)),model = "random")
                    
                    
                    outcome3 <- as.data.frame(outcome1$coefficients)[2:5,] %>% mutate(outcome=outcome,p=pFtest(m2, m1)$p.value) %>% 
                      mutate(est=Estimate, stderr=`Std. Error`,lci=est-1.96*stderr, hci=est+1.96*stderr, beta=paste0(round(est,digits=2), " (",round(lci,digits=2)," to ",round(hci,digits=2),")")) %>% select(est,lci,hci,beta,outcome,p)
                  }
                  else{
                    #panel_data <- pdata.frame(df_long, index = c("idauniq","year"))
                    outcome1 <- plm(as.formula(paste0(outcome,"~as.factor(frail)+as.factor(comorbid)",covar)),data = filter(panel_data,!is.na(get(outcome))),model = "random") %>% summary()
                    outcome2 <- plm(as.formula(paste0(outcome,"~as.factor(frail)*as.factor(comorbid)",covar)),data = filter(panel_data,!is.na(get(outcome))),model = "random") %>% summary()
                    
                    m1 <- plm(as.formula(paste0(outcome,"~as.factor(frail)+as.factor(comorbid)",covar)),data = filter(panel_data,!is.na(get(outcome)& r11demene != 1)),model = "random")
                    m2 <- plm(as.formula(paste0(outcome,"~as.factor(frail)*as.factor(comorbid)",covar)),data = filter(panel_data,!is.na(get(outcome)& r11demene != 1)),model = "random")
                    
                    
                    
                    outcome3 <- as.data.frame(outcome1$coefficients)[2:5,] %>% mutate(outcome=outcome,p=pFtest(m2, m1)$p.value) %>% 
                      mutate(est=Estimate, stderr=`Std. Error`,lci=est-1.96*stderr, hci=est+1.96*stderr, beta=paste0(round(est,digits=2), " (",round(lci,digits=2)," to ",round(hci,digits=2),")")) %>% select(est,lci,hci,beta,outcome,p)
                  }
                  interaction_hrs <- rbind(interaction_hrs,outcome3)
                }
                return(interaction_hrs)
              }
            interaction_hrs <- interact_hrs()
              
              ##Dementia
              outcome1 <-coxph(Surv(dem_time, new_dem) ~ (as.factor(frail))+(as.factor(comorbid))+ragender + r11agey_b+raeducl+ wealth5 + r11smokef + r11drink, data = filter(w11,dem_11!=1))
              outcome2 <-coxph(Surv(dem_time, new_dem) ~ as.factor(frail)*as.factor(comorbid)+ragender + r11agey_b+raeducl+ wealth5 + r11smokef + r11drink, data = filter(w11,dem_11!=1))
              
              outcome3 <- as.data.frame(summary(outcome1)$coefficients)[1:4, ] %>%
                mutate(
                  outcome = "dementia",
                  p =anova(outcome1, outcome2, test = "LRT")$`Pr(>|Chi|)`[2],  
                  HR = exp(coef),                                 
                  lci = exp(coef - 1.96 * `se(coef)`),
                  hci = exp(coef + 1.96 * `se(coef)`),
                  beta = paste0(round(HR, 2), " (", round(lci, 2), "–", round(hci, 2), ")")
                ) %>%
                select(HR, lci, hci, beta, outcome, p)
              
              inter_hrs <- bind_rows(outcome3 %>% rename(est=HR), interaction_hrs)
              
### SHARE w5-w8, 2012-2018#### 
               share <- read_dta("E:/Tutor/Open aging cohort/SHARE/H_SHARE_f.dta")
               
               share_w8 <- read_dta("E:/Tutor/Open aging cohort/SHARE/sharew8_rel8-0-0_ALL_datasets_stata/sharew8_rel8-0-0_gv_imputations.dta") %>%
                 select(c(mergeid,lifehap)) %>% mutate(hap8=5-lifehap) %>% distinct(mergeid,.keep_all = TRUE)
               share_w7 <- read_dta("E:/Tutor/Open aging cohort/SHARE/sharew7_rel8-0-0_ALL_datasets_stata/sharew7_rel8-0-0_gv_imputations.dta") %>%
                 select(c(mergeid,lifehap)) %>% mutate(hap7=5-lifehap) %>% distinct(mergeid,.keep_all = TRUE)
               share_w6 <- read_dta("E:/Tutor/Open aging cohort/SHARE/sharew6_rel8-0-0_ALL_datasets_stata/sharew6_rel8-0-0_gv_imputations.dta") %>%
                 select(c(mergeid,lifehap)) %>% mutate(hap6=5-lifehap) %>% distinct(mergeid,.keep_all = TRUE)
               share_w5 <- read_dta("E:/Tutor/Open aging cohort/SHARE/sharew5_rel8-0-0_ALL_datasets_stata/sharew5_rel8-0-0_gv_imputations.dta") %>%
                 select(c(mergeid,lifehap)) %>% mutate(hap5=5-lifehap) %>% distinct(mergeid,.keep_all = TRUE)

               w5 <- share %>% 
                 filter(r5iwstat == 1) %>%
                 mutate(
                   bmi_4_5 = ifelse(!is.na(r4weight) & !is.na(r5weight), (r5weight - r4weight) / r4weight, NA),
                   bmi_c_5 = ifelse(bmi_4_5 <= -0.1 | r5bmicat == 1, 1, 0),
                   tired = ifelse(r5fatig == 1, 1, 0),
                   lowpa = ifelse(r5vgactx == 5 & r5mdactx == 5, 1, 0),
                   slow = ifelse(r5walkra == 1, 1, 0)
                 ) %>%
                 mutate(r5mgrip = pmax(r5lgrip, r5rgrip)) %>%
                 group_by(ragender, r5bmicat) %>%
                 mutate(weak = ifelse(r5mgrip <= quantile(r5mgrip, 0.2, na.rm = TRUE), 1, 0)) %>%
                 ungroup() %>%
                 mutate(
                   sum = rowSums(.[c("bmi_c_5", "tired", "lowpa", "slow", "weak")], na.rm = TRUE),
                   frail = case_when(sum >= 3 ~ 3,
                                     sum == 0 ~ 1,
                                     T ~ 2)
                 ) %>%
                 filter(rowSums(!is.na(select(., bmi_c_5, tired, lowpa, slow, weak))) >= 4) %>%
                 mutate(
                   t5 = ifelse(r5iwstat == 1, 0, NA),
                   t6 = ifelse(r6iwstat == 1, 2, NA),
                   t7 = ifelse(r7iwstat == 1, 4, NA),
                   t8 = ifelse(r8iwstat == 1, 6, NA),
                   dep_8 = ifelse(r8eurod >= 4, 1, 0),
                   dep_7 = ifelse(r7eurod >= 4, 1, 0),
                   dep_6 = ifelse(r6eurod >= 4, 1, 0),
                   dep_5 = ifelse(r5eurod >= 4, 1, 0),
                   new_dep = pmax(dep_6, dep_7, dep_8, na.rm = TRUE),
                   dep_8t = ifelse(dep_8 == 1, 6, NA),
                   dep_7t = ifelse(dep_7 == 1, 4, NA),
                   dep_6t = ifelse(dep_6 == 1, 2, NA),
                   dep_time = ifelse(new_dep == 1, pmin(dep_6t, dep_7t, dep_8t, na.rm = TRUE), pmax(t5, t6, t7, t8, na.rm = TRUE)),
                   dem_5 = ifelse(r5alzdeme == 1, 1, 0),
                   dem_6 = ifelse(r6alzdeme == 1, 1, 0),
                   dem_7 = ifelse(r7alzdeme == 1, 1, 0),
                   dem_8 = ifelse(r8alzdeme == 1, 1, 0),
                   new_dem = pmax(dem_6, dem_7, dem_8, na.rm = TRUE),
                   dem_8t = ifelse(dem_8 == 1, 6, NA),
                   dem_7t = ifelse(dem_7 == 1, 4, NA),
                   dem_6t = ifelse(dem_6 == 1, 2, NA),
                   dem_time = ifelse(new_dem == 1, pmin(dem_6t, dem_7t, dem_8t, na.rm = TRUE), pmax(t5, t6, t7, t8, na.rm = TRUE))
                 ) %>%
                 mutate(
                   con_sum = rowSums(across(c("r5hibpe", "r5diabe", "r5hearte", "r5stroke"), ~ pmax(as.numeric(.), 0)), na.rm = TRUE),
                   comorbid = case_when(con_sum >= 2 ~ 3,
                                        con_sum == 0 ~ 1,
                                        T ~ 2),
                   r8health = ifelse(r8shlt >= 4, 1, 0)
                 ) %>%
                 mutate(
                   frail_comorbid = ifelse(frail == 3, paste(frail, comorbid, sep = "_"), frail),
                   frail_comorbid2 = paste(frail, comorbid, sep = "_")
                 ) %>%
                 left_join(share_w5, by = "mergeid") %>%
                 left_join(share_w6, by = "mergeid") %>%
                 left_join(share_w7, by = "mergeid") %>%
                 left_join(share_w8, by = "mergeid") %>%
                 mutate(
                   t_1 = ifelse(r5iwstat == 1, 2012, NA),
                   t_2 = ifelse(r6iwstat == 1, 2014, NA),
                   t_3 = ifelse(r7iwstat == 1, 2016, NA),
                   t_4 = ifelse(r8iwstat == 1, 2018, NA)
                 ) %>%
                 mutate(
                   r5shlt = ifelse(r5shlt >= 0, 6 - r5shlt, NA),
                   r6shlt = ifelse(r6shlt >= 0, 6 - r6shlt, NA),
                   r7shlt = ifelse(r7shlt >= 0, 6 - r7shlt, NA),
                   r8shlt = ifelse(r8shlt >= 0, 6 - r8shlt, NA)
                 ) %>%
                 mutate(
                   r5all = rowSums(.[c("r5tr20", "r5orient", "r5ser7")] * (.[c("r5tr20", "r5orient", "r5ser7")] >= 0), na.rm = TRUE),
                   r6all = rowSums(.[c("r6tr20", "r6orient", "r6ser7")] * (.[c("r6tr20", "r6orient", "r6ser7")] >= 0), na.rm = TRUE),
                   r7all = rowSums(.[c("r7tr20", "r7orient", "r7ser7")] * (.[c("r7tr20", "r7orient", "r7ser7")] >= 0), na.rm = TRUE),
                   r8all = rowSums(.[c("r8tr20", "r8orient", "r8ser7")] * (.[c("r8tr20", "r8orient", "r8ser7")] >= 0), na.rm = TRUE),
                 ) %>%
                 mutate(wealth5 = ntile(h5atoth, 5)) %>%
         
                 mutate_at(vars(c("hap5", "hap6", "hap7", "hap8",
                                  "r5shlt", "r6shlt", "r7shlt", "r8shlt",
                                  "r5eurod", "r6eurod", "r7eurod", "r8eurod",
                                  "r5tr20", "r5orient", "r5ser7","r5all",
                                  "r6tr20", "r6orient", "r6ser7","r6all",
                                  "r7tr20", "r7orient", "r7ser7","r7all",
                                  "r8tr20", "r8orient", "r8ser7","r8all",
                                  
                                  
                                  )), scale)
               
               share_fig <- function(){
               w5_long <- w5 %>% select(c(mergeid,frail_comorbid2,frail, comorbid,
                                                        "hap5","hap6","hap7","hap8",
                                                        "r5shlt", "r6shlt", "r7shlt", "r8shlt",
                                                        "r5satlifez","r6satlifez","r7satlifez","r8satlifez",
                                                        "r5eurod", "r6eurod", "r7eurod", "r8eurod",
                                                        "r5tr20", "r5orient", "r5ser7","r5all",
                                                        "r6tr20", "r6orient", "r6ser7","r6all",
                                                        "r7tr20", "r7orient", "r7ser7","r7all",
                                                        "r8tr20", "r8orient", "r8ser7","r8all",
                                                        t_1,t_2,t_3,t_4))

  
               {#OLS regression
                 w5$frail_comorbid2 <- factor(w5$frail_comorbid2, levels = c("1_1", "1_2", "1_3", "2_1", "2_2", "2_3", "3_1", "3_2", "3_3"))
                 dummy_variables <- model.matrix(~ frail_comorbid2 - 1, data = w5)
                 w5_ols <- cbind(w5, dummy_variables) %>% select(-frail_comorbid2)
                 
                 out_cesd <- lm(r8eurod ~ frail_comorbid21_2 + frail_comorbid21_3 + frail_comorbid22_1 +frail_comorbid22_2 + frail_comorbid22_3 + frail_comorbid23_1 +frail_comorbid23_2 + frail_comorbid23_3 +ragender + r5agey + raeducl+ r5eurod, data = w5_ols) %>% summary()
                 out_sf <- lm(r8satlifez ~ frail_comorbid21_2 + frail_comorbid21_3 + frail_comorbid22_1 +frail_comorbid22_2 + frail_comorbid22_3 + frail_comorbid23_1 +frail_comorbid23_2 + frail_comorbid23_3 + ragender + r5agey + raeducl+r5satlifez, data = w5_ols) %>% summary()
                 out_ha <- lm(hap8 ~ frail_comorbid21_2 + frail_comorbid21_3 + frail_comorbid22_1 +frail_comorbid22_2 + frail_comorbid22_3 + frail_comorbid23_1 +frail_comorbid23_2 + frail_comorbid23_3 + ragender + r5agey + raeducl +hap5, data = w5_ols) %>% summary()
                 out_sh <- lm(r8shlt ~ frail_comorbid21_2 + frail_comorbid21_3 + frail_comorbid22_1 +frail_comorbid22_2 + frail_comorbid22_3 + frail_comorbid23_1 +frail_comorbid23_2 + frail_comorbid23_3 +ragender + r5agey + raeducl+r5shlt, data = w5_ols) %>% summary()
                 
                 out_tr <- lm(r8tr20 ~ r5tr20+frail_comorbid21_2 + frail_comorbid21_3 + frail_comorbid22_1 +frail_comorbid22_2 + frail_comorbid22_3 + frail_comorbid23_1 +frail_comorbid23_2 + frail_comorbid23_3 + ragender + r5agey + raeducl, data = filter(w5_ols,dem_5 != 1)) %>% summary()
                 out_or <- lm(r8orient ~ r5orient+frail_comorbid21_2 + frail_comorbid21_3 + frail_comorbid22_1 +frail_comorbid22_2 + frail_comorbid22_3 + frail_comorbid23_1 +frail_comorbid23_2 + frail_comorbid23_3 + ragender + r5agey + raeducl, data = filter(w5_ols,dem_5 != 1)) %>% summary()
                 out_se <- lm(r8ser7 ~ r5ser7+frail_comorbid21_2 + frail_comorbid21_3 + frail_comorbid22_1 +frail_comorbid22_2 + frail_comorbid22_3 + frail_comorbid23_1 +frail_comorbid23_2 + frail_comorbid23_3 + ragender + r5agey + raeducl, data = filter(w5_ols,dem_5 != 1)) %>% summary()
                 out_all <- lm(r8all ~ r5all+frail_comorbid21_2 + frail_comorbid21_3 + frail_comorbid22_1 +frail_comorbid22_2 + frail_comorbid22_3 + frail_comorbid23_1 +frail_comorbid23_2 + frail_comorbid23_3 + ragender + r5agey + raeducl, data = filter(w5_ols,dem_5 != 1)) %>% summary()
                 
                 # Mental wellbeing
                 f1 <- as.data.frame(out_cesd$coefficients)[2:9,] %>% mutate(analysis = "Depressive symptom") %>% add_row(analysis = "Depressive symptom") %>% add_row(analysis = "Depressive symptom") %>% add_row(analysis = "Depressive symptom") %>% add_row(analysis = "Depressive symptom")
                 f2 <- as.data.frame(out_sf$coefficients)[2:9,] %>% mutate(analysis = "Life satisfaction") %>% add_row(analysis = "Life satisfaction") %>% add_row(analysis = "Life satisfaction") %>% add_row(analysis = "Life satisfaction") %>% add_row(analysis = "Life satisfaction")
                 f3 <- as.data.frame(out_ha$coefficients)[2:9,] %>% mutate(analysis = "Happiness") %>% add_row(analysis = "Happiness") %>% add_row(analysis = "Happiness") %>% add_row(analysis = "Happiness") %>% add_row(analysis = "Happiness")
                 f4 <- as.data.frame(out_sh$coefficients)[2:9,] %>% mutate(analysis = "Self-reported health") %>% add_row(analysis = "Self-reported health") %>% add_row(analysis = "Self-reported health") %>% add_row(analysis = "Self-reported health") %>% add_row(analysis = "Self-reported health")
                 
                 forest_data <- rbind(f1, f2, f3, f4)
                 forest_data$Label <- rep(c("1", "≥2", "0", "1", "≥2", "0", "1", "≥2", "0 (Ref)", "Non-frail", "Pre-frail", "Frail"), times = 4)
                 forest_data$seq <- rep(c(3, 4, 6, 7, 8, 10, 11, 12, 2, 1, 5, 9), times = 4)
                 forest_data <- forest_data %>% arrange(seq) %>% mutate(seq = as.factor(seq),
                                                                        est = Estimate,
                                                                        se = `Std. Error`,
                                                                        est = ifelse(seq == 2, 0, est),
                                                                        subgroup = ifelse(is.na(est) == FALSE, paste0(seq, Label), est),
                                                                        lci = est - 1.96 * se,
                                                                        uci = est + 1.96 * se)
                 
                 row_labels <- forest_data %>% filter(analysis == "Depressive symptom") %>% select(subgroup, Label)
                 
                 p4_ols <- forest_plot(split(forest_data, ~analysis), col.key = "subgroup", row.labels = row_labels, exponentiate = F,
                             scalepoints = T, nullval = 0, xlab = "",
                             plot.margin = margin(1, 4, 1, 4, "mm"),
                             xlim = c(-1.7, 1.7), row.labels.heading="SHARE", base_size = 7,pointsize = 2,quiet = TRUE,panel.headings = rep("", 4),
                             xticks = c(-1.5, -1, -0.5, 0, 0.5, 1.0, 1.5),
                             base_line_size = 0.2)
                 p4_ols$plot <- p4_ols$plot + theme(panel.spacing.x  = unit(18, "mm"))
                 
                 # Cognition
                 f5 <- as.data.frame(out_tr$coefficients)[3:10,] %>% mutate(analysis = "Memory") %>% add_row(analysis = "Memory") %>% add_row(analysis = "Memory") %>% add_row(analysis = "Memory") %>% add_row(analysis = "Memory")
                 f6 <- as.data.frame(out_or$coefficients)[3:10,] %>% mutate(analysis = "Orientation") %>% add_row(analysis = "Orientation") %>% add_row(analysis = "Orientation") %>% add_row(analysis = "Orientation") %>% add_row(analysis = "Orientation")
                 f7 <- as.data.frame(out_se$coefficients)[3:10,] %>% mutate(analysis = "Numeracy") %>% add_row(analysis = "Numeracy") %>% add_row(analysis = "Numeracy") %>% add_row(analysis = "Numeracy") %>% add_row(analysis = "Numeracy")
                 f8 <- as.data.frame(out_all$coefficients)[3:10,] %>% mutate(analysis = "Global cognitive function") %>% add_row(analysis = "Global cognitive function") %>% add_row(analysis = "Global cognitive function") %>% add_row(analysis = "Global cognitive function") %>% add_row(analysis = "Global cognitive function")
                 forest_data2 <- rbind(f5, f6, f7, f8)
                 
                 forest_data2$Label <- rep(c("1", "≥2", "0", "1", "≥2", "0", "1", "≥2", "0 (Ref)", "Non-frail", "Pre-frail", "Frail"), times = 4)
                 forest_data2$seq <- rep(c(3, 4, 6, 7, 8, 10, 11, 12, 2, 1, 5, 9), times = 4)
                 forest_data2 <- forest_data2 %>% arrange(seq) %>% mutate(seq = as.factor(seq),
                                                                          est = Estimate,
                                                                          se = `Std. Error`,
                                                                          est = ifelse(seq == 2, 0, est),
                                                                          subgroup = ifelse(is.na(est) == FALSE, paste0(seq, Label), est),
                                                                          lci = est - 1.96 * se,
                                                                          uci = est + 1.96 * se)
                 
                 row_labels <- forest_data2 %>% filter(analysis == "Memory") %>% select(subgroup, Label)
                 
                 p4b_ols <- forest_plot(split(forest_data2, ~analysis), col.key = "subgroup", row.labels = row_labels, exponentiate = FALSE,
                                        scalepoints = TRUE, nullval = 0, xlab = "",
                                        plot.margin = margin(1, 4, 1, 4, "mm"),
                                        row.labels.heading="SHARE", base_size = 7,pointsize = 2,quiet = TRUE,panel.headings = rep("", 4),
                                        xlim        = c(-0.75, 0.75),
                                        xticks      = c(-0.6, -0.4,-0.2,0,0.2,0.4,0.6),
                                        base_line_size = 0.2)
                 p4b_ols$plot <- p4b_ols$plot + theme(panel.spacing.x  = unit(18, "mm"))
                 
               }
               
               
               df1 <- pivot_longer(w5_long, cols = starts_with("t_"), names_to = "time", values_to = "year")
               df2 <- pivot_longer(w5_long, cols = starts_with("hap"), names_to = "variable", values_to = "ha")
               df3 <- pivot_longer(w5_long, cols = ends_with("satlifez"), names_to = "variable", values_to = "sf")
               df4 <- pivot_longer(w5_long, cols = ends_with("eurod"), names_to = "variable", values_to = "cesd")
               df5 <- pivot_longer(w5_long, cols = ends_with("shlt"), names_to = "variable", values_to = "sh")
               df6 <- pivot_longer(w5_long, cols = ends_with("tr20"), names_to = "variable", values_to = "tr")
               df7 <- pivot_longer(w5_long, cols = ends_with("orient"), names_to = "variable", values_to = "or")
               df8 <- pivot_longer(w5_long, cols = ends_with("ser7"), names_to = "variable", values_to = "se")
               df9 <- pivot_longer(w5_long, cols = ends_with("all"), names_to = "variable", values_to = "all")
               

               
               df_long <- bind_cols(select(df1, c(mergeid, frail_comorbid2,frail, comorbid, year)), select(df2, c(ha)), select(df3, c(sf)), select(df4, c(cesd)), select(df5, c(sh)),
                                    select(df6, c(tr)), select(df7, c(or)), select(df8, c(se)), select(df9, c(all))) %>%
                 filter(!is.na(year)) %>% left_join(select(w5, c(mergeid, ragender, r5agey, raeducl, wealth5, r5smokev, r5drinkev, r5psyche, r5alzdeme)), by = "mergeid")

               panel_data <- pdata.frame(df_long, index = c("mergeid","year"))
               fx_cesd <- plm(cesd ~ frail_comorbid2 + ragender + r5agey + raeducl+ wealth5 + r5smokev + r5drinkev + r5psyche, data = filter(panel_data, !is.na(cesd)), model = "random")
               fx_sf <- plm(sf ~ frail_comorbid2 + ragender + r5agey + raeducl+ wealth5 + r5smokev + r5drinkev + r5psyche, data = filter(panel_data, !is.na(sf)), model = "random")
               fx_sh <- plm(sh ~ frail_comorbid2 + ragender + r5agey + raeducl+ wealth5 + r5smokev + r5drinkev + r5psyche, data = filter(panel_data, !is.na(sh)), model = "random")

               fx_tr <- plm(tr ~ frail_comorbid2 + ragender + r5agey + raeducl + wealth5 + r5smokev + r5drinkev + r5psyche, data = filter(panel_data, !is.na(tr) & r5alzdeme != 1), model = "random")
               fx_or <- plm(or ~ frail_comorbid2 + ragender + r5agey + raeducl + wealth5 + r5smokev + r5drinkev + r5psyche, data = filter(panel_data, !is.na(or) & r5alzdeme != 1), model = "random")
               fx_se <- plm(se ~ frail_comorbid2 + ragender + r5agey + raeducl + wealth5 + r5smokev + r5drinkev + r5psyche, data = filter(panel_data, !is.na(se) & r5alzdeme != 1), model = "random")
               fx_all <- plm(all ~ frail_comorbid2 + ragender + r5agey + raeducl + wealth5 + r5smokev + r5drinkev + r5psyche, data = filter(panel_data, !is.na(all) & r5alzdeme != 1), model = "random")
               
               panel_data <- pdata.frame(df_long, index = c("year"))
               fx_ha <- plm(ha ~ frail_comorbid2 + ragender + r5agey + raeducl+ wealth5 + r5smokev + r5drinkev + r5psyche, data = filter(panel_data, !is.na(ha)), model = "within")
               
               
               out_cesd <- summary(fx_cesd)
               out_sf <- summary(fx_sf)
               out_ha <- summary(fx_ha)
               out_sh <- summary(fx_sh)
               out_tr <- summary(fx_tr)
               out_or <- summary(fx_or)
               out_se <- summary(fx_se)
               out_all <- summary(fx_all)

               
               fix_coef <- function(out, label) {
                 if (label == "Happiness") {
                   df <- as.data.frame(out$coefficients)[1:8, ]
                 } else {
                   df <- as.data.frame(out$coefficients)[2:9, ]
                 }
                 
                 colnames(df) <- gsub("t-value", "z-value", colnames(df))
                 colnames(df) <- gsub("Pr\\(>\\|t\\|\\)", "Pr(>|z|)", colnames(df))
                 
                 df$analysis <- label
                 
                 blank <- df[1:4, ]
                 blank[,] <- NA
                 blank$analysis <- label
                 
                 bind_rows(df, blank)
               }
               
               f1 <- fix_coef(out_cesd, "Depressive symptom")
               f2 <- fix_coef(out_sf, "Life satisfaction")
               f3 <- fix_coef(out_ha, "Happiness")
               f4 <- fix_coef(out_sh, "Self-reported health")
               
               
               # 
               # f1 <- as.data.frame(out_cesd$coefficients)[2:9,] %>% mutate(analysis = "Depressive symptom") %>% add_row(analysis = "Depressive symptom") %>% add_row(analysis = "Depressive symptom") %>% add_row(analysis = "Depressive symptom") %>% add_row(analysis = "Depressive symptom")
               # f2 <- as.data.frame(out_sf$coefficients)[2:9,] %>% mutate(analysis = "Life satisfaction") %>% add_row(analysis = "Life satisfaction") %>% add_row(analysis = "Life satisfaction") %>% add_row(analysis = "Life satisfaction") %>% add_row(analysis = "Life satisfaction")
               # f3 <- as.data.frame(out_ha$coefficients)[1:8,] %>% mutate(analysis = "Happiness") %>% add_row(analysis = "Happiness") %>% add_row(analysis = "Happiness") %>% add_row(analysis = "Happiness") %>% add_row(analysis = "Happiness")
               # f4 <- as.data.frame(out_sh$coefficients)[2:9,] %>% mutate(analysis = "Self-reported health") %>% add_row(analysis = "Self-reported health") %>% add_row(analysis = "Self-reported health") %>% add_row(analysis = "Self-reported health") %>% add_row(analysis = "Self-reported health")
               forest_data <- rbind(f1, f2, f3, f4)
               forest_data_share <- rbind(f1, f2, f3, f4) %>% mutate(study="SHARE",level=rep(c("1_2", "1_3", "2_1", "2_2", "2_3", "3_1", "3_2", "3_3", "", "", "",""),times=4))
               forest_data$Label <- rep(c("1", "≥2", "0", "1", "≥2", "0", "1", "≥2", "0 (Ref)", "Non-frail", "Pre-frail", "Frail"), times = 4)
               forest_data$seq <- rep(c(3, 4, 6, 7, 8, 10, 11, 12, 2, 1, 5, 9), times = 4)
               forest_data <- forest_data %>% arrange(seq) %>% mutate(seq = as.factor(seq),
                                                                      est = Estimate,
                                                                      se = `Std. Error`,
                                                                      est = ifelse(seq == 2, 0, est),
                                                                      subgroup = ifelse(!is.na(est) == TRUE, paste0(seq, Label), est),
                                                                      lci = est - 1.96 * se,
                                                                      uci = est + 1.96 * se,
               )
               row_labels <- forest_data %>% filter(analysis == "Depressive symptom") %>% select(subgroup, Label)
               
               p4 <- forest_plot(split(forest_data, ~analysis), col.key = "subgroup", row.labels = row_labels, exponentiate = F,
                                 scalepoints = T, nullval = 0, xlab = "",
                                 plot.margin = margin(1, 4, 1, 4, "mm"),
                                 xlim = c(-1.7, 1.7), row.labels.heading="SHARE", base_size = 7,pointsize = 2,quiet = TRUE,panel.headings = rep("", 4),
                                 xticks = c(-1.5, -1, -0.5, 0, 0.5, 1.0, 1.5),
                                 base_line_size = 0.2)
               
               p4$plot <- p4$plot + theme(panel.spacing.x  = unit(18, "mm"))
               
               f5 <- as.data.frame(out_tr$coefficients)[2:9,] %>% mutate(analysis = "Memory") %>% add_row(analysis = "Memory") %>% add_row(analysis = "Memory") %>% add_row(analysis = "Memory") %>% add_row(analysis = "Memory")
               f6 <- as.data.frame(out_or$coefficients)[2:9,] %>% mutate(analysis = "Orientation") %>% add_row(analysis = "Orientation") %>% add_row(analysis = "Orientation") %>% add_row(analysis = "Orientation") %>% add_row(analysis = "Orientation")
               f7 <- as.data.frame(out_se$coefficients)[2:9,] %>% mutate(analysis = "Numeracy") %>% add_row(analysis = "Numeracy") %>% add_row(analysis = "Numeracy") %>% add_row(analysis = "Numeracy") %>% add_row(analysis = "Numeracy")
               f8 <- as.data.frame(out_all$coefficients)[2:9,] %>% mutate(analysis = "Global cognitive function") %>% add_row(analysis = "Global cognitive function") %>% add_row(analysis = "Global cognitive function") %>% add_row(analysis = "Global cognitive function") %>% add_row(analysis = "Global cognitive function")
               
               forest_data2 <- rbind(f5, f6, f7, f8)
               forest_data2_share <- rbind(f5, f6, f7, f8) %>% mutate(study="SHARE",level=rep(c("1_2", "1_3", "2_1", "2_2", "2_3", "3_1", "3_2", "3_3", "", "", "",""),times=4))
               forest_data2$Label <- rep(c("1", "≥2", "0", "1", "≥2", "0", "1", "≥2", "0 (Ref)", "Non-frail", "Pre-frail", "Frail"), times = 4)
               forest_data2$seq <- rep(c(3, 4, 6, 7, 8, 10, 11, 12, 2, 1, 5, 9), times = 4)
               
               forest_data2 <- forest_data2 %>%
                 arrange(seq) %>%
                 mutate(seq = as.factor(seq),
                        est = Estimate,
                        se = `Std. Error`,
                        est = ifelse(seq == 2, 0, est),
                        subgroup = ifelse(!is.na(est) == TRUE, paste0(seq, Label), est),
                        lci = est - 1.96 * se,
                        uci = est + 1.96 * se)
               
               row_labels <- forest_data2 %>% filter(analysis == "Memory") %>% select(subgroup, Label)
               
               p4_b <- forest_plot(split(forest_data2, ~analysis), col.key = "subgroup", row.labels = row_labels, exponentiate = FALSE,
                                   scalepoints = TRUE, nullval = 0, xlab = "",
                                   plot.margin = margin(1, 4, 1, 4, "mm"),
                                   row.labels.heading="SHARE", base_size = 7,pointsize = 2,quiet = TRUE,panel.headings = rep("", 4),
                                   xlim        = c(-0.65, 0.65),
                                   xticks      = c(-0.6, -0.4,-0.2,0,0.2,0.4,0.6),
                                   base_line_size = 0.2)
               p4_b$plot <- p4_b$plot + theme(panel.spacing.x  = unit(18, "mm"))
               
               return(list(p4 = p4$plot, p4_b = p4_b$plot, p4_ols=p4_ols$plot, p4b_ols=p4b_ols$plot,df_long=df_long,forest=forest_data_share, forest2=forest_data2_share))
               }
               
               res_share <- share_fig()
  
               p4 <- res_share$p4
               p4_b <- res_share$p4_b
               p4_ols <- res_share$p4_ols
               p4b_ols <- res_share$p4b_ols
               share_long <- res_share$df_long
               forest_data_share <- res_share$forest
               forest_data2_share <- res_share$forest2
               
               
               #Dementia
               out_dem <- coxph(Surv(dem_time, new_dem) ~ relevel(as.factor(frail_comorbid2),ref="1_1")+ ragender + r5agey + raeducl + wealth5 + r5smokev + r5drinkev + r5psyche, data = filter(w5,dem_5!=1)) %>% summary
               
               f9 <- as.data.frame(out_dem$coefficients)[1:8,] %>% mutate(analysis="Dementia") %>%  add_row(analysis = "Dementia")%>%  add_row(analysis = "Dementia")%>%  add_row(analysis = "Dementia")%>%  add_row(analysis = "Dementia")
               forest_data3 <-  rbind(f9)
               forest_data3_share <-  f9
               forest_data3$Label <- rep(c("1","≥2","0","1","≥2","0","1","≥2","0 (Ref)","Non-frail","Pre-frail","Frail"),times=1)
               forest_data3$seq <- rep(c(3,4,6,7,8,10,11,12,2,1,5,9),times=1)
               forest_data3<- forest_data3 %>% arrange(seq) %>% mutate(seq=as.factor(seq),
                                                                       est=coef,
                                                                       se=`se(coef)`,
                                                                       est=ifelse(seq==2,0,est),
                                                                       subgroup=ifelse(is.na(est)==F,paste0(seq,Label),est),
                                                                       lci=est-1.96*se,
                                                                       uci=est+1.96*se,
               )
               row_labels <- forest_data3 %>% filter(analysis=="Dementia") %>% select(subgroup,Label)
               p4_a <- forest_plot(split(forest_data3,~analysis), col.key = "subgroup",row.labels = row_labels, exponentiate = T,logscale=T,
                                   scalepoints=T,nullval=1,xlab="HR (95% CI)",
                                   row.labels.heading="SHARE", plot.margin = margin(1, 4, 1, 4, "mm"),
                                   #xlim        = c(-1, 1),
                                   #xticks      = c(-1,-0.5,0,0.5,1.0),
                                   base_line_size = 0.2)
               
        #Interaction of frailty and multimorbidity
               interact_share <- function(){
                 outcomes <- c("all","cesd","ha","sf","sh")
                 interaction_share <- data.frame()
                 df_long <- share_long
                 covar <- "+ragender + r5agey + raeducl+ wealth5 + r5smokev + r5drinkev + r5psyche"
                 
                 for (outcome in outcomes) {
                   if(outcome=="ha"){
                     panel_data <- pdata.frame(df_long, index = c("year"))
                     model <- "within"
                   } else {
                     panel_data <- pdata.frame(df_long, index = c("mergeid","year"))
                     model <- "random"
                   }
                   
                   if(outcome=="all"){
                     outcome1 <- plm(as.formula(paste0(outcome,"~as.factor(frail)+as.factor(comorbid)",covar)),
                                     data = filter(panel_data, !is.na(get(outcome) & r5alzdeme != 1)),
                                     model = model) %>% summary()
                     outcome2 <- plm(as.formula(paste0(outcome,"~as.factor(frail)*as.factor(comorbid)",covar)),
                                     data = filter(panel_data, !is.na(get(outcome) & r5alzdeme != 1)),
                                     model = model) %>% summary()
                     m1 <- plm(as.formula(paste0(outcome,"~as.factor(frail)+as.factor(comorbid)",covar)),
                               data = filter(panel_data, !is.na(get(outcome) & r5alzdeme != 1)), model = model)
                     m2 <- plm(as.formula(paste0(outcome,"~as.factor(frail)*as.factor(comorbid)",covar)),
                               data = filter(panel_data, !is.na(get(outcome) & r5alzdeme != 1)), model = model)
                     
                     outcome3 <- as.data.frame(outcome1$coefficients)[2:5, ] %>%
                       mutate(outcome = outcome,
                              p = pFtest(m2, m1)$p.value,
                              est = Estimate,
                              stderr = `Std. Error`,
                              lci = est - 1.96 * stderr,
                              hci = est + 1.96 * stderr,
                              beta = paste0(round(est, 2), " (", round(lci, 2), " to ", round(hci, 2), ")")) %>%
                       select(est, lci, hci, beta, outcome, p)
                   } else {
                     outcome1 <- plm(as.formula(paste0(outcome,"~as.factor(frail)+as.factor(comorbid)",covar)),
                                     data = filter(panel_data, !is.na(get(outcome))), model = model) %>% summary()
                     outcome2 <- plm(as.formula(paste0(outcome,"~as.factor(frail)*as.factor(comorbid)",covar)),
                                     data = filter(panel_data, !is.na(get(outcome))), model = model) %>% summary()
                     m1 <- plm(as.formula(paste0(outcome,"~as.factor(frail)+as.factor(comorbid)",covar)),
                               data = filter(panel_data, !is.na(get(outcome) & r5alzdeme != 1)), model = model)
                     m2 <- plm(as.formula(paste0(outcome,"~as.factor(frail)*as.factor(comorbid)",covar)),
                               data = filter(panel_data, !is.na(get(outcome) & r5alzdeme != 1)), model = model)
                     
                     if (outcome == "ha") {
                       coef_rows <- 1:4
                     } else {
                       coef_rows <- 2:5
                     }
                     
                     outcome3 <- as.data.frame(outcome1$coefficients)[coef_rows, ] %>%
                       mutate(outcome = outcome,
                              p = pFtest(m2, m1)$p.value,
                              est = Estimate,
                              stderr = `Std. Error`,
                              lci = est - 1.96 * stderr,
                              hci = est + 1.96 * stderr,
                              beta = paste0(round(est, 2), " (", round(lci, 2), " to ", round(hci, 2), ")")) %>%
                       select(est, lci, hci, beta, outcome, p)
                   }
                   
                   interaction_share <- rbind(interaction_share, outcome3)
                 }
                 return(interaction_share)
               }
               
               interaction_share <- interact_share()
               
               ##Dementia
               coxph(Surv(dem_time, new_dem) ~ (as.factor(frail))+(as.factor(comorbid))+ ragender + r5agey + raeducl + wealth5 + r5smokev + r5drinkev + r5psyche, data = filter(w5,dem_5!=1)) %>% summary
               coxph(Surv(dem_time, new_dem) ~ (as.factor(frail))+(as.factor(comorbid))+frail*comorbid+ragender + ragender + r5agey + raeducl + wealth5 + r5smokev + r5drinkev + r5psyche, data = filter(w5,dem_5!=1)) %>% summary
               
               ##Dementia
               outcome1 <- coxph(Surv(dem_time, new_dem) ~ (as.factor(frail))+(as.factor(comorbid))+ ragender + r5agey + raeducl + wealth5 + r5smokev + r5drinkev + r5psyche, data = filter(w5,dem_5!=1))
               outcome2 <- coxph(Surv(dem_time, new_dem) ~ as.factor(frail)*as.factor(comorbid)+ragender + ragender + r5agey + raeducl + wealth5 + r5smokev + r5drinkev + r5psyche, data = filter(w5,dem_5!=1)) 
               
               outcome3 <- as.data.frame(summary(outcome1)$coefficients)[1:4, ] %>%
                 mutate(
                   outcome = "dementia",
                   p =anova(outcome1, outcome2, test = "LRT")$`Pr(>|Chi|)`[2],  
                   HR = exp(coef),                                 
                   lci = exp(coef - 1.96 * `se(coef)`),
                   hci = exp(coef + 1.96 * `se(coef)`),
                   beta = paste0(round(HR, 2), " (", round(lci, 2), "–", round(hci, 2), ")")
                 ) %>%
                 select(HR, lci, hci, beta, outcome, p)
               
               
               inter_share <- bind_rows(outcome3 %>% rename(est=HR), interaction_share)
               
### KLoSA w4-w7, 2012-2018#### 
               klosa <- read_dta("E:/Tutor/Open aging cohort/KLoSA/H_KLoSA_e2.dta")           
               w4 <- klosa %>% filter(r4iwstat == 1) %>%
                 mutate(
                   # bmi_missing = ifelse(!is.na(r11mweight) & !is.na(r10mweight), 1, 0),
                   bmi_4_3 = ifelse(!is.na(r4weight) & !is.na(r3weight), (r4weight - r3weight) / r3weight, NA),
                   bmi_c_4 = ifelse(bmi_4_3 <= -0.1 | r4bmicat == 1, 1, 0),
                   tired = ifelse(r4effortl >= 3 | r4goingl >= 3, 1, 0),
                   lowpa = ifelse(r4vigact == 0, 1, 0),
                 ) %>%
                 # group_by(ragender, r4height) %>%
                 mutate(slow = ifelse(r4bedb_k == 1, 1, 0)) %>% ungroup() %>% mutate(r4mgrip = pmax(r4lgrip, r4rgrip)) %>%
                 group_by(ragender, r4bmicat) %>%
                 mutate(weak = ifelse(r4mgrip <= quantile(r4mgrip, 0.2, na.rm = TRUE), 1, 0)) %>% ungroup() %>%
                 mutate(sum = rowSums(.[c("bmi_c_4", "tired", "lowpa", "slow", "weak")], na.rm = TRUE),
                        frail = case_when(
                          sum >= 3 ~ 3,
                          sum == 0 ~ 1,
                          TRUE ~ 2
                        )) %>% filter(rowSums(!is.na(select(., bmi_c_4, tired, lowpa, slow, weak))) >= 4) %>%
                 mutate(
                   t4 = ifelse(r4iwstat == 1, 0, NA),
                   t5 = ifelse(r5iwstat == 1, 2, NA),
                   t6 = ifelse(r6iwstat == 1, 4, NA),
                   t7 = ifelse(r7iwstat == 1, 6, NA),
                   dep_4 = ifelse(r4cesd10a >= 10, 1, 0),
                   dep_5 = ifelse(r5cesd10b >= 10, 1, 0),
                   dep_6 = ifelse(r6cesd10b >= 10, 1, 0),
                   dep_7 = ifelse(r7cesd10b >= 10, 1, 0),
                   new_dep = pmax(dep_5, dep_6, dep_7, na.rm = TRUE),
                   dep_7t = ifelse(dep_7 == 1, 6, NA),
                   dep_6t = ifelse(dep_6 == 1, 4, NA),
                   dep_5t = ifelse(dep_5 == 1, 2, NA),
                   dep_time = ifelse(new_dep == 1, pmin(dep_5t, dep_6t, dep_7t, na.rm = TRUE), pmax(t4, t5, t6, t7, na.rm = TRUE)),
                   # dem_5 = ifelse(r5alzdeme == 1, 1, 0),
                   # dem_6 = ifelse(r6alzdeme == 1, 1, 0),
                   # dem_7 = ifelse(r7alzdeme == 1, 1, 0),
                   # dem_8 = ifelse(r8alzdeme == 1, 1, 0),
                   # new_dem = pmax(dem_6, dem_7, dem_8, na.rm = TRUE),
                   # dem_8t = ifelse(dem_8 == 1, 6, NA),
                   # dem_7t = ifelse(dem_7 == 1, 4, NA),
                   # dem_6t = ifelse(dem_6 == 1, 2, NA),
                   # dem_time = ifelse(new_dem == 1, pmin(dem_6t, dem_7t, dem_8t, na.rm = TRUE), pmax(t5, t6, t7, t8, na.rm = TRUE))
                 ) %>%
                 mutate(
                   con_sum = rowSums(across(c("r4hibpe", "r4diabe", "r4hearte", "r4stroke"), ~ pmax(as.numeric(.), 0)), na.rm = TRUE),
                   comorbid = case_when(
                     con_sum >= 2 ~ 3,
                     con_sum == 0 ~ 1,
                     TRUE ~ 2
                   )
                 ) %>%
                 mutate(frail_comorbid = ifelse(frail == 3, paste(frail, comorbid, sep = "_"), frail),
                        frail_comorbid2 = paste(frail, comorbid, sep = "_")
                 ) %>%
                 mutate(
                   t_1 = ifelse(r4iwstat == 1, 2012, NA),
                   t_2 = ifelse(r5iwstat == 1, 2014, NA),
                   t_3 = ifelse(r6iwstat == 1, 2016, NA),
                   t_4 = ifelse(r7iwstat == 1, 2018, NA)
                 ) %>%
                 mutate(
                   r4shlt = ifelse(r4shlt >= 0, 6 - r4shlt, NA),
                   r5shlt = ifelse(r5shlt >= 0, 6 - r5shlt, NA),
                   r6shlt = ifelse(r6shlt >= 0, 6 - r6shlt, NA),
                   r7shlt = ifelse(r7shlt >= 0, 6 - r7shlt, NA),
                   r4satwlife_k=100-r4satwlife_k,
                   r5satwlife_k=100-r5satwlife_k,
                   r6satwlife_k=100-r6satwlife_k,
                   r7satwlife_k=100-r7satwlife_k,
                 ) %>%
                 mutate(wealth5 = ntile(r4atoth, 5)) %>%
                 mutate_at(vars(c("r4whappyl", "r5whappyl", "r6whappyl", "r7whappyl",
                                  "r4shlt", "r5shlt", "r6shlt", "r7shlt",
                                  "r4satwlife_k", "r5satwlife_k", "r6satwlife_k", "r7satwlife_k",
                                  "r4cesd10a", "r5cesd10b", "r6cesd10b", "r7cesd10b")), scale)
               
               
               klosa_fig <- function(){
               w4_long <- w4 %>% select(c(pid, frail_comorbid2,frail,comorbid,
                                          "r4whappyl", "r5whappyl", "r6whappyl", "r7whappyl",
                                          "r4shlt", "r5shlt", "r6shlt", "r7shlt",
                                          "r4satwlife_k", "r5satwlife_k", "r6satwlife_k", "r7satwlife_k",
                                          "r4cesd10a", "r5cesd10b", "r6cesd10b", "r7cesd10b",
                                          t_1, t_2, t_3, t_4))
              
               {#OLS regression
                 w4$frail_comorbid2 <- factor(w4$frail_comorbid2, levels = c("1_1", "1_2", "1_3", "2_1", "2_2", "2_3", "3_1", "3_2", "3_3"))
                 dummy_variables <- model.matrix(~ frail_comorbid2 - 1, data = w4)
                 w4_ols <- cbind(w4, dummy_variables) %>% select(-frail_comorbid2)
                 
                 out_cesd <- lm(r7cesd10b ~ frail_comorbid21_2 + frail_comorbid21_3 + frail_comorbid22_1 +frail_comorbid22_2 + frail_comorbid22_3 + frail_comorbid23_1 +frail_comorbid23_2 + frail_comorbid23_3 +ragender + r4agey + raeducl+wealth5+r4smokev+r4drinkev+r4psyche, data = w4_ols) %>% summary()
                 out_sf <- lm(r7satwlife_k ~ frail_comorbid21_2 + frail_comorbid21_3 + frail_comorbid22_1 +frail_comorbid22_2 + frail_comorbid22_3 + frail_comorbid23_1 +frail_comorbid23_2 + frail_comorbid23_3 +ragender + r4agey + raeducl+wealth5+r4smokev+r4drinkev+r4psyche, data = w4_ols) %>% summary()
                 out_ha <- lm(r7whappyl ~ frail_comorbid21_2 + frail_comorbid21_3 + frail_comorbid22_1 +frail_comorbid22_2 + frail_comorbid22_3 + frail_comorbid23_1 +frail_comorbid23_2 + frail_comorbid23_3 +ragender + r4agey + raeducl+wealth5+r4smokev+r4drinkev+r4psyche, data = w4_ols) %>% summary()
                 out_sh <- lm(r7shlt ~ frail_comorbid21_2 + frail_comorbid21_3 + frail_comorbid22_1 +frail_comorbid22_2 + frail_comorbid22_3 + frail_comorbid23_1 +frail_comorbid23_2 + frail_comorbid23_3 +ragender + r4agey + raeducl+wealth5+r4smokev+r4drinkev+r4psyche, data = w4_ols) %>% summary()
                 
                 # out_tr <- lm(r8tr20 ~ r5tr20+frail_comorbid21_2 + frail_comorbid21_3 + frail_comorbid22_1 +frail_comorbid22_2 + frail_comorbid22_3 + frail_comorbid23_1 +frail_comorbid23_2 + frail_comorbid23_3 + ragender + r5agey + raeducl, data = filter(w5_ols,dem_5 != 1)) %>% summary()
                 # out_or <- lm(r8orient ~ r5orient+frail_comorbid21_2 + frail_comorbid21_3 + frail_comorbid22_1 +frail_comorbid22_2 + frail_comorbid22_3 + frail_comorbid23_1 +frail_comorbid23_2 + frail_comorbid23_3 + ragender + r5agey + raeducl, data = filter(w5_ols,dem_5 != 1)) %>% summary()
                 # out_se <- lm(r8ser7 ~ r5ser7+frail_comorbid21_2 + frail_comorbid21_3 + frail_comorbid22_1 +frail_comorbid22_2 + frail_comorbid22_3 + frail_comorbid23_1 +frail_comorbid23_2 + frail_comorbid23_3 + ragender + r5agey + raeducl, data = filter(w5_ols,dem_5 != 1)) %>% summary()
                 # out_all <- lm(r8all ~ r5all+frail_comorbid21_2 + frail_comorbid21_3 + frail_comorbid22_1 +frail_comorbid22_2 + frail_comorbid22_3 + frail_comorbid23_1 +frail_comorbid23_2 + frail_comorbid23_3 + ragender + r5agey + raeducl, data = filter(w5_ols,dem_5 != 1)) %>% summary()
                 # 
                 # Mental wellbeing
                 f1 <- as.data.frame(out_cesd$coefficients)[2:9,] %>% mutate(analysis = "Depressive symptom") %>% add_row(analysis = "Depressive symptom") %>% add_row(analysis = "Depressive symptom") %>% add_row(analysis = "Depressive symptom") %>% add_row(analysis = "Depressive symptom")
                 f2 <- as.data.frame(out_sf$coefficients)[2:9,] %>% mutate(analysis = "Life satisfaction") %>% add_row(analysis = "Life satisfaction") %>% add_row(analysis = "Life satisfaction") %>% add_row(analysis = "Life satisfaction") %>% add_row(analysis = "Life satisfaction")
                 f3 <- as.data.frame(out_ha$coefficients)[2:9,] %>% mutate(analysis = "Happiness") %>% add_row(analysis = "Happiness") %>% add_row(analysis = "Happiness") %>% add_row(analysis = "Happiness") %>% add_row(analysis = "Happiness")
                 f4 <- as.data.frame(out_sh$coefficients)[2:9,] %>% mutate(analysis = "Self-reported health") %>% add_row(analysis = "Self-reported health") %>% add_row(analysis = "Self-reported health") %>% add_row(analysis = "Self-reported health") %>% add_row(analysis = "Self-reported health")
                 
                 forest_data <- rbind(f1, f2, f3, f4)
                 forest_data$Label <- rep(c("1", "≥2", "0", "1", "≥2", "0", "1", "≥2", "0 (Ref)", "Non-frail", "Pre-frail", "Frail"), times = 4)
                 forest_data$seq <- rep(c(3, 4, 6, 7, 8, 10, 11, 12, 2, 1, 5, 9), times = 4)
                 forest_data <- forest_data %>% arrange(seq) %>% mutate(seq = as.factor(seq),
                                                                        est = Estimate,
                                                                        se = `Std. Error`,
                                                                        est = ifelse(seq == 2, 0, est),
                                                                        subgroup = ifelse(is.na(est) == FALSE, paste0(seq, Label), est),
                                                                        lci = est - 1.96 * se,
                                                                        uci = est + 1.96 * se)
                 
                 row_labels <- forest_data %>% filter(analysis == "Depressive symptom") %>% select(subgroup, Label)
                 
                 p5_ols <- forest_plot(split(forest_data, ~analysis), col.key = "subgroup", row.labels = row_labels, exponentiate = F,
                                       scalepoints = T, nullval = 0, xlab = "",
                                       plot.margin = margin(1, 4, 1, 4, "mm"),
                                       xlim = c(-1.7, 1.7), row.labels.heading="KLoSA", base_size = 7,pointsize = 2,quiet = TRUE,panel.headings = rep("", 4),
                                       xticks = c(-1.5, -1, -0.5, 0, 0.5, 1.0, 1.5),
                                       base_line_size = 0.2)
                 p5_ols$plot <- p5_ols$plot + theme(panel.spacing.x  = unit(18, "mm"))
                 
                 # # Cognition
                 # f5 <- as.data.frame(out_tr$coefficients)[3:10,] %>% mutate(analysis = "Memory") %>% add_row(analysis = "Memory") %>% add_row(analysis = "Memory") %>% add_row(analysis = "Memory") %>% add_row(analysis = "Memory")
                 # f6 <- as.data.frame(out_or$coefficients)[3:10,] %>% mutate(analysis = "Orientation") %>% add_row(analysis = "Orientation") %>% add_row(analysis = "Orientation") %>% add_row(analysis = "Orientation") %>% add_row(analysis = "Orientation")
                 # f7 <- as.data.frame(out_se$coefficients)[3:10,] %>% mutate(analysis = "Numeracy") %>% add_row(analysis = "Numeracy") %>% add_row(analysis = "Numeracy") %>% add_row(analysis = "Numeracy") %>% add_row(analysis = "Numeracy")
                 # f8 <- as.data.frame(out_all$coefficients)[3:10,] %>% mutate(analysis = "Global cognitive function") %>% add_row(analysis = "Global cognitive function") %>% add_row(analysis = "Global cognitive function") %>% add_row(analysis = "Global cognitive function") %>% add_row(analysis = "Global cognitive function")
                 # forest_data2 <- rbind(f5, f6, f7, f8)
                 # 
                 # forest_data2$Label <- rep(c("1", "≥2", "0", "1", "≥2", "0", "1", "≥2", "0 (Ref)", "Non-frail", "Pre-frail", "Frail"), times = 4)
                 # forest_data2$seq <- rep(c(3, 4, 6, 7, 8, 10, 11, 12, 2, 1, 5, 9), times = 4)
                 # forest_data2 <- forest_data2 %>% arrange(seq) %>% mutate(seq = as.factor(seq),
                 #                                                          est = Estimate,
                 #                                                          se = `Std. Error`,
                 #                                                          est = ifelse(seq == 2, 0, est),
                 #                                                          subgroup = ifelse(is.na(est) == FALSE, paste0(seq, Label), est),
                 #                                                          lci = est - 1.96 * se,
                 #                                                          uci = est + 1.96 * se)
                 # 
                 # row_labels <- forest_data2 %>% filter(analysis == "Memory") %>% select(subgroup, Label)
                 # 
                 # p4b_ols <- forest_plot(split(forest_data2, ~analysis), col.key = "subgroup", row.labels = row_labels, exponentiate = FALSE,
                 #                        scalepoints = TRUE, nullval = 0, xlab = "",
                 #                        plot.margin = margin(1, 4, 1, 4, "mm"),
                 #                        row.labels.heading="SHARE", base_size = 7,pointsize = 2,quiet = TRUE,panel.headings = rep("", 4),
                 #                        xlim        = c(-0.75, 0.75),
                 #                        xticks      = c(-0.6, -0.4,-0.2,0,0.2,0.4,0.6),
                 #                        base_line_size = 0.2)
                 # p4b_ols$plot <- p4b_ols$plot + theme(panel.spacing.x  = unit(18, "mm"))
                 
               }
               
               
               df1 <- pivot_longer(w4_long, cols = starts_with("t_"), names_to = "time", values_to = "year")
               df2 <- pivot_longer(w4_long, cols = ends_with("whappyl"), names_to = "variable", values_to = "ha")
               df3 <- pivot_longer(w4_long, cols = ends_with("satwlife_k"), names_to = "variable", values_to = "sf")
               df4 <- pivot_longer(w4_long, cols = c("r4cesd10a", "r5cesd10b", "r6cesd10b", "r7cesd10b"), names_to = "variable", values_to = "cesd")
               df5 <- pivot_longer(w4_long, cols = ends_with("shlt"), names_to = "variable", values_to = "sh")
               
               df_long <- bind_cols(select(df1, c(pid, frail_comorbid2, frail,comorbid,year)), select(df2, c(ha)), select(df3, c(sf)), select(df4, c(cesd)), select(df5, c(sh))) %>%
                 filter(!is.na(year)) %>% left_join(select(w4, c(pid, ragender, r4agey, raeducl,wealth5, r4smokev, r4drinkev, r4psyche)), by = "pid")
               
               panel_data <- pdata.frame(df_long, index = c("pid","year"))
               
               fx_cesd <- plm(cesd ~ frail_comorbid2 + ragender + r4agey + raeducl+wealth5+r4smokev+r4drinkev+r4psyche, data = filter(panel_data, !is.na(cesd)), model = "random")
               fx_sf <- plm(sf ~ frail_comorbid2 + ragender + r4agey + raeducl+wealth5+r4smokev+r4drinkev+r4psyche, data = filter(panel_data, !is.na(sf)), model = "random")
               fx_ha <- plm(ha ~ frail_comorbid2 + ragender + r4agey + raeducl+wealth5+r4smokev+r4drinkev+r4psyche, data = filter(panel_data, !is.na(ha)), model = "random")
               
               panel_data <- pdata.frame(df_long, index = c("year"))
               fx_sh <- plm(sh ~ frail_comorbid2 + ragender + r4agey + raeducl+wealth5+r4smokev+r4drinkev+r4psyche, data = filter(panel_data, !is.na(sh)), model = "within")
               
               
               fix_coef <- function(out, label) {
                 if (label == "Self-reported health") {
                   df <- as.data.frame(out$coefficients)[1:8, ]
                 } else {
                   df <- as.data.frame(out$coefficients)[2:9, ]
                 }
                 
                 colnames(df) <- gsub("t-value", "z-value", colnames(df))
                 colnames(df) <- gsub("Pr\\(>\\|t\\|\\)", "Pr(>|z|)", colnames(df))
                 
                 df$analysis <- label
                 
                 blank <- df[1:4, ]
                 blank[,] <- NA
                 blank$analysis <- label
                 
                 bind_rows(df, blank)
               }
               
               out_cesd <- summary(fx_cesd)
               out_sf <- summary(fx_sf)
               out_ha <- summary(fx_ha)
               out_sh <- summary(fx_sh)
               
               f1 <- fix_coef(out_cesd, "Depressive symptom")
               f2 <- fix_coef(out_sf, "Life satisfaction")
               f3 <- fix_coef(out_ha, "Happiness")
               f4 <- fix_coef(out_sh, "Self-reported health")
               
               # f1 <- as.data.frame(out_cesd$coefficients)[2:9,] %>% mutate(analysis = "Depressive symptom") %>% add_row(analysis = "Depressive symptom") %>% add_row(analysis = "Depressive symptom") %>% add_row(analysis = "Depressive symptom") %>% add_row(analysis = "Depressive symptom")
               # f2 <- as.data.frame(out_sf$coefficients)[2:9,] %>% mutate(analysis = "Life satisfaction") %>% add_row(analysis = "Life satisfaction") %>% add_row(analysis = "Life satisfaction") %>% add_row(analysis = "Life satisfaction") %>% add_row(analysis = "Life satisfaction")
               # f3 <- as.data.frame(out_ha$coefficients)[2:9,] %>% mutate(analysis = "Happiness") %>% add_row(analysis = "Happiness") %>% add_row(analysis = "Happiness") %>% add_row(analysis = "Happiness") %>% add_row(analysis = "Happiness")
               # f4 <- as.data.frame(out_sh$coefficients)[1:8,] %>% mutate(analysis = "Self-reported health") %>% add_row(analysis = "Self-reported health") %>% add_row(analysis = "Self-reported health") %>% add_row(analysis = "Self-reported health") %>% add_row(analysis = "Self-reported health")
               # 
               forest_data <- rbind(f1, f2, f3, f4)
               forest_data_klosa <- rbind(f1, f2, f3, f4) %>% mutate(study="KLoSA",level=rep(c("1_2", "1_3", "2_1", "2_2", "2_3", "3_1", "3_2", "3_3", "", "", "",""),times=4))
               forest_data$Label <- rep(c("1", "≥2", "0", "1", "≥2", "0", "1", "≥2", "0 (Ref)", "Non-frail", "Pre-frail", "Frail"), times = 4)
               forest_data$seq <- rep(c(3, 4, 6, 7, 8, 10, 11, 12, 2, 1, 5, 9), times = 4)
               
               forest_data <- forest_data %>% arrange(seq) %>%
                 mutate(
                   seq = as.factor(seq),
                   est = Estimate,
                   se = `Std. Error`,
                   est = ifelse(seq == 2, 0, est),
                   subgroup = ifelse(is.na(est) == FALSE, paste0(seq, Label), est),
                   lci = est - 1.96 * se,
                   uci = est + 1.96 * se
                 )
               
               row_labels <- forest_data %>% filter(analysis == "Depressive symptom") %>% select(subgroup, Label)
               
               
               p5 <- forest_plot(split(forest_data, ~analysis), col.key = "subgroup", row.labels = row_labels, exponentiate = F,
                                 scalepoints = T, nullval = 0, xlab = "β (95% CI)",
                                 plot.margin = margin(1, 4, 4, 4, "mm"),
                                 xlim = c(-1.7, 1.7), row.labels.heading="KLoSA", base_size = 7,pointsize = 2,quiet = TRUE,
                                 panel.headings = rep("", 4),
                                 xticks = c(-1.5, -1, -0.5, 0, 0.5, 1.0, 1.5),
                                 base_line_size = 0.2)
               p5$plot <- p5$plot + theme(panel.spacing.x  = unit(18, "mm"))
               
               return(list(p5 = p5$plot, p5_ols=p5_ols$plot, df_long=df_long,forest=forest_data_klosa))
               
               }
               
               res_klosa <- klosa_fig()
               p5 <- res_klosa$p5
               p5_ols <- res_klosa$p5_ols
               klosa_long <- res_klosa$df_long
               forest_data_klosa <- res_klosa$forest
               
               
               #Interaction of frailty and multimorbidity
               interact_klosa <- function(){
                 
                 outcomes <- c("cesd","ha","sf","sh")
                 interaction_klosa <- data.frame()
                 covar <- "+ragender + r4agey + raeducl+wealth5+r4smokev+r4drinkev+r4psyche"
                 df_long <- klosa_long
                 for (outcome in outcomes) {
                   if(outcome=="sh"){
                     panel_data <- pdata.frame(df_long, index = c("year"))
                     model <- "within"
                   }
                   else{
                     panel_data <- pdata.frame(df_long, index = c("pid","year"))
                     model <- "random"
                   }
                   
                   if(outcome=="sh"){
                     #panel_data <- pdata.frame(df_long, index = c("year"))
                     outcome1 <- plm(as.formula(paste0(outcome,"~as.factor(frail)+as.factor(comorbid)",covar)),data = filter(panel_data,!is.na(get(outcome))),model = model) %>% summary()
                     outcome2 <- plm(as.formula(paste0(outcome,"~as.factor(frail)*as.factor(comorbid)",covar)),data = filter(panel_data,!is.na(get(outcome))),model = model) %>% summary()
                     
                     m1 <- plm(as.formula(paste0(outcome,"~as.factor(frail)+as.factor(comorbid)",covar)),data = filter(panel_data,!is.na(get(outcome))),model = model)
                     m2 <- plm(as.formula(paste0(outcome,"~as.factor(frail)*as.factor(comorbid)",covar)),data = filter(panel_data,!is.na(get(outcome))),model = model)
                     
                     
                     outcome3 <- as.data.frame(outcome1$coefficients)[1:4,] %>% mutate(outcome=outcome,p=pFtest(m2, m1)$p.value) %>% 
                       mutate(est=Estimate, stderr=`Std. Error`,lci=est-1.96*stderr, hci=est+1.96*stderr, beta=paste0(round(est,digits=2), " (",round(lci,digits=2)," to ",round(hci,digits=2),")")) %>% select(est,lci,hci,beta,outcome,p)
                   }
                   else{
                     #panel_data <- pdata.frame(df_long, index = c("idauniq","year"))
                     outcome1 <- plm(as.formula(paste0(outcome,"~as.factor(frail)+as.factor(comorbid)",covar)),data = filter(panel_data,!is.na(get(outcome))),model = model) %>% summary()
                     outcome2 <- plm(as.formula(paste0(outcome,"~as.factor(frail)*as.factor(comorbid)",covar)),data = filter(panel_data,!is.na(get(outcome))),model = model) %>% summary()
                     
                     m1 <- plm(as.formula(paste0(outcome,"~as.factor(frail)+as.factor(comorbid)",covar)),data = filter(panel_data,!is.na(get(outcome))),model = model)
                     m2 <- plm(as.formula(paste0(outcome,"~as.factor(frail)*as.factor(comorbid)",covar)),data = filter(panel_data,!is.na(get(outcome))),model = model)
                     
                     outcome3 <- as.data.frame(outcome1$coefficients)[2:5,] %>% mutate(outcome=outcome,p=pFtest(m2, m1)$p.value) %>% 
                       mutate(est=Estimate, stderr=`Std. Error`,lci=est-1.96*stderr, hci=est+1.96*stderr, beta=paste0(round(est,digits=2), " (",round(lci,digits=2)," to ",round(hci,digits=2),")")) %>% select(est,lci,hci,beta,outcome,p)
                   }
                   interaction_klosa<- rbind(interaction_klosa,outcome3)
                 }
                 return(interaction_klosa)
               }
               
               interaction_klosa<- interact_klosa()
               empty_rows <- data.frame(
                 est = rep(NA, 8),
                 lci = rep(NA, 8),
                 hci = rep(NA, 8),
                 beta = rep(NA, 8),
                 outcome = rep("all", 8),
                 p = rep(NA, 8)
               )
               
               interaction_klosa <- rbind(empty_rows, interaction_klosa)
               
               interaction <- cbind(inter_else,inter_charls,inter_hrs,inter_share,interaction_klosa)
               write.csv(interaction,file="E:\\Tutor\\Multi-region study\\frailty and mental-wellbeing\\interaction.csv")
               
               
               
###Figure 1####
               country_mapping <- data.frame(
                 isocountry = c(1,2,3,4,40,56,100,191,196,203,208,233,246,250,276,300,348,372,376,380,428,440,442,470,528,616,620,642,703,705,724,752,756),
                 country_name = c("USA","UK","Korea","China","Austria","Belgium","Bulgaria","Croatia","Cyprus","Czech Republic","Denmark","Estonia","Finland","France","Germany","Greece","Hungary","Ireland","Israel","Italy","Latvia","Lithuania","Luxembourg","Malta","Netherlands","Poland","Portugal","Romania","Slovakia","Slovenia","Spain","Sweden","Switzerland")
               )
               
               w11_summary <- w11 %>% mutate(isocountry = 1) %>% select(isocountry, frail, comorbid) # HRS
               w6_summary <- w6 %>% mutate(isocountry = 2) %>% select(isocountry, frail, comorbid) # ELSA
               w5_summary <- w5 %>% select(isocountry, frail, comorbid) # SHARE
               w4_summary <- w4 %>% mutate(isocountry = 3) %>% select(isocountry, frail, comorbid) # KLoSA
               w2_summary <- w2 %>% mutate(isocountry = 4) %>% select(isocountry, frail, comorbid) # CHARLS
               
               all <- rbind(w11_summary, w6_summary, w5_summary, w4_summary, w2_summary)

               all_summary <- all %>%
                 group_by(isocountry, frail, comorbid) %>%
                 summarise(count = n()) %>%
                 group_by(isocountry, frail) %>%
                 mutate(
                   percent = count / sum(count) * 100,
                   frail = factor(frail, levels = c(1, 2, 3), labels = c("Non-frail", "Pre-frail", "Frail")),
                   comorbid = factor(comorbid, levels = c(1, 2, 3), labels = c("0", "1", "≥2"))
                 ) %>% merge(country_mapping, by = "isocountry", all.x = TRUE)
               
               all_summary2 <- all %>%
                 group_by(isocountry, frail, comorbid) %>%
                 summarise(count = n()) %>%
                 group_by(isocountry) %>%
                 mutate(total_count = sum(count),
                        percent = count / total_count * 100) %>%
                 mutate(frail = factor(frail, levels = c(1, 2, 3), labels = c("Non-frail", "Pre-frail", "Frail")),
                                      comorbid=factor(comorbid,levels = c(1, 2, 3),labels = c("0", "1", "≥2"))
                                      )%>% merge(country_mapping, by = "isocountry", all.x = TRUE)
               
               pp1 <- ggplot(all_summary2, aes(x = factor(frail), y = percent, fill = factor(comorbid))) +
                 geom_bar(stat = "identity", position = position_stack(), width = 1, color = "black", size = 0.2) +
                 facet_grid(~factor(country_name, levels = c("USA", "UK", "Austria", "Belgium", "Czech Republic", "Denmark",
                                                             "Estonia", "France", "Germany", "Israel", "Italy", "Luxembourg",
                                                             "Netherlands", "Slovenia", "Spain", "Sweden", "Switzerland",
                                                             "Korea", "China")), scales = "free_x", space = "free", switch = "y") +
                 labs(title = "",
                      x = "",
                      y = "Prevalence (%)") +
                 scale_fill_manual(values = c("lightgreen", "lightblue", "lightcoral"),
                                   name = "No. of disease") +
                 theme_minimal() + theme(panel.grid = element_blank(),
                                         axis.line = element_line(color = "black", size = 0.2),
                                         axis.ticks = element_line(color = "black", size = 0.2),
                                         panel.spacing = unit(0, "lines"),
                                         axis.text.x = element_blank(),
                                         strip.text = element_text(angle = 30, hjust = 0, vjust = 0, size = 8),
                                         strip.background = element_blank(),
                                         strip.placement = "outside",
                                         legend.position = "top",
                                         legend.text = element_text(size = 8),
                                         legend.title = element_text(size = 8),
                                         legend.key.size = unit(0.2, "cm"),
                                         axis.title.y = element_text(size = 8)) +
                 scale_x_discrete(expand = c(0, 0)) +
                 coord_cartesian(xlim = c(0, 4), expand = FALSE) +
                 geom_hline(yintercept = 0, color = "black", linetype = "solid", size = 0.2)
               

               pp2 <- ggplot(all_summary, aes(x = factor(frail), y = percent, fill = factor(comorbid))) +
                 geom_bar(stat = "identity", position = position_stack(), width = 1, color = "black", size = 0.2) +
                 facet_grid(~isocountry, scales = "free_x", space = "free", switch = "y") +
                 labs(title = "",
                      x = "",
                      y = "Prevalence (%)") +
                 scale_fill_manual(values = c("lightgreen", "lightblue", "lightcoral"),
                                   name = "No. of disease") +
                 theme_minimal() + theme(panel.grid = element_blank(),
                                         axis.line = element_line(color = "black", size = 0.2),
                                         axis.ticks = element_line(color = "black", size = 0.2),
                                         panel.spacing = unit(0, "lines"),
                                         axis.text.x = element_text(angle = 45, hjust = 1),
                                         strip.background = element_blank(),
                                         strip.placement = "outside",
                                         legend.position = "none",
                                         strip.text = element_blank(),
                                         axis.title.y = element_text(size = 8)) +
                 scale_x_discrete(expand = c(0, 0)) +
                 coord_cartesian(xlim = c(0, 4), expand = FALSE) +
                 geom_hline(yintercept = 0, color = "black", linetype = "solid", size = 0.2)
               
               
               plot_grid(pp1,pp2,ncol=1,align = "v",rel_heights =3:2,labels = "AUTO", label_size = 10)
               
               
###Meta-analysis for all outcomes####
               ##Clear the RAM
               # rm(list=setdiff(ls(), c("forest_data_elsa", "forest_data2_elsa",
               #                         "forest_data_charls", "forest_data2_charls",
               #                         "forest_data_hrs", "forest_data2_hrs",
               #                         "forest_data_share", "forest_data2_share",
               #                         "forest_data_klosa"
               # )))
               ###Mental well-being outcome
               forest_data_all <- rbind(forest_data_elsa, forest_data_charls, forest_data_hrs,forest_data_share,forest_data_klosa)         
               
               run_meta_analysis <- function(data) {
                 
                 data <- data %>% filter(!is.na(Estimate))
                 unique_analysis <- unique(data$analysis)
                 
                 forest_plots <- list()
                 results <- data.frame()
                 
                 for (analysis_var in unique_analysis) {
                   
                   filtered_data <- data %>%filter(analysis == analysis_var)
                   
                   result <- meta::metagen(
                     filtered_data$Estimate,
                     filtered_data$`Std. Error`,
                     sm = "Beta",
                     studlab = filtered_data$study,
                     data = filtered_data,
                     comb.random=T,comb.fixed=F,
                     subgroup = filtered_data$level,
                     showweights = TRUE,
                     method.tau="REML"
                   )
                   
                   results <- rbind(results,data.frame(level=result$subgroup.levels,
                                                       Estimate=result$TE.random.w,
                                                       `Std. Error`=result$seTE.random.w,
                                                       analysis=analysis_var
                   ))
                   
                   pdf(file = paste0("E:\\Tutor\\Multi-region study\\frailty and mental-wellbeing\\Tables and figures\\forestplot_",analysis_var,".pdf"), width = 150/25.4, height =380/25.4)
                   meta::forest(result, layout = "JAMA", xlim = c(-1.5, 1.5))
                   dev.off()
                   
                 }
                 return(results)
               }
               
               meta_results <- run_meta_analysis(forest_data_all)
               
               meta_results_mental <- forest_data_all %>% filter(study=="ELSA") %>% select(analysis,level) %>% left_join(meta_results,by=c("analysis","level"))
               
               meta_results_mental$Label <- rep(c("1", "≥2", "0", "1", "≥2", "0", "1", "≥2", "0 (Ref)", "Non-frail", "Pre-frail", "Frail"), times = 4)
               meta_results_mental$seq <- rep(c(3, 4, 6, 7, 8, 10, 11, 12, 2, 1, 5, 9), times = 4)
               forest_data <- meta_results_mental %>% arrange(seq) %>% mutate(seq = as.factor(seq),
                                                                              est = Estimate,
                                                                              se = `Std..Error`,
                                                                              est = ifelse(seq == 2, 0, est),
                                                                              subgroup = ifelse(is.na(est) == FALSE, paste0(seq, Label), est),
                                                                              lci = est - 1.96 * se,
                                                                              uci = est + 1.96 * se)
               
               row_labels <- forest_data %>% filter(analysis == "Depressive symptom") %>% select(subgroup, Label)
               
               p1 <- forest_plot(split(forest_data, ~analysis), col.key = "subgroup", row.labels = row_labels, exponentiate = FALSE,
                           scalepoints = T, nullval = 0, xlab = "β (95% CI)",pointsize = 2,base_size = 7,
                           title = "", plot.margin = margin(4, 4, 4, 4, "mm"),
                           xlim = c(-1.7, 1.7),
                           xticks = c(-1.5, -0.75, 0, 0.75, 1.5),
                           base_line_size = 0.1)
               
               ###Cognitive outcome
               forest_data2_all <- rbind(forest_data2_elsa, forest_data2_charls, forest_data2_hrs,forest_data2_share)             
               meta_results <- run_meta_analysis(forest_data2_all)
               
               meta_results_cog <- forest_data2_all %>% filter(study=="ELSA") %>% select(analysis,level) %>% left_join(meta_results,by=c("analysis","level"))
               
               meta_results_cog$Label <- rep(c("1", "≥2", "0", "1", "≥2", "0", "1", "≥2", "0 (Ref)", "Non-frail", "Pre-frail", "Frail"), times = 4)
               meta_results_cog$seq <- rep(c(3, 4, 6, 7, 8, 10, 11, 12, 2, 1, 5, 9), times = 4)
               forest_data <- meta_results_cog %>% arrange(seq) %>% mutate(seq = as.factor(seq),
                                                                              est = Estimate,
                                                                              se = `Std..Error`,
                                                                              est = ifelse(seq == 2, 0, est),
                                                                              subgroup = ifelse(is.na(est) == FALSE, paste0(seq, Label), est),
                                                                              lci = est - 1.96 * se,
                                                                              uci = est + 1.96 * se)
               
               row_labels <- forest_data %>% filter(analysis == "Memory") %>% select(subgroup, Label)
               
               p2 <-forest_plot(split(forest_data, ~analysis), col.key = "subgroup", row.labels = row_labels, exponentiate = FALSE,
                           scalepoints = T, nullval = 0, xlab = "β (95% CI)",pointsize = 2,base_size = 7,
                           title = "", plot.margin = margin(1, 4, 4, 4, "mm"),
                           xlim = c(-1.7, 1.7),
                           xticks = c(-1.5, -0.75, 0, 0.75, 1.5),
                           base_line_size = 0.1)
               
               
               ###Dementia
               forest_data3_all <- rbind(forest_data3_elsa %>% mutate(study="ELSA"), 
                                         forest_data3_charls %>% mutate(study="CHARLS"), 
                                         forest_data3_hrs %>% mutate(study="HRS"),
                                         forest_data3_share %>% mutate(study="SHARE")) %>% 
                 mutate(level=rep(c("1_2", "1_3", "2_1", "2_2", "2_3", "3_1", "3_2", "3_3", "", "", "",""),times=4)) %>% 
                 rename(Estimate=coef,`Std. Error`=`se(coef)`)
               meta_results <- run_meta_analysis(forest_data3_all)
               
               meta_results_dem <- forest_data3_all %>% slice(1:12) %>% select(analysis,level) %>% left_join(meta_results,by=c("analysis","level"))
               
               meta_results_dem$Label <- rep(c("1", "≥2", "0", "1", "≥2", "0", "1", "≥2", "0 (Ref)", "Non-frail", "Pre-frail", "Frail"), times = 1)
               meta_results_dem$seq <- rep(c(3, 4, 6, 7, 8, 10, 11, 12, 2, 1, 5, 9), times = 1)
               forest_data <- meta_results_dem %>% arrange(seq) %>% mutate(seq = as.factor(seq),
                                                                           est = Estimate,
                                                                           se = `Std..Error`,
                                                                           est = ifelse(seq == 2, 0, est),
                                                                           subgroup = ifelse(is.na(est) == FALSE, paste0(seq, Label), est),
                                                                           lci = est - 1.96 * se,
                                                                           uci = est + 1.96 * se)
               
               row_labels <- forest_data %>% filter(analysis == "Dementia") %>% select(subgroup, Label)
               
               p3 <-forest_plot(split(forest_data, ~analysis), col.key = "subgroup", row.labels = row_labels, exponentiate = T,
                                scalepoints = T, nullval = 1, xlab = "HR (95% CI)",pointsize = 2, base_size = 7,
                                title = "", plot.margin = margin(1, 140, 4, 4, "mm"),
                                xlim = c(0.8, 6),
                                xticks = c(1, 2,4,6),
                                base_line_size = 0.1)
               
               
               cairo_pdf("E:/Tutor/Multi-region study/frailty and mental-wellbeing/Tables and figures/Figures/Figure 4.pdf", height =5, width =7.6)
                 plot_grid(p1$plot, p2$plot, p3$plot,nrow=3,labels = "auto",label_size = 6)
               dev.off()
               
               ### Saving working image--------
               save.image("E:/Tutor/Multi-region study/frailty and mental-wellbeing/Data/all.rdata")
