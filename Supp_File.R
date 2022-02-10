#1)Imports, functions and DB ####
library(mlr3)
library(mlr3learners)
library(mlr3tuning)
library(mlr3pipelines)
library(mlr3verse)
library(mlr3extralearners)
library(readxl)
library(tidyverse)
library(kableExtra)
library(qdapTools)
library(gbm)
library(ResourceSelection)
library(DescTools)
library("DALEX")
library("DALEXtra")
library(atable)
library(xlsx)

plot(c(0,1),c(0,1), col="grey",type="l",xlab = "Mean Prediction",ylab="Observed Fraction")
reliability.plot <- function(obs, pred, bins=10, scale=T) {
  # Plots a reliability chart and histogram of a set of predicitons from a classifier
  #
  # Args:
  # obs: Vector of true labels. Should be binary (0 or 1)
  # pred: Vector of predictions of each observation from the classifier. Should be real
  # number
  # bins: The number of bins to use in the reliability plot
  # scale: Scale the pred to be between 0 and 1 before creating reliability plot
  require(plyr)
  library(Hmisc)
  min.pred <- min(pred)
  max.pred <- max(pred)
  min.max.diff <- max.pred - min.pred
  if (scale) {
    pred <- (pred - min.pred) / min.max.diff
  }
  bin.pred <- cut(pred, bins)
  k <- ldply(levels(bin.pred), function(x) {
    idx <- x == bin.pred
    c(sum(obs[idx]) / length(obs[idx]), mean(pred[idx]))
  })
  is.nan.idx <- !is.nan(k$V2)
  k <- k[is.nan.idx,]
  return(k)
}

fit.isoreg <- function(iso, x0)
{
  o = iso$o
  if (is.null(o))
    o = 1:length(x)
  x = iso$x[o]
  y = iso$yf
  ind = cut(x0, breaks = x, labels = FALSE, include.lowest = TRUE)
  min.x <- min(x)
  max.x <- max(x)
  adjusted.knots <- iso$iKnots[c(1, which(iso$yf[iso$iKnots] > 0))]
  fits = sapply(seq(along = x0), function(i) {
    j = ind[i]
    
    # Handles the case where unseen data is outside range of the training data
    if (is.na(j)) {
      if (x0[i] > max.x) j <- length(x)
      else if (x0[i] < min.x) j <- 1
    }
    
    # Find the upper and lower parts of the step
    upper.step.n <- min(which(adjusted.knots > j))
    upper.step <- adjusted.knots[upper.step.n]
    lower.step <- ifelse(upper.step.n==1, 1, adjusted.knots[upper.step.n -1] )
    
    # Perform a liner interpolation between the start and end of the step
    denom <- x[upper.step] - x[lower.step]
    denom <- ifelse(denom == 0, 1, denom)
    val <- y[lower.step] + (y[upper.step] - y[lower.step]) * (x0[i] - x[lower.step]) / (denom)
    
    # Ensure we bound the probabilities to [0, 1]
    val <- ifelse(val > 1, max.x, val)
    val <- ifelse(val < 0, min.x, val)
    val <- ifelse(is.na(val), max.x, val) # Bit of a hack, NA when at right extreme of distribution
    val
  })
  fits
}

Gastros_FINAL <- read_excel("Dataset.xlsx")
Gastros_FINAL <- rename_with(Gastros_FINAL, ~ gsub(" ", "_", .x))

#2)Create 90DM variable####
Gastros_FINAL$Fecha_cirugía <- as.Date(Gastros_FINAL$Fecha_cirugía)
Gastros_FINAL$Fecha_de_exitus <- as.Date(Gastros_FINAL$Fecha_de_exitus)

Gastros_FINAL$Exitus_90d <- (Gastros_FINAL$Fecha_de_exitus - Gastros_FINAL$Fecha_cirugía) < 90 

no_exitus <- Gastros_FINAL[is.na(Gastros_FINAL$Fecha_de_exitus),]
Gastros_FINAL <- Gastros_FINAL[!is.na(Gastros_FINAL$Fecha_de_exitus),]

no_exitus$Exitus_90d[ as.Date("2021-10-01") - no_exitus$Fecha_cirugía > 90] <- FALSE
Gastros_FINAL <- rbind(Gastros_FINAL, no_exitus)

Gastros_FINAL$Exitus_90d<- factor(Gastros_FINAL$Exitus_90d,
                                  levels = c(TRUE, FALSE),
                                  labels = c("Yes",
                                             "No"))
table(Gastros_FINAL$Exitus_90d)
#3)Recode Comorbilidad de Charlson####
Gastros_FINAL$Comorbilidad_de_Charlson <- gsub("\\(", "", Gastros_FINAL$Comorbilidad_de_Charlson)
Gastros_FINAL$Comorbilidad_de_Charlson <- gsub("\\)", "", Gastros_FINAL$Comorbilidad_de_Charlson)
Gastros_FINAL$Comorbilidad_de_Charlson <- gsub(" ", "_", Gastros_FINAL$Comorbilidad_de_Charlson)
Gastros_FINAL$Comorbilidad_de_Charlson <- gsub("-", "_", Gastros_FINAL$Comorbilidad_de_Charlson)
Gastros_FINAL$Comorbilidad_de_Charlson <- gsub(",_", ", ", Gastros_FINAL$Comorbilidad_de_Charlson)

Gastros_FINAL <- cbind(Gastros_FINAL, mtabulate(strsplit(Gastros_FINAL$Comorbilidad_de_Charlson, ", ")))

#4)Create Volume_activity ####
Gastros_FINAL$Year <- format(Gastros_FINAL$Fecha_cirugía,"%Y")
volume_act <- Gastros_FINAL %>% group_by(Year, Centro) %>% summarise(count = n())
colnames(volume_act)[3] <- "Volume_activity"

Gastros_FINAL <- left_join(Gastros_FINAL, volume_act)

#5)Recode variables ####
Gastros_FINAL <- rename(Gastros_FINAL, Albumina = `Albúmina_(gr/L)_*(la_más_cercana_a_la_IQ)`)
Gastros_FINAL$Intencion_acceso <- factor(Gastros_FINAL$Acceso_estómago, 
                                         levels = c("Abierto", "Laparoscópico", "Laparoscópico con conversión", "Robótico"),
                                         labels =  c("Open", "Minimal invasive approach", "Minimal invasive approach", 
                                                     "Minimal invasive approach"))
Gastros_FINAL$Tipo_gastrectomia_TotalParcial <- factor(Gastros_FINAL$Tipo_gastrectomía, 
                                                       levels = c("Distal subtotal", "Total", 
                                                                  "Total ampliada (con resección de esófago distal)",
                                                                  "Total profiláctica (en portadores de mutación del Gen CDH1 u otros)",
                                                                  "Proximal"),
                                                       labels = c("Subtotal", "Total", "Total", "Total", "Subtotal"))

#6)MLR3 hyperparameter tuning pipeline####
#Define variables for modelling without preprocessing (as previously done)

Gastros_FINAL$TIPO_CIR <- factor(Gastros_FINAL$Tipo_de_cirugía,
                                 levels = c("Electiva", "Urgente", "De rescate (tumor que se hace resecable tras tratamiento complementario de inducción)"),
                                 labels = c("Elective", "Urgent", "Urgent"))
Gastros_FINAL$QT_PERIOP <- factor(Gastros_FINAL$Tratamiento_neodyuvante, levels = levels(as.factor(Gastros_FINAL$Tratamiento_neodyuvante)),
                                  labels = c("No", "Chemo-Radiotherapy", "Chemotherapy", "Chemotherapy", "Chemotherapy"))
Gastros_FINAL$Indice_ASA <- factor(Gastros_FINAL$Índice_ASA, levels = levels(as.factor(Gastros_FINAL$Índice_ASA)),
                                   labels = c("score I", "score II", "score III", "score IV", "score IV"))
Gastros_FINAL$ECOG <- factor(Gastros_FINAL$Escala_funcional_Zubrod,
                             levels = levels(as.factor(Gastros_FINAL$Escala_funcional_Zubrod)),
                             labels = c("perfomance status 0","perfomance status 1","perfomance status 2",
                                        "perfomance status >= 3","perfomance status >= 3"))
Gastros_FINAL$Porcentaje_de_peso_perdido <- factor(Gastros_FINAL$Porcentaje_de_peso_perdido, 
                                                   levels = levels(as.factor(Gastros_FINAL$Porcentaje_de_peso_perdido)),
                                                   labels = c("0 - 5%", "6 - 10%", "> 10%"))
Gastros_FINAL$Estadio_clínico_cT <- as.factor(Gastros_FINAL$Estadio_clínico_cT)
Gastros_FINAL$Estadio_clínico_cN <- factor(Gastros_FINAL$Estadio_clínico_cN, levels = c("-", "+"), labels = c(0,1))

variables <- c("Genero", "Edad", "IC", "ECOG", "Porcentaje_de_peso_perdido", "Hb_preoperatoria",
               "Infarto_de_miocardio", "Insuficiencia_cardiaca_congestiva",
               "Enfermedad_pulmonar_crónica","Enfermedad_del_tejido_conectivo", "Enfermedad_vascular_periférica",
               "Enfermedad_cerebro_vascular", "Demencia", "Enfermedad_ulcerosa", "Diabetes_Mellitus_sin_complicaciones",
               "Diabetes_Mellitus_con_lesión_orgánica", "Leucemia", "Linfoma_maligno", "Enfermedad_hepática_crónica_moderada_o_severa",
                 "Hemiplejia", "Tumor_maligno_neoplasias", "Tumor_sólido_con_metástasis", "Enfermedad_renal_moderada_o_grave",
               "SIDA", "Albumina" ,"TIPO_CIR", "QT_PERIOP", "Estadio_clínico_cT", "Estadio_clínico_cN",#QT PERIOP
               "Intencion_acceso", "Tipo_gastrectomia_TotalParcial","Volume_activity","Indice_ASA",  "Exitus_90d")

Gastros_FINAL_mlr3 <- Gastros_FINAL %>% dplyr::select(variables)
Gastros_FINAL_mlr3 %>% group_by(Exitus_90d) %>% tally
colnames(Gastros_FINAL_mlr3) <- c("Gender", "Age", "BMI_index", "ECOG", "Weigth_loss", "Preoperative_Hemoglobin",
                                  "Myocardial_infarction", "Congestive_heart_failure",
                                  "Chronic_pulmonary_disease", "Connective_tissue_disease", "Peripheral_vascular_disease",
                                  "Cerebrovascular_disease", "Dementia", "Peptic_ulcer_disease", "Diabetes_mellitus_uncomplicated",
                                  "Diabetes_mellitus_end_organ_damage", "Leukemia", "Malignant_lymphoma", "Liver_disease_moderate_to_severe",
                                  "Hemiplegia", "Malignant_tumor_present", "Metastatic_tumor_present", "Moderate_to_severe_renal_disease",
                                  "AIDS", "Preoperative_Albumin" ,"Surgery", "Neoadjuvant_therapy", #QT PERIOP
                                  "Clinical_stage_cT", "Clinical_stage_cN",
                                  "Approach", "Gastrectomy","Volume_activity","ASA", "Exitus_90d")
task = TaskClassif$new(id = 'Gastro', Gastros_FINAL_mlr3, target = 'Exitus_90d')


write_csv(Gastros_FINAL_mlr3[1,], "test_EURECCA.csv", col_names = TRUE)
#Create mlr3 pipeline for training first round of models
ord_to_int = po("colapply", applicator = as.integer,
                affect_columns = selector_type("ordered"))
lrn = lrn("classif.xgboost", nrounds = 100, predict_type = "prob")
p0 = ppl("robustify", task, lrn) %>>%  ord_to_int %>>% po("imputeoor")
p2 = flt("anova")

learners_all = as.data.table(list_mlr3learners()) #select = c("id", "mlr3_package", "required_packages"))
learners_to_try <- learners_all %>% filter(class == "classif") %>% filter(grepl("weights", properties)) %>% filter(grepl("integer", feature_types))
learners_to_try <- learners_to_try %>% dplyr::select(c(name, id))
learners_to_try <- as.data.frame(learners_to_try)
learners_to_try <- learners_to_try[c(5,10,11,16,20,21),]
myvar <- list()
for (i in learners_to_try$name) { myvar[[i]]  <-  lrn(learners_to_try[learners_to_try$name == i,][[2]],  id = i, predict_type = "prob" )}
p3 = ppl("branch", myvar, prefix_branchops = "lrn_")
gr_smote = #Best oversampling method
  po("colapply", id = "int_to_num",
     applicator = as.numeric, affect_columns = selector_type("integer")) %>>%
  po("smote", dup_size = 10) %>>%
  po("colapply", id = "num_to_int",
     applicator = function(x) as.integer(round(x, 0L)), affect_columns = selector_type("numeric"))
gr = p0 %>>% gr_smote %>>% p2 %>>% p3
plot(gr, html=TRUE)
glrn = GraphLearner$new(gr)
params <- as.data.table(glrn$param_set)

ps = ParamSet$new(list(
  #ParamFct$new("encode.method", levels = c("one-hot", "treatment")),
  #ParamFct$new("aov_branch.selection", levels = c("anova", "nothing")),
  ParamDbl$new("anova.filter.frac", lower = 0.75, upper = 1),
  ParamFct$new("lrn_branch.selection", levels = c(paste(learners_to_try$name))),
  ParamInt$new("ranger.mtry", lower = 1, upper = 4),
  ParamInt$new("ranger.num.trees", lower = 300, upper = 1000),
  ParamInt$new("gbm.n.trees", lower = 100, upper = 5000),
  ParamInt$new("glmboost.mstop",lower = 1, upper = 1000),
  ParamInt$new("rfsrc.mtry", lower = 1, upper = 10),
  ParamInt$new("rfsrc.ntree", lower = 300, upper = 2000)))
#ParamInt$new("rf.mtry", lower = 1, upper = 10),
# ParamInt$new("xgb.nrounds", lower = 1, upper = 500),
# ParamDbl$new("svm.cost", lower = -12, upper = 4),
# ParamDbl$new("svm.gamma", lower = -12, upper = -1)))
ps$add_dep("ranger.mtry", "lrn_branch.selection", CondEqual$new("ranger"))
ps$add_dep("ranger.num.trees", "lrn_branch.selection", CondEqual$new("ranger"))
ps$add_dep("gbm.n.trees", "lrn_branch.selection", CondEqual$new("gbm"))
ps$add_dep("glmboost.mstop", "lrn_branch.selection", CondEqual$new("glmboost"))
ps$add_dep("rfsrc.ntree", "lrn_branch.selection", CondEqual$new("rfsrc"))
ps$add_dep("rfsrc.mtry", "lrn_branch.selection", CondEqual$new("rfsrc"))

instance = tune(
  method = "random_search",
  task = task,
  learner = glrn,
  search_space = ps,
  resampling = rsmp("cv", folds = 5),
  measure = msr("classif.auc"),
  term_evals = 12000
)

autoplot(instance)
instance$result_learner_param_vals
data <- as.data.table(instance$archive)
ggplot(data, aes(x=anova.filter.frac, y=classif.auc, color=lrn_branch.selection))+
  geom_point()+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  facet_wrap(~lrn_branch.selection)
ggsave("lrn_select_aov_with_finetunings.png")

#GLMboost works best out of the box (loking at default plots) but random forest peaks best with finetuning

#Best 3 classfiers were cv_glmnet, ranger and glmboost. Let's fine tune:
#1 Ranger: mtry=1
ranger = lrn("classif.ranger", predict_type = "prob", id="ranger")
ranger$param_set$values = list(mtry = 1, num.threads=12)
gr = p0 %>>% gr_smote %>>% p2 %>>% ranger
plot(gr)

glrn = GraphLearner$new(gr)
params <- as.data.table(glrn$param_set)

ps = ParamSet$new(list(
  ParamDbl$new("anova.filter.frac", lower = 0.92, upper = 1),
  ParamInt$new("ranger.num.trees", lower = 1000, upper = 2000),
  ParamInt$new("ranger.min.node.size", lower = 30, upper = 80),
  ParamDbl$new("ranger.sample.fraction", lower = 0.2, upper = 1)))

instance = tune(
  method = "random_search",
  task = task,
  learner = glrn,
  search_space = ps,
  resampling = rsmp("cv", folds = 5),
  measure = msr("classif.auc"),
  term_evals = 500
)

autoplot(instance)
instance$result_learner_param_vals
data <- as.data.table(instance$archive)
ggplot(data, aes(x=ranger.min.node.size, y=classif.auc, color=ranger.num.trees))+
  geom_point()+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggsave("./ranger_aov_num.trees_node.size_sample.fraction.png")

#With nested resampling
ps = ParamSet$new(list(
  ParamDbl$new("anova.filter.frac", lower = 0.94, upper = 0.99),
  ParamInt$new("ranger.num.trees", lower = 1000, upper = 2000),
  ParamInt$new("ranger.min.node.size", lower = 50, upper = 100),
  ParamDbl$new("ranger.sample.fraction", lower = 0.2, upper = 0.6)))

inner_resampling = rsmp("cv", folds = 5)
at = AutoTuner$new(
  learner = glrn,
  resampling = inner_resampling,
  search_space = ps,
  measure = msr("classif.auc"),
  terminator = trm("evals", n_evals = 1000),
  tuner = tnr("random_search")
)

outer_resampling = rsmp("cv", folds = 10)

rr = resample(task, at, outer_resampling, store_models = TRUE)

extract_inner_tuning_results(rr)
# anova.filter.frac ranger.num.trees ranger.min.node.size ranger.sample.fraction learner_param_vals  x_domain classif.auc
# 1:         0.9439803             1587                   69              0.2009548         <list[24]> <list[4]>   0.7542898
# 2:         0.9844909             1147                   95              0.2064160         <list[24]> <list[4]>   0.7486464
# 3:         0.9882372             1571                   70              0.3242656         <list[24]> <list[4]>   0.7517644
# 4:         0.9871095             1398                   81              0.2852851         <list[24]> <list[4]>   0.7573092
# 5:         0.9405545             1741                   76              0.2523068         <list[24]> <list[4]>   0.7628593
# 6:         0.9553703             1732                   78              0.2052082         <list[24]> <list[4]>   0.7573025
# 7:         0.9412467             1650                   54              0.3699149         <list[24]> <list[4]>   0.7635410
# 8:         0.9482514             1637                   75              0.2490927         <list[24]> <list[4]>   0.7557170
# 9:         0.9541142             1826                   50              0.2366836         <list[24]> <list[4]>   0.7675478
# 10:         0.9853851             1780                   52              0.2126556         <list[24]> <list[4]>   0.7551023

rr$score(measures = msr("classif.auc"))
# task task_id         learner                                                                                                      learner_id
# 1: <TaskClassif[46]>  Gastro <AutoTuner[38]> char_to_fct.fixfactors.encode.removeconstants.colapply.imputeoor.int_to_num.smote.num_to_int.anova.ranger.tuned
# 2: <TaskClassif[46]>  Gastro <AutoTuner[38]> char_to_fct.fixfactors.encode.removeconstants.colapply.imputeoor.int_to_num.smote.num_to_int.anova.ranger.tuned
# 3: <TaskClassif[46]>  Gastro <AutoTuner[38]> char_to_fct.fixfactors.encode.removeconstants.colapply.imputeoor.int_to_num.smote.num_to_int.anova.ranger.tuned
# 4: <TaskClassif[46]>  Gastro <AutoTuner[38]> char_to_fct.fixfactors.encode.removeconstants.colapply.imputeoor.int_to_num.smote.num_to_int.anova.ranger.tuned
# 5: <TaskClassif[46]>  Gastro <AutoTuner[38]> char_to_fct.fixfactors.encode.removeconstants.colapply.imputeoor.int_to_num.smote.num_to_int.anova.ranger.tuned
# 6: <TaskClassif[46]>  Gastro <AutoTuner[38]> char_to_fct.fixfactors.encode.removeconstants.colapply.imputeoor.int_to_num.smote.num_to_int.anova.ranger.tuned
# 7: <TaskClassif[46]>  Gastro <AutoTuner[38]> char_to_fct.fixfactors.encode.removeconstants.colapply.imputeoor.int_to_num.smote.num_to_int.anova.ranger.tuned
# 8: <TaskClassif[46]>  Gastro <AutoTuner[38]> char_to_fct.fixfactors.encode.removeconstants.colapply.imputeoor.int_to_num.smote.num_to_int.anova.ranger.tuned
# 9: <TaskClassif[46]>  Gastro <AutoTuner[38]> char_to_fct.fixfactors.encode.removeconstants.colapply.imputeoor.int_to_num.smote.num_to_int.anova.ranger.tuned
# 10: <TaskClassif[46]>  Gastro <AutoTuner[38]> char_to_fct.fixfactors.encode.removeconstants.colapply.imputeoor.int_to_num.smote.num_to_int.anova.ranger.tuned
# resampling resampling_id iteration              prediction classif.auc
# 1: <ResamplingCV[19]>            cv         1 <PredictionClassif[19]>   0.7854785 <--
# 2: <ResamplingCV[19]>            cv         2 <PredictionClassif[19]>   0.8085399 <--
# 3: <ResamplingCV[19]>            cv         3 <PredictionClassif[19]>   0.8145695 <--
# 4: <ResamplingCV[19]>            cv         4 <PredictionClassif[19]>   0.7417785
# 5: <ResamplingCV[19]>            cv         5 <PredictionClassif[19]>   0.6939494
# 6: <ResamplingCV[19]>            cv         6 <PredictionClassif[19]>   0.7311672
# 7: <ResamplingCV[19]>            cv         7 <PredictionClassif[19]>   0.7075903
# 8: <ResamplingCV[19]>            cv         8 <PredictionClassif[19]>   0.7783557
# 9: <ResamplingCV[19]>            cv         9 <PredictionClassif[19]>   0.6854305
# 10: <ResamplingCV[19]>            cv        10 <PredictionClassif[19]>   0.7636964
rr$aggregate(measures = msr("classif.auc"))
autoplot(rr, type = "roc")
rr$prediction()$confusion
rr$prediction()

#2. cv_glmnet
#Seems to work nice with autotuning and aov~=1
cv_glmnet = lrn("classif.cv_glmnet", predict_type = "prob", id="cv_glmnet")
cv_glmnet$param_set
gr = p0 %>>% gr_smote %>>% p2 %>>% cv_glmnet
plot(gr)

glrn = GraphLearner$new(gr)
params <- as.data.table(glrn$param_set)

ps = ParamSet$new(list(
  ParamDbl$new("anova.filter.frac", lower = 0.95, upper = 1)))

measure = msr("classif.auc")
resampling = rsmp("cv", folds = 5)
tuner = tnr("random_search")

at = AutoTuner$new(glrn, 
                   resampling, 
                   measure, 
                   terminator = trm("evals", n_evals = 1000), 
                   tuner, 
                   ps)

outer_resampling = rsmp("cv", folds = 10)

rr = mlr3::resample(task, at, outer_resampling, store_models = TRUE)

extract_inner_tuning_results(rr)
# anova.filter.frac learner_param_vals  x_domain classif.auc
# 1:            0.9500         <list[19]> <list[1]>   0.7418060
# 2:            1.0000         <list[19]> <list[1]>   0.7509446
# 3:            0.9875         <list[19]> <list[1]>   0.7630220
# 4:            1.0000         <list[19]> <list[1]>   0.7346677
# 5:            0.9500         <list[19]> <list[1]>   0.7462240
# 6:            0.9750         <list[19]> <list[1]>   0.7354581
# 7:            0.9875         <list[19]> <list[1]>   0.7264616
# 8:            1.0000         <list[19]> <list[1]>   0.7434846
# 9:            0.9625         <list[19]> <list[1]>   0.7224332
# 10:            1.0000         <list[19]> <list[1]>   0.7337413

rr$score(measures = msr("classif.auc"))
# task task_id         learner                                                                                                         learner_id
# 1: <TaskClassif[46]>  Gastro <AutoTuner[38]> char_to_fct.fixfactors.encode.removeconstants.colapply.imputeoor.int_to_num.smote.num_to_int.anova.cv_glmnet.tuned
# 2: <TaskClassif[46]>  Gastro <AutoTuner[38]> char_to_fct.fixfactors.encode.removeconstants.colapply.imputeoor.int_to_num.smote.num_to_int.anova.cv_glmnet.tuned
# 3: <TaskClassif[46]>  Gastro <AutoTuner[38]> char_to_fct.fixfactors.encode.removeconstants.colapply.imputeoor.int_to_num.smote.num_to_int.anova.cv_glmnet.tuned
# 4: <TaskClassif[46]>  Gastro <AutoTuner[38]> char_to_fct.fixfactors.encode.removeconstants.colapply.imputeoor.int_to_num.smote.num_to_int.anova.cv_glmnet.tuned
# 5: <TaskClassif[46]>  Gastro <AutoTuner[38]> char_to_fct.fixfactors.encode.removeconstants.colapply.imputeoor.int_to_num.smote.num_to_int.anova.cv_glmnet.tuned
# 6: <TaskClassif[46]>  Gastro <AutoTuner[38]> char_to_fct.fixfactors.encode.removeconstants.colapply.imputeoor.int_to_num.smote.num_to_int.anova.cv_glmnet.tuned
# 7: <TaskClassif[46]>  Gastro <AutoTuner[38]> char_to_fct.fixfactors.encode.removeconstants.colapply.imputeoor.int_to_num.smote.num_to_int.anova.cv_glmnet.tuned
# 8: <TaskClassif[46]>  Gastro <AutoTuner[38]> char_to_fct.fixfactors.encode.removeconstants.colapply.imputeoor.int_to_num.smote.num_to_int.anova.cv_glmnet.tuned
# 9: <TaskClassif[46]>  Gastro <AutoTuner[38]> char_to_fct.fixfactors.encode.removeconstants.colapply.imputeoor.int_to_num.smote.num_to_int.anova.cv_glmnet.tuned
# 10: <TaskClassif[46]>  Gastro <AutoTuner[38]> char_to_fct.fixfactors.encode.removeconstants.colapply.imputeoor.int_to_num.smote.num_to_int.anova.cv_glmnet.tuned
# resampling resampling_id iteration              prediction classif.auc
# 1: <ResamplingCV[19]>            cv         1 <PredictionClassif[19]>   0.6728548
# 2: <ResamplingCV[19]>            cv         2 <PredictionClassif[19]>   0.6769774
# 3: <ResamplingCV[19]>            cv         3 <PredictionClassif[19]>   0.6611643
# 4: <ResamplingCV[19]>            cv         4 <PredictionClassif[19]>   0.7195460
# 5: <ResamplingCV[19]>            cv         5 <PredictionClassif[19]>   0.8070762
# 6: <ResamplingCV[19]>            cv         6 <PredictionClassif[19]>   0.8815436 <-- BEST
# 7: <ResamplingCV[19]>            cv         7 <PredictionClassif[19]>   0.7687027
# 8: <ResamplingCV[19]>            cv         8 <PredictionClassif[19]>   0.7133333
# 9: <ResamplingCV[19]>            cv         9 <PredictionClassif[19]>   0.7673841
# 10: <ResamplingCV[19]>            cv        10 <PredictionClassif[19]>   0.7373408
as.data.table(rr)$learner[[6]]$tuning_result

#3. glmboost
glmboost = lrn("classif.glmboost", predict_type = "prob", id="glmboost")
gr = p0 %>>% gr_smote %>>% p2 %>>% glmboost
plot(gr)

glrn = GraphLearner$new(gr)
params <- as.data.table(glrn$param_set)

ps = ParamSet$new(list(
  ParamDbl$new("anova.filter.frac", lower = 0.95, upper = 1.0),
  ParamInt$new("glmboost.mstop",lower = 50, upper = 300)))

measure = msr("classif.auc")
resampling = rsmp("cv", folds = 5)
tuner = tnr("random_search")

at = AutoTuner$new(glrn, 
                   resampling, 
                   measure, 
                   terminator = trm("evals", n_evals = 1000), 
                   tuner, 
                   ps)

outer_resampling = rsmp("cv", folds = 10)

rr = mlr3::resample(task, at, outer_resampling, store_models = TRUE)
extract_inner_tuning_results(rr)
# anova.filter.frac glmboost.mstop learner_param_vals  x_domain classif.auc
# 1:         0.9871327            271         <list[20]> <list[2]>   0.7495342
# 2:         0.9677103            123         <list[20]> <list[2]>   0.7424157
# 3:         0.9690231            145         <list[20]> <list[2]>   0.7368123
# 4:         0.9573479            170         <list[20]> <list[2]>   0.7412797
# 5:         0.9988765             96         <list[20]> <list[2]>   0.7245906
# 6:         0.9986413            208         <list[20]> <list[2]>   0.7433215
# 7:         0.9641703            204         <list[20]> <list[2]>   0.7350279
# 8:         0.9927401            243         <list[20]> <list[2]>   0.7300510 <-- BEST
# 9:         0.9555628            191         <list[20]> <list[2]>   0.7374703
# 10:         0.9655165            250         <list[20]> <list[2]>   0.7428020
rr$score(measures = msr("classif.auc"))
# resampling resampling_id iteration              prediction classif.auc
# 1: <ResamplingCV[19]>            cv         1 <PredictionClassif[19]>   0.6544230
# 2: <ResamplingCV[19]>            cv         2 <PredictionClassif[19]>   0.7791667
# 3: <ResamplingCV[19]>            cv         3 <PredictionClassif[19]>   0.8434630
# 4: <ResamplingCV[19]>            cv         4 <PredictionClassif[19]>   0.7455467
# 5: <ResamplingCV[19]>            cv         5 <PredictionClassif[19]>   0.6812914
# 6: <ResamplingCV[19]>            cv         6 <PredictionClassif[19]>   0.7203159
# 7: <ResamplingCV[19]>            cv         7 <PredictionClassif[19]>   0.7686388
# 8: <ResamplingCV[19]>            cv         8 <PredictionClassif[19]>   0.8673379 <-- BEST
# 9: <ResamplingCV[19]>            cv         9 <PredictionClassif[19]>   0.7325637
# 10: <ResamplingCV[19]>            cv        10 <PredictionClassif[19]>   0.6942568

#7)Recreate best models for each and get metrics ####
#Anova filtering not worthy; remove it for final models
p2_f = po("filter", filter = flt("anova"))
p2_f$param_set$values = list("filter.frac" = 1)
gr_fil = p0 %>>% gr_smote %>>% p2_f
graph_data <- gr_fil$train(task)
#Recreate mlr3 dataset
data_ready <- graph_data[[1]]$data()
library(stringi)
colnames(data_ready) <- stri_trans_general(colnames(data_ready),"Latin-ASCII")
task_p = TaskClassif$new(id = 'Gastro_p_FINAL', data_ready, target = 'Exitus_90d')

# Use best conditions for each model
#Ranger
learner_ranger = mlr_learners$get("classif.ranger")
learner_ranger$predict_type = "prob"
learner_ranger$param_set$values <- list(mtry = 1, num.threads=12, num.trees=1571, sample.fraction=0.3242656, min.node.size=70)
resampling = rsmp("cv",folds = 10)
rr_ranger = mlr3::resample(task_p, learner_ranger, resampling, store_models = TRUE)
rr_ranger$aggregate(msr("classif.auc"))
# classif.auc 
# 0.8443033 

predictiontables <- lapply(rr_ranger$predictions(), data.table::as.data.table)
allpred <- data.table::rbindlist(predictiontables, idcol = "fold")
print(allpred)
cvAUC::ci.cvAUC(predictions = allpred$prob.Yes,
                labels = allpred$truth, folds = allpred$fold)
# $cvAUC
# [1] 0.8443033
# 
# $se
# [1] 0.001761582
# 
# $ci
# [1] 0.8408507 0.8477559
# 
# $confidence
# [1] 0.95


p <- autoplot(rr_ranger, type = "roc")
p + scale_color_manual(values = "darkblue")+
  theme_classic()+
  labs(title = "RF model")+
  theme(legend.position = "None",
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        axis.title.x = element_text(size=15),
        axis.title.y = element_text(size=15),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12))
ggsave("ROC_RF_model_TNM.pdf")

#cv_glmnet
learner_cv_glmnet = mlr_learners$get("classif.cv_glmnet")
learner_cv_glmnet$predict_type = "prob"
resampling = rsmp("cv", folds = 10)
rr_cv_glmnet = mlr3::resample(task_p, learner_cv_glmnet, resampling, store_models = TRUE)
rr_cv_glmnet$aggregate(msr("classif.auc"))
# classif.auc 
# 0.7963208 
autoplot(rr_cv_glmnet, type = "roc")
p <- autoplot(rr_cv_glmnet, type = "roc")
p + scale_color_manual(values = "darkblue")+
  theme_classic()+
  labs(title = "cv-Enet model")+
  theme(legend.position = "None",
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        axis.title.x = element_text(size=15),
        axis.title.y = element_text(size=15),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12))
ggsave("ROC_cv-Enet_model_TNM.pdf")

predictiontables <- lapply(rr_cv_glmnet$predictions(), data.table::as.data.table)
allpred <- data.table::rbindlist(predictiontables, idcol = "fold")
print(allpred)
cvAUC::ci.cvAUC(predictions = allpred$prob.Yes,
                labels = allpred$truth, folds = allpred$fold)
# $cvAUC
# [1] 0.7963208
# 
# $se
# [1] 0.006257665
# 
# $ci
# [1] 0.7840560 0.8085856
# 
# $confidence
# [1] 0.95

#glmboost
learner_glmboost = mlr_learners$get("classif.glmboost")
learner_glmboost$predict_type = "prob"
learner_glmboost$param_set$values <- list(mstop = 243)
resampling = rsmp("cv", folds = 10)
rr_glmboost = mlr3::resample(task_p, learner_glmboost, resampling, store_models = TRUE)
rr_glmboost$aggregate(msr("classif.auc"))
# classif.auc 
# 0.7971632 
autoplot(rr_glmboost, type = "roc")
p <- autoplot(rr_glmboost, type = "roc")
p + scale_color_manual(values = "darkblue")+
  theme_classic()+
  labs(title = "glmboost model")+
  theme(legend.position = "None",
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        axis.title.x = element_text(size=15),
        axis.title.y = element_text(size=15),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12))
ggsave("ROC_glmboost_model_TNM.pdf")

predictiontables <- lapply(rr_glmboost$predictions(), data.table::as.data.table)
allpred <- data.table::rbindlist(predictiontables, idcol = "fold")
print(allpred)
cvAUC::ci.cvAUC(predictions = allpred$prob.Yes,
                labels = allpred$truth, folds = allpred$fold)
# $cvAUC
# [1] 0.7971632
# 
# $se
# [1] 0.006239892
# 
# $ci
# [1] 0.7849332 0.8093931
# 
# $confidence
# [1] 0.95

#Ensemble
myvar <- list()
learners_all = as.data.table(list_mlr3learners()) #select = c("id", "mlr3_package", "required_packages"))
learners_to_try <- learners_all %>% filter(class == "classif") %>% filter(grepl("weights", properties)) %>% filter(grepl("integer", feature_types))
learners_to_try <- learners_to_try %>% dplyr::select(c(name, id))
learners_to_try <- as.data.frame(learners_to_try)

for (i in learners_to_try$name[c(5,11,20)]) { myvar[[i]]  <-  po("learner_cv", learner = lrn(learners_to_try[learners_to_try$name == i,][[2]],  id = i, predict_type = "prob" ))}
myvar$ranger$param_set$values <- list(mtry = 1, num.trees=1571, sample.fraction=0.3242656, min.node.size=70,
                                      resampling.method = "cv", resampling.folds = 3,resampling.keep_response =FALSE,
                                      num.threads = 1)
myvar$glmboost$param_set$values <- list(mstop = 243, resampling.method = "cv", resampling.folds = 3,resampling.keep_response =FALSE)

log_reg_lrn = lrn("classif.log_reg", predict_type = "prob")

graph = gunion(myvar) %>>%
  po("featureunion") %>>% log_reg_lrn

graph$plot() # Plot pipeline

pipe <- GraphLearner$new(graph) # Convert pipeline to learner
pipe$predict_type <- 'prob' # We want to predict probabilities and not classes.

resampling = rsmp("cv", folds = 10)
rr_ensembl = mlr3::resample(task_p, pipe, resampling, store_models = TRUE)
rr_ensembl$aggregate(msr("classif.auc"))
# classif.auc 
# 0.8474963 
predictiontables <- lapply(rr_ensembl$predictions(), data.table::as.data.table)
allpred <- data.table::rbindlist(predictiontables, idcol = "fold")
print(allpred)
cvAUC::ci.cvAUC(predictions = allpred$prob.Yes,
                labels = allpred$truth, folds = allpred$fold)
# $cvAUC
# [1] 0.8474963
# 
# $se
# [1] 0.005531383
# 
# $ci
# [1] 0.8366550 0.8583376
# 
# $confidence
# [1] 0.95


autoplot(rr_ensembl, type = "roc")
p <- autoplot(rr_ensembl, type = "roc")
p + scale_color_manual(values = "darkblue")+
  theme_classic()+
  labs(title = "Ensemble model")+
  theme(legend.position = "None",
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        axis.title.x = element_text(size=15),
        axis.title.y = element_text(size=15),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12))
ggsave("ROC_ensemble_model_TNM.pdf")

#8)Calibration####### 
#from: https://www.analyticsvidhya.com/blog/2016/07/platt-scaling-isotonic-regression-minimize-logloss-error/
#Enet
to_calibration_enet <- as.data.table(rr_cv_glmnet$prediction())
y <- as.data.frame(to_calibration_enet[,2])
y_prob <- as.data.frame(to_calibration_enet[,c(4,5)])
str(y_prob)
y_num <- as.numeric(factor(y$truth, levels = c("No", "Yes"), labels = c(0,1)))-1

idx <- duplicated(y_prob$prob.Yes)
y_prob_unique <- y_prob$prob.Yes[!idx]
y_unique<- y_num[!idx]

iso.model <- isoreg(y_prob_unique, y_unique)
y_prob_cal <- fit.isoreg(iso.model, y_num)
plot(iso.model,plot.type = "row")

result_cv_isotonic <- fit.isoreg(iso.model, y_prob_unique)

pdf("Unscaled_calibration_enet_TNM.pdf")
calibrate.plot(y_num, y_prob$prob.Yes, shade.col = "gray", line.par = list(col="darkblue"), rug.par = list(side=1), bty="n",
               main="Unscaled calibration", xlab = "", ylab = "")
mtext(side=1, line=2, "Predicted value", col="black", font=2,cex=1.2)
mtext(side=2, line=3, "Observed average", col="black", font=2,cex=1.2)
text(x=0.8, y=0.2, "      Intercept = -0.037\nSlope = 1.009",col="black", font=2, cex=0.8)
dev.off()
coef(lm(y_num ~ y_prob$prob.Yes))

pdf("Scaled_calibration_enet_TNM.pdf")
calibrate.plot(y_unique, result_cv_isotonic, shade.col = "gray", line.par = list(col="darkblue"), rug.par = list(side=1), bty="n",
               main="Scaled calibration", xlab = "", ylab = "")
mtext(side=1, line=2, "Predicted value", col="black", font=2,cex=1.2)
mtext(side=2, line=3, "Observed average", col="black", font=2,cex=1.2)
text(x=0.8, y=0.2, "      Intercept = 0.007\nSlope = 1.012",col="black", font=2, cex=0.8)
dev.off()
coef(lm(y_unique ~ result_cv_isotonic))

library(ResourceSelection)
hoslem.test(y_num,y_prob$prob.Yes,g=10)
# Hosmer and Lemeshow goodness of fit (GOF) test
# 
# data:  y_num, y_prob$prob.Yes
# X-squared = 51.072, df = 8, p-value = 2.542e-08

hoslem.test(y_unique,result_cv_isotonic,g=10)
# Hosmer and Lemeshow goodness of fit (GOF) test
# 
# data:  y_unique, result_cv_isotonic
# X-squared = 16.976, df = 8, p-value = 0.03036

library(DescTools)
BrierScore(y_num,y_prob$prob.Yes)
#[1] 0.1795065 

BrierScore(y_unique,result_cv_isotonic)
#[1] 0.1767407 

#glmboost
to_calibration_glmboost <- as.data.table(rr_glmboost$prediction())
y <- as.data.frame(to_calibration_glmboost[,2])
y_prob <- as.data.frame(to_calibration_glmboost[,c(4,5)])
str(y_prob)
y_num <- as.numeric(factor(y$truth, levels = c("No", "Yes"), labels = c(0,1)))-1

idx <- duplicated(y_prob$prob.Yes)
y_prob_unique <- y_prob$prob.Yes[!idx]
y_unique<- y_num[!idx]

iso.model <- isoreg(y_prob_unique, y_unique)
y_prob_cal <- fit.isoreg(iso.model, y_num)
plot(iso.model,plot.type = "row")

result_cv_isotonic <- fit.isoreg(iso.model, y_prob_unique)

pdf("Unscaled_calibration_glmboost_TNM.pdf")
calibrate.plot(y_num, y_prob$prob.Yes, shade.col = "gray", line.par = list(col="darkblue"), rug.par = list(side=1), bty="n",
               main="Unscaled calibration", xlab = "", ylab = "")
mtext(side=1, line=2, "Predicted value", col="black", font=2,cex=1.2)
mtext(side=2, line=3, "Observed average", col="black", font=2,cex=1.2)
text(x=0.8, y=0.2, "      Intercept = -0.061\nSlope = 1.125",col="black", font=2, cex=0.8)
dev.off()
coef(lm(y_num ~ y_prob$prob.Yes))

pdf("Scaled_calibration_glmboost_TNM.pdf")
calibrate.plot(y_unique, result_cv_isotonic, shade.col = "gray", line.par = list(col="darkblue"), rug.par = list(side=1), bty="n",
               main="Scaled calibration", xlab = "", ylab = "")
mtext(side=1, line=2, "Predicted value", col="black", font=2,cex=1.2)
mtext(side=2, line=3, "Observed average", col="black", font=2,cex=1.2)
text(x=0.8, y=0.2, "      Intercept = 0.005\nSlope = 1.018",col="black", font=2, cex=0.8)
dev.off()
coef(lm(y_unique ~ result_cv_isotonic))

library(ResourceSelection)
hoslem.test(y_num,y_prob$prob.Yes,g=10)
# Hosmer and Lemeshow goodness of fit (GOF) test
# 
# data:  y_num, y_prob$prob.Yes
# X-squared = 66.084, df = 8, p-value = 2.945e-11
hoslem.test(y_unique,result_cv_isotonic,g=10)
# Hosmer and Lemeshow goodness of fit (GOF) test
# 
# data:  y_unique, result_cv_isotonic
# X-squared = 21.816, df = 8, p-value = 0.005268

library(DescTools)
BrierScore(y_num,y_prob$prob.Yes)
#[1] 0.1801745
BrierScore(y_unique,result_cv_isotonic)
#[1] 0.175967

#RF
to_calibration <- as.data.table(rr_ranger$prediction())
y <- as.data.frame(to_calibration[,2])
y_prob <- as.data.frame(to_calibration[,c(4,5)])
str(y_prob)
y_num <- as.numeric(factor(y$truth, levels = c("No", "Yes"), labels = c(0,1)))-1

idx <- duplicated(y_prob$prob.Yes)
y_prob_unique <- y_prob$prob.Yes[!idx]
y_unique<- y_num[!idx]

iso.model <- isoreg(y_prob_unique, y_unique)
saveRDS(iso.model, "iso.model.Rds")
y_prob_cal <- fit.isoreg(iso.model, y_num)
plot(iso.model,plot.type = "row")

result_cv_isotonic <- fit.isoreg(iso.model, y_prob_unique)

pdf("Unscaled_calibration_RF_TNM.pdf")
calibrate.plot(y_num, y_prob$prob.Yes, shade.col = "gray", line.par = list(col="darkblue"), rug.par = list(side=1), bty="n",
               main="Unscaled calibration", xlab = "", ylab = "")
mtext(side=1, line=2, "Predicted value", col="black", font=2,cex=1.2)
mtext(side=2, line=3, "Observed average", col="black", font=2,cex=1.2)
text(x=0.8, y=0.2, "      Intercept = -0.866\nSlope = 3.183",col="black", font=2, cex=0.8)
dev.off()
coef(lm(y_num ~ y_prob$prob.Yes))

pdf("Scaled_calibration_RF_TNM.pdf")
calibrate.plot(y_unique, result_cv_isotonic, shade.col = "gray", line.par = list(col="darkblue"), rug.par = list(side=1), bty="n",
               main="Scaled calibration", xlab = "", ylab = "")
mtext(side=1, line=2, "Predicted value", col="black", font=2,cex=1.2)
mtext(side=2, line=3, "Observed average", col="black", font=2,cex=1.2)
text(x=0.8, y=0.2, "      Intercept = 0.013\nSlope = 1.001",col="black", font=2, cex=0.8)
dev.off()
coef(lm(y_unique ~ result_cv_isotonic))

library(ResourceSelection)
hoslem.test(y_num,y_prob$prob.Yes,g=10)
# Hosmer and Lemeshow goodness of fit (GOF) test
# 
# data:  y_num, y_prob$prob.Yes
# X-squared = 905.49, df = 8, p-value < 2.2e-16

hoslem.test(y_unique,result_cv_isotonic,g=10)
# Hosmer and Lemeshow goodness of fit (GOF) test
# 
# data:  y_unique, result_cv_isotonic
# X-squared = 22.842, df = 8, p-value = 0.003573

library(DescTools)
BrierScore(y_num,y_prob$prob.Yes)
#[1] 0.1961074
BrierScore(y_unique,result_cv_isotonic)
#[1] 0.1521286

#Esenmble
to_calibration_ens <- as.data.table(rr_ensembl$prediction())
y <- as.data.frame(to_calibration_ens[,2])
y_prob <- as.data.frame(to_calibration_ens[,c(4,5)])
str(y_prob)
y_num <- as.numeric(factor(y$truth, levels = c("No", "Yes"), labels = c(0,1)))-1

idx <- duplicated(y_prob$prob.Yes)
y_prob_unique <- y_prob$prob.Yes[!idx]
y_unique<- y_num[!idx]

iso.model <- isoreg(y_prob_unique, y_unique)
y_prob_cal <- fit.isoreg(iso.model, y_num)
plot(iso.model,plot.type = "row")

result_cv_isotonic <- fit.isoreg(iso.model, y_prob_unique)

pdf("Unscaled_calibration_Ensemble_TNM.pdf")
calibrate.plot(y_num, y_prob$prob.Yes, shade.col = "gray", line.par = list(col="darkblue"), rug.par = list(side=1), bty="n",
               main="Unscaled calibration", xlab = "", ylab = "")
mtext(side=1, line=2, "Predicted value", col="black", font=2,cex=1.2)
mtext(side=2, line=3, "Observed average", col="black", font=2,cex=1.2)
text(x=0.8, y=0.2, "      Intercept = -0.005\nSlope = 1.007",col="black", font=2, cex=0.8)
dev.off()
coef(lm(y_num ~ y_prob$prob.Yes))

pdf("Scaled_calibration_Ensemble_TNM.pdf")
calibrate.plot(y_unique, result_cv_isotonic, shade.col = "gray", line.par = list(col="darkblue"), rug.par = list(side=1), bty="n",
               main="Scaled calibration", xlab = "", ylab = "")
mtext(side=1, line=2, "Predicted value", col="black", font=2,cex=1.2)
mtext(side=2, line=3, "Observed average", col="black", font=2,cex=1.2)
text(x=0.8, y=0.2, "      Intercept = 0.013\nSlope = 1.013",col="black", font=2, cex=0.8)
dev.off()
coef(lm(y_unique ~ result_cv_isotonic))

library(ResourceSelection)
hoslem.test(y_num,y_prob$prob.Yes,g=10)
# Hosmer and Lemeshow goodness of fit (GOF) test 
# 
# data:  y_num, y_prob$prob.Yes
# X-squared = 29.016, df = 8, p-value = 0.000315
hoslem.test(y_unique,result_cv_isotonic,g=10)
# Hosmer and Lemeshow goodness of fit (GOF) test 
# 
# data:  y_unique, result_cv_isotonic
# X-squared = 23.355, df = 8, p-value = 0.002937
library(DescTools)
BrierScore(y_num,y_prob$prob.Yes)
#[1] 0.1552412 

BrierScore(y_unique,result_cv_isotonic)
#[1] 0.1536264 


#9)Model interpretation####
#GLMboost
glmboost_task = as_task_classif(data_ready, target = 'Exitus_90d')
y= data_ready$Exitus_90d
y= factor(y, levels = c("Yes", "No"), labels = c(1,0))
y=as.numeric(as.character(y))
data_ready_2 <- data_ready[,-1]
learner_p = mlr_learners$get("classif.glmboost")
learner_p$predict_type = "prob"
learner_p$train(glmboost_task)
glmboost_exp <- explain_mlr3(learner_p,
                           data     = data_ready_2,
                           y        = y,
                           label    = "GLMboost",
                           colorize = FALSE)
glmboost_exp$model_info$type #DEFAULTS TO AUC

explain_glmboost <- model_parts(glmboost_exp, B=10000)

from_0_to_100 <- explain_glmboost %>% group_by(variable) %>% summarise(mean = mean(dropout_loss), sd = sd(dropout_loss))
baseline = from_0_to_100$mean[from_0_to_100$variable == "_baseline_"]
full = from_0_to_100$mean[from_0_to_100$variable == "_full_model_"]

from_0_to_100$Percentage <- (from_0_to_100$mean - baseline)/(full - baseline)


ggplot(from_0_to_100, aes(x=Percentage, y= reorder(variable, -Percentage)))+
  geom_boxplot() 

#Recode variables to generate final figure
explain_2_glmboost <- explain_glmboost
explain_2_glmboost$variable <- gsub("\\.", "_", explain_2_glmboost$variable)
explain_2_glmboost$variable <- gsub("ECOG_perfomance_status____3", "ECOG_>3", explain_2_glmboost$variable)
explain_2_glmboost$variable <- gsub("ECOG_perfomance_status_0", "ECOG_0", explain_2_glmboost$variable)
explain_2_glmboost$variable <- gsub("Weigth_loss___10_", "Weigth_loss_>10%", explain_2_glmboost$variable)
explain_2_glmboost$variable <- gsub("Weigth_loss_6___10_", "Weigth_loss_6-10%", explain_2_glmboost$variable)
explain_2_glmboost$variable <- gsub("Weigth_loss_0___5_", "Weigth_loss_0-5%", explain_2_glmboost$variable)
explain_2_glmboost$variable <- gsub("ECOG_perfomance_status_1", "ECOG_1", explain_2_glmboost$variable)
explain_2_glmboost$variable <- gsub("ECOG_perfomance_status_2", "ECOG_2", explain_2_glmboost$variable)
explain_2_glmboost$variable <- gsub("Preoperative_Hemoglobin", "Preoperative_hemoglobin", explain_2_glmboost$variable)
explain_2_glmboost$variable <- gsub("Preoperative_Albumin", "Preoperative_albumin", explain_2_glmboost$variable)
explain_2_glmboost$variable <- gsub("Neoadjuvant_therapy_Chemotherapy", "Neoadjuvant_chemotherapy", explain_2_glmboost$variable)
explain_2_glmboost$variable <- gsub("Neoadjuvant_therapy_No", "None_neoadjuvant_therapy", explain_2_glmboost$variable)
explain_2_glmboost$variable <- gsub("Neoadjuvant_therapy_Chemo_Radiotherapy", "Neoadjuvant_chemoradiotherapy", explain_2_glmboost$variable)
explain_2_glmboost$variable <- gsub("Approach_Open", "Open_approach", explain_2_glmboost$variable)
explain_2_glmboost$variable <- gsub("Approach_Minimal_invasive_approach", "Minimally_invasive_approach", explain_2_glmboost$variable)
explain_2_glmboost$variable <- gsub("Gender_Hombre", "Male_gender", explain_2_glmboost$variable)
explain_2_glmboost$variable <- gsub("Gender_Mujer", "Female_gender", explain_2_glmboost$variable)
explain_2_glmboost$variable <- gsub("Surgery_Urgent", "Timing_of_surgery_urgent", explain_2_glmboost$variable)
explain_2_glmboost$variable <- gsub("Surgery_Elective", "Timing_of_surgery_elective", explain_2_glmboost$variable)
explain_2_glmboost$variable <- gsub("Gastrectomy_Subtotal", "Subtotal_gastrectomy", explain_2_glmboost$variable)
explain_2_glmboost$variable <- gsub("Gastrectomy_Total", "Total_gastrectomy", explain_2_glmboost$variable)
explain_2_glmboost$variable <- gsub("Gastrectomy_Subtotal", "Subtotal_gastrectomy", explain_2_glmboost$variable)
explain_2_glmboost$variable <- gsub("Approach_Open", "Open_approach", explain_2_glmboost$variable)

library(ggsci)
p <- plot(explain_2_glmboost, show_boxplots=FALSE) + 
  scale_color_manual(values = "darkblue")+
  theme_gray()+
  ylim(0.70,0.80)+
  labs(title = "Feature Importance for the GLMboost model", subtitle = element_blank())+
  theme(legend.position = "None",
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        axis.text.x = element_text(size=10),
        axis.title.x = element_text(size=15),
        axis.text.y = element_text(size=8))
p
g_glmboost <- ggplotGrob(p)
g_glmboost$grobs[[2]]$children$geom_linerange.segments.3832$gp$lwd <- rep(10, 
                                                                          length(g_glmboost$grobs[[2]]$children$geom_linerange.segments.3832$gp$lwd))
pdf("GLMboost_importance_TNM.pdf")
plot(g_glmboost)
dev.off()

#Enet
enet_task = as_task_classif(data_ready, target = 'Exitus_90d')
y= data_ready$Exitus_90d
y= factor(y, levels = c("Yes", "No"), labels = c(1,0))
y=as.numeric(as.character(y))
data_ready_2 <- data_ready[,-1]
learner_p = mlr_learners$get("classif.cv_glmnet")
learner_p$predict_type = "prob"
learner_p$train(enet_task)
enet_exp <- explain_mlr3(learner_p,
                         data     = data_ready_2,
                         y        = y,
                         label    = "Enet",
                         colorize = FALSE)
enet_exp$model_info$type #DEFAULTS TO AUC

explain_enet <- model_parts(enet_exp, B=10000)
from_0_to_100 <- explain_enet %>% group_by(variable) %>% summarise(mean = mean(dropout_loss), sd = sd(dropout_loss))
baseline = from_0_to_100$mean[from_0_to_100$variable == "_baseline_"]
full = from_0_to_100$mean[from_0_to_100$variable == "_full_model_"]

from_0_to_100$Percentage <- (from_0_to_100$mean - baseline)/(full - baseline)


ggplot(from_0_to_100, aes(x=Percentage, y= reorder(variable, -Percentage)))+
  geom_boxplot() 

#Recode variables to generate final figure
explain_2_enet <- explain_enet
explain_2_enet$variable <- gsub("\\.", "_", explain_2_enet$variable)
explain_2_enet$variable <- gsub("ECOG_perfomance_status____3", "ECOG_>3", explain_2_enet$variable)
explain_2_enet$variable <- gsub("ECOG_perfomance_status_0", "ECOG_0", explain_2_enet$variable)
explain_2_enet$variable <- gsub("Weigth_loss___10_", "Weigth_loss_>10%", explain_2_enet$variable)
explain_2_enet$variable <- gsub("Weigth_loss_6___10_", "Weigth_loss_6-10%", explain_2_enet$variable)
explain_2_enet$variable <- gsub("Weigth_loss_0___5_", "Weigth_loss_0-5%", explain_2_enet$variable)
explain_2_enet$variable <- gsub("ECOG_perfomance_status_1", "ECOG_1", explain_2_enet$variable)
explain_2_enet$variable <- gsub("ECOG_perfomance_status_2", "ECOG_2", explain_2_enet$variable)
explain_2_enet$variable <- gsub("Preoperative_Hemoglobin", "Preoperative_hemoglobin", explain_2_enet$variable)
explain_2_enet$variable <- gsub("Preoperative_Albumin", "Preoperative_albumin", explain_2_enet$variable)
explain_2_enet$variable <- gsub("Neoadjuvant_therapy_Chemotherapy", "Neoadjuvant_chemotherapy", explain_2_enet$variable)
explain_2_enet$variable <- gsub("Neoadjuvant_therapy_No", "None_neoadjuvant_therapy", explain_2_enet$variable)
explain_2_enet$variable <- gsub("Neoadjuvant_therapy_Chemo_Radiotherapy", "Neoadjuvant_chemoradiotherapy", explain_2_enet$variable)
explain_2_enet$variable <- gsub("Approach_Open", "Open_approach", explain_2_enet$variable)
explain_2_enet$variable <- gsub("Approach_Minimal_invasive_approach", "Minimally_invasive_approach", explain_2_enet$variable)
explain_2_enet$variable <- gsub("Gender_Hombre", "Male_gender", explain_2_enet$variable)
explain_2_enet$variable <- gsub("Gender_Mujer", "Female_gender", explain_2_enet$variable)
explain_2_enet$variable <- gsub("Surgery_Urgent", "Timing_of_surgery_urgent", explain_2_enet$variable)
explain_2_enet$variable <- gsub("Surgery_Elective", "Timing_of_surgery_elective", explain_2_enet$variable)
explain_2_enet$variable <- gsub("Gastrectomy_Subtotal", "Subtotal_gastrectomy", explain_2_enet$variable)
explain_2_enet$variable <- gsub("Gastrectomy_Total", "Total_gastrectomy", explain_2_enet$variable)
explain_2_enet$variable <- gsub("Gastrectomy_Subtotal", "Subtotal_gastrectomy", explain_2_enet$variable)
explain_2_enet$variable <- gsub("Approach_Open", "Open_approach", explain_2_enet$variable)

library(ggsci)
p <- plot(explain_2_enet, show_boxplots=FALSE) + 
  scale_color_manual(values = "darkblue")+
  theme_gray()+
  ylim(0.73,0.80)+
  labs(title = "Feature Importance for the Enet model", subtitle = element_blank())+
  theme(legend.position = "None",
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        axis.text.x = element_text(size=10),
        axis.title.x = element_text(size=15),
        axis.text.y = element_text(size=8))

g_enet <- ggplotGrob(p)
g_enet$grobs[[2]]$children$geom_linerange.segments.4441$gp$lwd <- rep(10, length(g_enet$grobs[[2]]$children$geom_linerange.segments.4441$gp$lwd))
pdf("Enet_importance_TNM.pdf")
plot(g_enet)
dev.off()

#RF
ranger_task = as_task_classif(data_ready, target = 'Exitus_90d')
y= data_ready$Exitus_90d
y= factor(y, levels = c("Yes", "No"), labels = c(1,0))
y=as.numeric(as.character(y))
data_ready_2 <- data_ready[,-1]
learner_p = mlr_learners$get("classif.ranger")
learner_p$predict_type = "prob"
learner_p$param_set$values <- list(mtry = 1, num.threads=12, num.trees=1571, sample.fraction=0.3242656, min.node.size=70)
learner_p$train(ranger_task)
ranger_exp <- explain_mlr3(learner_p,
                           data     = data_ready_2,
                           y        = y,
                           label    = "Ranger RF",
                           colorize = FALSE)
ranger_exp$model_info$type #DEFAULTS TO AUC

explain <- model_parts(ranger_exp, B=10000)

from_0_to_100 <- explain %>% group_by(variable) %>% summarise(mean = mean(dropout_loss), sd = sd(dropout_loss))
baseline = from_0_to_100$mean[from_0_to_100$variable == "_baseline_"]
full = from_0_to_100$mean[from_0_to_100$variable == "_full_model_"]

from_0_to_100$Percentage <- (from_0_to_100$mean - baseline)/(full - baseline)


ggplot(from_0_to_100, aes(x=Percentage, y= reorder(variable, -Percentage)))+
  geom_boxplot() 

#Recode variables to generate final figure
explain_2 <- explain
explain_2$variable <- gsub("\\.", "_", explain$variable)
explain_2$variable <- gsub("ECOG_perfomance_status____3", "ECOG_>3", explain_2$variable)
explain_2$variable <- gsub("ECOG_perfomance_status_0", "ECOG_0", explain_2$variable)
explain_2$variable <- gsub("Weigth_loss___10_", "Weigth_loss_>10%", explain_2$variable)
explain_2$variable <- gsub("Weigth_loss_6___10_", "Weigth_loss_6-10%", explain_2$variable)
explain_2$variable <- gsub("Weigth_loss_0___5_", "Weigth_loss_0-5%", explain_2$variable)
explain_2$variable <- gsub("ECOG_perfomance_status_1", "ECOG_1", explain_2$variable)
explain_2$variable <- gsub("ECOG_perfomance_status_2", "ECOG_2", explain_2$variable)
explain_2$variable <- gsub("Preoperative_Hemoglobin", "Preoperative_hemoglobin", explain_2$variable)
explain_2$variable <- gsub("Preoperative_Albumin", "Preoperative_albumin", explain_2$variable)
explain_2$variable <- gsub("Neoadjuvant_therapy_Chemotherapy", "Neoadjuvant_chemotherapy", explain_2$variable)
explain_2$variable <- gsub("Neoadjuvant_therapy_No", "None_neoadjuvant_therapy", explain_2$variable)
explain_2$variable <- gsub("Neoadjuvant_therapy_Chemo_Radiotherapy", "Neoadjuvant_chemoradiotherapy", explain_2$variable)
explain_2$variable <- gsub("Approach_Open", "Open_approach", explain_2$variable)
explain_2$variable <- gsub("Approach_Minimal_invasive_approach", "Minimally_invasive_approach", explain_2$variable)
explain_2$variable <- gsub("Gender_Hombre", "Male_gender", explain_2$variable)
explain_2$variable <- gsub("Gender_Mujer", "Female_gender", explain_2$variable)
explain_2$variable <- gsub("Surgery_Urgent", "Timing_of_surgery_urgent", explain_2$variable)
explain_2$variable <- gsub("Surgery_Elective", "Timing_of_surgery_elective", explain_2$variable)
explain_2$variable <- gsub("Gastrectomy_Subtotal", "Subtotal_gastrectomy", explain_2$variable)
explain_2$variable <- gsub("Gastrectomy_Total", "Total_gastrectomy", explain_2$variable)
explain_2$variable <- gsub("Gastrectomy_Subtotal", "Subtotal_gastrectomy", explain_2$variable)
explain_2$variable <- gsub("Approach_Open", "Open_approach", explain_2$variable)

library(ggsci)
p <- plot(explain_2, show_boxplots=FALSE) + 
  scale_color_manual(values = "darkblue")+
  theme_gray()+
  ylim(0.835,0.870)+
  labs(title = "Feature Importance for the RF model", subtitle = element_blank())+
  theme(legend.position = "None",
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        axis.text.x = element_text(size=10),
        axis.title.x = element_text(size=15),
        axis.text.y = element_text(size=8))
p
g <- ggplotGrob(p)
g$grobs[[2]]$children$geom_linerange.segments.5050$gp$lwd <- rep(10, length(g$grobs[[2]]$children$geom_linerange.segments.5050$gp$lwd))
pdf("Random_forest_importance_TNM.pdf")
plot(g)
dev.off()

#Ensemble
ensemble_task = as_task_classif(data_ready, target = 'Exitus_90d')
y= data_ready$Exitus_90d
y= factor(y, levels = c("Yes", "No"), labels = c(1,0))
y=as.numeric(as.character(y))
data_ready_2 <- data_ready[,-1]

myvar <- list()
learners_all = as.data.table(list_mlr3learners()) #select = c("id", "mlr3_package", "required_packages"))
learners_to_try <- learners_all %>% filter(class == "classif") %>% filter(grepl("weights", properties)) %>% filter(grepl("integer", feature_types))
learners_to_try <- learners_to_try %>% dplyr::select(c(name, id))
learners_to_try <- as.data.frame(learners_to_try)

for (i in learners_to_try$name[c(5,11,20)]) { myvar[[i]]  <-  po("learner_cv", learner = lrn(learners_to_try[learners_to_try$name == i,][[2]],  id = i, predict_type = "prob" ))}
myvar$ranger$param_set$values <- list(mtry = 1, num.trees=1571, sample.fraction=0.3242656, min.node.size=70,
                                      resampling.method = "cv", resampling.folds = 3,resampling.keep_response =FALSE,
                                      num.threads = 1)
myvar$glmboost$param_set$values <- list(mstop = 243, resampling.method = "cv", resampling.folds = 3,resampling.keep_response =FALSE)

log_reg_lrn = lrn("classif.log_reg", predict_type = "prob")

graph = gunion(myvar) %>>%
  po("featureunion") %>>% log_reg_lrn

graph$plot() # Plot pipeline

pipe <- GraphLearner$new(graph) # Convert pipeline to learner
pipe$predict_type <- 'prob' # We want to predict probabilities and not classes.

pipe$train(ensemble_task)
ensemble_exp <- explain_mlr3(pipe,
                           data     = ensemble_task$data()[,-1],
                           y        = as.integer(ensemble_task$data()[, 1] == 'Yes'),
                           # predict_function = predict_function_custom,
                           # residual_function = residual_function_custom,
                           label    = "Ensemble")

ensemble_exp$model_info$type #<- "classification" #DEFAULTS TO AUC

explain_ens <- model_parts(ensemble_exp, B=10000)

from_0_to_100 <- explain_ens %>% group_by(variable) %>% summarise(mean = mean(dropout_loss), sd = sd(dropout_loss))
baseline = from_0_to_100$mean[from_0_to_100$variable == "_baseline_"]
full = from_0_to_100$mean[from_0_to_100$variable == "_full_model_"]

from_0_to_100$Percentage <- (from_0_to_100$mean - baseline)/(full - baseline)


ggplot(from_0_to_100, aes(x=Percentage, y= reorder(variable, -Percentage)))+
  geom_boxplot() 

#Recode variables to generate final figure
explain_2_ens <- explain_ens
explain_2_ens$variable <- gsub("\\.", "_", explain_2_ens$variable)
explain_2_ens$variable <- gsub("ECOG_perfomance_status____3", "ECOG_>3", explain_2_ens$variable)
explain_2_ens$variable <- gsub("ECOG_perfomance_status_0", "ECOG_0", explain_2_ens$variable)
explain_2_ens$variable <- gsub("Weigth_loss___10_", "Weigth_loss_>10%", explain_2_ens$variable)
explain_2_ens$variable <- gsub("Weigth_loss_6___10_", "Weigth_loss_6-10%", explain_2_ens$variable)
explain_2_ens$variable <- gsub("Weigth_loss_0___5_", "Weigth_loss_0-5%", explain_2_ens$variable)
explain_2_ens$variable <- gsub("ECOG_perfomance_status_1", "ECOG_1", explain_2_ens$variable)
explain_2_ens$variable <- gsub("ECOG_perfomance_status_2", "ECOG_2", explain_2_ens$variable)
explain_2_ens$variable <- gsub("Preoperative_Hemoglobin", "Preoperative_hemoglobin", explain_2_ens$variable)
explain_2_ens$variable <- gsub("Preoperative_Albumin", "Preoperative_albumin", explain_2_ens$variable)
explain_2_ens$variable <- gsub("Neoadjuvant_therapy_Chemotherapy", "Neoadjuvant_chemotherapy", explain_2_ens$variable)
explain_2_ens$variable <- gsub("Neoadjuvant_therapy_No", "None_neoadjuvant_therapy", explain_2_ens$variable)
explain_2_ens$variable <- gsub("Neoadjuvant_therapy_Chemo_Radiotherapy", "Neoadjuvant_chemoradiotherapy", explain_2_ens$variable)
explain_2_ens$variable <- gsub("Approach_Open", "Open_approach", explain_2_ens$variable)
explain_2_ens$variable <- gsub("Approach_Minimal_invasive_approach", "Minimally_invasive_approach", explain_2_ens$variable)
explain_2_ens$variable <- gsub("Gender_Hombre", "Male_gender", explain_2_ens$variable)
explain_2_ens$variable <- gsub("Gender_Mujer", "Female_gender", explain_2_ens$variable)
explain_2_ens$variable <- gsub("Surgery_Urgent", "Timing_of_surgery_urgent", explain_2_ens$variable)
explain_2_ens$variable <- gsub("Surgery_Elective", "Timing_of_surgery_elective", explain_2_ens$variable)
explain_2_ens$variable <- gsub("Gastrectomy_Subtotal", "Subtotal_gastrectomy", explain_2_ens$variable)
explain_2_ens$variable <- gsub("Gastrectomy_Total", "Total_gastrectomy", explain_2_ens$variable)
explain_2_ens$variable <- gsub("Gastrectomy_Subtotal", "Subtotal_gastrectomy", explain_2_ens$variable)
explain_2_ens$variable <- gsub("Approach_Open", "Open_approach", explain_2_ens$variable)

library(ggsci)
p <- plot(explain_2_ens, show_boxplots=FALSE) + 
  scale_color_manual(values = "darkblue")+
  theme_gray()+
  ylim(0.845,0.875)+
  labs(title = "Feature Importance for the ensemble model", subtitle = element_blank())+
  theme(legend.position = "None",
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        axis.text.x = element_text(size=10),
        axis.title.x = element_text(size=15),
        axis.text.y = element_text(size=8))

p
g <- ggplotGrob(p)
g$grobs[[2]]$children$geom_linerange.segments.1029$gp$lwd <- rep(10, length(g$grobs[[2]]$children$geom_linerange.segments.1029$gp$lwd))
pdf("Ensemble_importance_TNM.pdf")
plot(g)
dev.off()

#10) Internal-External Validation on Jan to Sept 2021 data####
#Prepare validation test 
SEGRIC_2021_ENE_SEP <- read_excel("Internal_external_validation.xlsx")
SEGRIC_2021_ENE_SEP <- rename_with(SEGRIC_2021_ENE_SEP, ~ gsub(" ", "_", .x))

SEGRIC_2021_ENE_SEP$Fecha_cirugía <- as.Date(SEGRIC_2021_ENE_SEP$Fecha_cirugía)
SEGRIC_2021_ENE_SEP$Fecha_de_exitus <- as.Date(SEGRIC_2021_ENE_SEP$Fecha_de_exitus)

SEGRIC_2021_ENE_SEP$Exitus_90d <- (SEGRIC_2021_ENE_SEP$Fecha_de_exitus - SEGRIC_2021_ENE_SEP$Fecha_cirugía) < 90 

SEGRIC_2021_ENE_SEP$Exitus_90d[is.na(SEGRIC_2021_ENE_SEP$Fecha_de_exitus)] <- FALSE
SEGRIC_2021_ENE_SEP$Exitus_90d<- factor(SEGRIC_2021_ENE_SEP$Exitus_90d,
                                  levels = c(TRUE, FALSE),
                                  labels = c("Yes",
                                             "No"))
table(SEGRIC_2021_ENE_SEP$Exitus_90d)
SEGRIC_2021_ENE_SEP$Comorbilidad_de_Charlson <- gsub("\\(", "", SEGRIC_2021_ENE_SEP$Comorbilidad_de_Charlson)
SEGRIC_2021_ENE_SEP$Comorbilidad_de_Charlson <- gsub("\\)", "", SEGRIC_2021_ENE_SEP$Comorbilidad_de_Charlson)
SEGRIC_2021_ENE_SEP$Comorbilidad_de_Charlson <- gsub(" ", "_", SEGRIC_2021_ENE_SEP$Comorbilidad_de_Charlson)
SEGRIC_2021_ENE_SEP$Comorbilidad_de_Charlson <- gsub("-", "_", SEGRIC_2021_ENE_SEP$Comorbilidad_de_Charlson)
SEGRIC_2021_ENE_SEP$Comorbilidad_de_Charlson <- gsub(",_", ", ", SEGRIC_2021_ENE_SEP$Comorbilidad_de_Charlson)

SEGRIC_2021_ENE_SEP <- cbind(SEGRIC_2021_ENE_SEP, mtabulate(strsplit(SEGRIC_2021_ENE_SEP$Comorbilidad_de_Charlson, ", ")))

SEGRIC_2021_ENE_SEP$Year <- format(SEGRIC_2021_ENE_SEP$Fecha_cirugía,"%Y")
volume_act <- SEGRIC_2021_ENE_SEP %>% group_by(Year, Centro) %>% summarise(count = n())
colnames(volume_act)[3] <- "Volume_activity"

SEGRIC_2021_ENE_SEP <- left_join(SEGRIC_2021_ENE_SEP, volume_act)

SEGRIC_2021_ENE_SEP <- rename(SEGRIC_2021_ENE_SEP, Albumina = `Albúmina_(gr/L)_*(la_más_cercana_a_la_IQ)`)
SEGRIC_2021_ENE_SEP$Intencion_acceso <- factor(SEGRIC_2021_ENE_SEP$Acceso_estómago, 
                                         levels = c("Abierto", "Laparoscópico", "Laparoscópico con conversión", "Robótico"),
                                         labels =  c("Open", "Minimal invasive approach", "Minimal invasive approach", 
                                                     "Minimal invasive approach"))
SEGRIC_2021_ENE_SEP$Tipo_gastrectomia_TotalParcial <- factor(SEGRIC_2021_ENE_SEP$Tipo_gastrectomía, 
                                                       levels = c("Distal subtotal", "Total", 
                                                                  "Total ampliada (con resección de esófago distal)",
                                                                  "Total profiláctica (en portadores de mutación del Gen CDH1 u otros)",
                                                                  "Proximal"),
                                                       labels = c("Subtotal", "Total", "Total", "Total", "Subtotal"))

SEGRIC_2021_ENE_SEP$TIPO_CIR <- factor(SEGRIC_2021_ENE_SEP$Tipo_de_cirugía,
                                 levels = c("Electiva", "Urgente", "De rescate (tumor que se hace resecable tras tratamiento complementario de inducción)"),
                                 labels = c("Elective", "Urgent", "Urgent"))
SEGRIC_2021_ENE_SEP$QT_PERIOP <- factor(SEGRIC_2021_ENE_SEP$Tratamiento_neodyuvante, levels = levels(as.factor(SEGRIC_2021_ENE_SEP$Tratamiento_neodyuvante)),
                                  labels = c("No", "Chemotherapy"))
SEGRIC_2021_ENE_SEP$Indice_ASA <- factor(SEGRIC_2021_ENE_SEP$Índice_ASA, levels = levels(as.factor(SEGRIC_2021_ENE_SEP$Índice_ASA)),
                                   labels = c("score I", "score II", "score III", "score IV"))
SEGRIC_2021_ENE_SEP$ECOG <- factor(SEGRIC_2021_ENE_SEP$Escala_funcional_Zubrod,
                             levels = levels(as.factor(SEGRIC_2021_ENE_SEP$Escala_funcional_Zubrod)),
                             labels = c("perfomance status 0","perfomance status 1","perfomance status 2",
                                        "perfomance status >= 3"))
SEGRIC_2021_ENE_SEP$Porcentaje_de_peso_perdido <- factor(SEGRIC_2021_ENE_SEP$Porcentaje_de_peso_perdido, 
                                                   levels = levels(as.factor(SEGRIC_2021_ENE_SEP$Porcentaje_de_peso_perdido)),
                                                   labels = c("0 - 5%", "6 - 10%", "> 10%"))
SEGRIC_2021_ENE_SEP$Estadio_clínico_cT <- as.factor(SEGRIC_2021_ENE_SEP$Estadio_clínico_cT)
SEGRIC_2021_ENE_SEP$Estadio_clínico_cN <- factor(SEGRIC_2021_ENE_SEP$Estadio_clínico_cN, levels = c("-", "+"), labels = c(0,1))
#"SIDA","Enfermedad_del_tejido_conectivo",
variables <- c("Genero", "Edad", "IC", "ECOG", "Porcentaje_de_peso_perdido", "Hb_preoperatoria",
               "Infarto_de_miocardio", "Insuficiencia_cardiaca_congestiva",
               "Enfermedad_pulmonar_crónica", "Enfermedad_vascular_periférica",
               "Enfermedad_cerebro_vascular", "Demencia", "Enfermedad_ulcerosa", "Diabetes_Mellitus_sin_complicaciones",
               "Diabetes_Mellitus_con_lesión_orgánica", "Leucemia", "Linfoma_maligno", "Enfermedad_hepática_crónica_moderada_o_severa",
               "Hemiplejia", "Tumor_maligno_neoplasias", "Tumor_sólido_con_metástasis", "Enfermedad_renal_moderada_o_grave",
                "Albumina" ,"TIPO_CIR", "QT_PERIOP", "Estadio_clínico_cT", "Estadio_clínico_cN",#QT PERIOP
               "Intencion_acceso", "Tipo_gastrectomia_TotalParcial","Volume_activity","Indice_ASA",  "Exitus_90d")
SEGRIC_2021_ENE_SEP_mlr3 <- SEGRIC_2021_ENE_SEP %>% dplyr::select(variables)

SEGRIC_2021_ENE_SEP_mlr3 %>% group_by(Exitus_90d) %>% tally
colnames(SEGRIC_2021_ENE_SEP_mlr3) <- c("Gender", "Age", "BMI_index", "ECOG", "Weigth_loss", "Preoperative_Hemoglobin",
                                  "Myocardial_infarction", "Congestive_heart_failure",
                                  "Chronic_pulmonary_disease", "Peripheral_vascular_disease",
                                  "Cerebrovascular_disease", "Dementia", "Peptic_ulcer_disease", "Diabetes_mellitus_uncomplicated",
                                  "Diabetes_mellitus_end_organ_damage", "Leukemia", "Malignant_lymphoma", "Liver_disease_moderate_to_severe",
                                  "Hemiplegia", "Malignant_tumor_present", "Metastatic_tumor_present", "Moderate_to_severe_renal_disease",
                                  "Preoperative_Albumin" ,"Surgery", "Neoadjuvant_therapy", #QT PERIOP
                                  "Clinical_stage_cT", "Clinical_stage_cN",
                                  "Approach", "Gastrectomy","Volume_activity","ASA", "Exitus_90d")
SEGRIC_2021_ENE_SEP_mlr3$AIDS <- 0L
SEGRIC_2021_ENE_SEP_mlr3$Connective_tissue_disease <- 0L
task = TaskClassif$new(id = 'Gastro', Gastros_FINAL_mlr3, target = 'Exitus_90d')

ord_to_int = po("colapply", applicator = as.integer,
                affect_columns = selector_type("ordered"))
lrn = lrn("classif.xgboost", nrounds = 100, predict_type = "prob")
p0 = ppl("robustify", task, lrn) %>>%  ord_to_int %>>% po("imputeoor")
gr_smote = #Best oversampling method
  po("colapply", id = "int_to_num",
     applicator = as.numeric, affect_columns = selector_type("integer")) %>>%
  po("smote", dup_size = 10) %>>%
  po("colapply", id = "num_to_int",
     applicator = function(x) as.integer(round(x, 0L)), affect_columns = selector_type("numeric"))
p2_f = po("filter", filter = flt("anova"))
p2_f$param_set$values = list("filter.frac" = 1)
#Ranger
learner_ranger = mlr_learners$get("classif.ranger")
learner_ranger$predict_type = "prob"
learner_ranger$param_set$values <- list(mtry = 1, num.threads=12, num.trees=1571, sample.fraction=0.3242656, min.node.size=70)
gr_ranger = p0 %>>% gr_smote %>>% p2_f %>>% learner_ranger
gl_ranger = GraphLearner$new(gr_ranger)
gl_ranger$train(task)

#cv-Enet
learner_cv_glmnet = mlr_learners$get("classif.cv_glmnet")
learner_cv_glmnet$predict_type = "prob"
gr_cv_enet = p0 %>>% gr_smote %>>% p2_f %>>% learner_cv_glmnet
gl_cv_enet = GraphLearner$new(gr_cv_enet)
gl_cv_enet$train(task)

#glmboost
learner_glmboost = mlr_learners$get("classif.glmboost")
learner_glmboost$predict_type = "prob"
learner_glmboost$param_set$values <- list(mstop = 243)
gr_glmboost = p0 %>>% gr_smote %>>% p2_f %>>% learner_glmboost
gl_glmboost = GraphLearner$new(gr_glmboost)
gl_glmboost$train(task)

#ensemble
myvar <- list()
learners_all = as.data.table(list_mlr3learners()) #select = c("id", "mlr3_package", "required_packages"))
learners_to_try <- learners_all %>% filter(class == "classif") %>% filter(grepl("weights", properties)) %>% filter(grepl("integer", feature_types))
learners_to_try <- learners_to_try %>% dplyr::select(c(name, id))
learners_to_try <- as.data.frame(learners_to_try)
for (i in learners_to_try$name[c(5,11,20)]) { myvar[[i]]  <-  po("learner_cv", learner = lrn(learners_to_try[learners_to_try$name == i,][[2]],  id = i, predict_type = "prob" ))}
myvar$ranger$param_set$values <- list(mtry = 1, num.trees=1571, sample.fraction=0.3242656, min.node.size=70,
                                      resampling.method = "cv", resampling.folds = 3,resampling.keep_response =FALSE,
                                      num.threads = 1)
myvar$glmboost$param_set$values <- list(mstop = 243, resampling.method = "cv", resampling.folds = 3,resampling.keep_response =FALSE)
log_reg_lrn = lrn("classif.log_reg", predict_type = "prob")
graph = gunion(myvar) %>>%
  po("featureunion") %>>% log_reg_lrn
graph$plot() # Plot pipeline
pipe <- GraphLearner$new(graph) # Convert pipeline to learner
pipe$predict_type <- 'prob' # We want to predict probabilities and not classes.
gr_ensemble = p0 %>>% gr_smote %>>% p2_f %>>% pipe
gl_ensemble = GraphLearner$new(gr_ensemble)
gl_ensemble$train(task)


task_pred <- TaskClassif$new(id = 'Gastro_val', SEGRIC_2021_ENE_SEP_mlr3, target = 'Exitus_90d')
ranger_valid <- gl_ranger$predict(task_pred)
predictiontables_ranger <-as.data.table(ranger_valid)
cvAUC::ci.cvAUC(predictions = predictiontables_ranger$prob.Yes,
                labels = predictiontables_ranger$truth)
# $cvAUC
# [1] 0.8294057
# 
# $se
# [1] 0.04397084
# 
# $ci
# [1] 0.7432245 0.9155870
# 
# $confidence
# [1] 0.95

enet_valid <- gl_cv_enet$predict(task_pred)
predictiontables_enet <-as.data.table(enet_valid)
cvAUC::ci.cvAUC(predictions = predictiontables_enet$prob.Yes,
                labels = predictiontables_enet$truth)
# $cvAUC
# [1] 0.7843238
# 
# $se
# [1] 0.05427195
# 
# $ci
# [1] 0.6779527 0.8906948
# 
# $confidence
# [1] 0.95

glmboost_valid <- gl_glmboost$predict(task_pred)
predictiontables_glmboost <-as.data.table(glmboost_valid)
cvAUC::ci.cvAUC(predictions = predictiontables_glmboost$prob.Yes,
                labels = predictiontables_glmboost$truth)

# $cvAUC
# [1] 0.7886783
# 
# $se
# [1] 0.05533468
# 
# $ci
# [1] 0.6802243 0.8971323
# 
# $confidence
# [1] 0.95

ensemble_valid <- gl_ensemble$predict(task_pred)
predictiontables_ensemble <-as.data.table(ensemble_valid)
cvAUC::ci.cvAUC(predictions = predictiontables_ensemble$prob.Yes,
                labels = predictiontables_ensemble$truth)

# $cvAUC
# [1] 0.8288934
# 
# $se
# [1] 0.04458013
# 
# $ci
# [1] 0.7415180 0.9162689
# 
# $confidence
# [1] 0.95


#Check differences between the two datasets
SEGRIC_2021_ENE_SEP_mlr3_compare <- SEGRIC_2021_ENE_SEP_mlr3
Gastros_FINAL_mlr3_compare <- Gastros_FINAL_mlr3
SEGRIC_2021_ENE_SEP_mlr3_compare$Split <- "Valid"
Gastros_FINAL_mlr3_compare$Split <- "Train"

to_compare <- bind_rows(Gastros_FINAL_mlr3_compare, SEGRIC_2021_ENE_SEP_mlr3_compare)
to_compare[7:24] <- lapply(to_compare[7:24], factor, levels=c(1,0), labels = c("Yes", "No"))
the_table <- atable(Gender + Age + BMI_index + ECOG + Weigth_loss + Preoperative_Hemoglobin + Myocardial_infarction + 
                      Congestive_heart_failure + Chronic_pulmonary_disease + Peripheral_vascular_disease + 
                      Cerebrovascular_disease + Dementia + Peptic_ulcer_disease + Diabetes_mellitus_uncomplicated + 
                      Diabetes_mellitus_end_organ_damage + Leukemia + Malignant_lymphoma + AIDS + Connective_tissue_disease+
                      Liver_disease_moderate_to_severe + Hemiplegia + Malignant_tumor_present + Metastatic_tumor_present + 
                      Moderate_to_severe_renal_disease + Preoperative_Albumin + Surgery + Neoadjuvant_therapy + 
                      Clinical_stage_cT + Clinical_stage_cN + Approach + Gastrectomy + Volume_activity + ASA + 
                      Exitus_90d ~ Split, 
                    data = to_compare, format_to="Word")

the_table <- the_table[-c(5,6)]
the_table[,1] <- gsub("missing", "NA", the_table[,1])
write.xlsx2(the_table, "../FINAL/Train_test_split.xlsx", row.names =  FALSE, )


#11)Session info####
print(sessionInfo())
# R version 3.6.3 (2020-02-29)
# Platform: x86_64-pc-linux-gnu (64-bit)
# Running under: Ubuntu 18.04.6 LTS
# 
# Matrix products: default
# BLAS:   /usr/lib/x86_64-linux-gnu/blas/libblas.so.3.7.1
# LAPACK: /usr/lib/x86_64-linux-gnu/lapack/liblapack.so.3.7.1
# 
# locale:
#   [1] LC_CTYPE=ca_ES.UTF-8       LC_NUMERIC=C               LC_TIME=ca_ES.UTF-8        LC_COLLATE=ca_ES.UTF-8     LC_MONETARY=ca_ES.UTF-8   
# [6] LC_MESSAGES=ca_ES.UTF-8    LC_PAPER=ca_ES.UTF-8       LC_NAME=C                  LC_ADDRESS=C               LC_TELEPHONE=C            
# [11] LC_MEASUREMENT=ca_ES.UTF-8 LC_IDENTIFICATION=C       
# 
# attached base packages:
#   [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
#   [1] DALEXtra_2.1.1          DALEX_2.3.0             ggplotify_0.1.0         ggfortify_0.4.13        stringi_1.7.6           DescTools_0.99.44      
# [7] ResourceSelection_0.3-5 gbm_2.1.8               qdapTools_1.3.5         kableExtra_1.3.4        forcats_0.5.1           stringr_1.4.0          
# [13] dplyr_1.0.7             purrr_0.3.4             readr_2.1.1             tidyr_1.1.4             tibble_3.1.6            ggplot2_3.3.5          
# [19] tidyverse_1.3.1         readxl_1.3.1            mlr3extralearners_0.4.5 mlr3verse_0.2.2         mlr3pipelines_0.4.0     mlr3tuning_0.9.0       
# [25] paradox_0.7.1           mlr3learners_0.5.1      mlr3_0.13.0            
# 
# loaded via a namespace (and not attached):
#   [1] utf8_1.2.2             reticulate_1.22        tidyselect_1.1.1       htmlwidgets_1.5.4      grid_3.6.3             ranger_0.13.1         
# [7] mlr3misc_0.9.5         mlr3proba_0.4.2        munsell_0.5.0          codetools_0.2-18       bbotk_0.4.0            chron_2.3-56          
# [13] xgboost_1.5.0.2        future_1.23.0          withr_2.4.3            colorspace_2.0-2       knitr_1.36             uuid_1.0-3            
# [19] rstudioapi_0.13        ROCR_1.0-11            dictionar6_0.1.3       listenv_0.8.0          labeling_0.4.2         repr_1.1.3            
# [25] lgr_0.4.3              cvAUC_1.1.0            parallelly_1.29.0      vctrs_0.3.8            generics_0.1.1         clusterCrit_1.2.8     
# [31] xfun_0.29              R6_2.5.1               clue_0.3-60            mlr3measures_0.4.0     bitops_1.0-7           gridGraphics_0.5-1    
# [37] assertthat_0.2.1       scales_1.1.1           rootSolve_1.8.2.3      gtable_0.3.0           globals_0.14.0         lmom_2.8              
# [43] rlang_0.4.12           systemfonts_1.0.3      randomForestSRC_2.14.0 splines_3.6.3          smotefamily_1.3.1      broom_0.7.10          
# [49] checkmate_2.0.0        yaml_2.2.1             modelr_0.1.8           backports_1.4.1        DiagrammeR_1.0.6.1     inum_1.0-4            
# [55] tools_3.6.3            param6_0.2.3           ellipsis_0.3.2         stabs_0.6-4            RColorBrewer_1.1-2     proxy_0.4-26          
# [61] Rcpp_1.0.7             base64enc_0.1-3        visNetwork_2.1.0       RCurl_1.98-1.5         rpart_4.1-15           haven_2.4.3           
# [67] cluster_2.1.0          fs_1.5.2               magrittr_2.0.1         data.table_1.14.2      reprex_2.0.1           mvtnorm_1.1-3         
# [73] distr6_1.6.2           mboost_2.9-5           hms_1.1.1              evaluate_0.14          ooplah_0.1.0           gridExtra_2.3         
# [79] shape_1.4.6            compiler_3.6.3         mlr3cluster_0.1.2      ingredients_2.2.0      crayon_1.4.2           htmltools_0.5.2       
# [85] tzdb_0.2.0             Formula_1.2-4          libcoin_1.0-9          expm_0.999-6           Exact_3.1              lubridate_1.8.0       
# [91] DBI_1.1.1              dbplyr_2.1.1           MASS_7.3-53            rappdirs_0.3.3         boot_1.3-28            data.tree_1.0.0       
# [97] mlr3data_0.5.0         Matrix_1.2-18          cli_3.1.0              quadprog_1.5-8         parallel_3.6.3         igraph_1.2.10         
# [103] pkgconfig_2.0.3        skimr_2.1.3            xml2_1.3.3             foreach_1.5.1          svglite_2.0.0          mlr3fselect_0.6.0     
# [109] webshot_0.5.2          rvest_1.0.2            yulab.utils_0.0.4      digest_0.6.29          mlr3filters_0.4.2      rmarkdown_2.11        
# [115] cellranger_1.1.0       gld_2.6.3              set6_0.2.4             lifecycle_1.0.1        jsonlite_1.7.2         viridisLite_0.4.0     
# [121] fansi_0.5.0            pillar_1.6.4           lattice_0.20-44        fastmap_1.1.0          httr_1.4.2             survival_3.2-13       
# [127] glue_1.5.1             mlr3viz_0.5.7          FNN_1.1.3              png_0.1-7              iterators_1.0.13       glmnet_4.1-3          
# [133] class_7.3-17           nnls_1.4               palmerpenguins_0.1.0   partykit_1.2-15        e1071_1.7-9            future.apply_1.8.1   
print(version)
# _                           
# platform       x86_64-pc-linux-gnu         
# arch           x86_64                      
# os             linux-gnu                   
# system         x86_64, linux-gnu           
# status                                     
# major          3                           
# minor          6.3                         
# year           2020                        
# month          02                          
# day            29                          
# svn rev        77875                       
# language       R                           
# version.string R version 3.6.3 (2020-02-29)
# nickname       Holding the Windsock  
# FIN ####  


