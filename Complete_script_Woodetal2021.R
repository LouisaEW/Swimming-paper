################################################################################
# Scripts to perfrom analysis as described in
# Wood et al. (2021) 'Perceptions of ecosystem services and disservices associated with open water swimming'
#
# Adapted and/or written by Louisa E Wood, University of Portsmouth 
#
# Code is provided as is, without support 

################################################################################

###############################################################################
####### Code for factor analysis ######
################################################################################

#load packages
library(psych)
library(readxl)
library(REdaS)
library(GPArotation)
library(ggplot2)
library(corrplot)
library(FactoMineR)
library(factoextra)
library(psych)
library(GPArotation)
library(nFactors)
library(parallel)
library(corrplot)
library(EFAutilities)
library(lavaan)
library(parameters)
library(see)
library(magrittr)
library(dplyr)
library(performance)

#load data
Question14_EFA_firsthalf<-read.csv("Question14_EFA_firsthalf_genderextract.csv")
full<-read.csv("Question14_EFA_continuous_genderextract.csv")

#investigating the number of factors 
fa.parallel(Question14_EFA_firsthalf, fm = "minres")
vss(Question14_EFA_firsthalf)

ev <- eigen(cor(Question14_EFA_firsthalf))
ap <- parallel(subject=nrow(Question14_EFA_firsthalf),var=ncol(Question14_EFA_firsthalf),rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)  
plotnScree(nS) 

n <- n_factors(Question14_EFA_firsthalf, rotate = "oblimin", fm = "minres")
summary(n)
as.data.frame(n)
plot(n) + theme_modern()

#To visualize the diagram of the exploratory factor analysis
f <- fa(Question14_EFA_firsthalf, nfactors = 3, fm="ml")
fa.diagram(f, digits = 3,cut=.4)
print(f$loadings, cutoff = .4)

#so three questions need to be dropped
# to check if after refitting the structure is confirmed...

f <- fa(Question14_EFA_firsthalf0.4Refitted, nfactors = 3, fm="ml")
fa.diagram(f, digits = 3,cut=.4)


#Confirmatory analysis
Question14_EFA_secondhalf0.4Refitted<-read.csv("Question14_EFA_secondhalf_refitted_3.csv")

structure <- psych::fa(Question14_EFA_firsthalf0.4Refitted, nfactors = 3,fm="ml") %>%  efa_to_cfa(threshold = .20)
per <- lavaan::cfa(structure, data = Question14_EFA_secondhalf0.4Refitted)
performance::compare_performance(per)

# the indexes are very close to be significant, many are 0.895 or 0.897 when they should be > 0.9...
# if we want something more robust, it would be better to use a stronger threshold (0.5 instead of 0.4) and drop three more questions: 

f <- fa(Question14_EFA_firsthalf, nfactors = 3, fm="ml")
fa.diagram(f, digits = 3,cut=.5)
print(f$loadings, cutoff = .5)

# to check if after refitting the structure is confirmed...
Question14_EFA_firsthalf0.5Refitted<-read.csv("Question14_EFA_firsthalf_refitted_genderextract.csv")
f <- fa(Question14_EFA_firsthalf0.5Refitted, nfactors = 3, fm="ml")
fa.diagram(f, digits = 3,cut=.5)
print(f$loadings, cutoff = .5)

#confirmatory
Question14_EFA_secondhalf0.5Refitted<-read.csv("Question14_EFA_secondhalf_refitted_genderextract.csv")
structure <- psych::fa(Question14_EFA_firsthalf0.5Refitted, nfactors = 3,fm="ml") %>%  efa_to_cfa(threshold = .20)
per <- lavaan::cfa(structure, data = Question14_EFA_secondhalf0.5Refitted)
performance::compare_performance(per)

# This time all indexes (with the exception of RMSEA, that was not significant also in the previous structure) are all showing a satisfactory fit.
# I think it really depends if we want to keep those three extra questions (e.g. physical fitness) or not. Also please consider that we should also be able to explain the "content" of each factor...
# In the factor n 3 for example, can we clearly explain why "physical activity" or "selflearn" seem associated with clear thinking", "anxiety" and "daily activities"? 
# If so, we could keep it, otherwise, I feel it is better to remove it and have a more robust structure to apply to all database.   

#To refit the model on the whole database
Full_Answers_RandomizedRefitted<-read.csv("Question14_EFA_continuous_refitted_genderextract.csv")

f <- fa(Full_Answers_RandomizedRefitted, nfactors = 3, fm="ml")
fa.diagram(f, digits = 3,cut=.50)
print(f$loadings, cutoff = .5)

#To obtain scores for the whole database

f <- fa(Question14_EFA_firsthalf0.5Refitted, nfactors = 3, fm="ml")
options(max.print=999999)
factor.scores(Full_Answers_RandomizedRefitted, f)->d
d
d2<-d$scores
print(d2)
write.csv(d2,'factors_genderremoved.csv')

################################################################################
#Ecosystem disservices
###############################################################################

#read in data
firsthalf<-read.csv("Question16_EFA_firsthalf_genderextract.csv")
full<-read.csv("Question16_EFA_continuous_genderextract.csv")

##To visualize the diagram of the exploratory factor analysis

f <- fa(firsthalf, nfactors = 5, fm="ml")
print(f)
print(f$loadings, cutoff = .4)
fa.diagram(f, digits = 3,cut=.4)

#to refit model on first half of the data
firsthalfrefitted <- read.csv("Question16_EFA_firsthalf_refitted_genderextract.csv")
f <- fa(firsthalfrefitted, nfactors = 5, fm="ml")
print(f)
print(f$loadings, cutoff = .4)
fa.diagram(f, digits = 3,cut=.4)

#Confirmatory analysis
SecondhalfRefitted<-read.csv("Question16_EFA_secondhalf_refitted_genderextract.csv")

structure <- psych::fa(firsthalfrefitted, nfactors = 5,fm="ml") %>%  efa_to_cfa(threshold = .20)
per <- lavaan::cfa(structure, data = SecondhalfRefitted)
performance::compare_performance(per)

#To refit the model on the whole database
Full_Answers_RandomizedRefitted<-read.csv("Question16_EFA_continuous_refitted_genderextract.csv")

f <- fa(Full_Answers_RandomizedRefitted, nfactors = 5, fm="ml")
fa.diagram(f, digits = 3,cut=.40)
print(f$loadings, cutoff = .40)

#To obtain scores for the whole database

f <- fa(firsthalfrefitted, nfactors = 5, fm="ml")
options(max.print=999999)
factor.scores(Full_Answers_RandomizedRefitted, f)->d
d
d2<-d$scores
print(d2)
write.csv(d2,'factors_genderextract.csv')

################################################################################

###############################################################################
####### Code to run GLM plots ######
################################################################################

library(ggplot2)
library(sjPlot)
aggregate (Social~Distance, `factors_dem_refitted_5_genderextractfrommain(ES)`, length)

colorBlindBlack10  <- c("#808080","#009E73","#56B4E9", "#E69F00", "#000000","#ecdd13" , "#D55E00", "#0072B2", "#CC79A7", "#bfbfbf")

glm(Social_transform~ Age, family = "gaussian",data= `factors_dem_refitted_5_genderextractfrommain(ES)`)->a
glm(Social_transform~ Gender, family = "gaussian",data= `factors_dem_refitted_5_genderextractfrommain(ES)`)->b
glm(Social_transform~ Naturalness, family = "gaussian",`factors_dem_refitted_5_genderextractfrommain(ES)`)->c
glm(Social_transform~ Sitetype, family = "gaussian",data= `factors_dem_refitted_5_genderextractfrommain(ES)`)->d
glm(Social_transform~ Pay_access, family = "gaussian",data= `factors_dem_refitted_5_genderextractfrommain(ES)`)->e
glm(Social_transform~ Distance, family = "gaussian",data= `factors_dem_refitted_5_genderextractfrommain(ES)`)->f
glm(Social_transform~ Duration, family = "gaussian",data= `factors_dem_refitted_5_genderextractfrommain(ES)`)->g
glm(Social_transform~ Summerfreq_cat2, family = "gaussian",data= `factors_dem_refitted_5_genderextractfrommain(ES)`)->h
glm(Social_transform~ Winterfreq_cat2, family = "gaussian",data= `factors_dem_refitted_5_genderextractfrommain(ES)`)->i
glm(Social_transform~ Env_group, family = "gaussian",data= `factors_dem_refitted_5_genderextractfrommain(ES)`)->j


tiff('Social.tiff', units="in", width=10, height=11, res=1000, compression = 'lzw')
plot_models(a,c, d, e, f, g,h,i,j, b, colors = colorBlindBlack10, grid = FALSE, show.p = TRUE,show.values = TRUE,p.shape = TRUE,
            legend.pval.title = "p-value", wrap.labels = 60, show.legend = TRUE,auto.label = TRUE, vline.color="grey", line.size=1,spacing = 0.6,
            dot.size = 5,title = 'Social factor', axis.labels = c("Male (90)","Yes (35)", "Never(109)", "More than twice a week (67)", "Less than weekly (183)","More than twice a week (207)", "Less than weekly (88)", "More than two hrs (5)","One to two hrs (81)", "Less than 30 mins (108)",
                                                                  ">20 km (90)", "10-20 km (109)","<1 km (88)","Yes (184)", "Marine/estuarine (210)","Semi-wild (91)","Manmade (95)",
                                                                  "55-74 (135)", "18-34 (65)"), m.labels = c("Age - 35-54 (305)","Gender - Female (415)","Naturalness - Wild (319)", "Site type - Freshwater (295)","Pay access - No (321)",
                                                                                                             "Distance - 2-10 km (218)","Duration - 30 mins to one hr (311)", "Summer freq. - One/two times a week (210)","Winter freq. - One/two times a week (146)", "Environmental group - No (470)"),
            legend.title = "Trait - Baseline (n)")+set_theme(legend.inside = FALSE,base = theme_classic(), axis.textsize.x = 1, 
                                                             axis.textsize.y = 1.2, legend.item.backcol = "white",legend.size = 1.2, legend.title.size = 1.3, geom.label.size = 2, 
                                                             title.size = 1.8, title.align = "left", axis.title.size = 1.2)

dev.off()
-----------------------------------------------------------------------------
  #Mental factor graph 
  
glm(Mental_transform~ Age, family = "gaussian",data= `factors_dem_refitted_5_genderextractfrommain(ES)`)->a
glm(Mental_transform~ Gender, family = "gaussian",data= `factors_dem_refitted_5_genderextractfrommain(ES)`)->b
glm(Mental_transform~ Naturalness, family = "gaussian",`factors_dem_refitted_5_genderextractfrommain(ES)`)->c
glm(Mental_transform~ Sitetype, family = "gaussian",data= `factors_dem_refitted_5_genderextractfrommain(ES)`)->d
glm(Mental_transform~ Pay_access, family = "gaussian",data= `factors_dem_refitted_5_genderextractfrommain(ES)`)->e
glm(Mental_transform~ Distance, family = "gaussian",data= `factors_dem_refitted_5_genderextractfrommain(ES)`)->f
glm(Mental_transform~ Duration, family = "gaussian",data= `factors_dem_refitted_5_genderextractfrommain(ES)`)->g
glm(Mental_transform~ Summerfreq_cat2, family = "gaussian",data= `factors_dem_refitted_5_genderextractfrommain(ES)`)->h
glm(Mental_transform~ Winterfreq_cat2, family = "gaussian",data= `factors_dem_refitted_5_genderextractfrommain(ES)`)->i
glm(Mental_transform~ Env_group, family = "gaussian",data= `factors_dem_refitted_5_genderextractfrommain(ES)`)->j


tiff('Mental.tiff', units="in", width=10, height=11, res=1000, compression = 'lzw')
plot_models(a,c, d, e, f, g,h,i,j, b, colors = colorBlindBlack10, grid = FALSE, show.p = TRUE,show.values = TRUE,p.shape = TRUE, 
            legend.pval.title = "p-value", wrap.labels = 60, show.legend = TRUE,auto.label = TRUE, vline.color="grey", line.size=1,spacing = 0.6,
            dot.size = 5,title = 'Mental factor', axis.labels = c("Male (90)","Yes (35)", "Never(109)", "More than twice a week (67)", "Less than weekly (183)","More than twice a week (207)", "Less than weekly (88)", "More than two hrs (5)","One to two hrs (81)", "Less than 30 mins (108)",
                                                                  ">20 km (90)", "10-20 km (109)","<1 km (88)","Yes (184)", "Marine/estuarine (210)","Semi-wild (91)","Manmade (95)",
                                                                  "55-74 (135)", "18-34 (65)"), m.labels = c("Age - 35-54 (305)","Gender - Female (415)","Naturalness - Wild (319)", "Site type - Freshwater (295)","Pay access - No (321)",
                                                                                                             "Distance - 2-10 km (218)","Duration - 30 mins to one hr (311)", "Summer freq. - One/two times a week (210)","Winter freq. - One/two times a week (146)", "Environmental group - No (470)"),
            legend.title = "Trait - Baseline (n)")+set_theme(legend.inside = FALSE,base = theme_classic(), axis.textsize.x = 1, 
                                                             axis.textsize.y = 1.2, legend.item.backcol = "white",legend.size = 1.2, legend.title.size = 1.3, geom.label.size = 2, 
                                                             title.size = 1.8, title.align = "left", axis.title.size = 1.2)

dev.off()
------------------------------------------------------------------------
  #Spiritual factor graph 
  
glm(Spiritual_transform~ Age, family = "gaussian",data= `factors_dem_refitted_5_genderextractfrommain(ES)`)->a
glm(Spiritual_transform~ Gender, family = "gaussian",data= `factors_dem_refitted_5_genderextractfrommain(ES)`)->b
glm(Spiritual_transform~ Naturalness, family = "gaussian",`factors_dem_refitted_5_genderextractfrommain(ES)`)->c
glm(Spiritual_transform~ Sitetype, family = "gaussian",data= `factors_dem_refitted_5_genderextractfrommain(ES)`)->d
glm(Spiritual_transform~ Pay_access, family = "gaussian",data= `factors_dem_refitted_5_genderextractfrommain(ES)`)->e
glm(Spiritual_transform~ Distance, family = "gaussian",data= `factors_dem_refitted_5_genderextractfrommain(ES)`)->f
glm(Spiritual_transform~ Duration, family = "gaussian",data= `factors_dem_refitted_5_genderextractfrommain(ES)`)->g
glm(Spiritual_transform~ Summerfreq_cat2, family = "gaussian",data= `factors_dem_refitted_5_genderextractfrommain(ES)`)->h
glm(Spiritual_transform~ Winterfreq_cat2, family = "gaussian",data= `factors_dem_refitted_5_genderextractfrommain(ES)`)->i
glm(Spiritual_transform~ Env_group, family = "gaussian",data= `factors_dem_refitted_5_genderextractfrommain(ES)`)->j

tiff('Spiritual.tiff', units="in", width=10, height=11, res=1000, compression = 'lzw')
plot_models(a,c, d, e, f, g,h,i,j, b, colors = colorBlindBlack10, grid = FALSE, show.p = TRUE,show.values = TRUE,p.shape = TRUE, 
            legend.pval.title = "p-value", wrap.labels = 60, show.legend = TRUE,auto.label = TRUE, vline.color="grey", line.size=1,spacing = 0.6,
            dot.size = 5,title = 'Spiritual factor', axis.labels = c("Male (90)","Yes (35)", "Never(109)", "More than twice a week (67)", "Less than weekly (183)","More than twice a week (207)", "Less than weekly (88)", "More than two hrs (5)","One to two hrs (81)", "Less than 30 mins (108)",
                                                                     ">20 km (90)", "10-20 km (109)","<1 km (88)","Yes (184)", "Marine/estuarine (210)","Semi-wild (91)","Manmade (95)",
                                                                     "55-74 (135)", "18-34 (65)"), m.labels = c("Age - 35-54 (305)","Gender - Female (415)","Naturalness - Wild (319)", "Site type - Freshwater (295)","Pay access - No (321)",
                                                                                                                "Distance - 2-10 km (218)","Duration - 30 mins to one hr (311)", "Summer freq. - One/two times a week (210)","Winter freq. - One/two times a week (146)", "Environmental group - No (470)"),
            legend.title = "Trait - Baseline (n)")+set_theme(legend.inside = FALSE,base = theme_classic(), axis.textsize.x = 1, 
                                                             axis.textsize.y = 1.2, legend.item.backcol = "white",legend.size = 1.2, legend.title.size = 1.3, geom.label.size = 2, 
                                                             title.size = 1.8, title.align = "left", axis.title.size = 1.2)

dev.off()

-----------------------------------------------------------------------------
  -----------------------------------------------------------------------------
  
  #(EDS)
  
  #ML1
  
glm(ML1~ Age, family = "gaussian",data= `factors_dem_refitted_genderextractfrommain(EDS)`)->a
glm(ML1~ Gender, family = "gaussian",data= `factors_dem_refitted_genderextractfrommain(EDS)`)->b
glm(ML1~ Naturalness, family = "gaussian",`factors_dem_refitted_genderextractfrommain(EDS)`)->c
glm(ML1~ Sitetype, family = "gaussian",data= `factors_dem_refitted_genderextractfrommain(EDS)`)->d
glm(ML1~ Pay_access, family = "gaussian",data= `factors_dem_refitted_genderextractfrommain(EDS)`)->e
glm(ML1~ Distance, family = "gaussian",data= `factors_dem_refitted_genderextractfrommain(EDS)`)->f
glm(ML1~ Duration, family = "gaussian",data= `factors_dem_refitted_genderextractfrommain(EDS)`)->g
glm(ML1~ Summerfreq_cat2, family = "gaussian",data= `factors_dem_refitted_genderextractfrommain(EDS)`)->h
glm(ML1~ Winterfreq_cat2, family = "gaussian",data= `factors_dem_refitted_genderextractfrommain(EDS)`)->i
glm(ML1~ Env_group, family = "gaussian",data= `factors_dem_refitted_genderextractfrommain(EDS)`)->j

tiff('ML1.tiff', units="in", width=10, height=11, res=1000, compression = 'lzw')
plot_models(a,c, d, e, f, g,h,i,j, b, colors = colorBlindBlack10, grid = FALSE, show.p = TRUE,show.values = TRUE,p.shape = TRUE, 
            legend.pval.title = "p-value", wrap.labels = 60, show.legend = TRUE,auto.label = TRUE, vline.color="grey", line.size=1,spacing = 0.6,
            dot.size = 5,title = 'ML1', axis.labels = c("Male (90)","Yes (35)", "Never(109)", "More than twice a week (67)", "Less than weekly (183)","More than twice a week (207)", "Less than weekly (88)", "More than two hrs (5)","One to two hrs (81)", "Less than 30 mins (108)",
                                                        ">20 km (90)", "10-20 km (109)","<1 km (88)","Yes (184)", "Marine/estuarine (210)","Semi-wild (91)","Manmade (95)",
                                                        "55-74 (135)", "18-34 (65)"), m.labels = c("Age - 35-54 (305)","Gender - Female (415)","Naturalness - Wild (319)", "Site type - Freshwater (295)","Pay access - No (321)",
                                                                                                   "Distance - 2-10 km (218)","Duration - 30 mins to one hr (311)", "Summer freq. - One/two times a week (210)","Winter freq. - One/two times a week (146)", "Environmental group - No (470)"),
            legend.title = "Trait - Baseline (n)")+set_theme(legend.inside = FALSE,base = theme_classic(), axis.textsize.x = 1, 
                                                             axis.textsize.y = 1.2, legend.item.backcol = "white",legend.size = 1.2, legend.title.size = 1.3, geom.label.size = 2, 
                                                             title.size = 1.8, title.align = "left", axis.title.size = 1.2)

dev.off()

-----------------------------------------------
  #  ML2
  
glm(ML2~ Age, family = "gaussian",data= `factors_dem_refitted_genderextractfrommain(EDS)`)->a
glm(ML2~ Gender, family = "gaussian",data= `factors_dem_refitted_genderextractfrommain(EDS)`)->b
glm(ML2~ Naturalness, family = "gaussian",`factors_dem_refitted_genderextractfrommain(EDS)`)->c
glm(ML2~ Sitetype, family = "gaussian",data= `factors_dem_refitted_genderextractfrommain(EDS)`)->d
glm(ML2~ Pay_access, family = "gaussian",data= `factors_dem_refitted_genderextractfrommain(EDS)`)->e
glm(ML2~ Distance, family = "gaussian",data= `factors_dem_refitted_genderextractfrommain(EDS)`)->f
glm(ML2~ Duration, family = "gaussian",data= `factors_dem_refitted_genderextractfrommain(EDS)`)->g
glm(ML2~ Summerfreq_cat2, family = "gaussian",data= `factors_dem_refitted_genderextractfrommain(EDS)`)->h
glm(ML2~ Winterfreq_cat2, family = "gaussian",data= `factors_dem_refitted_genderextractfrommain(EDS)`)->i
glm(ML2~ Env_group, family = "gaussian",data= `factors_dem_refitted_genderextractfrommain(EDS)`)->j

tiff('ML2.tiff', units="in", width=10, height=11, res=1000, compression = 'lzw')
plot_models(a,c, d, e, f, g,h,i,j, b, colors = colorBlindBlack10, grid = FALSE, show.p = TRUE,show.values = TRUE,p.shape = TRUE, 
            legend.pval.title = "p-value", wrap.labels = 60, show.legend = TRUE,auto.label = TRUE, vline.color="grey", line.size=1,spacing = 0.6,
            dot.size = 5,title = 'ML2', axis.labels = c("Male (90)","Yes (35)", "Never(109)", "More than twice a week (67)", "Less than weekly (183)","More than twice a week (207)", "Less than weekly (88)", "More than two hrs (5)","One to two hrs (81)", "Less than 30 mins (108)",
                                                        ">20 km (90)", "10-20 km (109)","<1 km (88)","Yes (184)", "Marine/estuarine (210)","Semi-wild (91)","Manmade (95)",
                                                        "55-74 (135)", "18-34 (65)"), m.labels = c("Age - 35-54 (305)","Gender - Female (415)","Naturalness - Wild (319)", "Site type - Freshwater (295)","Pay access - No (321)",
                                                                                                   "Distance - 2-10 km (218)","Duration - 30 mins to one hr (311)", "Summer freq. - One/two times a week (210)","Winter freq. - One/two times a week (146)", "Environmental group - No (470)"),
            legend.title = "Trait - Baseline (n)")+set_theme(legend.inside = FALSE,base = theme_classic(), axis.textsize.x = 1, 
                                                             axis.textsize.y = 1.2, legend.item.backcol = "white",legend.size = 1.2, legend.title.size = 1.3, geom.label.size = 2, 
                                                             title.size = 1.8, title.align = "left", axis.title.size = 1.2)

dev.off()

-----------------------------------------------
  #  ML3
  
glm(ML3~ Age, family = "gaussian",data= `factors_dem_refitted_genderextractfrommain(EDS)`)->a
glm(ML3~ Gender, family = "gaussian",data= `factors_dem_refitted_genderextractfrommain(EDS)`)->b
glm(ML3~ Naturalness, family = "gaussian",`factors_dem_refitted_genderextractfrommain(EDS)`)->c
glm(ML3~ Sitetype, family = "gaussian",data= `factors_dem_refitted_genderextractfrommain(EDS)`)->d
glm(ML3~ Pay_access, family = "gaussian",data= `factors_dem_refitted_genderextractfrommain(EDS)`)->e
glm(ML3~ Distance, family = "gaussian",data= `factors_dem_refitted_genderextractfrommain(EDS)`)->f
glm(ML3~ Duration, family = "gaussian",data= `factors_dem_refitted_genderextractfrommain(EDS)`)->g
glm(ML3~ Summerfreq_cat2, family = "gaussian",data= `factors_dem_refitted_genderextractfrommain(EDS)`)->h
glm(ML3~ Winterfreq_cat2, family = "gaussian",data= `factors_dem_refitted_genderextractfrommain(EDS)`)->i
glm(ML3~ Env_group, family = "gaussian",data= `factors_dem_refitted_genderextractfrommain(EDS)`)->j

tiff('ML3.tiff', units="in", width=10, height=11, res=1000, compression = 'lzw')
plot_models(a,c, d, e, f, g,h,i,j, b, colors = colorBlindBlack10, grid = FALSE, show.p = TRUE,show.values = TRUE,p.shape = TRUE, 
            legend.pval.title = "p-value", wrap.labels = 60, show.legend = TRUE,auto.label = TRUE, vline.color="grey", line.size=1,spacing = 0.6,
            dot.size = 5,title = 'ML3', axis.labels = c("Male (90)","Yes (35)", "Never(109)", "More than twice a week (67)", "Less than weekly (183)","More than twice a week (207)", "Less than weekly (88)", "More than two hrs (5)","One to two hrs (81)", "Less than 30 mins (108)",
                                                        ">20 km (90)", "10-20 km (109)","<1 km (88)","Yes (184)", "Marine/estuarine (210)","Semi-wild (91)","Manmade (95)",
                                                        "55-74 (135)", "18-34 (65)"), m.labels = c("Age - 35-54 (305)","Gender - Female (415)","Naturalness - Wild (319)", "Site type - Freshwater (295)","Pay access - No (321)",
                                                                                                   "Distance - 2-10 km (218)","Duration - 30 mins to one hr (311)", "Summer freq. - One/two times a week (210)","Winter freq. - One/two times a week (146)", "Environmental group - No (470)"),
            legend.title = "Trait - Baseline (n)")+set_theme(legend.inside = FALSE,base = theme_classic(), axis.textsize.x = 1, 
                                                             axis.textsize.y = 1.2, legend.item.backcol = "white",legend.size = 1.2, legend.title.size = 1.3, geom.label.size = 2, 
                                                             title.size = 1.8, title.align = "left", axis.title.size = 1.2)

dev.off()


-----------------------------------------------
  #  ML4
  
glm(ML4~ Age, family = "gaussian",data= `factors_dem_refitted_genderextractfrommain(EDS)`)->a
glm(ML4~ Gender, family = "gaussian",data= `factors_dem_refitted_genderextractfrommain(EDS)`)->b
glm(ML4~ Naturalness, family = "gaussian",`factors_dem_refitted_genderextractfrommain(EDS)`)->c
glm(ML4~ Sitetype, family = "gaussian",data= `factors_dem_refitted_genderextractfrommain(EDS)`)->d
glm(ML4~ Pay_access, family = "gaussian",data= `factors_dem_refitted_genderextractfrommain(EDS)`)->e
glm(ML4~ Distance, family = "gaussian",data= `factors_dem_refitted_genderextractfrommain(EDS)`)->f
glm(ML4~ Duration, family = "gaussian",data= `factors_dem_refitted_genderextractfrommain(EDS)`)->g
glm(ML4~ Summerfreq_cat2, family = "gaussian",data= `factors_dem_refitted_genderextractfrommain(EDS)`)->h
glm(ML4~ Winterfreq_cat2, family = "gaussian",data= `factors_dem_refitted_genderextractfrommain(EDS)`)->i
glm(ML4~ Env_group, family = "gaussian",data= `factors_dem_refitted_genderextractfrommain(EDS)`)->j

tiff('ML4.tiff', units="in", width=10, height=11, res=1000, compression = 'lzw')
plot_models(a,c, d, e, f, g,h,i,j, b, colors = colorBlindBlack10, grid = FALSE, show.p = TRUE,show.values = TRUE,p.shape = TRUE, 
            legend.pval.title = "p-value", wrap.labels = 60, show.legend = TRUE,auto.label = TRUE, vline.color="grey", line.size=1,spacing = 0.6,
            dot.size = 5,title = 'ML4', axis.labels = c("Male (90)","Yes (35)", "Never(109)", "More than twice a week (67)", "Less than weekly (183)","More than twice a week (207)", "Less than weekly (88)", "More than two hrs (5)","One to two hrs (81)", "Less than 30 mins (108)",
                                                        ">20 km (90)", "10-20 km (109)","<1 km (88)","Yes (184)", "Marine/estuarine (210)","Semi-wild (91)","Manmade (95)",
                                                        "55-74 (135)", "18-34 (65)"), m.labels = c("Age - 35-54 (305)","Gender - Female (415)","Naturalness - Wild (319)", "Site type - Freshwater (295)","Pay access - No (321)",
                                                                                                   "Distance - 2-10 km (218)","Duration - 30 mins to one hr (311)", "Summer freq. - One/two times a week (210)","Winter freq. - One/two times a week (146)", "Environmental group - No (470)"),
            legend.title = "Trait - Baseline (n)")+set_theme(legend.inside = FALSE,base = theme_classic(), axis.textsize.x = 1, 
                                                             axis.textsize.y = 1.2, legend.item.backcol = "white",legend.size = 1.2, legend.title.size = 1.3, geom.label.size = 2, 
                                                             title.size = 1.8, title.align = "left", axis.title.size = 1.2)

dev.off()

-----------------------------------------------
  #  ML5
  
glm(ML5~ Age, family = "gaussian",data= `factors_dem_refitted_genderextractfrommain(EDS)`)->a
glm(ML5~ Gender, family = "gaussian",data= `factors_dem_refitted_genderextractfrommain(EDS)`)->b
glm(ML5~ Naturalness, family = "gaussian",`factors_dem_refitted_genderextractfrommain(EDS)`)->c
glm(ML5~ Sitetype, family = "gaussian",data= `factors_dem_refitted_genderextractfrommain(EDS)`)->d
glm(ML5~ Pay_access, family = "gaussian",data= `factors_dem_refitted_genderextractfrommain(EDS)`)->e
glm(ML5~ Distance, family = "gaussian",data= `factors_dem_refitted_genderextractfrommain(EDS)`)->f
glm(ML5~ Duration, family = "gaussian",data= `factors_dem_refitted_genderextractfrommain(EDS)`)->g
glm(ML5~ Summerfreq_cat2, family = "gaussian",data= `factors_dem_refitted_genderextractfrommain(EDS)`)->h
glm(ML5~ Winterfreq_cat2, family = "gaussian",data= `factors_dem_refitted_genderextractfrommain(EDS)`)->i
glm(ML5~ Env_group, family = "gaussian",data= `factors_dem_refitted_genderextractfrommain(EDS)`)->j

tiff('ML5.tiff', units="in", width=10, height=11, res=1000, compression = 'lzw')
plot_models(a,c, d, e, f, g,h,i,j, b, colors = colorBlindBlack10, grid = FALSE, show.p = TRUE,show.values = TRUE,p.shape = TRUE, 
            legend.pval.title = "p-value", wrap.labels = 60, show.legend = TRUE,auto.label = TRUE, vline.color="grey", line.size=1,spacing = 0.6,
            dot.size = 5,title = 'ML5', axis.labels = c("Male (90)","Yes (35)", "Never(109)", "More than twice a week (67)", "Less than weekly (183)","More than twice a week (207)", "Less than weekly (88)", "More than two hrs (5)","One to two hrs (81)", "Less than 30 mins (108)",
                                                        ">20 km (90)", "10-20 km (109)","<1 km (88)","Yes (184)", "Marine/estuarine (210)","Semi-wild (91)","Manmade (95)",
                                                        "55-74 (135)", "18-34 (65)"), m.labels = c("Age - 35-54 (305)","Gender - Female (415)","Naturalness - Wild (319)", "Site type - Freshwater (295)","Pay access - No (321)",
                                                                                                   "Distance - 2-10 km (218)","Duration - 30 mins to one hr (311)", "Summer freq. - One/two times a week (210)","Winter freq. - One/two times a week (146)", "Environmental group - No (470)"),
            legend.title = "Trait - Baseline (n)")+set_theme(legend.inside = FALSE,base = theme_classic(), axis.textsize.x = 1, 
                                                             axis.textsize.y = 1.2, legend.item.backcol = "white",legend.size = 1.2, legend.title.size = 1.3, geom.label.size = 2, 
                                                             title.size = 1.8, title.align = "left", axis.title.size = 1.2)
dev.off()

################################################################################

###############################################################################
####### Code to run Likert plots ######
################################################################################

library(dplyr)
library(ggplot2)
library(tidyr)
library(likert)
library(plyr)


#ecosystem service figure

AQ<-read.csv("Question14_EFA_genderremoved.csv")
View(AQ)                 
#Create a title
Title_AQ <- "Well-being indicator statements"

#Specify the levels of the factors
mylevels = c("Very low motivation", "Low motivation", "Moderate motivation", "High motivation", "Very high motivation")

#Convert to a dataframe
DF_AQ <- as.data.frame(unclass(AQ))

#Convert the columns to factors
#I am sure there is a more elegant way of doing this, but I was lazy to look for it :P

AQ$Open.water.swimming.is.the.best.opportunity.for.recreation.in.the.area <- factor(AQ$Open.water.swimming.is.the.best.opportunity.for.recreation.in.the.area, levels = mylevels)
AQ$Open.water.swimming.leaves.me.feeling.more.physically.healthy.and.fit <- factor(AQ$Open.water.swimming.leaves.me.feeling.more.physically.healthy.and.fit  , levels = mylevels)
AQ$Open.water.swimming.improves.my.ability.to.think.clearly <- factor(AQ$Open.water.swimming.improves.my.ability.to.think.clearly, levels = mylevels)
AQ$Open.water.swimming.increases.my.self.esteem <- factor(AQ$Open.water.swimming.increases.my.self.esteem, levels = mylevels)
AQ$Open.water.swimming.reduces.my.feelings.of.depression.and.anxiety <- factor(AQ$Open.water.swimming.reduces.my.feelings.of.depression.and.anxiety, levels = mylevels)

AQ$Open.water.swimming.allows.me.to.recover.from.illness.faster <- factor(AQ$Open.water.swimming.allows.me.to.recover.from.illness.faster, levels = mylevels)
AQ$Open.water.swimming.has.made.me.learn.more.about.nature <- factor(AQ$Open.water.swimming.has.made.me.learn.more.about.nature, levels = mylevels)
AQ$Open.water.swimming.has.made.me.learn.more.about.myself <- factor(AQ$Open.water.swimming.has.made.me.learn.more.about.myself, levels = mylevels)
AQ$I.always.have.fun.when.I.go.open.water.swimming <- factor(AQ$I.always.have.fun.when.I.go.open.water.swimming, levels = mylevels)

AQ$Open.water.swimming.gives.me.a.sense.of.freedom <- factor(AQ$Open.water.swimming.gives.me.a.sense.of.freedom, levels = mylevels)
AQ$I.feel.a.sense.of.community.when.open.water.swimming <- factor(AQ$I.feel.a.sense.of.community.when.open.water.swimming , levels = mylevels)
AQ$I.value.socialising.when.I.go.open.water.swimming <- factor(AQ$I.feel.a.sense.of.community.when.open.water.swimming, levels = mylevels)


AQ$Open.water.swimming.has.given.me.a.lot.of.memorable.experiences <- factor(AQ$Open.water.swimming.has.given.me.a.lot.of.memorable.experiences, levels = mylevels)
AQ$Open.water.swimming.makes.me.feel.connected.to.nature <- factor(AQ$Open.water.swimming.makes.me.feel.connected.to.nature, levels = mylevels)
AQ$Open.water.swimming.gives.me.a.sense.of.spiritual.satisfaction <- factor(AQ$Open.water.swimming.gives.me.a.sense.of.spiritual.satisfaction, levels = mylevels)

AQ$Open.water.swimming.inspires.me <- factor(AQ$Open.water.swimming.inspires.me, levels = mylevels)
AQ$Open.water.swimming.gives.me.an.appreciation.for.life <- factor(AQ$Open.water.swimming.gives.me.an.appreciation.for.life, levels = mylevels)
AQ$Open.water.swimming.gives.me.a.sense.of.belonging.to.something.greater.than.myself<- factor(AQ$Open.water.swimming.gives.me.a.sense.of.belonging.to.something.greater.than.myself, levels = mylevels)
AQ$Open.water.swimming.gives.me.a.sense.of.identity<- factor(AQ$Open.water.swimming.gives.me.a.sense.of.identity, levels = mylevels)
AQ$I.miss.open.water.swimming.when.I.have.not.done.it.in.a.long.time<- factor(AQ$I.miss.open.water.swimming.when.I.have.not.done.it.in.a.long.time, levels = mylevels)
AQ$Going.open.water.swimming.makes.me.feel.like.I.am.more.able.to.do.daily.activities<- factor(AQ$Going.open.water.swimming.makes.me.feel.like.I.am.more.able.to.do.daily.activities, levels = mylevels)

#Rename the y axis values
AQ2 <- rename (AQ, c("Open.water.swimming.is.the.best.opportunity.for.recreation.in.the.area" = "Best opportunity for recreation in the area", 
                     "Open.water.swimming.leaves.me.feeling.more.physically.healthy.and.fit" = "Leaves me feeling more physically healthy and fit", 
                     "Open.water.swimming.improves.my.ability.to.think.clearly" = "Improves my ability to think clearly",
                     "Open.water.swimming.increases.my.self.esteem" = "Increases my self esteem",
                     "Open.water.swimming.reduces.my.feelings.of.depression.and.anxiety" = "Reduces my feelings of depression and anxiety",  
                     "Open.water.swimming.allows.me.to.recover.from.illness.faster" = "Allows me to recover from illness faster",  
                     "Open.water.swimming.has.made.me.learn.more.about.nature" = "Has made me learn more about nature",  
                     "Open.water.swimming.has.made.me.learn.more.about.myself" = "Has made me learn more about myself",  
                     "I.always.have.fun.when.I.go.open.water.swimming" = "I always have fun when I go open water swimming",  
                     "Open.water.swimming.gives.me.a.sense.of.freedom" = "Open water swimming gives me a sense of freedom",
                     "I.feel.a.sense.of.community.when.open.water.swimming" = "I feel a sense of community when open water swimming",  
                     "I.value.socialising.when.I.go.open.water.swimming" = "I value socialising when I go open water swimming",  
                     "Open.water.swimming.has.given.me.a.lot.of.memorable.experiences" = "Open water swimming has given me a lot of memorable experiences",
                     "Open.water.swimming.makes.me.feel.connected.to.nature" = "Open water swimming makes me feel connected to nature",  
                     "Open.water.swimming.gives.me.a.sense.of.spiritual.satisfaction" = "Open water swimming gives me a sense of spiritual satisfaction",  
                     "Open.water.swimming.inspires.me" = "Open water swimming inspires me",  
                     "Open.water.swimming.gives.me.an.appreciation.for.life" = "Open water swimming gives me an appreciation for life",  
                     "Open.water.swimming.gives.me.a.sense.of.belonging.to.something.greater.than.myself" = "Open water swimming gives me a sense of belonging to something greater than myself",  
                     "Open.water.swimming.gives.me.a.sense.of.identity" = "Open water swimming gives me a sense of identity",
                     "I.miss.open.water.swimming.when.I.have.not.done.it.in.a.long.time" = "I miss open water swimming when I have not done it in a long time",  
                     "Going.open.water.swimming.makes.me.feel.like.I.am.more.able.to.do.daily.activities" = "Going open water swimming makes me feel like I am more able to do daily activities"))                 

#Convert the columns to factors
DF_AQ <- as.data.frame(unclass(AQ2))

#Build plot
pAQ <- likert(DF_AQ) 
plot(pAQ, centered = TRUE, include.center = TRUE, ordered=TRUE) + ggtitle(Title_AQ) 

#Nice colours!!
library(RColorBrewer)
display.brewer.all(colorblindFriendly = TRUE)
brewer.pal(5, 'RdBu')->myColor
plot(pAQ, centered = TRUE, include.center = TRUE, ordered=TRUE,col=myColor) + ggtitle(Title_AQ)

#or
brewer.pal(5, 'PRGn')->myColor
plot(pAQ, centered = TRUE, include.center = TRUE, ordered=TRUE,col=myColor) + ggtitle(Title_AQ)

#ecosystem disservice plot
DQ<-read.csv("Question16_EFA_genderextract.csv")
View(DQ)                 
#Create a title
Title_DQ <- "Ecosystem disservice indicator statements"

#Specify the levels of the factors
mylevels = c("Very low concern", "Low concern", "Moderate concern", "High concern", "Very high concern")

#Convert to a dataframe
DF_DQ <- as.data.frame(unclass(DQ))

#Convert the columns to factors
#I am sure there is a more elegant way of doing this, but I was lazy to look for it :P

DQ$Qu16_Algae <- factor(DQ$Qu16_Algae, levels = mylevels)
DQ$Qu16_Seaweed <- factor(DQ$Qu16_Seaweed, levels = mylevels)
DQ$Qu16_Mud <- factor(DQ$Qu16_Mud, levels = mylevels)
DQ$Qu16_Rocks <- factor(DQ$Qu16_Rocks, levels = mylevels)
DQ$Qu16_Sickness <- factor(DQ$Qu16_Sickness, levels = mylevels)
DQ$Qu16_Animalattack <- factor(DQ$Qu16_Animalattack, levels = mylevels)
DQ$Qu16_Plantallergies <- factor(DQ$Qu16_Plantallergies, levels = mylevels)
DQ$Qu16_Insectbites <- factor(DQ$Qu16_Insectbites, levels = mylevels)
DQ$Qu16_Accidents <- factor(DQ$Qu16_Accidents, levels = mylevels)
DQ$Qu16_Coldwater <- factor(DQ$Qu16_Coldwater, levels = mylevels)
DQ$Qu16_Currents <- factor(DQ$Qu16_Currents, levels = mylevels)
DQ$Qu16_Sunburn <- factor(DQ$Qu16_Sunburn, levels = mylevels)
DQ$Qu16_Lightening <- factor(DQ$Qu16_Lightening, levels = mylevels)
DQ$Qu16_Flashflood <- factor(DQ$Qu16_Flashflood, levels = mylevels)
DQ$Qu16_Smells <- factor(DQ$Qu16_Smells, levels = mylevels)
DQ$Qu16_Waterquality <- factor(DQ$Qu16_Waterquality, levels = mylevels)
DQ$Qu16_Soilerosion <- factor(DQ$Qu16_Soilerosion, levels = mylevels)
DQ$Qu16_Landslide <- factor(DQ$Qu16_Landslide, levels = mylevels)
DQ$Qu16_Crime <- factor(DQ$Qu16_Crime, levels = mylevels)
DQ$Qu16_Rubbish <- factor(DQ$Qu16_Rubbish, levels = mylevels)
DQ$Qu16_Unattractive <- factor(DQ$Qu16_Unattractive, levels = mylevels)
DQ$Qu16_Boattraffic <- factor(DQ$Qu16_Boattraffic, levels = mylevels)
DQ$Qu16_Otherusers <- factor(DQ$Qu16_Otherusers, levels = mylevels)

#Rename the y axis values
DQ <- rename (DQ, c(Qu16_Algae= "Algae", 
                    Qu16_Seaweed= "Seaweed", 
                    Qu16_Mud = "Mud",
                    Qu16_Rocks = "Rocks",
                    Qu16_Sickness = "Sickness",  
                    Qu16_Animalattack = "Animal attack", 
                    Qu16_Plantallergies = "Plant allergies", 
                    Qu16_Insectbites = "Insect bites", 
                    Qu16_Accidents = "Accidents", 
                    Qu16_Coldwater = "Cold water", 
                    Qu16_Currents = "Currents", 
                    Qu16_Sunburn = "Sun burn", 
                    Qu16_Lightening = "Lightening", 
                    Qu16_Flashflood = "Flash flood", 
                    Qu16_Smells = "Smells", 
                    Qu16_Waterquality = "Water quality", 
                    Qu16_Soilerosion = "Soil erosion", 
                    Qu16_Landslide = "Landslide", 
                    Qu16_Crime = "Crime", 
                    Qu16_Rubbish = "Rubbish", 
                    Qu16_Unattractive = "Unattractive surroundings", 
                    Qu16_Boattraffic = "Boat traffic", 
                    Qu16_Otherusers = "Other users"))

#Convert the columns to factors
DF_DQ <- as.data.frame(unclass(DQ))

#Build plot
pAQ <- likert(DF_DQ) 
plot(pAQ, centered = TRUE, include.center = TRUE, ordered=TRUE,col=myColor) + ggtitle(Title_DQ)

#Nice colours!!
library(RColorBrewer)
display.brewer.all(colorblindFriendly = TRUE)
brewer.pal(5, 'RdBu')->myColor
plot(pAQ, centered = TRUE, include.center = TRUE, ordered=TRUE,col=myColor) + ggtitle(Title_DQ)

#or
brewer.pal(5, 'PRGn')->myColor
plot(pAQ, centered = TRUE, include.center = TRUE, ordered=TRUE,col=myColor) + ggtitle(Title_DQ)                   

