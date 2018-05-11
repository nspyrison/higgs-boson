# Nicholas SPyrison
# 26 Feb 2018
# froester review

library(randomForest)
library(party)
set.seed(0)
library(class) #knn()
library(gmodels) #CrossTable()
#knn: https://rstudio-pubs-static.s3.amazonaws.com/123438_3b9052ed40ec4cd2854b72d1aa154df9.html
#knn/caret: https://www.datacamp.com/community/tutorials/machine-learning-in-r


###data
#train = read.csv("./data/training.csv")  #load training data, 250,000 x 33
#train[train==-999] <- NA  # set NAs
#train$Label=as.numeric(train$Label)-1  # Change Label to 0,1. b = 0, s = 1
#str(train)
#head(train)
#omit_train <- na.omit(train)  # from 250,000, to 68,114. 27.25% row-wise complete, w/e.
omit_train <- read.csv("./data/training_naOmitterd.csv")[]
#data <- omit_train[1:1000]

### Random Forests
#http://trevorstephens.com/kaggle-titanic-tutorial/r-part-5-random-forests/
#compared 10,100 and 1000 trees, 100 is best bang for buck, though 52.2 Mb.

#fit10 <- randomForest(as.factor(Label) ~ DER_mass_MMC+DER_mass_transverse_met_lep+
#                        DER_mass_vis+DER_pt_h+DER_deltaeta_jet_jet+DER_mass_jet_jet+
#                        DER_prodeta_jet_jet+DER_deltar_tau_lep+DER_pt_tot+DER_sum_pt+
#                        DER_pt_ratio_lep_tau+DER_met_phi_centrality+DER_lep_eta_centrality,
#                      data=omit_train, 
#                      importance=TRUE, 
#                      ntree=10)
fit100 <- randomForest(as.factor(Label) ~ DER_mass_MMC+DER_mass_transverse_met_lep+
                      DER_mass_vis+DER_pt_h+DER_deltaeta_jet_jet+DER_mass_jet_jet+
                      DER_prodeta_jet_jet+DER_deltar_tau_lep+DER_pt_tot+DER_sum_pt+
                      DER_pt_ratio_lep_tau+DER_met_phi_centrality+DER_lep_eta_centrality,
                      data=omit_train, 
                      importance=TRUE, 
                      ntree=100) #huge; 52.2 Mb ram.
#fit1000 <- randomForest(as.factor(Label) ~ DER_mass_MMC+DER_mass_transverse_met_lep+
#                         DER_mass_vis+DER_pt_h+DER_deltaeta_jet_jet+DER_mass_jet_jet+
#                         DER_prodeta_jet_jet+DER_deltar_tau_lep+DER_pt_tot+DER_sum_pt+
#                         DER_pt_ratio_lep_tau+DER_met_phi_centrality+DER_lep_eta_centrality,
#                       data=omit_train, 
#                       importance=TRUE, 
#                       ntree=1000)

#names(omit_train)
varImpPlot(fit100)
table(omit_train$Label, fit100$predicted)
#table(omit_train$Label, fit100$predicted)/rbind(table(omit_train$Label),table(omit_train$Label))


###Party Random Forests
#https://www.r-bloggers.com/random-forests-in-r/
library(party)
fit <- cforest(as.factor(Label) ~ DER_mass_MMC+DER_mass_transverse_met_lep+
                 DER_mass_vis+DER_pt_h+DER_deltaeta_jet_jet+DER_mass_jet_jet+
                 DER_prodeta_jet_jet+DER_deltar_tau_lep+DER_pt_tot+DER_sum_pt+
                 DER_pt_ratio_lep_tau+DER_met_phi_centrality+DER_lep_eta_centrality,
               data = omit_train, 
               controls=cforest_unbiased(ntree=25, mtry=3))

###

#training Sample with 300 observations
train=sample(1:nrow(Boston),300)
?Boston  #to search on the dataset



