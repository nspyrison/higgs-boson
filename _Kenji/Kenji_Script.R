# Choose library path to C:/Users/kenji/Documents/R Codes/Tools
.libPaths("C:/Users/kenji/Documents/R Codes/Tools")
# Set environment variables just in case
Sys.setenv("HOME" = "C:/Users/kenji/Documents/R Codes/Tools")
Sys.setenv(R_USER = "C:/Users/kenji/Documents/R Codes/Tools")
Sys.setenv(R_LIBS_USER = "C:/Users/kenji/Documents/R Codes/Tools")
Sys.setenv(R_LIBS_USERS = "C:/Users/kenji/Documents/R Codes/Tools")

# Install tidyverse because its apparently useful
install.packages("tidyverse")
library(tidyverse)

# Install devtools and load withr and curl as per error message before loading devtools
install.packages("devtools")
library(withr)
library(curl)
library(devtools)

# Install and load httr in order to install github packages from Andeek
install.packages("httr")
library(httr)
devtools::install_github("andeek/rpart", lib = "C:/Users/kenji/Documents/R Codes/Tools")
devtools::install_github("andeek/forestr", lib = "C:/Users/kenji/Documents/R Codes/Tools")

# Load libraries as per Andeek
install.packages("proto")
install.packages("digest")
install.packages("ggplot2", dep = TRUE) # was not found so needed to install
library(devtools)
if (packageVersion("devtools") < "1.9.1") {
  message("Please upgrade devtools")
}
devtools::install_deps()
devtools::install_github("hadley/ggplot2")
install.packages("GGally") # was not found so needed to install
install.packages("knitr") # was not found so needed to install
install.packages("grDevices")
install.packages("plotly")
install.packages("caret")

library(forestr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(GGally)
library(knitr)
library(grDevices)
library(plotly)
library(caret)

#### Kaggle Data ####
Random_Submission <- read.csv(file = "C:/Users/kenji/Documents/R Codes/Higgs Boson Data/random_submission.csv", header = TRUE)
Test_Higgs <- read.csv(file = "C:/Users/kenji/Documents/R Codes/Higgs Boson Data/test.csv", header = TRUE)
Training_Higgs <- read.csv(file = "C:/Users/kenji/Documents/R Codes/Higgs Boson Data/training.csv", header = TRUE)
True_Higgs <- read.csv(file = "C:/Users/kenji/Documents/R Codes/Higgs Boson Data/atlas-higgs-challenge-2014-v2.csv", header = TRUE)


devtools::install_github("njtierney/naniar")
library("naniar")

Train_H <- Training_Higgs %>% mutate(Label_01=factor(Label, levels=c("s","b"), labels=c("1","0")))

####### BASEWORK FOR THE RANDOM FOREST SPLITTING #######
# Load data as per Andeek
data("higgs_25")
data("test_higgs_25")

library(RCurl)
library(dplyr)

# data from https://github.com/ledell/h2oEnsemble-benchmarks
# subset of the original HIGGS.csv file
higgs_1M <- getURL("https://s3.amazonaws.com/uciml-higgs/higgs_1M.csv")

# last 500k observations of HIGGS.csv are the test set.
higgs_test <- getURL("https://s3.amazonaws.com/uciml-higgs/higgs_test.csv")
labels_higgs_test <- getURL("https://s3.amazonaws.com/uciml-higgs/labels_higgs_test.csv")

higgs_1M <- read.csv("data-raw/higgs_1M.csv")
higgs_test <- read.csv("data-raw/higgs_test.csv")

names(higgs_1M) <- c("class", "lepton_pT", "lepton_eta", "lepton_phi", "missing_energy_magnitude",
                     "missing_energy_phi", "jet_1_pt", "jet_1_eta", "jet_1_phi", "jet_1_b_tag", "jet_2_pt",
                     "jet_2_eta", "jet_2_phi", "jet_2_b_tag", "jet_3_pt", "jet_3_eta", "jet_3_phi",
                     "jet_3_b_tag", "jet_4_pt", "jet_4_eta", "jet_4_phi", "jet_4_b_tag", "m_jj", "m_jjj",
                     "m_lv", "m_jlv", "m_bb", "m_wbb", "m_wwbb")
names(higgs_test) <- names(higgs_1M)
names(labels_higgs_test) <- "class"

higgs_1M$class <- factor(higgs_1M$class)
higgs_test$class <- factor(higgs_test$class)

# create samples of varying unbalancedness
set.seed(503503) #reproducible samples
n <- 200
p <- c(.02, .05, .1, .25)


higgs_5 <- higgs_1M %>%
  filter(class == "0") %>%
  sample_n(n*(1-p[2])) %>%
  rbind(higgs_1M %>%
          filter(class == "1") %>%
          sample_n(n*(p[2])))

higgs_10 <- higgs_1M %>%
  filter(class == "0") %>%
  sample_n(n*(1-p[3])) %>%
  rbind(higgs_1M %>%
          filter(class == "1") %>%
          sample_n(n*(p[3])))

higgs_25 <- higgs_1M %>%
  filter(class == "0") %>%
  sample_n(n*(1-p[4])) %>%
  rbind(higgs_1M %>%
          filter(class == "1") %>%
          sample_n(n*(p[4])))

test_higgs_5 <- higgs_test %>%
  filter(class == "0") %>%
  sample_n(n*(1-p[2])) %>%
  rbind(higgs_test %>%
          filter(class == "1") %>%
          sample_n(n*(p[2])))

test_higgs_10 <- higgs_test %>%
  filter(class == "0") %>%
  sample_n(n*(1-p[3])) %>%
  rbind(higgs_test %>%
          filter(class == "1") %>%
          sample_n(n*(p[3])))

test_higgs_25 <- higgs_test %>%
  filter(class == "0") %>%
  sample_n(n*(1-p[4])) %>%
  rbind(higgs_test %>%
          filter(class == "1") %>%
          sample_n(n*(p[4])))

devtools::use_data(higgs_5, higgs_10, higgs_25, test_higgs_5, test_higgs_10, test_higgs_25, overwrite = TRUE)

# Plot
ggpairs(higgs_25, columns = 2:22, columnLabels = 1:21, axisLabels = "none", mapping = ggplot2::aes(colour = class, alpha = 0.5), upper = list(continuous = 'blank')) # add transparency value alpha = 0.5 to mapping function if needed

# Tain forrest
rf_gini_25 <- forestr(class ~ ., data = higgs_25 %>% select(-starts_with("m_")))                                                      
rf_info_25 <- forestr(class ~ ., data = higgs_25 %>% select(-starts_with("m_")), parms = list(split = "information"))
rf_extr_25 <- forestr(class ~ ., data = higgs_25 %>% select(-starts_with("m_")), method = "extremes", parm = list(classOfInterest = "1"))
rf_puri_25 <- forestr(class ~ ., data = higgs_25 %>% select(-starts_with("m_")), method = "purity")

# Test four forests
rf_gini.pred_25 <- predict(rf_gini_25, test_higgs_25 %>% select(-starts_with("m_")))
rf_info.pred_25 <- predict(rf_info_25, test_higgs_25 %>% select(-starts_with("m_")))
rf_extr.pred_25 <- predict(rf_extr_25, test_higgs_25 %>% select(-starts_with("m_")))
rf_puri.pred_25 <- predict(rf_puri_25, test_higgs_25 %>% select(-starts_with("m_")))

#Importance
rf_gini.imp_25 <- importance(rf_gini_25)
rf_info.imp_25 <- importance(rf_info_25)
rf_extr.imp_25 <- importance(rf_extr_25)
rf_puri.imp_25 <- importance(rf_puri_25)

