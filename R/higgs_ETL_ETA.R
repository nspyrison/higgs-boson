# Nicholas SPyrison
# 26 Feb 2018
# initial explore of higgs boson data.
# 11/05/2018 cleaned up

#see browseURL("https://www.kaggle.com/c/higgs-boson/discussion/8216")
#see browseURL("http://opendata.cern.ch/record/328")

library(dplyr)

train = read.csv("./data/training.csv")  #load training data, 250,000 x 33

train <- as_tibble(train)
train[train == -999] <- NA  # set NAs
train$Label <- as.numeric(train$Label) - 1  #Label to 0,1. b = 0, s = 1
train_clean <- na.omit(train)  # from 250,000, to 68,114. 27.25% row-wise complete.
naniar::miss_var_summary(train)

##write.csv(omit_train, file = "./data/training_naOmitterd.csv") #export omitted data.
#save(train_clean, file="./data/training_clean.rda")
load("./data/training_clean.rda")

#train_y = train[,32:33]  #weight and label
DER_clean <- train_clean[,c(1:14,32:33)]
PRI_clean <- train_clean[,c(1,15:33)]

#sample 1000
DER_samp <- DER_clean[sample(nrow(DER_clean), 1000), ]
GGally::ggpairs(DER_samp[,2:14])

PRI_samp <- PRI_clean[sample(nrow(PRI_clean), 1000), ]
GGally::ggpairs(PRI_samp[,2:18])


### Jet 2 has a much higher rate of signal compared with Jet 3!
aggregate(PRI_samp$Label ~ PRI_samp$PRI_jet_num, FUN = mean)
# Jet 2: 53.27% signal, Jet 3: 32.03% signal.
library(ggplot2)
ggplot(PRI_samp, aes(Label, Weight)) + geom_point(stat="identity", alpha=1/20)
  #weight is higher for the more likely case, 




### Tour
library(tourr)
library(MASS)


data <- PRI_samp[1:500, 2:18] #no label, weights
col <- ifelse(PRI_samp$Label == 1, 'red', 'black')
pch <- PRI_samp$Label + 16
  # black = background, red = signal.

## to slow to animate, create the tour and save to file.
#animate_xy(data, guided_tour(index = holes), col=col, pch=pch)
#animate_xy(data, guided_tour(index = holes), col=col, pch=pch)
##animate_density2d(tour, guided_tour(index = holes), col=pal, pch=3)

### tour output
holes_tour <-
  save_history(data, guided_tour(index = holes), max_bases = 50)
render(
  data,
  planned_tour(holes_tour),
  display_xy(col = col, pch = pch),
  frames = 50,
  "pdf",
  "./tour_output/higgs_holestour.pdf",
  width = 4,
  height = 4
)

render(
  data,
  planned_tour(holes_tour),
  display_xy(col = col, pch = pch),
  frames = 50,
  "png",
  "./tour_output/higgs_holesend.png",
  width = 600,
  height = 600
)
