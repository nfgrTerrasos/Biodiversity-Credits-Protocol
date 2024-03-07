library(landscapemetrics)
library(terra)
library(raster)
library(hrbrthemes)
library(viridis)
library(ggsci)
library(ggpubr)
library(sp)
library(sf)
library(plotrix)

landscape = raster(file.choose()) #Import the Raster file

check_landscape(landscape) #Check the topology of the Raster file

metric.names = lsm_abbreviations_names #Review the avaiable information for landscape analysis

metric.names
tail(metric.names)
view(metric.names)
metric.total = calculate_lsm(landscape,  #raster file
                             what = c("lsm_p_core", #Patch area core
                                      "lsm_c_cpland", #Core Area Percent of Landscape
                                      "lsm_l_cohesion", # Landscape cohesion
                                      "lsm_c_np", #Number of patches
                                      "lsm_l_np", #Fragmentation of the landscape
                                      "lsm_l_pd")) ##Normalize fragmentation of the landscape
metric.total
names(metric.total)

unique(metric.total[,"class"]) #Check the parameters are included in the matrix

fix(metric.total) #Add Variable related to natural and anthropized land cover
natural.metric.total = subset(metric.total, Tipo == "Natural") #Analyzed based on natural landscape

patch = subset(natural.metric.total, level=="patch") #Subset with the Type: Patch
min.patch = min(patch$value)
max.patch = max(patch$value)
mean.patch = mean(patch$value)

FCI.patch = (((mean.patch-min.patch)/(max.patch-min.patch))*100)# Value for the Type: Patch in landscape analysis

class = subset(natural.metric.total, level=="class")#Subset with the Type: Class
cpland.class = subset(class, metric=="cpland")
min.class = min(cpland.class$value)
max.class = max(cpland.class$value)
mean.class = mean(cpland.class$value)

FCJ.class = (((mean.class-min.class)/(max.class-min.class))*100) # Value for the Type: Class in landscape analysis

landscape.metric = subset(metric.total, metric=="pd") #This parameter have one value, so it comes from initial database
FCK.landscape = landscape.metric$value # Value for the Type: Landscape in landscape analysis

LH = ((0.25*FCI.patch) + (0.25*FCJ.class) + (0.50*FCK.landscape)) #Connectivity factor formula

print(LH) #Connectivity factor value


#This information may help to prepare a report about the condition on the territory

plot(landscape)
show_landscape(landscape)
show_patches(landscape, class = "all", labels = TRUE)
lsm_c_te(landscape)

moving_window = matrix(1,nrow=3, ncol=3)
result = window_lsm(landscape, window = moving_window, what = c("lsm_l_pr,", "lsm_l_np"))
plot(result[[1]]$lsm_l_np)
