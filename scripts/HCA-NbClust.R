# -----------------------------------------------------------
# Project: The CoRonavIruS Health Impact Survey (CRISIS) – Adapted for Autism and Related Neurodevelopmental conditions (AFAR)
# hclust for Hierarchical Clustering Analysis (HCA) and NbClust to determine optimal cluster solution with dendrogram display 
# Date: June 30, 2021
# Author: Patricia Segura
# -----------------------------------------------------------

# load required libraries
library('dplyr')
library('gplots')
library('corrplot')
library('NbClust')
library('RColorBrewer')
library('dplyr')
library('ggdendro')
library('dendextend')


# Load the data (column-wise :  n features for HCA; row-wise : n subject data)
df<- read.csv("~/Desktop/CRISISAFAR_All_sample_completeobs.csv")
# convert to standardized z scores 
df2 <- as.data.frame(scale(df))



## HCA ##
# calculate Euclidean distances
d <- dist(df2, method = "euclidean")
# using Ward method
fit_d <- hclust(d, method="ward.D2")


## NbClust computation ##
nc <- NbClust(data = df2, 
              diss = NULL, 
              distance = "euclidean", 
              
              # (2 < # clusters < 15) 
              min.nc = 2, max.nc = 15, 
              
              # uses ward.D2
              method = "ward.D2"
              
)

ncTable <- as.data.frame(nc$Best.nc[1,])
names(ncTable) <- "Euclidean"
ncTable <- tibble::rownames_to_column(ncTable, "indexes")

# display NbClust results
barplot(table(nc$Best.nc[1,]), 
        xlab="Clusters (#)",ylab="Indexes (#)",
        col="#69b3a2", border="black",
        las=2 
)
title("Number of clusters chosen by 26 criteria", adj = 0, line = 0)
# save table in .txt file
write.table(ncTable, '~/Desktop/NbClust_optimal_number_of_clusters.txt', sep = " ", row.names = F)


## Display as dendrogram ##
n.solution <- max(nc$Best.partition)
cluster_grp <- cutree(fit_d, k=n.solution) # cut tree into cluster optimal solution
cluster_col <- brewer.pal(n=n.solution, name="Set1")

dend <-  as.dendrogram(fit_d) 
dend2 <- dend %>%
  set("branches_lwd", .6) %>% 
  set("branches_k_color", cluster_col, k = 4) %>%
  set("labels_colors", cluster_col, k = 4) %>%
  set("labels_cex", 0) 

gg <- as.ggdend(dend2)
g <- ggplot(gg, horiz = F, theme =  theme_classic(base_size=15)) + 
  
  theme(
    
    panel.grid = element_blank(),
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(), 
        axis.line = element_line(colour = 'black', size = 1.0), 
        axis.ticks = element_line(colour = "black", size = 1.0),
        axis.text=element_text(size=20)
        
        ) + 
  
  labs(
    
  title = "HCA  - Optimal cluster solution",
  subtitle = "ward.D2 method &  euclid. dist",
  caption = "Source: CRISIS-AFAR dataset",
  x = "Number of points",y="Height"
  
) 

g


## Include optimal solution column in original dataset ##
clust_d <- df %>% mutate(clusterID_Euc = cluster_grp)

# frequency per cluster group
table(clust_d$clusterID)



