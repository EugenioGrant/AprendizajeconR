#...............................................................
# Code:     Machine Learning with R 
# Obj:      Unsupervised Learning: Clustering 
# author:   E. Grant
#...............................................................

#...............................................................
# 1. Working directory and libraries  ----
#...............................................................

# Define Path
setwd("C:/Users/e.grant.SKIM/Google Drive/SKIM/05. Tools/40. R Kmeans") # Change it!

# Needed Packages    - Note: Missing libraries can be installed with command "Install.Package("packagename")"
library(tidyverse)   # Tidiverse: Packages for data manipulation, exploration and visualization.
library(NbClust)     # NbClust: A Package providing 30 indices for determining the best number of clusters.
library(factoextra)  # factoextra: extract and visualize the output of multivariate data analyses.
library(cluster)     # Methods: for Cluster analysis
library(gmodels)     # Crosstabulations modeled after PROC FREQ in SAS or CROSSTABS in SPSS

#...............................................................
# 2. Data Pre Processing ----
#...............................................................

# Read our data file
actividades_tbl <- read.csv("activities_db.csv", na = "")

# Remove any missing value 
actividades_tbl <- na.omit(actividades_tbl)

# Glimpse the data set
glimpse(actividades_tbl)

# View the firt 5 rows
head(actividades_tbl)

# Explore our dataset key variables
table(actividades_tbl$sexo, useNA = "ifany")
prop.table(table(actividades_tbl$sexo, useNA = "ifany"))
prop.table(table(actividades_tbl$edad, useNA = "ifany"))
prop.table(table(actividades_tbl$nse, useNA = "ifany"))
summary(actividades_tbl$nse)

#...............................................................
# 3. Training a model on the data ----
#...............................................................

# Keep vector of activities
vector <- actividades_tbl %>% select(-id, -sexo, -edad, -nse) # set rownames
vector <- as_tibble(vector)

# For K-means values matters so we scale variables (also getting postives & negatives)
vector_z <- as.data.frame(lapply(vector, scale))

# Compute and visualize k-means clustering
#? kmeans
set.seed(86)

# Let's test 2 groups
activities_clusters <- kmeans(vector_z, 2, nstart = 500)
fviz_cluster(activities_clusters, data = vector_z, ellipse.type = "convex")+ theme_minimal()
activities_clusters$size

# Let's test 3 groups
activities_clusters <- kmeans(vector_z, 3, nstart = 500)
fviz_cluster(activities_clusters, data = vector_z, ellipse.type = "convex")+ theme_minimal()
activities_clusters$size

#...............................................................
# 4. We need to define the k number of clusters ----
#...............................................................

# Elbow method
# the basic idea behind cluster partitioning methods, such as k-means clustering, 
# is to define clusters such that the total intra-cluster variation 
# (known as total within-cluster variation or total within-cluster sum of square) is minimized
fviz_nbclust(vector_z, kmeans, method = "wss") +
  geom_vline(xintercept = 5, linetype = 5)+
  labs(subtitle = "Elbow method")

# Silhouette method
# In short, the average silhouette approach measures the quality of a clustering. 
# That is, it determines how well each object lies within its cluster. 
# A high average silhouette width indicates a good clustering.
fviz_nbclust(vector_z, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

# This can take a while
# 30 indices for determining the number of clusters and proposes to user the best clustering scheme 
# from the different results obtained by varying all combinations of number of clusters, distance measures, 
# and clustering methods.  https://www.rdocumentation.org/packages/NbClust/versions/3.0/topics/NbClust

nc <- NbClust(vector_z, distance="euclidean", min.nc=2, max.nc=5, method="kmeans")

# We now have a k let's cluster again
activities_clusters <- kmeans(vector_z, 4, nstart = 500)
fviz_cluster(activities_clusters, data = vector_z, ellipse.type = "convex") + theme_minimal()
activities_clusters$size

# Export Centers
activities_clusters$centers
centers_tbl <- as.data.frame(activities_clusters$centers)
write_csv(centers_tbl,"centers_tbl.csv",na = "")

# Put the cluster into our df
actividades_tbl$cluster <-  activities_clusters$cluster
write.csv(actividades_tbl,"actividades_tbl_clust.csv", na ="")
table(actividades_tbl$cluster)

# Evalute Results
#?CrossTable
CrossTable(actividades_tbl$sexo, actividades_tbl$cluster,digits = 1, 
          prop.t = FALSE, prop.r = FALSE, prop.chisq = FALSE, format="SPSS")

CrossTable(actividades_tbl$edad, actividades_tbl$cluster,digits = 1, 
           prop.t = FALSE, prop.r = FALSE, prop.chisq = FALSE, format="SPSS")

CrossTable(actividades_tbl$nse, actividades_tbl$cluster,digits = 1, 
           prop.t = FALSE, prop.r = FALSE, prop.chisq = FALSE, format="SPSS")

