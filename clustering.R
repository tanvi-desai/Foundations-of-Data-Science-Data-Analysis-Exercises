# This mini-project is based on the K-Means exercise from 'R in Action'
# Go here for the original blog post and solutions
# http://www.r-bloggers.com/k-means-clustering-from-r-in-action/

# Exercise 0: Install these packages if you don't have them already

# install.packages(c("cluster", "rattle","NbClust"))

# Now load the data and look at the first few rows
data(wine, package="rattle")
head(wine)

# Exercise 1: Remove the first column from the data and scale
# it using the scale() function
wine1 <- scale(wine[-1])


# Now we'd like to cluster the data using K-Means. 
# How do we decide how many clusters to use if you don't know that already?
# We'll try two methods.

# Method 1: A plot of the total within-groups sums of squares against the 
# number of clusters in a K-means solution can be helpful. A bend in the 
# graph can suggest the appropriate number of clusters. 

wssplot <- function(wine1, nc=15, seed=1234){
	              wss <- (nrow(wine1)-1)*sum(apply(wine1,2,var))
               	      for (i in 2:nc){
		        set.seed(seed)
	                wss[i] <- sum(kmeans(wine1, centers=i)$withinss)}
	                
		      plot(1:nc, wss, type="b", xlab="Number of Clusters",
	                        ylab="Within groups sum of squares")
	   }

wssplot(wine1)

# Exercise 2:
#   * How many clusters does this method suggest?
##       This method suggests 3 clusters.

#   * Why does this method work? What's the intuition behind it?
##       This method works because it chooses a small k value with a small SSE, which is represented by the 'elbow'. The intuition behind it is that SSE decreases toward 0 as we increase k and when SSE is 0, k will be equal to the number of data points in the dataset. In this case, each data point becomes its own cluster, making the error negligible.
##       Therefore, by this method we can choose a small k with a low SSE value.
#   * Look at the code for wssplot() and figure out how it works

# Method 2: Use the NbClust library, which runs many experiments
# and gives a distribution of potential number of clusters.

library(NbClust)
set.seed(1234)
nc <- NbClust(wine1, min.nc=2, max.nc=15, method="kmeans")
barplot(table(nc$Best.n[1,]),
	          xlab="Numer of Clusters", ylab="Number of Criteria",
		            main="Number of Clusters Chosen by 26 Criteria")


# Exercise 3: How many clusters does this method suggest?
##            This method suggests 3 clusters.


# Exercise 4: Once you've picked the number of clusters, run k-means 
# using this number of clusters. Output the result of calling kmeans()
# into a variable fit.km
fit.km <- kmeans(wine1, 3, nstart = 25)
fit.km

# Now we want to evaluate how well this clustering does.

# Exercise 5: using the table() function, show how the clusters in fit.km$clusters
# compares to the actual wine types in wine$Type. Would you consider this a good
# clustering?
clus_table <- table(fit.km$cluster, wine$Type)
##    We would consider this a good clustering because, for the most part, there is agreement between the clusters and the types of wine. 

# Exercise 6:
# * Visualize these clusters using  function clusplot() from the cluster library
# * Would you consider this a good clustering?

library (cluster)
clus <- as.data.frame(clus_table)
clusplot(wine1, clus = fit.km$cluster)

## We would consider this a good clustering.
