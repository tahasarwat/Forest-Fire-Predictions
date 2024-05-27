# Install and load required packages
install.packages(c("readxl", "e1071", "corrplot"))
library(readxl)
library(e1071)

# Read the Excel file
file_path <- "E:/Fiverr Project/Bejaia Region Dataset.xlsx"
data <- read_excel(file_path)

# Assuming your last column (12th column) contains the classes (not fire and fire), you can extract this column and then remove it from the dataset before clustering
classes <- data[, 12]
data <- data[, -12]  # Remove the last column (classes) from the dataset

# Perform C-Means Clustering
result <- cmeans(data, centers = 2, m = 2, verbose = TRUE)

# View the cluster centers
print(result$centers)

# View the membership matrix
print(result$membership)

# Set up smaller plot margins
par(mar = c(3, 3, 2, 1))  # c(bottom, left, top, right)

# Open a new plot window with a specific size
windows(width = 8, height = 6)

# Plot the clusters
plot(data, col = result$cluster, main = "C-Means Clustering", pch = 20)

# Add points for cluster centers
points(result$centers, col = 1:2, pch = 8, cex = 2)

# Add legend
legend("topright", legend = c("fire", "not fire"), col = 1:2, pch = 10, cex = 1)


library(cluster)

# Calculate Silhouette Score using fuzzy memberships manually
n <- nrow(data)
silhouette_widths <- numeric(n)

for (i in 1:n) {
  # Calculate a(i) (average distance of point i to points in its own cluster)
  a_i <- sum(result$membership[i, ]^2 * result$distance[i, ]^2) / sum(result$membership[i, ]^2)
  
  # Calculate b(i) (average distance of point i to points in nearest cluster)
  b_i <- min(colSums((result$centers - data[i, ])^2))
  
  # Calculate silhouette width for point i
  silhouette_widths[i] <- (b_i - a_i) / max(a_i, b_i)
}

# Calculate mean silhouette width
mean_silhouette_width <- mean(silhouette_widths)

# Print Silhouette Score
print(paste("Silhouette Score:", mean_silhouette_width))

# Plot Silhouette Histogram
hist(silhouette_widths, breaks = 20, main = "Silhouette Histogram for C-Means Clustering")

silhouette_widths
n
