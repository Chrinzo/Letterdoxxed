# Letterboxd Network Analysis Project
# University Project - R Implementation

# Load required libraries
library(igraph)
library(ggplot2)
library(stats)

# Set working directory and load data
cat("Loading datasets...\n")
movie_data <- read.csv("movie_data.csv", stringsAsFactors = FALSE)
rating_export <- read.csv("rating_export.csv", stringsAsFactors = FALSE)
user_export <- read.csv("user_export.csv", stringsAsFactors = FALSE)

# Data preprocessing and cleaning
cat("Preprocessing data...\n")

# Clean movie data
movie_data <- movie_data[!is.na(movie_data$movie_id) & !is.na(movie_data$movie_title), ]
movie_data$movie_title <- trimws(movie_data$movie_title)
movie_data$popularity <- as.numeric(movie_data$popularity)

# Clean rating data
rating_export <- rating_export[!is.na(rating_export$movie_id) & 
                                 !is.na(rating_export$user_id) & 
                                 !is.na(rating_export$rating_val), ]
rating_export <- rating_export[rating_export$rating_val >= 0.5 & 
                                 rating_export$rating_val <= 5.0, ]

# Clean user data
user_export <- user_export[!is.na(user_export$user_id), ]
user_export$display_name <- trimws(user_export$display_name)

# Print data summaries
cat("Dataset summaries:\n")
cat("Movies:", nrow(movie_data), "\n")
cat("Ratings:", nrow(rating_export), "\n")
cat("Users:", nrow(user_export), "\n")

# === PART 1: PREPARE DATA FOR NETWORK ANALYSIS ===

cat("Preparing network data...\n")

# Calculate movie statistics
movie_stats <- aggregate(rating_export[, c("rating_val")], 
                         by = list(movie_id = rating_export$movie_id), 
                         FUN = function(x) c(count = length(x), avg_rating = mean(x)))
movie_stats <- data.frame(movie_id = movie_stats$movie_id, 
                          rating_count = movie_stats$x[,1], 
                          avg_rating = movie_stats$x[,2])

# Select top movies by number of ratings (for computational efficiency)
top_movies <- movie_stats[order(movie_stats$rating_count, decreasing = TRUE), ][1:80, ]
top_movies <- merge(top_movies, movie_data, by = "movie_id", all.x = TRUE)

# Select active users who rated many of these popular movies
user_activity <- aggregate(rating_export$movie_id, 
                           by = list(user_id = rating_export$user_id), 
                           FUN = length)
names(user_activity)[2] <- "movies_rated"
active_users <- user_activity[user_activity$movies_rated >= 5, ]
active_users <- active_users[order(active_users$movies_rated, decreasing = TRUE), ][1:150, ]

# Create filtered dataset for network analysis
network_data <- rating_export[rating_export$movie_id %in% top_movies$movie_id & 
                                rating_export$user_id %in% active_users$user_id, ]

cat("Network data contains", nrow(network_data), "ratings\n")

# === PART 2: CREATE MOVIE SIMILARITY NETWORK ===

cat("Creating movie similarity network...\n")

# Get unique movies and users in our network
unique_movies <- unique(network_data$movie_id)
unique_users <- unique(network_data$user_id)

# Create user-movie rating matrix
rating_matrix <- matrix(NA, nrow = length(unique_users), ncol = length(unique_movies))
rownames(rating_matrix) <- unique_users
colnames(rating_matrix) <- unique_movies

# Fill the matrix
for(i in 1:nrow(network_data)) {
  user_idx <- which(unique_users == network_data$user_id[i])
  movie_idx <- which(unique_movies == network_data$movie_id[i])
  rating_matrix[user_idx, movie_idx] <- network_data$rating_val[i]
}

# Calculate movie-movie correlation matrix
movie_similarity <- cor(rating_matrix, use = "pairwise.complete.obs")
movie_similarity[is.na(movie_similarity)] <- 0

# Create edge list for movie network (only keep strong similarities)
similarity_threshold <- 0.4
movie_edges <- c()
edge_weights <- c()

for(i in 1:(ncol(movie_similarity)-1)) {
  for(j in (i+1):ncol(movie_similarity)) {
    if(movie_similarity[i,j] > similarity_threshold) {
      movie_edges <- c(movie_edges, colnames(movie_similarity)[i], colnames(movie_similarity)[j])
      edge_weights <- c(edge_weights, movie_similarity[i,j])
    }
  }
}

# Create edge matrix
edge_matrix <- matrix(movie_edges, ncol = 2, byrow = TRUE)

# Create igraph object
movie_graph <- graph_from_edgelist(edge_matrix, directed = FALSE)
E(movie_graph)$weight <- edge_weights

# Add node attributes
movie_nodes <- top_movies[top_movies$movie_id %in% V(movie_graph)$name, ]
movie_nodes <- movie_nodes[match(V(movie_graph)$name, movie_nodes$movie_id), ]

V(movie_graph)$title <- movie_nodes$movie_title
V(movie_graph)$rating_count <- movie_nodes$rating_count
V(movie_graph)$avg_rating <- movie_nodes$avg_rating
V(movie_graph)$popularity <- movie_nodes$popularity

# === PART 3: NETWORK VISUALIZATION ===

cat("Creating network visualizations...\n")

# Set node colors based on average rating (using base R colors)
rating_colors <- rainbow(5, start = 0, end = 0.8)
V(movie_graph)$color <- rating_colors[cut(V(movie_graph)$avg_rating, 
                                          breaks = 5, labels = FALSE)]

# Set node sizes based on number of ratings
V(movie_graph)$size <- pmax(3, pmin(15, log(V(movie_graph)$rating_count) * 2))

# Set edge width based on similarity strength
E(movie_graph)$width <- E(movie_graph)$weight * 3

# Create the main network plot
par(mfrow = c(1, 1), mar = c(1, 1, 3, 1))
plot(movie_graph, 
     layout = layout_with_fr(movie_graph),
     vertex.label = ifelse(V(movie_graph)$rating_count > quantile(V(movie_graph)$rating_count, 0.85), 
                           substr(V(movie_graph)$title, 1, 15), ""),
     vertex.label.cex = 0.7,
     vertex.label.color = "black",
     vertex.label.dist = 1.5,
     edge.color = "gray70",
     main = "Letterboxd Movie Network\n(Nodes = Movies, Edges = Rating Similarity)",
     sub = "Node size = popularity, Color = avg rating")

# Add legend for colors
legend("bottomright", 
       legend = c("Low Rating", "Below Avg", "Average", "Above Avg", "High Rating"),
       col = rating_colors, 
       pch = 19, 
       title = "Avg Rating", 
       cex = 0.8)

# === PART 4: NETWORK ANALYSIS ALGORITHMS ===

cat("Performing network analysis...\n")

# 1. Basic Network Properties
cat("\n=== NETWORK PROPERTIES ===\n")
cat("Number of nodes (movies):", vcount(movie_graph), "\n")
cat("Number of edges:", ecount(movie_graph), "\n")
cat("Network density:", round(edge_density(movie_graph), 4), "\n")
cat("Average path length:", round(average.path.length(movie_graph), 2), "\n")
cat("Clustering coefficient:", round(transitivity(movie_graph), 3), "\n")

# 2. Community Detection
communities <- cluster_louvain(movie_graph)
cat("Number of communities found:", length(communities), "\n")

# Visualize communities
par(mfrow = c(1, 2), mar = c(1, 1, 3, 1))

# Original network
plot(movie_graph, 
     layout = layout_with_fr(movie_graph),
     vertex.label = "",
     vertex.size = V(movie_graph)$size,
     vertex.color = V(movie_graph)$color,
     edge.width = E(movie_graph)$width,
     edge.color = "gray70",
     main = "Original Network\n(Color = Avg Rating)")

# Community network
community_colors <- rainbow(length(communities))
V(movie_graph)$community_color <- community_colors[membership(communities)]

plot(movie_graph, 
     layout = layout_with_fr(movie_graph),
     vertex.label = "",
     vertex.size = V(movie_graph)$size,
     vertex.color = V(movie_graph)$community_color,
     edge.width = E(movie_graph)$width,
     edge.color = "gray70",
     main = "Community Structure\n(Louvain Algorithm)")

# 3. Centrality Measures
betweenness_centrality <- betweenness(movie_graph, normalized = TRUE)
closeness_centrality <- closeness(movie_graph, normalized = TRUE)
eigenvector_centrality <- eigen_centrality(movie_graph)$vector
pagerank_centrality <- page_rank(movie_graph)$vector

# Create centrality results dataframe
centrality_results <- data.frame(
  movie_id = V(movie_graph)$name,
  movie_title = V(movie_graph)$title,
  avg_rating = V(movie_graph)$avg_rating,
  rating_count = V(movie_graph)$rating_count,
  betweenness = betweenness_centrality,
  closeness = closeness_centrality,
  eigenvector = eigenvector_centrality,
  pagerank = pagerank_centrality,
  community = membership(communities)
)

# Show top movies by different centrality measures
cat("\n=== TOP MOVIES BY CENTRALITY ===\n")
cat("Top 5 by Betweenness Centrality:\n")
top_betweenness <- centrality_results[order(centrality_results$betweenness, decreasing = TRUE), ][1:5, ]
print(top_betweenness[, c("movie_title", "betweenness", "avg_rating")])

cat("\nTop 5 by PageRank:\n")
top_pagerank <- centrality_results[order(centrality_results$pagerank, decreasing = TRUE), ][1:5, ]
print(top_pagerank[, c("movie_title", "pagerank", "avg_rating")])

# === PART 5: STATISTICAL ANALYSIS WITH GGPLOT2 ===

cat("\nCreating statistical visualizations with ggplot2...\n")

# 1. Rating Distribution
rating_dist_df <- data.frame(table(rating_export$rating_val))
names(rating_dist_df) <- c("Rating", "Frequency")
rating_dist_df$Rating <- as.numeric(as.character(rating_dist_df$Rating))

p1 <- ggplot(rating_dist_df, aes(x = Rating, y = Frequency)) +
  geom_col(fill = "steelblue", alpha = 0.7) +
  geom_text(aes(label = Frequency), vjust = -0.5, size = 3) +
  labs(title = "Distribution of Letterboxd Ratings",
       x = "Rating", 
       y = "Number of Ratings") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(0.5, 5, 0.5))

print(p1)

# 2. User Activity Distribution
user_ratings_count <- aggregate(rating_export$rating_val, 
                                by = list(user_id = rating_export$user_id), 
                                FUN = length)
names(user_ratings_count) <- c("user_id", "num_ratings")

p2 <- ggplot(user_ratings_count, aes(x = num_ratings)) +
  geom_histogram(bins = 30, fill = "lightgreen", alpha = 0.7, color = "black") +
  labs(title = "Distribution of User Activity",
       x = "Number of Ratings per User",
       y = "Number of Users") +
  theme_minimal() +
  scale_x_log10()

print(p2)

# 3. Movie Popularity vs Rating
p3 <- ggplot(movie_stats, aes(x = rating_count, y = avg_rating)) +
  geom_point(alpha = 0.6, color = "darkblue") +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(title = "Movie Popularity vs Average Rating",
       x = "Number of Ratings (log scale)",
       y = "Average Rating") +
  theme_minimal() +
  scale_x_log10()

print(p3)

# 4. Centrality Measures Comparison
centrality_long <- data.frame(
  movie_title = rep(centrality_results$movie_title, 4),
  measure = rep(c("Betweenness", "Closeness", "Eigenvector", "PageRank"), 
                each = nrow(centrality_results)),
  value = c(centrality_results$betweenness, centrality_results$closeness,
            centrality_results$eigenvector, centrality_results$pagerank),
  avg_rating = rep(centrality_results$avg_rating, 4)
)

# Show top movies for each centrality measure
top_centrality <- centrality_long[order(centrality_long$value, decreasing = TRUE), ]
top_centrality <- do.call(rbind, by(top_centrality, top_centrality$measure, function(x) head(x, 5)))

p4 <- ggplot(top_centrality, aes(x = reorder(movie_title, value), y = value, fill = measure)) +
  geom_col() +
  facet_wrap(~measure, scales = "free") +
  coord_flip() +
  labs(title = "Top 5 Movies by Centrality Measures",
       x = "Movie Title",
       y = "Centrality Score") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.y = element_text(size = 8))

print(p4)

# 5. Network Properties Visualization
network_props <- data.frame(
  Property = c("Nodes", "Edges", "Density", "Avg Path Length", "Clustering", "Communities"),
  Value = c(vcount(movie_graph), ecount(movie_graph), 
            round(edge_density(movie_graph), 4),
            round(average.path.length(movie_graph), 2),
            round(transitivity(movie_graph), 3),
            length(communities))
)

p5 <- ggplot(network_props, aes(x = Property, y = Value)) +
  geom_col(fill = "orange", alpha = 0.7) +
  geom_text(aes(label = Value), vjust = -0.5) +
  labs(title = "Network Properties Summary",
       x = "Network Property",
       y = "Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p5)

# 6. Community Size Distribution
community_sizes <- data.frame(table(membership(communities)))
names(community_sizes) <- c("Community", "Size")
community_sizes$Community <- as.numeric(as.character(community_sizes$Community))

p6 <- ggplot(community_sizes, aes(x = reorder(Community, Size), y = Size)) +
  geom_col(fill = "purple", alpha = 0.7) +
  geom_text(aes(label = Size), vjust = -0.5) +
  labs(title = "Community Size Distribution",
       x = "Community ID",
       y = "Number of Movies") +
  theme_minimal()

print(p6)

# === PART 6: SIMPLE RECOMMENDATION SYSTEM ===

cat("\n=== RECOMMENDATION SYSTEM ===\n")

# Function to get movie recommendations based on network neighbors
get_recommendations <- function(movie_title, n_recommendations = 5) {
  # Find the movie in our network
  movie_idx <- which(V(movie_graph)$title == movie_title)
  
  if(length(movie_idx) == 0) {
    cat("Movie not found in network\n")
    return(NULL)
  }
  
  # Get neighbors
  neighbors_idx <- neighbors(movie_graph, movie_idx)
  
  if(length(neighbors_idx) == 0) {
    cat("No connected movies found\n")
    return(NULL)
  }
  
  # Get neighbor information
  neighbor_info <- data.frame(
    title = V(movie_graph)$title[neighbors_idx],
    avg_rating = V(movie_graph)$avg_rating[neighbors_idx],
    rating_count = V(movie_graph)$rating_count[neighbors_idx]
  )
  
  # Sort by average rating
  neighbor_info <- neighbor_info[order(neighbor_info$avg_rating, decreasing = TRUE), ]
  
  return(head(neighbor_info, n_recommendations))
}

# Example recommendation
sample_movie <- V(movie_graph)$title[which.max(V(movie_graph)$rating_count)]
cat("Recommendations for:", sample_movie, "\n")
recommendations <- get_recommendations(sample_movie)
if(!is.null(recommendations)) {
  print(recommendations)
}

# === PART 7: EXPORT RESULTS ===

cat("\nExporting results...\n")

# Export centrality analysis
write.csv(centrality_results, "movie_centrality_analysis.csv", row.names = FALSE)

# Export network summary
write.csv(network_props, "network_summary.csv", row.names = FALSE)

# Export community information
community_info <- data.frame(
  movie_id = V(movie_graph)$name,
  movie_title = V(movie_graph)$title,
  community = membership(communities),
  avg_rating = V(movie_graph)$avg_rating,
  rating_count = V(movie_graph)$rating_count
)
write.csv(community_info, "movie_communities.csv", row.names = FALSE)

cat("\n=== PROJECT COMPLETE ===\n")
cat("Files created:\n")
cat("- movie_centrality_analysis.csv: Centrality measures for each movie\n")
cat("- network_summary.csv: Overall network properties\n")
cat("- movie_communities.csv: Community assignments for movies\n")
cat("\nNetwork analysis complete!\n")

