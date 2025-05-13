# Install required packages if not already installed
if (!require("readxl")) install.packages("readxl")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("dplyr")) install.packages("dplyr")
if (!require("stringr")) install.packages("stringr")

library(readxl)
library(ggplot2)
library(dplyr)
library(stringr)

# Set the working directory (adjust if needed)
setwd("Desktop/Joao/")

# === 1. Load the Excel spreadsheet
raw_data <- read_excel("TableS1_newer_v2.xlsx", col_names = FALSE)

# === 2. Use the second row as the column header
new_header <- raw_data[2, ]
data <- raw_data[-c(1,2), ]
colnames(data) <- as.character(unlist(new_header))

# Check available column names
print(colnames(data))

# === 3. Extract country information from the "Institution Address" column
institution_column <- "Institution Address"  # Adjust if necessary

data <- data %>%
  mutate(Country = case_when(
    str_detect(!!sym(institution_column), regex("Brazil", ignore_case = TRUE)) ~ "Brazil",
    str_detect(!!sym(institution_column), regex("Argentina", ignore_case = TRUE)) ~ "Argentina",
    str_detect(!!sym(institution_column), regex("Chile", ignore_case = TRUE)) ~ "Chile",
    str_detect(!!sym(institution_column), regex("Colombia", ignore_case = TRUE)) ~ "Colombia",
    str_detect(!!sym(institution_column), regex("Ecuador", ignore_case = TRUE)) ~ "Ecuador",
    str_detect(!!sym(institution_column), regex("Peru", ignore_case = TRUE)) ~ "Peru",
    str_detect(!!sym(institution_column), regex("Mexico", ignore_case = TRUE)) ~ "Mexico",
    str_detect(!!sym(institution_column), regex("USA|United States", ignore_case = TRUE)) ~ "USA",
    str_detect(!!sym(institution_column), regex("Italy", ignore_case = TRUE)) ~ "Italy",
    TRUE ~ "Other"
  ))

# Preview the country classification
print(head(data[, c(institution_column, "Country")]))

# === 4. Calculate publication frequency by country
country_df <- data %>%
  count(Country) %>%
  arrange(desc(n))

# === 5. Plot Figure 1a - Top 10 countries by number of publications
ggplot(country_df[1:10, ], aes(x = reorder(Country, -n), y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  labs(title = "Top Countries by Number of Publications",
       x = "Country", y = "Number of Publications") +
  coord_flip()

ggsave("Figure_1a_Country_Publications_from_XLSX.pdf", width = 8, height = 5)

# === 6. Plot Figure 1b - Latin American countries only
latam_countries <- c("Brazil", "Argentina", "Chile", "Colombia", "Ecuador", "Peru", "Mexico")

latam_df <- country_df %>%
  filter(Country %in% latam_countries)

ggplot(latam_df, aes(x = reorder(Country, -n), y = n)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  theme_minimal() +
  labs(title = "Latin American Countries: Neotropical AMR Research",
       x = "Country", y = "Number of Publications") +
  coord_flip()

ggsave("Figure_1b_LATAM_Publications_from_XLSX.pdf", width = 8, height = 5)

# === 7. Export publication count by country
write.csv(country_df, "Country_Publication_Counts.csv", row.names = FALSE)

# === 8. Export full metadata for supplementary material
metadata <- data %>%
  select(`Authors`,
         `Article Title`,
         `Source Title`,
         `Publication Year`,
         `DOI`,
         `Document Type`,
         `Institution Address`,
         `Institution`,
         `Country`)
write.csv(metadata, "TableS1_Metadata_Complete.csv", row.names = FALSE)

# === 9. Load igraph for co-authorship network
if (!require("igraph")) install.packages("igraph")
library(igraph)

# === 10. Build co-authorship pairs assuming authors are separated by ";"
coauthor_pairs <- data.frame()

for (i in 1:nrow(data)) {
  authors_raw <- data$Authors[i]
  if (!is.na(authors_raw)) {
    authors <- unlist(strsplit(authors_raw, ";"))
    authors <- str_trim(authors)
    if (length(authors) > 1) {
      pairs <- as.data.frame(t(combn(authors, 2)))
      colnames(pairs) <- c("Author1", "Author2")
      coauthor_pairs <- rbind(coauthor_pairs, pairs)
    }
  }
}

# Rename columns
colnames(coauthor_pairs) <- c("Author1", "Author2")

# === 11. Count co-authorship frequency
coauthor_counts <- coauthor_pairs %>%
  group_by(Author1, Author2) %>%
  summarise(weight = n(), .groups = "drop")

# === 12. Build weighted undirected graph
coauthor_graph <- graph_from_data_frame(coauthor_counts, directed = FALSE)

# Check edge weight summary
summary(E(coauthor_graph)$weight)

# === 13. Plot full co-authorship network
pdf("Figure_1c_Coauthorship_Network.pdf", width = 10, height = 10)
plot(coauthor_graph,
     vertex.size = 5,
     vertex.label.cex = 0.5,
     vertex.label.color = "black",
     edge.width = E(coauthor_graph)$weight,
     layout = layout_with_fr(coauthor_graph),
     main = "Co-authorship Network")
dev.off()

# Optional: Save as PNG
png("Figure_1c_Coauthorship_Network.png", width = 1200, height = 1200, res = 200)
plot(coauthor_graph,
     vertex.size = 5,
     vertex.label.cex = 0.5,
     vertex.label.color = "black",
     edge.width = E(coauthor_graph)$weight,
     layout = layout_with_fr(coauthor_graph),
     main = "Co-authorship Network")
dev.off()

# === 14. Filter the network for clearer visualization

# Calculate author degree
author_degree <- degree(coauthor_graph)
summary(author_degree)

# Number of authors with degree ≥ 2
sum(author_degree >= 2)

# Define filtering thresholds
min_degree <- 5
min_edge_weight <- 2

# Filter edges by weight
edges_to_keep <- E(coauthor_graph)[E(coauthor_graph)$weight >= min_edge_weight]
length(edges_to_keep)

# Create subgraph with selected edges
filtered_graph <- subgraph.edges(coauthor_graph, edges_to_keep, delete.vertices = TRUE)

# Check degree distribution in the filtered graph
author_degree_filtered <- degree(filtered_graph)
summary(author_degree_filtered)
sum(author_degree_filtered >= min_degree)

# Filter nodes by degree
final_nodes <- V(filtered_graph)[degree(filtered_graph) >= min_degree]
final_subgraph <- induced_subgraph(filtered_graph, final_nodes)

# Plot the final filtered co-authorship network
if (vcount(final_subgraph) == 0) {
  cat("⚠ Final subgraph is empty. Try lowering the thresholds.\n")
} else {
  pdf("Figure_1c_Coauthorship_Network_Filtered.pdf", width = 10, height = 10)
  plot(final_subgraph,
       vertex.size = 6,
       vertex.label.cex = 0.6,
       vertex.label.color = "black",
       edge.width = E(final_subgraph)$weight,
       layout = layout_with_fr(final_subgraph),
       main = "Filtered Co-authorship Network")
  dev.off()
}
