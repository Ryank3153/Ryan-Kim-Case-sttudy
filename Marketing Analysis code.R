
library(dplyr)
library(ggplot2)
library(cluster)
library(factoextra)
library(tidyr)
library(gt)
library(readxl)
library(scales) 

Data360 <- read_excel(file.choose())

data_360_scaled <- scale(Data360)


library(dplyr)
library(ggplot2)
library(cluster)
library(factoextra)
library(tidyr)
library(gt)
library(readxl)
library(scales)

Data360 <- read_excel(file.choose())


data_360_scaled <- scale(Data360)


# APPENDIX 1


# 1. Scree Plot 
fviz_nbclust(data_360_scaled, kmeans, method = "wss", linecolor = "#2A363B") +
  geom_vline(xintercept = 3, linetype = "dashed", color = "#E84A5F", linewidth = 1.2) +
  theme_minimal() +
  labs(title = "Figure 2: Optimal Number of Clusters (Scree Plot)",
       subtitle = "The 'elbow' at k=3 represents the optimal balance between model complexity and variance.",
       x = "Number of Clusters (k)",
       y = "Total Within Sum of Squares (WSS)",
       caption = "Note: The red dashed line indicates the recommended number of segments (k=3).") +
  theme(plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(color = "darkgray", size = 11),
        plot.caption = element_text(face = "italic", color = "gray50", hjust = 0))

#1.1 Dendrogram 
dist_360 <- dist(data_360_scaled, method = "euclidean")
hc_360 <- hclust(dist_360, method = "ward.D2")
res_hcut <- hcut(data_360_scaled, k = 3, hc_method = "ward.D2", hc_metric = "euclidean")


fviz_dend(res_hcut, k = 3, type = "circular", 
          k_colors = c("#FF6B6B", "#4ECDC4", "#45B7D1"),
          show_labels = FALSE, 
          main = "Figure 3: Market Segmentation Dendrogram (360buy)") +
  labs(subtitle = "What does this graph show?\nThis dendrogram groups 1,000 customers based on their behavioral similarities.\n\nWhat do the colors mean?\nThe tree is cut into 3 distinct branches:\n[ Red ] Cluster 1  |  [ Mint ] Cluster 2  |  [ Blue ] Cluster 3",
       caption = "Method: Hierarchical clustering using Euclidean distance and Ward.D2 linkage.") +
  theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        plot.subtitle = element_text(color = "#4A4A4A", size = 10, hjust = 0.5, face = "bold"),
        plot.caption = element_text(face = "italic", color = "gray50", hjust = 1))
# APPENDIX 2: Heatmap-Styled Tables 

segments_mean <- Data360_final %>%
  group_by(Cluster) %>%
  summarise(across(everything(), mean))


segments_mean %>%
  gt() %>%
  tab_header(
    title = md('**Cluster Profiles: Mean Values**'),
    subtitle = "360buy Customer Segmentation"
  ) %>%
  fmt_number(columns = everything(), decimals = 2) %>%
  data_color(
    columns = -Cluster,
    colors = scales::col_numeric(palette = c("#F7FBFF", "#08306B"), domain = NULL)
  )


# APPENDIX 3: Violin Plot

Data360_Demographics <- Data360_final %>%
  mutate(
    CusGen = factor(CusGen, levels = c(0, 1), labels = c("Male", "Female")),
    LevEdn = factor(LevEdn, levels = c(1, 2, 3), 
                    labels = c("Undergrad", "Master's", "Ph.D."))
  )


ggplot(Data360_Demographics, aes(x = CusGen, y = CusAgeYr, fill = LevEdn)) +
  geom_violin(trim = FALSE, alpha = 0.7) +
  geom_boxplot(width = 0.1, position = position_dodge(0.9), fill = "white", alpha = 0.5) +
  theme_minimal() +
  labs(title = "Age Density by Gender and Education",
       subtitle = "Violin Plot Analysis",
       x = "Gender", y = "Age (Years)", fill = "Education") +
  scale_fill_manual(values = c("#FFD166", "#06D6A0", "#118AB2"))



# APPENDIX 4: Cluster Characteristics HEATMAP

long_mean <- segments_mean %>%
  pivot_longer(cols = -Cluster, names_to = "Variable", values_to = "Mean_Score") %>%
  # Exclude demographics to focus strictly on shopping behaviors
  filter(!Variable %in% c("CusAgeYr", "CusGen", "LevEdn", "LevIncome"))


ggplot(long_mean, aes(x = Cluster, y = Variable, fill = Mean_Score)) +
  geom_tile(color = "white", size = 1) +
  geom_text(aes(label = round(Mean_Score, 2)), color = "black", fontface = "bold") +
  scale_fill_gradient(low = "#E0F7FA", high = "#006064") +
  theme_minimal() +
  labs(title = "Behavioral Heatmap of Customer Segments",
       subtitle = "Darker colors indicate higher importance/preference",
       x = "Customer Segment", y = "Shopping Behaviors & Preferences", fill = "Mean Score")



# APPENDIX 5: Modern Donut Chart 


cluster_sizes <- Data360_final %>%
  group_by(Cluster) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  mutate(Percentage = Count / sum(Count),
         ymax = cumsum(Percentage),
         ymin = c(0, head(ymax, n = -1)),
         Label = paste0("Cluster ", Cluster, "\n", percent(Percentage, accuracy = 0.1)))

ggplot(cluster_sizes, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = Cluster)) +
  geom_rect() +
  coord_polar(theta = "y") +
  geom_text(aes(x = 3.5, y = (ymin + ymax)/2, label = Label), 
            color = "white", fontface = "bold", size = 4) +
  theme_void() +
  xlim(c(2, 4)) + # This creates the "hole" in the middle
  labs(title = "Market Size Distribution (Donut Chart)") +
  scale_fill_manual(values = c("#2A363B", "#E84A5F", "#99B898")) +
  theme(legend.position = "none")



# APPENDIX 6: 100% Stacked Bar Chart 

ggplot(Data360_final, aes(x = Cluster, fill = as.factor(LevIncome))) +
  geom_bar(position = "fill", color = "white") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  labs(title = "Income Level Composition by Segment",
       subtitle = "100% Stacked Bar Chart",
       x = "Customer Segment", y = "Proportion of Customers", fill = "Income Level") +
  scale_fill_brewer(palette = "Blues")
