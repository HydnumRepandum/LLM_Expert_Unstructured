## ----setup, include=FALSE------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ------------------------------------------------------------------------------------------------
library(ggplot2)
library(reshape2)
library(DescTools)
library(readxl)
library(dplyr)
library(tidyr)


## ------------------------------------------------------------------------------------------------

similarity<- read_excel("C:/Users/edgar/Desktop/assistanat/code pour la similarité/2023 llm.xlsx")



## ------------------------------------------------------------------------------------------------
summary(similarity)


## ------------------------------------------------------------------------------------------------
  
char_lengths <- nchar(similarity$Summary)

  
mean_length <- mean(char_lengths)

  
median_length <- median(char_lengths)

  
cat("Longueur moyenne des caractères:", mean_length, "\n")
cat("Longueur médiane des caractères:", median_length, "\n")


## ------------------------------------------------------------------------------------------------
similarity$agrement_embedding <- ifelse(similarity$Similarite_Cosinus_llama_human >= 0.75, 1, ifelse(similarity$Similarite_Cosinus_llama_human >= 0.55 & similarity$Similarite_Cosinus_llama_human < 0.75, 3, 0))

similarity$agrement_embedding<- ifelse(similarity$agrement_embedding == 3, 2, similarity$agrement_embedding)
similarity$Aggrement_llama_human_manual<- ifelse(similarity$Aggrement_llama_human_manual == 3, 2, similarity$Aggrement_llama_human_manual)


## ------------------------------------------------------------------------------------------------
# Créez une matrice de confusion
confusion.matrix.conf_matrix <- table(similarity$agrement_embedding, similarity$Aggrement_llama_human_manual)

# Affichez la matrice de confusion
print(confusion.matrix.conf_matrix)


## ------------------------------------------------------------------------------------------------

accuracy.conf_matrix=sum(diag(confusion.matrix.conf_matrix))/sum(confusion.matrix.conf_matrix)

print(accuracy.conf_matrix) 
#
CohenKappa(confusion.matrix.conf_matrix, conf.level = 0.95)



## ------------------------------------------------------------------------------------------------

conf_matrix_melt <- melt(confusion.matrix.conf_matrix)

# Créez la heatmap avec ggplot2
ggplot(conf_matrix_melt, aes(x=Var1, y=Var2, fill=value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  labs(x = "Humain", y = "Angle", fill = "Count") +
  ggtitle("Heatmap") +
  theme_minimal()


## ----eval=FALSE, include=FALSE-------------------------------------------------------------------
## #non revoir car c'est seulement dans un cadre binaire
## library(MLmetrics)
## actual = similarity$Aggrement_llama_human_manual
## predicted = similarity$agrement_embedding
## 
## # Calculate F1_Score
## F1_Score(predicted,actual)


## ------------------------------------------------------------------------------------------------

similarity$agrement_embedding_mixtral <- ifelse(similarity$Similarite_Cosinus_mixtral_human >= 0.75, 1, ifelse(similarity$Similarite_Cosinus_mixtral_human >= 0.55 & similarity$Similarite_Cosinus_mixtral_human < 0.75, 3, 0))

similarity$agrement_embedding_mixtral<- ifelse(similarity$agrement_embedding_mixtral == 3, 2, similarity$agrement_embedding_mixtral)
similarity$Aggrement_mixtral_human_manual<- ifelse(similarity$Aggrement_mixtral_human_manual == 3, 2, similarity$Aggrement_mixtral_human_manual)



## ----eval=FALSE, include=FALSE-------------------------------------------------------------------
## similarity$agrement_embedding_mixtral <- ifelse(similarity$Similarite_Cosinus_mixtral_human >= 0.62, 1, ifelse(similarity$Similarite_Cosinus_mixtral_human >= 0.58 & similarity$Similarite_Cosinus_mixtral_human < 0.62, 3, 0))
## 
## similarity$agrement_embedding_mixtral<- ifelse(similarity$agrement_embedding_mixtral == 3, 2, similarity$agrement_embedding_mixtral)
## similarity$Aggrement_mixtral_human_manual<- ifelse(similarity$Aggrement_mixtral_human_manual == 3, 2, similarity$Aggrement_mixtral_human_manual)
## 
## 


## ------------------------------------------------------------------------------------------------
# Créez une matrice de confusion
confusion.matrix.conf_matrix <- table(similarity$agrement_embedding_mixtral, similarity$Aggrement_mixtral_human_manual)

# Affichez la matrice de confusion
print(confusion.matrix.conf_matrix)


## ------------------------------------------------------------------------------------------------

accuracy.conf_matrix=sum(diag(confusion.matrix.conf_matrix))/sum(confusion.matrix.conf_matrix)

print(accuracy.conf_matrix) 
#
CohenKappa(confusion.matrix.conf_matrix, conf.level = 0.95)



## ------------------------------------------------------------------------------------------------

conf_matrix_melt <- melt(confusion.matrix.conf_matrix)

# Créez la heatmap avec ggplot2
ggplot(conf_matrix_melt, aes(x=Var1, y=Var2, fill=value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  labs(x = "Humain", y = "Angle", fill = "Count") +
  ggtitle("Heatmap") +
  theme_minimal()


## ------------------------------------------------------------------------------------------------
similarity$Aggrement_llama_human_manual <- ifelse(similarity$Aggrement_llama_human_manual == "0", "No agreement",
                                        ifelse(similarity$Aggrement_llama_human_manual == "1", "Strong agreement",
                                        ifelse(similarity$Aggrement_llama_human_manual == "2", "Approximate agreement",
                                               similarity$Aggrement_llama_human_manual)))

similarity$Aggrement_llama_human_manual <- factor(similarity$Aggrement_llama_human_manual,
                                                 levels = c("No agreement", "Approximate agreement", "Strong agreement"))

p1<- ggplot(similarity) +
  aes(x = Aggrement_llama_human_manual) +
  geom_bar(fill = "#1368BB", color = "black", width = 0.5) + 
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.3, color = "black", size = 3) +  
  labs(x = "Agreement llama Human") +
  theme_minimal()
print(p1)


## ------------------------------------------------------------------------------------------------

similarity$Aggrement_mixtral_human_manual <- ifelse(similarity$Aggrement_mixtral_human_manual == "0", "No agreement",
                                        ifelse(similarity$Aggrement_mixtral_human_manual == "1", "Strong agreement",
                                        ifelse(similarity$Aggrement_mixtral_human_manual == "2", "Approximate agreement",
                                               similarity$Aggrement_mixtral_human_manual)))

similarity$Aggrement_mixtral_human_manual <- factor(similarity$Aggrement_mixtral_human_manual,
                                                 levels = c("No agreement", "Approximate agreement", "Strong agreement"))


p2 <- ggplot(similarity) +
  aes(x = Aggrement_mixtral_human_manual) +
  geom_bar(fill = "#1368BB", color = "black", width = 0.5) +  
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.3, color = "black", size = 3) +  
  labs(x = "Agreement mixtral Human") +
  theme_minimal()

print(p2)



## ------------------------------------------------------------------------------------------------
 
similarity_long <- similarity %>%
  pivot_longer(
    cols = c(Aggrement_mixtral_human_manual, Aggrement_llama_human_manual),
    names_to = "Model",
    values_to = "Agreement"
  ) %>%
  mutate(
    Model = recode(
      Model,
      Aggrement_mixtral_human_manual = "Mixtral",
      Aggrement_llama_human_manual = "Llama"
    )
  )

  
similarity_summary <- similarity_long %>%
  group_by(Model, Agreement) %>%
  summarise(Count = n()) %>%
  ungroup() %>%
  group_by(Model) %>%
  mutate(Percentage = Count / sum(Count) * 100)

  
p_combined_1 <- ggplot(similarity_summary, aes(x = Agreement, y = Count, fill = Model)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.5), color = "black", width = 0.5) +
  ggtitle("First Iteration") +
  geom_text(
    aes(label = paste0(Count, "\n(", round(Percentage, 1), "%)")),
    position = position_dodge(width = 0.5),
    vjust = -0.3,
    size = 8,    
    color = "black"
  ) +
  labs(x = "Agreement Category", y = "Count", fill = "Model") +
  scale_y_continuous(limits = c(0, 350)) +   
  theme_minimal(base_size = 20) +    
  theme(
    axis.title = element_text(size = 40),     
    axis.text = element_text(size = 40),      
    legend.title = element_text(size = 35),   
    legend.text = element_text(size = 30),     
    axis.title.x = element_text(margin = margin(t = 40)),
    axis.title.y = element_text(margin = margin(r = 40)),
    plot.title = element_text(size = 50, hjust = 0.5, margin = margin(b = 50)),
  )

  
ggsave(
  "C:/Users/edgar/Desktop/assistanat/code pour la similarité/combined_comparison_plot_with_counts_and_percentages_large_text_v.1.png",    
  plot = p_combined_1,
  width = 25,
  height = 18,
  units = "in",
  dpi = 300
)

  
print(p_combined_1)


## ------------------------------------------------------------------------------------------------


# Remplacer "Neutral" par NA dans la colonne "Target population using llm and human review"
similarity$Target_llm_human_review <- ifelse(similarity$Target_llm_human_review == "neutral", 
                                                                    NA, 
                                                                    similarity$Target_llm_human_review)

table(similarity$Target_llm_human_review == "neutral")
table(is.na(similarity$Target_llm_human_review))
similarity_clean <- similarity %>% filter(!is.na(Target_llm_human_review))

p3 <- ggplot(similarity_clean) +
  aes(x = Aggrement_mixtral_human_manual) +
  geom_bar(fill = "#1368BB", color = "black", width = 0.5) +  
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.3, color = "black", size = 3) +  
  labs(x = "Agreement mixtral Human") +
  theme_minimal()

print(p3)



## ------------------------------------------------------------------------------------------------
p4<- ggplot(similarity_clean) +
  aes(x = Aggrement_llama_human_manual) +
  geom_bar(fill = "#1368BB", color = "black", width = 0.5) +  
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.3, color = "black", size = 3) +  
  labs(x = "Agreement llama Human") +
  theme_minimal()

ggsave("C:/Users/edgar/Desktop/assistanat/code pour la similarité/p1.pdf", plot = p1, width = 25, height = 18, units = "in")
print(p4)



## ------------------------------------------------------------------------------------------------
 
similarity_long <- similarity_clean %>%
  pivot_longer(
    cols = c(Aggrement_mixtral_human_manual, Aggrement_llama_human_manual),
    names_to = "Model",
    values_to = "Agreement"
  ) %>%
  mutate(
    Model = recode(
      Model,
      Aggrement_mixtral_human_manual = "Mixtral",
      Aggrement_llama_human_manual = "Llama"
    )
  )

  
similarity_summary <- similarity_long %>%
  group_by(Model, Agreement) %>%
  summarise(Count = n()) %>%
  ungroup() %>%
  group_by(Model) %>%
  mutate(Percentage = Count / sum(Count) * 100)

  
p_combined_1_NA <- ggplot(similarity_summary, aes(x = Agreement, y = Count, fill = Model)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.5), color = "black", width = 0.5) +
  ggtitle("First Iteration") +
  geom_text(
    aes(label = paste0(Count, "\n(", round(Percentage, 1), "%)")),
    position = position_dodge(width = 0.5),
    vjust = -0.3,
    size = 8,    
    color = "black"
  ) +
  labs(x = "Agreement Category", y = "Count", fill = "Model") +
  scale_y_continuous(limits = c(0, 350)) +   
  theme_minimal(base_size = 20) +    
  theme(
    axis.title = element_text(size = 40),     
    axis.text = element_text(size = 40),      
    legend.title = element_text(size = 35),   
    legend.text = element_text(size = 30),     
    axis.title.x = element_text(margin = margin(t = 40)),
    axis.title.y = element_text(margin = margin(r = 40)),
    plot.title = element_text(size = 50, hjust = 0.5, margin = margin(b = 50)),
  )

  
ggsave(
  "C:/Users/edgar/Desktop/assistanat/code pour la similarité/combined_comparison_plot_with_counts_and_percentages_large_text_v.1_NA.png",    
  plot = p_combined_1_NA,
  width = 25,
  height = 18,
  units = "in",
  dpi = 300
)

  
print(p_combined_1_NA)


## ------------------------------------------------------------------------------------------------

similarity<- read_excel("C:/Users/edgar/Desktop/assistanat/code pour la similarité/2023 fichier de base Copie final.xlsx")



## ------------------------------------------------------------------------------------------------
summary(similarity)


## ------------------------------------------------------------------------------------------------
  
char_lengths <- nchar(similarity$Summary)

  
mean_length <- mean(char_lengths)

  
median_length <- median(char_lengths)

  
cat("Longueur moyenne des caractères:", mean_length, "\n")
cat("Longueur médiane des caractères:", median_length, "\n")


## ------------------------------------------------------------------------------------------------
similarity$agrement_embedding <- ifelse(similarity$Similarite_Cosinus_llama_human >= 0.70, 1, ifelse(similarity$Similarite_Cosinus_llama_human >= 0.55 & similarity$Similarite_Cosinus_llama_human < 0.70, 3, 0))

similarity$agrement_embedding<- ifelse(similarity$agrement_embedding == 3, 2, similarity$agrement_embedding)
similarity$Aggrement_llama_human_manual<- ifelse(similarity$Aggrement_llama_human_manual == 3, 2, similarity$Aggrement_llama_human_manual)


## ------------------------------------------------------------------------------------------------
# Créez une matrice de confusion
confusion.matrix.conf_matrix <- table(similarity$agrement_embedding, similarity$Aggrement_llama_human_manual)

# Affichez la matrice de confusion
print(confusion.matrix.conf_matrix)


## ------------------------------------------------------------------------------------------------

accuracy.conf_matrix=sum(diag(confusion.matrix.conf_matrix))/sum(confusion.matrix.conf_matrix)

print(accuracy.conf_matrix) 
#
CohenKappa(confusion.matrix.conf_matrix, conf.level = 0.95)



## ------------------------------------------------------------------------------------------------

conf_matrix_melt <- melt(confusion.matrix.conf_matrix)

# Créez la heatmap avec ggplot2
ggplot(conf_matrix_melt, aes(x=Var1, y=Var2, fill=value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  labs(x = "Humain", y = "Angle", fill = "Count") +
  ggtitle("Heatmap") +
  theme_minimal()


## ----eval=FALSE, include=FALSE-------------------------------------------------------------------
## #non revoir car c'est seulement dans un cadre binaire
## library(MLmetrics)
## actual = similarity$Aggrement_llama_human_manual
## predicted = similarity$agrement_embedding
## 
## # Calculate F1_Score
## F1_Score(predicted,actual)


## ------------------------------------------------------------------------------------------------

similarity$agrement_embedding_qwen <- ifelse(similarity$Similarite_Cosinus_qwen_human >= 0.70, 1, ifelse(similarity$Similarite_Cosinus_qwen_human >= 0.55 & similarity$Similarite_Cosinus_qwen_human < 0.70, 3, 0))

similarity$agrement_embedding_qwen<- ifelse(similarity$agrement_embedding_qwen == 3, 2, similarity$agrement_embedding_qwen)
similarity$Aggrement_qwen_human_manual<- ifelse(similarity$Aggrement_qwen_human_manual == 3, 2, similarity$Aggrement_qwen_human_manual)



## ----eval=FALSE, include=FALSE-------------------------------------------------------------------
## similarity$agrement_embedding_qwen <- ifelse(similarity$Similarite_Cosinus_qwen_human >= 0.62, 1, ifelse(similarity$Similarite_Cosinus_qwen_human >= 0.58 & similarity$Similarite_Cosinus_qwen_human < 0.62, 3, 0))
## 
## similarity$agrement_embedding_qwen<- ifelse(similarity$agrement_embedding_qwen == 3, 2, similarity$agrement_embedding_qwen)
## similarity$Aggrement_qwen_human_manual<- ifelse(similarity$Aggrement_qwen_human_manual == 3, 2, similarity$Aggrement_qwen_human_manual)
## 
## 


## ------------------------------------------------------------------------------------------------
# Créez une matrice de confusion
confusion.matrix.conf_matrix <- table(similarity$agrement_embedding_qwen, similarity$Aggrement_qwen_human_manual)

# Affichez la matrice de confusion
print(confusion.matrix.conf_matrix)


## ------------------------------------------------------------------------------------------------

accuracy.conf_matrix=sum(diag(confusion.matrix.conf_matrix))/sum(confusion.matrix.conf_matrix)

print(accuracy.conf_matrix) 
#
CohenKappa(confusion.matrix.conf_matrix, conf.level = 0.95)



## ------------------------------------------------------------------------------------------------

conf_matrix_melt <- melt(confusion.matrix.conf_matrix)

# Créez la heatmap avec ggplot2
ggplot(conf_matrix_melt, aes(x=Var1, y=Var2, fill=value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  labs(x = "Humain", y = "Angle", fill = "Count") +
  ggtitle("Heatmap") +
  theme_minimal()


## ------------------------------------------------------------------------------------------------

library(ggplot2)


similarity$Aggrement_llama_human_manual <- ifelse(similarity$Aggrement_llama_human_manual == "0", "No agreement",
                                        ifelse(similarity$Aggrement_llama_human_manual == "1", "Strong agreement",
                                        ifelse(similarity$Aggrement_llama_human_manual == "2", "Approximate agreement",
                                               similarity$Aggrement_llama_human_manual)))

similarity$Aggrement_llama_human_manual <- factor(similarity$Aggrement_llama_human_manual,
                                                 levels = c("No agreement", "Approximate agreement", "Strong agreement"))

p5<- ggplot(similarity) +
  aes(x = Aggrement_llama_human_manual) +
  geom_bar(fill = "#1368BB", color = "black", width = 0.5) +  
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.3, color = "black", size = 3) + 
  labs(x = "Agreement llama Human") +
  theme_minimal()

print(5)



## ------------------------------------------------------------------------------------------------

similarity$Aggrement_qwen_human_manual <- ifelse(similarity$Aggrement_qwen_human_manual == "0", "No agreement",
                                        ifelse(similarity$Aggrement_qwen_human_manual == "1", "Strong agreement",
                                        ifelse(similarity$Aggrement_qwen_human_manual == "2", "Approximate agreement",
                                               similarity$Aggrement_qwen_human_manual)))

similarity$Aggrement_qwen_human_manual <- factor(similarity$Aggrement_qwen_human_manual,
                                                 levels = c("No agreement", "Approximate agreement", "Strong agreement"))


p6 <- ggplot(similarity) +
  aes(x = Aggrement_qwen_human_manual) +
  geom_bar(fill = "#1368BB", color = "black", width = 0.5) +  
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.3, color = "black", size = 3) +  
  labs(x = "Agreement Qwen2 Human") +
  theme_minimal()

print(p6)



## ------------------------------------------------------------------------------------------------
 
similarity_long <- similarity %>%
  pivot_longer(
    cols = c(Aggrement_qwen_human_manual, Aggrement_llama_human_manual),
    names_to = "Model",
    values_to = "Agreement"
  ) %>%
  mutate(
    Model = recode(
      Model,
      Aggrement_qwen_human_manual = "qwen2",
      Aggrement_llama_human_manual = "Llama"
    )
  )

  
similarity_summary <- similarity_long %>%
  group_by(Model, Agreement) %>%
  summarise(Count = n()) %>%
  ungroup() %>%
  group_by(Model) %>%
  mutate(Percentage = Count / sum(Count) * 100)

  
p_combined_2 <- ggplot(similarity_summary, aes(x = Agreement, y = Count, fill = Model)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.5), color = "black", width = 0.5) +
  ggtitle("Second Iteration") +
  geom_text(
    aes(label = paste0(Count, "\n(", round(Percentage, 1), "%)")),
    position = position_dodge(width = 0.5),
    vjust = -0.3,
    size = 8,    
    color = "black"
  ) +
  labs(x = "Agreement Category", y = "Count", fill = "Model") +
  scale_y_continuous(limits = c(0, 350)) +   
  theme_minimal(base_size = 20) +    
  theme(
    axis.title = element_text(size = 40),     
    axis.text = element_text(size = 40),      
    legend.title = element_text(size = 35),   
    legend.text = element_text(size = 30),     
    axis.title.x = element_text(margin = margin(t = 40)),
    axis.title.y = element_text(margin = margin(r = 40)),
    plot.title = element_text(size = 50, hjust = 0.5, margin = margin(b = 50))
  )

  
ggsave(
  "C:/Users/edgar/Desktop/assistanat/code pour la similarité/combined_comparison_plot_with_counts_and_percentages_large_text_v.2.png",    
  plot = p_combined_2,
  width = 25,
  height = 18,
  units = "in",
  dpi = 300
)

  
print(p_combined_2)


## ------------------------------------------------------------------------------------------------


# Remplacer "Neutral" par NA dans la colonne "Target population using llm and human review"
similarity$Target_llm_human_review <- ifelse(similarity$Target_llm_human_review == "neutral", 
                                                                    NA, 
                                                                    similarity$Target_llm_human_review)

table(similarity$Target_llm_human_review == "neutral")
table(is.na(similarity$Target_llm_human_review))
similarity_clean <- similarity %>% filter(!is.na(Target_llm_human_review))

p7 <- ggplot(similarity_clean) +
  aes(x = Aggrement_qwen_human_manual) +
  geom_bar(fill = "#1368BB", color = "black", width = 0.5) +  
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.3, color = "black", size = 3) +  
  labs(x = "Agreement Qwen2 Human") +
  theme_minimal()

print(p7)




## ------------------------------------------------------------------------------------------------
p8<- ggplot(similarity_clean) +
  aes(x = Aggrement_llama_human_manual) +
  geom_bar(fill = "#1368BB", color = "black", width = 0.5) +  
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.3, color = "black", size = 3) +  
  labs(x = "Agreement llama Human") +
  theme_minimal()
print(p8)



## ------------------------------------------------------------------------------------------------
 
similarity_long <- similarity_clean %>%
  pivot_longer(
    cols = c(Aggrement_qwen_human_manual, Aggrement_llama_human_manual),
    names_to = "Model",
    values_to = "Agreement"
  ) %>%
  mutate(
    Model = recode(
      Model,
      Aggrement_qwen_human_manual = "qwen2",
      Aggrement_llama_human_manual = "Llama"
    )
  )

  
similarity_summary <- similarity_long %>%
  group_by(Model, Agreement) %>%
  summarise(Count = n()) %>%
  ungroup() %>%
  group_by(Model) %>%
  mutate(Percentage = Count / sum(Count) * 100)

  
p_combined_2_NA <- ggplot(similarity_summary, aes(x = Agreement, y = Count, fill = Model)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.5), color = "black", width = 0.5) +   ggtitle("Second Iteration") +
  geom_text(
    aes(label = paste0(Count, "\n(", round(Percentage, 1), "%)")),
    position = position_dodge(width = 0.5),
    vjust = -0.3,
    size = 8,    
    color = "black"
  ) +
  labs(x = "Agreement Category", y = "Count", fill = "Model") +
  scale_y_continuous(limits = c(0, 350)) +   
  theme_minimal(base_size = 20) +    
  theme(
    axis.title = element_text(size = 40),     
    axis.text = element_text(size = 40),      
    legend.title = element_text(size = 35),   
    legend.text = element_text(size = 30),     
    axis.title.x = element_text(margin = margin(t = 40)),
    axis.title.y = element_text(margin = margin(r = 40)),
    plot.title = element_text(size = 50, hjust = 0.5, margin = margin(b = 50)),
  )

  
ggsave(
  "C:/Users/edgar/Desktop/assistanat/code pour la similarité/combined_comparison_plot_with_counts_and_percentages_large_text_v.2_NA.png",    
  plot = p_combined_2_NA,
  width = 25,
  height = 18,
  units = "in",
  dpi = 300
)

  
print(p_combined_2_NA)

## ------------------------------------------------------------------------------------------------

similarity<- read_excel("C:/Users/edgar/Desktop/assistanat/code pour la similarité/llm.xlsx")



## ----eval=FALSE, include=FALSE-------------------------------------------------------------------
## summary(similarity)


## ------------------------------------------------------------------------------------------------
  
char_lengths <- nchar(similarity$Summary)

  
mean_length <- mean(char_lengths)

  
median_length <- median(char_lengths)

  
cat("Longueur moyenne des caractères:", mean_length, "\n")
cat("Longueur médiane des caractères:", median_length, "\n")


## ------------------------------------------------------------------------------------------------
similarity$agrement_embedding <- ifelse(similarity$Similarite_Cosinus_llama_human >= 0.70, 1, ifelse(similarity$Similarite_Cosinus_llama_human >= 0.55 & similarity$Similarite_Cosinus_llama_human < 0.70, 3, 0))

similarity$agrement_embedding<- ifelse(similarity$agrement_embedding == 3, 2, similarity$agrement_embedding)
similarity$Aggrement_llama_human_manual<- ifelse(similarity$Aggrement_llama_human_manual == 3, 2, similarity$Aggrement_llama_human_manual)


## ------------------------------------------------------------------------------------------------
# Créez une matrice de confusion
confusion.matrix.conf_matrix <- table(similarity$agrement_embedding, similarity$Aggrement_llama_human_manual)

# Affichez la matrice de confusion
print(confusion.matrix.conf_matrix)


## ------------------------------------------------------------------------------------------------

accuracy.conf_matrix=sum(diag(confusion.matrix.conf_matrix))/sum(confusion.matrix.conf_matrix)

print(accuracy.conf_matrix) 
#
CohenKappa(confusion.matrix.conf_matrix, conf.level = 0.95)



## ------------------------------------------------------------------------------------------------

conf_matrix_melt <- melt(confusion.matrix.conf_matrix)

# Créez la heatmap avec ggplot2
ggplot(conf_matrix_melt, aes(x=Var1, y=Var2, fill=value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  labs(x = "Humain", y = "Angle", fill = "Count") +
  ggtitle("Heatmap") +
  theme_minimal()

## ----eval=FALSE, include=FALSE-------------------------------------------------------------------
## #non revoir car c'est seulement dans un cadre binaire
## library(MLmetrics)
## actual = similarity$Aggrement_llama_human_manual
## predicted = similarity$agrement_embedding
## 
## # Calculate F1_Score
## F1_Score(predicted,actual)


## ------------------------------------------------------------------------------------------------

similarity$agrement_embedding_qwen <- ifelse(similarity$Similarite_Cosinus_qwen_human >= 0.70, 1, ifelse(similarity$Similarite_Cosinus_qwen_human >= 0.55 & similarity$Similarite_Cosinus_qwen_human < 0.70, 3, 0))

similarity$agrement_embedding_qwen<- ifelse(similarity$agrement_embedding_qwen == 3, 2, similarity$agrement_embedding_qwen)
similarity$Aggrement_qwen_human_manual<- ifelse(similarity$Aggrement_qwen_human_manual == 3, 2, similarity$Aggrement_qwen_human_manual)



## ----eval=FALSE, include=FALSE-------------------------------------------------------------------
## similarity$agrement_embedding_qwen <- ifelse(similarity$Similarite_Cosinus_qwen_human >= 0.62, 1, ifelse(similarity$Similarite_Cosinus_qwen_human >= 0.58 & similarity$Similarite_Cosinus_qwen_human < 0.62, 3, 0))
## 
## similarity$agrement_embedding_qwen<- ifelse(similarity$agrement_embedding_qwen == 3, 2, similarity$agrement_embedding_qwen)
## similarity$Aggrement_qwen_human_manual<- ifelse(similarity$Aggrement_qwen_human_manual == 3, 2, similarity$Aggrement_qwen_human_manual)
## 
## 


## ------------------------------------------------------------------------------------------------
# Créez une matrice de confusion
confusion.matrix.conf_matrix <- table(similarity$agrement_embedding_qwen, similarity$Aggrement_qwen_human_manual)

# Affichez la matrice de confusion
print(confusion.matrix.conf_matrix)


## ------------------------------------------------------------------------------------------------

accuracy.conf_matrix=sum(diag(confusion.matrix.conf_matrix))/sum(confusion.matrix.conf_matrix)

print(accuracy.conf_matrix) 
#
CohenKappa(confusion.matrix.conf_matrix, conf.level = 0.95)



## ------------------------------------------------------------------------------------------------

conf_matrix_melt <- melt(confusion.matrix.conf_matrix)

# Créez la heatmap avec ggplot2
ggplot(conf_matrix_melt, aes(x=Var1, y=Var2, fill=value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  labs(x = "Humain", y = "Angle", fill = "Count") +
  ggtitle("Heatmap") +
  theme_minimal()


## ------------------------------------------------------------------------------------------------

library(ggplot2)


similarity$Aggrement_llama_human_manual <- ifelse(similarity$Aggrement_llama_human_manual == "0", "No agreement",
                                        ifelse(similarity$Aggrement_llama_human_manual == "1", "Strong agreement",
                                        ifelse(similarity$Aggrement_llama_human_manual == "2", "Approximate agreement",
                                               similarity$Aggrement_llama_human_manual)))

similarity$Aggrement_llama_human_manual <- factor(similarity$Aggrement_llama_human_manual,
                                                 levels = c("No agreement", "Approximate agreement", "Strong agreement"))

p9<- ggplot(similarity) +
  aes(x = Aggrement_llama_human_manual) +
  geom_bar(fill = "#1368BB", color = "black", width = 0.5) +  
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.3, color = "black", size = 3) +  
  labs(x = "Agreement llama Human") +
  theme_minimal()
print(p9)



## ------------------------------------------------------------------------------------------------

similarity$Aggrement_qwen_human_manual <- ifelse(similarity$Aggrement_qwen_human_manual == "0", "No agreement",
                                        ifelse(similarity$Aggrement_qwen_human_manual == "1", "Strong agreement",
                                        ifelse(similarity$Aggrement_qwen_human_manual == "2", "Approximate agreement",
                                               similarity$Aggrement_qwen_human_manual)))

similarity$Aggrement_qwen_human_manual <- factor(similarity$Aggrement_qwen_human_manual,
                                                 levels = c("No agreement", "Approximate agreement", "Strong agreement"))


p10 <- ggplot(similarity) +
  aes(x = Aggrement_qwen_human_manual) +
  geom_bar(fill = "#1368BB", color = "black", width = 0.5) +  
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.3, color = "black", size = 3) +  
  labs(x = "Agreement Qwen2 Human") +
  theme_minimal()

print(p10)



## ------------------------------------------------------------------------------------------------
 
similarity_long <- similarity %>%
  pivot_longer(
    cols = c(Aggrement_qwen_human_manual, Aggrement_llama_human_manual),
    names_to = "Model",
    values_to = "Agreement"
  ) %>%
  mutate(
    Model = recode(
      Model,
      Aggrement_qwen_human_manual = "qwen2",
      Aggrement_llama_human_manual = "Llama"
    )
  )

  
similarity_summary <- similarity_long %>%
  group_by(Model, Agreement) %>%
  summarise(Count = n()) %>%
  ungroup() %>%
  group_by(Model) %>%
  mutate(Percentage = Count / sum(Count) * 100)

  
p_combined_3 <- ggplot(similarity_summary, aes(x = Agreement, y = Count, fill = Model)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.5), color = "black", width = 0.5) +
  ggtitle("Third Iteration") +
  geom_text(
    aes(label = paste0(Count, "\n(", round(Percentage, 1), "%)")),
    position = position_dodge(width = 0.5),
    vjust = -0.3,
    size = 8,    
    color = "black"
  ) +
  labs(x = "Agreement Category", y = "Count", fill = "Model") +
  scale_y_continuous(limits = c(0, 350)) +   
  theme_minimal(base_size = 20) +    
  theme(
    axis.title = element_text(size = 40),     
    axis.text = element_text(size = 40),      
    legend.title = element_text(size = 35),   
    legend.text = element_text(size = 30),     
    axis.title.x = element_text(margin = margin(t = 40)),
    axis.title.y = element_text(margin = margin(r = 40)),
    plot.title = element_text(size = 50, hjust = 0.5, margin = margin(b = 50)),
  )

  
ggsave(
  "C:/Users/edgar/Desktop/assistanat/code pour la similarité/combined_comparison_plot_with_counts_and_percentages_large_text_v.3.png",    
  plot = p_combined_3,
  width = 25,
  height = 18,
  units = "in",
  dpi = 300
)

  
print(p_combined_3)


## ------------------------------------------------------------------------------------------------


# Remplacer "Neutral" par NA dans la colonne "Target population using llm and human review"
similarity$Target_llm_human_review <- ifelse(similarity$Target_llm_human_review == "neutral", 
                                                                    NA, 
                                                                    similarity$Target_llm_human_review)

table(similarity$Target_llm_human_review == "neutral")
table(is.na(similarity$Target_llm_human_review))
similarity_clean <- similarity %>% filter(!is.na(Target_llm_human_review))

p11 <- ggplot(similarity_clean) +
  aes(x = Aggrement_qwen_human_manual) +
  geom_bar(fill = "#1368BB", color = "black", width = 0.5) +  
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.3, color = "black", size = 3) +  
  labs(x = "Agreement Qwen2 Human") +
  theme_minimal()

print(p11)



## ------------------------------------------------------------------------------------------------
p12<- ggplot(similarity_clean) +
  aes(x = Aggrement_llama_human_manual) +
  geom_bar(fill = "#1368BB", color = "black", width = 0.5) + 
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.3, color = "black", size = 3) + 
  labs(x = "Agreement llama Human") +
  theme_minimal()

ggsave("C:/Users/edgar/Desktop/assistanat/code pour la similarité/p1.pdf", plot = p1, width = 25, height = 18, units = "in")
print(p12)




## ------------------------------------------------------------------------------------------------
 
similarity_long <- similarity_clean %>%
  pivot_longer(
    cols = c(Aggrement_qwen_human_manual, Aggrement_llama_human_manual),
    names_to = "Model",
    values_to = "Agreement"
  ) %>%
  mutate(
    Model = recode(
      Model,
      Aggrement_qwen_human_manual = "qwen2",
      Aggrement_llama_human_manual = "Llama"
    )
  )

  
similarity_summary <- similarity_long %>%
  group_by(Model, Agreement) %>%
  summarise(Count = n()) %>%
  ungroup() %>%
  group_by(Model) %>%
  mutate(Percentage = Count / sum(Count) * 100)

  
p_combined_3_NA <- ggplot(similarity_summary, aes(x = Agreement, y = Count, fill = Model)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.5), color = "black", width = 0.5) +
  ggtitle("Third Iteration") +
  geom_text(
    aes(label = paste0(Count, "\n(", round(Percentage, 1), "%)")),
    position = position_dodge(width = 0.5),
    vjust = -0.3,
    size = 8,    
    color = "black"
  ) +
  labs(x = "Agreement Category", y = "Count", fill = "Model") +
  scale_y_continuous(limits = c(0, 350)) +   
  theme_minimal(base_size = 20) +    
  theme(
    axis.title = element_text(size = 40),     
    axis.text = element_text(size = 40),      
    legend.title = element_text(size = 35),   
    legend.text = element_text(size = 30),     
    axis.title.x = element_text(margin = margin(t = 40)),
    axis.title.y = element_text(margin = margin(r = 40)),
    plot.title = element_text(size = 50, hjust = 0.5, margin = margin(b = 50)),
  )

  
ggsave(
  "C:/Users/edgar/Desktop/assistanat/code pour la similarité/combined_comparison_plot_with_counts_and_percentages_large_text_v.3_NA.png",    
  plot = p_combined_3_NA,
  width = 25,
  height = 18,
  units = "in",
  dpi = 300
)

  
print(p_combined_3_NA)

## ------------------------------------------------------------------------------------------------
#Maintenant que nous avons tous nos graphiques pour nos trois iterations nous allons les merged ensemble 

library(patchwork)



## ------------------------------------------------------------------------------------------------
combined_plot <- p_combined_1 + p_combined_2 + p_combined_3 +
  plot_layout(ncol = 2) 

print(combined_plot)

  
ggsave(
  "C:/Users/edgar/Desktop/assistanat/code pour la similarité/combined_plot_patchwork.png",
  plot = combined_plot,
  width = 45,    
  height = 20,    
  units = "in",
  limitsize = FALSE,
  dpi = 300
)

## ------------------------------------------------------------------------------------------------
combined_plot_NA <- p_combined_1_NA + p_combined_2_NA + p_combined_3_NA +
  plot_layout(ncol = 2) 

print(combined_plot_NA)

  
ggsave(
  "C:/Users/edgar/Desktop/assistanat/code pour la similarité/combined_plot_patchwork_NA.png",
  plot = combined_plot_NA,
  width = 45,    
  height = 20,    
  units = "in",
  dpi = 300
)


## ------------------------------------------------------------------------------------------------
knitr::purl("Code_for_visualisation.R")

