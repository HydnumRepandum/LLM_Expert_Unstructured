---
title: "similarity"
author: "Edgar Mathevet"
date: "`r Sys.Date()`"
output:
  word_document: default
  html_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r}
library(ggplot2)
library(reshape2)
library(DescTools)
library(readxl)
```

```{r}

similarity<- read_excel("C:/Users/edgar/Desktop/assistanat/code pour la similarité/2023 fichier de base Copie final.xlsx")

```

```{r}
summary(similarity)
```

```{r}
# Calculez le nombre de caractères pour chaque élément de la variable Summary
char_lengths <- nchar(similarity$Summary)

# Calculez la moyenne des longueurs de caractères
mean_length <- mean(char_lengths)

# Calculez la médiane des longueurs de caractères
median_length <- median(char_lengths)

# Affichez les résultats
cat("Longueur moyenne des caractères:", mean_length, "\n")
cat("Longueur médiane des caractères:", median_length, "\n")
```
On regarde pour llama_human

```{r}
similarity$agrement_embedding <- ifelse(similarity$Similarite_Cosinus_llama_human >= 0.75, 1, ifelse(similarity$Similarite_Cosinus_llama_human >= 0.55 & similarity$Similarite_Cosinus_llama_human < 0.75, 3, 0))

similarity$agrement_embedding<- ifelse(similarity$agrement_embedding == 3, 2, similarity$agrement_embedding)
similarity$Aggrement_llama_human_manual<- ifelse(similarity$Aggrement_llama_human_manual == 3, 2, similarity$Aggrement_llama_human_manual)
```


```{r}
# Créez une matrice de confusion
confusion.matrix.conf_matrix <- table(similarity$agrement_embedding, similarity$Aggrement_llama_human_manual)

# Affichez la matrice de confusion
print(confusion.matrix.conf_matrix)
```

```{r}

accuracy.conf_matrix=sum(diag(confusion.matrix.conf_matrix))/sum(confusion.matrix.conf_matrix)

print(accuracy.conf_matrix) 
#
CohenKappa(confusion.matrix.conf_matrix, conf.level = 0.95)

```

```{r}
# Convertissez la matrice de confusion en data frame pour ggplot2
conf_matrix_melt <- melt(confusion.matrix.conf_matrix)

# Créez la heatmap avec ggplot2
ggplot(conf_matrix_melt, aes(x=Var1, y=Var2, fill=value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  labs(x = "Humain", y = "Angle", fill = "Count") +
  ggtitle("Heatmap") +
  theme_minimal()
```
```{r eval=FALSE, include=FALSE}
#non revoir car c'est seulement dans un cadre binaire
library(MLmetrics)
actual = similarity$Aggrement_llama_human_manual
predicted = similarity$agrement_embedding
 
# Calculate F1_Score
F1_Score(predicted,actual)
```


####### maintenant 

On recommence pour Qwen_human

```{r}

similarity$agrement_embedding_qwen <- ifelse(similarity$Similarite_Cosinus_qwen_human >= 0.75, 1, ifelse(similarity$Similarite_Cosinus_qwen_human >= 0.55 & similarity$Similarite_Cosinus_qwen_human < 0.75, 3, 0))

similarity$agrement_embedding_qwen<- ifelse(similarity$agrement_embedding_qwen == 3, 2, similarity$agrement_embedding_qwen)
similarity$Aggrement_qwen_human_manual<- ifelse(similarity$Aggrement_qwen_human_manual == 3, 2, similarity$Aggrement_qwen_human_manual)

```

#valeur qui optimise l'accuracy
```{r eval=FALSE, include=FALSE}
similarity$agrement_embedding_qwen <- ifelse(similarity$Similarite_Cosinus_qwen_human >= 0.62, 1, ifelse(similarity$Similarite_Cosinus_qwen_human >= 0.58 & similarity$Similarite_Cosinus_qwen_human < 0.62, 3, 0))

similarity$agrement_embedding_qwen<- ifelse(similarity$agrement_embedding_qwen == 3, 2, similarity$agrement_embedding_qwen)
similarity$Aggrement_qwen_human_manual<- ifelse(similarity$Aggrement_qwen_human_manual == 3, 2, similarity$Aggrement_qwen_human_manual)


```


```{r}
# Créez une matrice de confusion
confusion.matrix.conf_matrix <- table(similarity$agrement_embedding_qwen, similarity$Aggrement_qwen_human_manual)

# Affichez la matrice de confusion
print(confusion.matrix.conf_matrix)
```

```{r}

accuracy.conf_matrix=sum(diag(confusion.matrix.conf_matrix))/sum(confusion.matrix.conf_matrix)

print(accuracy.conf_matrix) 
#
CohenKappa(confusion.matrix.conf_matrix, conf.level = 0.95)

```

```{r}
# Convertissez la matrice de confusion en data frame pour ggplot2
conf_matrix_melt <- melt(confusion.matrix.conf_matrix)

# Créez la heatmap avec ggplot2
ggplot(conf_matrix_melt, aes(x=Var1, y=Var2, fill=value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  labs(x = "Humain", y = "Angle", fill = "Count") +
  ggtitle("Heatmap") +
  theme_minimal()
```

##On fait nos graphiques

```{r}

library(ggplot2)


similarity$Aggrement_llama_human_manual <- ifelse(similarity$Aggrement_llama_human_manual == "0", "No agreement",
                                        ifelse(similarity$Aggrement_llama_human_manual == "1", "Strong agreement",
                                        ifelse(similarity$Aggrement_llama_human_manual == "2", "Approximat agreement",
                                               similarity$Aggrement_llama_human_manual)))

similarity$Aggrement_llama_human_manual <- factor(similarity$Aggrement_llama_human_manual,
                                                 levels = c("No agreement", "Approximat agreement", "Strong agreement"))

p1<- ggplot(similarity) +
  aes(x = Aggrement_llama_human_manual) +
  geom_bar(fill = "#1368BB", color = "black", width = 0.5) +  # Ajoute un contour noir et ajuste la largeur
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.3, color = "black", size = 3) +  # Ajoute les étiquettes au-dessus des barres
  labs(x = "Agreement llama Human") +
  theme_gray()

ggsave("C:/Users/edgar/Desktop/assistanat/code pour la similarité/p1.pdf", plot = p1, width = 25, height = 18, units = "in")
print(p1)

```

```{r}

similarity$Aggrement_qwen_human_manual <- ifelse(similarity$Aggrement_qwen_human_manual == "0", "No agreement",
                                        ifelse(similarity$Aggrement_qwen_human_manual == "1", "Strong agreement",
                                        ifelse(similarity$Aggrement_qwen_human_manual == "2", "Approximat agreement",
                                               similarity$Aggrement_qwen_human_manual)))

similarity$Aggrement_qwen_human_manual <- factor(similarity$Aggrement_qwen_human_manual,
                                                 levels = c("No agreement", "Approximat agreement", "Strong agreement"))


p2 <- ggplot(similarity) +
  aes(x = Aggrement_qwen_human_manual) +
  geom_bar(fill = "#1368BB", color = "black", width = 0.5) +  # Ajoute un contour noir et ajuste la largeur
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.3, color = "black", size = 3) +  # Ajoute les étiquettes au-dessus des barres
  labs(x = "Agreement Qwen2 Human") +
  theme_gray()


ggsave("C:/Users/edgar/Desktop/assistanat/code pour la similarité/p2.pdf", plot = p2, width = 25, height = 18, units = "in")
print(p2)

```

####On va juste refaire les analyse en omettant les valeurs NA faites par l'humain


```{r}


# Remplacer "Neutral" par NA dans la colonne "Target population using llm and human review"
similarity$Target_llm_human_review <- ifelse(similarity$Target_llm_human_review == "neutral", 
                                                                    NA, 
                                                                    similarity$Target_llm_human_review)

table(similarity$Target_llm_human_review == "neutral")
table(is.na(similarity$Target_llm_human_review))
similarity_clean <- similarity %>% filter(!is.na(Target_llm_human_review))

p3 <- ggplot(similarity_clean) +
  aes(x = Aggrement_qwen_human_manual) +
  geom_bar(fill = "#1368BB", color = "black", width = 0.5) +  # Ajoute un contour noir et ajuste la largeur
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.3, color = "black", size = 3) +  # Ajoute les étiquettes au-dessus des barres
  labs(x = "Agreement Qwen2 Human") +
  theme_gray()


ggsave("C:/Users/edgar/Desktop/assistanat/code pour la similarité/p3.pdf", plot = p2, width = 25, height = 18, units = "in")
print(p3)


```

```{r}
p4<- ggplot(similarity_clean) +
  aes(x = Aggrement_llama_human_manual) +
  geom_bar(fill = "#1368BB", color = "black", width = 0.5) +  # Ajoute un contour noir et ajuste la largeur
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.3, color = "black", size = 3) +  # Ajoute les étiquettes au-dessus des barres
  labs(x = "Agreement llama Human") +
  theme_gray()

ggsave("C:/Users/edgar/Desktop/assistanat/code pour la similarité/p1.pdf", plot = p1, width = 25, height = 18, units = "in")
print(p4)
```
