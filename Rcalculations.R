library(readxl)
library(ggplot2)

# Wczytanie danych
data <- read_excel('C:/Users/wojte/OneDrive/Desktop/dane_mieszkania.xlsx')

# Standaryzacja danych
#selected_columns <- data[, c('squareMeters', 'rooms', 'centreDistance', 'poiCount')]
selected_columns <- data[, c('squareMeters', 'rooms', 'centreDistance')]
standardized_data <- scale(selected_columns)

# Analiza składowych głównych (PCA)
pca_result <- prcomp(standardized_data, center = TRUE, scale. = TRUE)

# Wyświetlenie proporcji wyjaśnionej wariancji
explained_variance <- pca_result$sdev^2 / sum(pca_result$sdev^2)
cumulative_explained_variance <- cumsum(explained_variance)
print(explained_variance)
print(cumulative_explained_variance)

# Przygotowanie danych do wizualizacji
pca_data <- data.frame(pca_result$x)
pca_data$group <- ifelse(data$price > median(data$price), 'price above median', 'price below median')

# Wizualizacja wyników PCA
ggplot(data = pca_data, aes(x = PC1, y = PC2, color = group)) +
  geom_point(alpha = 0.7) +
  theme_minimal() +
  ggtitle('PCA') +
  xlab('PC1') +
  ylab('PC2')


library(car)

# Przygotowanie danych
data$price <- as.numeric(data$price) # upewniamy się, że cena jest numeryczna

# Budowa modelu regresji liniowej bez zmiennej poiCount
model1 <- lm(price ~ squareMeters + rooms + centreDistance, data = data)

# Budowa modelu regresji liniowej z zmienną poiCount
model2 <- lm(price ~ squareMeters + rooms + centreDistance + poiCount, data = data)

# Podsumowanie modeli
summary(model1)
summary(model2)

# Analiza wariancji (ANOVA) porównująca oba modele
anova_result <- anova(model1, model2)

# Wyświetlenie wyników ANOVA
print(anova_result)

# Model z rooms
model3 <- lm(price ~ squareMeters + rooms + centreDistance, data = data)

# Model bez rooms
model4 <- lm(price ~ squareMeters + centreDistance, data = data)

# Podsumowanie modeli
summary(model3)
summary(model4)

# Analiza wariancji (ANOVA) porównująca modele z rooms i bez rooms
anova_result2 <- anova(model3, model4)

# Wyświetlenie wyników ANOVA porównania
print(anova_result2)

