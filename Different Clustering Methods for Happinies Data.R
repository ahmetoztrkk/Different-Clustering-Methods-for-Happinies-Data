# 1. K??s??m verisetinin y??klenmesi ve tan??mlay??c?? istatistikler ve korelasyon




getwd()
setwd("D:/R")
if (!require("readxl")) install.packages("readxl")
library(readxl)
file_path <-read.csv("D:/R/2019.csv",header = T)
x <- file_path
x <- x[4:9]
head(x)
# Orijinal veri ??zeti
summary(x)
# Gerekli paketlerin y??klenmesi ve ??a??r??lmas??
if(!require("dplyr"))install.packages("dplyr")
if (!require("ggplot2")) install.packages("ggplot2", dependencies = TRUE)
if (!require("corrplot")) install.packages("corrplot", dependencies = TRUE)
library(dplyr)
library(ggplot2)   # Daha estetik g??rselle??tirme i??in
library(corrplot)  # Korelasyon g??rselle??tirmesi i??in
# ??l??eklendirilmemi?? veri i??in boxplot
boxplot(x, main = "Boxplot (Orijinal Veri)", las = 2, col = rainbow(7))
# Veriyi ??l??eklendirme
df <- scale(x)
# ??l??eklendirilmi?? veri i??in boxplot
boxplot(df, main = "Boxplot (Scale Edilmi?? Veri)", las = 2, col = rainbow(7))
# Orijinal veri i??in histogramlar

for (i in 1:ncol(x)) {
  hist(x[, i], main = colnames(x)[i], col = rainbow(7), xlab = colnames(x)[i])
}

# Orijinal veri korelasyon matrisi
cor_happiness <- cor(x, use = "complete.obs")
corrplot.mixed(cor_happiness)
cor(x)




# 2. KISIM k??me say??s??n??n belirlenmesi ve g??rselle??tirmeleri





# Gerekli paketleri y??kleme ve ??a????rma
if (!require("factoextra")) install.packages("factoextra", dependencies = TRUE)
if (!require("cluster")) install.packages("cluster", dependencies = TRUE)

library(factoextra)  # K??meleme g??rselle??tirmesi
library(cluster)     # K??meleme algoritmalar??

fviz_dist(dist(df))

fviz_nbclust(df, kmeans, method = "wss",nstart=25)
fviz_nbclust(df, kmeans, method = "silhouette")
fviz_nbclust(x, kmeans, method = "gap_stat")

# K-Means algoritmas??n?? ??al????t??rma 
set.seed(123)
kmeans(df,3, iter.max = 10, nstart = 1)
km.res <- kmeans(df, 3, nstart = 25)
fviz_cluster(km.res, data = df,labelsize = 3,pointsize = 2)
km.res2 <- kmeans(df, 4, nstart = 25)
fviz_cluster(km.res2, data = df,labelsize = 3,pointsize = 2)
km.res3 <- kmeans(df, 5, nstart = 25)
fviz_cluster(km.res3, data = df,labelsize = 3,pointsize = 2)
# Her bir k??menin ortalama de??erlerini hesaplama
aggregate(x, by=list(cluster=km.res3$cluster), mean)

#K-Medoids???e G??re K??me Say??s?? Belirleme
fviz_nbclust(df, pam, method = "wss",nstart=25)
fviz_nbclust(df, pam, method = "silhouette")
fviz_nbclust(x, pam, method = "gap_stat")
# PAM algoritmas??n?? ??al????t??rma (k??melerin say??s?? 3)
pam.res <- pam(df, k = 3)

# PAM algoritmas??n??n sonu??lar??n?? inceleyelim
print(pam.res)
# K??meleme sonu??lar??n?? g??rselle??tirme (ggplot2 ile)
fviz_cluster(pam.res,
             palette = c("blue", "red","grey","green","orange"), 
             ellipse.type = "t", 
             repel = TRUE, 
             ggtheme = theme_classic()
)


# PAM algoritmas??n?? ??al????t??rma (k??melerin say??s?? 5)
pam.res2 <- pam(df, k = 5)
print(pam.res2)
fviz_cluster(pam.res2,
             palette = c("blue", "red","grey","green","orange"), 
             ellipse.type = "t", 
             repel = TRUE, 
             ggtheme = theme_classic()
)

pam.res3 <- pam(df, k = 7)
print(pam.res3)
fviz_cluster(pam.res3,
             palette = rainbow(7), 
             ellipse.type = "t", 
             repel = TRUE, 
             ggtheme = theme_classic()
)



#3. KISIM K??meler aras?? de??i??ime bakmak i??in boxplot by factor


#boxplot by factor
x <- as.data.frame(x)  
x$Cluster <- as.factor(km.res3$cluster)
for (i in 1:ncol(x)) {
  boxplot(x[, i] ~ x$Cluster, 
          main = paste("Boxplot of", colnames(x)[i], "by Cluster"),
          col = rainbow(5))  
}




# 4. KISIM  Temel bile??enler analizinin uygulanmas?? ve g??rselle??tirilmesi






# PCA uygulama
pca_result <- prcomp(df, center = TRUE, scale. = TRUE)
# PCA sonu??lar??n?? inceleyelim
summary(pca_result)

fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50))

# ??lk iki bile??eni kullanarak g??rselle??tirme
biplot(pca_result, main = "PCA Biplot")
res.pca <- prcomp(df, scale = TRUE)
fviz_pca_var(res.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

pca_result$rotation
transformed_data <- pca_result$x[, 1:2]
summary(transformed_data)
# PCA sonras?? K-Means algoritmas??
fviz_dist(dist(transformed_data))

df.pca <- scale(transformed_data)
fviz_nbclust(df.pca, kmeans, method = "wss",nstart=25)
fviz_nbclust(df.pca, kmeans, method = "silhouette")
fviz_nbclust(transformed_data, kmeans, method = "gap_stat")
set.seed(123)
km.pca <- kmeans(df.pca, 3, nstart = 25)
fviz_cluster(km.pca, data = df.pca, labelsize = 3, pointsize = 2)

km.pca2 <- kmeans(df.pca, 5, nstart = 25)
fviz_cluster(km.pca2, data = df.pca, labelsize = 3, pointsize = 2)


# PCA sonras?? PAM algoritmas??

fviz_nbclust(df.pca, pam, method = "wss",nstart=25)
fviz_nbclust(df.pca, pam, method = "silhouette")
fviz_nbclust(transformed_data, pam, method = "gap_stat")


pam.pca <- pam(df.pca, k = 3)
fviz_cluster(pam.pca, palette = c("blue", "red", "green"), 
             ellipse.type = "t", 
             repel = TRUE, 
             ggtheme = theme_classic())
pam.pca2 <- pam(df.pca, k = 5)
fviz_cluster(pam.pca2, palette = rainbow(5), 
             ellipse.type = "t", 
             repel = TRUE, 
             ggtheme = theme_classic())

# PCA sonras?? hiyerar??ik k??meleme
city_names <- file_path[, 2]
rownames(df.pca) <- city_names
rownames(transformed_data) <- city_names

res.agnes.pca <- agnes(x = df.pca, metric = "euclidean", method = "ward")
fviz_dend(res.agnes.pca, cex = 0.6, k = 3)


res.diana.pca <- diana(x = transformed_data, metric = "euclidean")
fviz_dend(res.diana.pca, cex = 0.6, k = 3)



#5. KISIM Hiyerar??ik k??melemenin uygulanmas?? ve g??rselle??tirilmesi 

city_names <- file_path[, 2]
rownames(df) <- city_names
rownames(x) <- city_names

library("cluster")
# Agglomerative Nesting (Hierarchical Clustering)
res.agnes <- agnes(x = x, # data matrix
                   stand = TRUE, # Standardize the data
                   metric = "euclidean", # metric for distance matrix
                   method = "ward" # Linkage method
)
# DIvisive ANAlysis Clustering
res.diana <- diana(x = x, # data matrix
                   stand = TRUE, # standardize the data
                   metric = "euclidean" # metric for distance matrix
)
fviz_dend(res.agnes, cex = 0.6, k = 5)







# 6.B??l??m PCA ??ncesi ve Sonras?? K-Means, K-Medoids ve Hiyerar??ik K??melemenin Sil??e Skoru Kar????la??t??rmas??

# PCA ??NCES??

# Sil??e skoru hesaplama fonksiyonu
silhouette_score <- function(data, clustering) {
  library(cluster)
  ss <- silhouette(clustering, dist(data))
  mean(ss[, 3])
}





# K-Means (PCA ??ncesi)
kmeans_silhouette_pre <- silhouette_score(df, km.res3$cluster)

# K-Medoids (PCA ??ncesi)
kmedoids_silhouette_pre <- silhouette_score(df, pam.res$clustering)

# Hiyerar??ik (PCA ??ncesi)
hierarchical_clusters_pre <- cutree(res.agnes, k = 5)
hierarchical_silhouette_pre <- silhouette_score(df, hierarchical_clusters_pre)


# PCA SONRASI

# K-Means (PCA Sonras??)
set.seed(123)
pca_kmeans <- kmeans(df.pca, 3, nstart = 25)
kmeans_silhouette_post <- silhouette_score(df.pca, pca_kmeans$cluster)

# K-Medoids (PCA Sonras??)
pca_pam <- pam(df.pca, k = 3)
kmedoids_silhouette_post <- silhouette_score(df.pca, pca_pam$clustering)

# Hiyerar??ik (PCA Sonras??)
pca_hierarchical <- agnes(x = df.pca, metric = "euclidean", method = "ward")
pca_hierarchical_clusters <- cutree(pca_hierarchical, k = 3)
hierarchical_silhouette_post <- silhouette_score(df.pca, pca_hierarchical_clusters)

# Sonu??lar?? Kar????la??t??rma
silhouette_results <- data.frame(
  Model = c("K-Means", "K-Medoids", "Hierarchical"),
  PCA_Pre = c(kmeans_silhouette_pre, kmedoids_silhouette_pre, hierarchical_silhouette_pre),
  PCA_Post = c(kmeans_silhouette_post, kmedoids_silhouette_post, hierarchical_silhouette_post)
)


print(silhouette_results)

# En uygun modelin belirlenmesi
en_uygun_model <- silhouette_results[which.max(silhouette_results$PCA_Post), ]
print(en_uygun_model)
file_path


aggregate(x, by = list(Cluster = km.pca2$cluster), mean)

x$Cluster <- factor(km.pca$cluster, labels = c("High Prosperity", "Medium Prosperity", "Low Prosperity"))
for (i in 1:(ncol(x) - 1)) {
  boxplot(x[, i] ~ x$Cluster, main = paste("Boxplot of", colnames(x)[i], "by Cluster"))
}
sort(x$Cluster)

km.pca
x <- as.data.frame(x)  
x$Cluster <- as.factor(km.pca$cluster)
for (i in 1:ncol(x)) {
  boxplot(x[, i] ~ x$Cluster, 
          main = paste("Boxplot of", colnames(x)[i], "by Cluster"),
          col = rainbow(5))  
}


library(ggplot2)
install.packages("rworldmap")
library(rworldmap)

# Veri haz??rl??????
map_data <- data.frame(Country = file_path$Country.or.region, Cluster = x$Cluster)
map <- joinCountryData2Map(map_data, joinCode = "NAME", nameJoinColumn = "Country")

# Harita ??izimi
mapCountryData(map, nameColumnToPlot = "Cluster", catMethod = "fixedWidth", colourPalette = "heat")
