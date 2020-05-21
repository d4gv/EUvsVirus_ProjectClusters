## prepare data for shiny
if (!require("tm")) install.packages("tm"); library("tm") 
if (!require("tsne")) install.packages("tsne"); library("tsne") 
if (!require("slam")) install.packages("slam"); library("slam") 
if (!require("reshape")) install.packages("reshape"); library("reshape") 


## Needs: 1) document term matrix
##        2) original data set
##        3) several files containing clustering results, each with 2 columns: ID (Project URL & clustering)
##
## Outputs: 1) a data frame of word-frequencies for word-cloud
##          2) a data frame with the project titles, descriptions and Cluster assignment

project_DTM <- readRDS("Data/project_DTM.rds") ## DTM
d <- read.delim("Data/all_data.tsv") ## orig data

cl1 <- read.delim("Data/lv_results_hierarchical_clustering.csv", sep = ",")[,-1] # cl1
cl2 <- read.delim("Data/LDA_150Topics_150cosine_average_4app.csv", sep = ",")[,-1] # cl2 

cl <- merge(cl1, cl2, by = "ID", all = FALSE)
colnames(cl)[-1] <- c("method.hc", "method.tm")


for (j in 2:ncol(cl)) {
  cl[, j] <- factor(paste("Cluster", cl[, j]), levels = paste("Cluster", 1:max(cl[, j])))
}
colnames(d)[3] <- "ID"
df <- merge(d, cl, by = "ID")

head(df)

# dimensionality reduction on DTM
tsne_dtm <- tsne(as.matrix(project_DTM))

##  combine all together:
df <- cbind(df, x1 = tsne_dtm[, 1], x2 = tsne_dtm[, 2])

levels(df$method.tm) <- paste("Cluster", 1:nlevels(df$method.tm))
levels(df$method.hc) <- paste("Cluster", 1:nlevels(df$method.hc))
colnames(df)[4:5] <- c("Title", "Description")

## make data frame of most frequent words for generating word clouds
df_word_clouds <- lapply(df[, grepl("method", colnames(df))], function(x) {
  m_cluster <- aggregate(as.matrix(project_DTM), by = list(x), sum)
  rownames(m_cluster) <- m_cluster$Group.1
  m_cluster <- m_cluster[, -1]
  long <- reshape(m_cluster, idvar = "Cluster",
                  ids = row.names(m_cluster),
                  times = names(m_cluster),
                  timevar = "word",
                  varying = list(names(m_cluster)),
                  direction = "long")
  colnames(long)[2] <- "freq"
  long$angle <- 90 * sample(c(0, 1), nrow(long),
                            replace = TRUE, prob = c(60, 40))
  long <- long[order(long$Cluster, long$freq , decreasing = TRUE), ]
  
  ## keep only the most common 30 words and eliminate the top 5
  max_words <- 25
  id <- c(sapply(which(!duplicated(long$Cluster)), function(i) i + 4:(max_words+5 - 1)))
  long[id, ]
})

save(df, df_word_clouds, file = "data_shiny_app.rda")
