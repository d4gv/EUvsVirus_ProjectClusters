## prepare data for shiny
if (!require("tm")) install.packages("tm"); library("tm") 
if (!require("Rtsne")) install.packages("Rtsne"); library("Rtsne") 
if (!require("slam")) install.packages("slam"); library("slam") 
if (!require("reshape")) install.packages("reshape"); library("reshape") 


## Needs: 1) document term matrix
##        2) original data set
##        3) several files containing clustering results, each with 2 columns: ID (Project URL & clustering)
##
## Outputs: 1) a data frame of word-frequencies for word-cloud
##          2) a data frame with the project titles, descriptions and Cluster assignment

# Load all data 
d <- read.delim("Data/all_data.tsv") ## orig data

# Generate the top 5 and top 10 words per document
project_titles_text <- apply(d[,c("title", "text")],
                             1, paste, collapse = " ")

## Build a corpus using the `Corpus` function: 
project_corpus <- Corpus(VectorSource(project_titles_text))

## Build Doc Term Matrix without stemming
project_DTM_no_stem <- DocumentTermMatrix(project_corpus,
                                  control = list(
                                    tokenize = "MC",
                                    stemming = FALSE, 
                                    stopwords = TRUE,
                                    removeNumbers = TRUE, 
                                    removePunctuation = TRUE,
                                    wordLengths = c(3, Inf)))

project_DTM_no_stem <- weightTfIdf(project_DTM_no_stem)
dim(project_DTM_no_stem)

## top words for each document
no_top <- c(5, 10)
top_words <- lapply(no_top, function(top)
  apply(as.matrix(project_DTM_no_stem), 1, function(x){
    w <- sort(x, decreasing = TRUE)[seq_len(top)]
    paste(names(w), collapse = ", ")    
  }))
names(top_words) <- paste("Top", no_top)

d <- cbind.data.frame(d, do.call("cbind", top_words))

# Turn top words into character instead of factor, so they become searchable as words in data table
df[["Top 5"]] <- as.character(df[["Top 5"]])
df[["Top 10"]] <- as.character(df[["Top 10"]])

## read in clustering results

cl1 <- read.delim("Data/lv_results_hierarchical_clustering.csv", sep = ",")[,-1] # cl1
cl2 <- read.delim("Data/rw_LDA_150Topics_150cosine_average_4app.csv", sep = ",")[,-1] # cl2 
cl3 <- read.delim("Data/ja-clust100.csv", sep = ",")[,-1] # cl3
cl3$cl <- cl3$cl + 1 # has cluster number 0
cl4 <- read.delim("Data/ja-clust200.csv", sep = ",")[,-1] # cl4 
cl4$cl <- cl4$cl + 1

## all clusters
cl <- suppressWarnings(merge(merge(merge(cl1, cl2, by = "ID", all = FALSE), 
                                   cl3,  by = "ID"), 
                             cl4,  by = "ID"))

colnames(cl)[-1] <- c("method.hc1", "method.tm", "method.hc2", "method.hc3")

colnames(d)[3] <- "ID"
df <- merge(d, cl, by = "ID")

str(df)

# Generate DTM matrix
project_corpus_2 <-  Corpus(VectorSource(apply(df[,c("title", "text")], 1, paste, collapse = " ")))
## Build Doc Term Matrix with stemming
project_DTM <- DocumentTermMatrix(project_corpus_2,
                                          control = list(
                                            tokenize = "MC",
                                            stopwords = TRUE,
                                            stemming = TRUE,
                                            removeNumbers = TRUE, 
                                            removePunctuation = TRUE,
                                            wordLengths = c(3, Inf)))


project_DTM <- weightTfIdf(project_DTM)
project_DTM <- removeSparseTerms(project_DTM, 0.99)

str(project_DTM)
dup_id <- which(duplicated(project_DTM))
if (length(dup_id) != 0) {
  df <- df[-which(duplicated(project_DTM)), ]
  project_DTM <- project_DTM[-which(duplicated(project_DTM)), ]
}

# dimensionality reduction on DTM 
tsne_dtm <- Rtsne(as.matrix(project_DTM), max_iter = 2000L, check_duplicates = FALSE)

##  combine all together:
df <- cbind(df, x1 = tsne_dtm$Y[, 1], x2 = tsne_dtm$Y[, 2])

for (i in grep("method", colnames(df))) {
  df[, i] <- factor(paste("Cluster", df[, i]), 
                    levels = paste("Cluster", sort(unique(df[, i]))))
 
}

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
