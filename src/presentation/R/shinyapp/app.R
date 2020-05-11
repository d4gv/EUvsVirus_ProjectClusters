#
# This is a Shiny web application for visualizing the clustering results of the EUvsVirus Hackathon projects
# To generate the app press the 'Run App' button above.
#
# The app consists of five panes: 
#    1) Figure 1: A two dimensional visualization of the clusters
#       * For this visualization the T-Distributed Stochastic Neighbor Embedding  (TSNE)
#         has been used as a dimensionality reduction tool on the Document term matrix. 
#       * The points represented in the graphs represent the average values of the two dimensions 
#         obtained by TSNE for each cluster (a sort of cluster centers).
#       * Alternatives: Multidimensional scaling on a distance matrix etc.
#       * The size of the points is proportional to the number of projects in each cluster.
#       * Clusters can be selected by clicking on the respective clusters or by brushing the graph (thus selecting more clusters).
#
#    2) Table 1: A table with descriptives for each clusters
#       * Contains the cluster number, the number of projects in each cluster (maybe number of challenges/sub-challenges under which the projects were submitted).
#       * If no cluster is selected from Figure 1, this table contains all clusters. Otherwise it contains the clusters selected in the graph.
#
#    3) Table 2: A table which contains information on the projects selected in Table 1. 
#       * Contains information on Cluster, Name of the project, maybe Challenge and Sub-challenge, Winner/No Winner
#       * Cluster selection in Table 1 happens when clicking on the respective row of the table. 
#       * If no row is clicked on, all projects in the clusters in Table 2 are shown.
# 
#    4) Table 3: A table containing the title and the text description of the projects selected in Table 2.
#       * Projects can be selected in Table 2 by clicking on the respective row of the table. 
# 
#    5) Figure 2: Word clouds for the clusters selected in Table 1
#       * Cluster selection in Table 1 happens when clicking on the respective row of the table. Maximum 6 clusters can be selected for the visualization.
#       * If no row is clicked on, no word clouds are generated.
# 

library(shiny)
if (!require("DT")) install.packages("DT"); library("DT") 

if (!require("tm")) install.packages("tm"); library("tm") 
if (!require("movMF")) install.packages("movMF"); library("movMF") 
if (!require("tsne")) install.packages("tsne"); library("tsne") 
if (!require("slam")) install.packages("slam"); library("slam") 
if (!require("ggplot2")) install.packages("ggplot2"); library("ggplot2") 
if (!require("ggiraph")) install.packages("ggiraph"); library("ggiraph") 
if (!require("wordcloud")) install.packages("wordcloud"); library("wordcloud") 

FILE_RES <- "results_clustering.rda"
if (!file.exists(FILE_RES)) {
    ## Download the useR_abstracts
    if(!nzchar(system.file(package = "corpus.useR.2008.abstracts"))) {
        templib <- tempfile(); dir.create(templib)
        install.packages("corpus.useR.2008.abstracts", lib = templib,
                         repos = "https://datacube.wu.ac.at/", type = "source")
        data("useR_2008_abstracts", package = "corpus.useR.2008.abstracts",
             lib.loc = templib)
    } else {
        data("useR_2008_abstracts", package = "corpus.useR.2008.abstracts")
    }

    abstracts_titles <- apply(useR_2008_abstracts[,c("Title", "Abstract")],
                              1, paste, collapse = " ")

    ## Corpus
    useR_2008_abstracts_corpus <- Corpus(VectorSource(abstracts_titles))
    ## Document term matrix
    useR_2008_abstracts_DTM <- DocumentTermMatrix(useR_2008_abstracts_corpus,
                                                  control = list(
                                                      tokenize = "MC",
                                                      stopwords = TRUE,
                                                      stemming = TRUE,
                                                      wordLengths = c(3, Inf)))
    ColSums <- col_sums(useR_2008_abstracts_DTM > 0)
    # sort(ColSums, decreasing = TRUE)[1:10]
    # summary(ColSums)
    useR_2008_abstracts_DTM <- useR_2008_abstracts_DTM[, ColSums >= 5 & ColSums <= 90]

    best_model <- movMF(useR_2008_abstracts_DTM,
                        k = 10, nruns = 20,
                        kappa = list(common = TRUE))

    clustering <- predict(best_model)

    ## must restrict m to have most frequent words for the TSNE!!
    ## otherwise it wont converge
    m <- as.matrix(useR_2008_abstracts_DTM)

    # dimensionality reduction
    tsne_m <- tsne(m)

    ##  combine all together:
    df <- cbind.data.frame(m, x1 = tsne_m[, 1], x2 = tsne_m[, 2],
                           Title   = useR_2008_abstracts[,"Title"],
                           Abstract   = useR_2008_abstracts[,"Abstract"],
                           Cluster = paste("Cluster", clustering)
                           )
    df$Cluster <- factor(df$Cluster,
                         levels = paste("Cluster", 1:max(clustering)))
    df <- df[order(df$Cluster), ]
    save(df, file = FILE_RES)
} else {
    load(FILE_RES)
}

centers <- aggregate(cbind(x1, x2) ~ Cluster, df, mean)
centers <- cbind.data.frame(centers, noProjects = c(unname(table(df$Cluster))))

make_freq_mat <- function(m) { 
  v <- sort(colSums(m), decreasing = TRUE)
  data.frame(word = names(v), freq = v)
}

# Define UI for application that draws a histogram
ui <- basicPage(
    titlePanel("Clustering of EUvsVirus Projects"),
    p("In order to select a cluster click or use the lasso selection tool."),
    ggiraphOutput("plot1",  width = "2000px", height="1200px"),
    sidebarLayout(
      sidebarPanel(
        h3("Selected clusters"),
        DT::dataTableOutput("table1")
      ),
      mainPanel(
        DT::dataTableOutput("table2"),
        h3("Description of projects selected in the above table"),
        DT::dataTableOutput("abstracts"),
        h3("Select maximum 6 clusters to visualize word clouds."),
        plotOutput("plot2", width = "750px", height="500px")
      )
    )
)

server <- function(input, output) {
    ## Plot the cluster centers based on the dimensionality reduction
    output$plot1 <- renderggiraph({
      gg_point = ggplot(data = centers) +
        geom_point_interactive(aes(x = x1, y = x2, size = noProjects,
                                   tooltip = Cluster, data_id = Cluster)) + 
        scale_size(range = c(1.4, 10), name = "Number of projects") + 
        theme(legend.position="none")
      
      girafe(ggobj = gg_point, width_svg = 10, height_svg = 10)
    })  
    
    ## Selected clusters
    selected_clusters_in_plot <- reactive({
      subset(centers, Cluster %in% input$plot1_selected)
    })

    selected_clusters_table <- reactive({
        d <- selected_clusters_in_plot()
        if (nrow(d) == 0){
            centers[c("Cluster", "noProjects")]
        } else {
            d[, c("Cluster", "noProjects")]
        }
    })

    output$table1 <- DT::renderDataTable({
        selected_clusters_table()
    })

    selected_clusters_2 <- reactive({
        d <- selected_clusters_table()
        s <- input$table1_rows_selected
        if (is.null(s)) s <- 1:nrow(d)
        d[s, ]
    })

    # highlight selected rows and print them
    output$table2 <- DT::renderDataTable({
        d <- selected_clusters_2()
        tmp <- subset(df, Cluster %in% d$Cluster)[, c("Cluster", "Title")]
        tmp
    })

    ## Documents in the selected clusters
    data_selected_clusters <- reactive({
        sel_cluster <- selected_clusters_2()
        subset(df, Cluster %in% sel_cluster$Cluster)
    })

    ## Documents in the selected titles
    data_selected_titles <- reactive({
        sel_cluster <- selected_clusters_2()
        d <- data_selected_clusters()
        s <- input$table2_rows_selected
        d[s, c("Title", "Abstract")]
    })

    ## Print the text of the documents in the selected clusters
    output$abstracts <- DT::renderDataTable({# renderText({
        data_selected_titles()
    })


    ## generate and plot the word cloud for the selected clusters max 6 groups
    output$plot2 <-  renderPlot({
        s <- input$table1_rows_selected
        if (is.null(s)) {

        } else {
            gmax <- 6
            d <- data_selected_clusters()
            dl <- split(d, factor(d$Cluster))
            ncl <- length(dl)
            if (ncl > gmax) dl <- dl[seq_len(gmax)]
            par(mfrow = c(ceiling(ncl / 3), min(ncl, 3)))
            #par(mfrow = c(1,3))
            lapply(dl, function(dat){
                freq_df <- make_freq_mat(dat[, -(ncol(dat) - 0:4)])
                suppressWarnings(wordcloud(words = freq_df$word, freq = freq_df$freq,
                                           max.words = 100, random.order=FALSE,
                                           rot.per=0.35,
                                           colors=brewer.pal(8, "Dark2")))
            })
        }

    })
    
}

shinyApp(ui, server)