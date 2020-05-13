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
if (!require("plotly")) install.packages("plotly"); library("plotly") 
if (!require("ggiraph")) install.packages("ggiraph"); library("ggiraph") 
if (!require("ggwordcloud")) install.packages("ggwordcloud"); library("ggwordcloud") 
if (!require("RColorBrewer")) install.packages("RColorBrewer"); library("RColorBrewer") 


FILE_RES <- "results_clustering.rda"
if (!file.exists(FILE_RES)) {
    ## Download the useR_abstracts
    if(!nzchar(system.file(package = "corpus.useR.2008.abstracts"))) {
        templib <- tempfile(); dir.create(templib)
        install.packages("corpus.useR.2008.abstracts", lib = templib,
                         repos = "https://datacube.wu.ac.at/", 
                         type = "source")
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
    
    ## Make frequency matrix
    library("reshape")
    m_cluster <- aggregate(m, by = list(paste("Cluster", clustering)), sum)
    m_cluster
    rownames(m_cluster) <- m_cluster$Group.1
    m_cluster <- m_cluster[, -1]
    long <- reshape(m_cluster, idvar = "Cluster", 
                    ids = row.names(m_cluster),
                    times = names(m_cluster), 
                    timevar = "word",
                    varying = list(names(m_cluster)), 
                    direction = "long")
    colnames(long)[2] <- "freq"
    long <- long[order(long$Cluster, long$freq , decreasing = TRUE), ]
    long$angle <- 90 * sample(c(0, 1), nrow(long), 
                              replace = TRUE, prob = c(60, 40))
    
    save(df, long, file = FILE_RES)
} else {
    load(FILE_RES)
}

centers <- aggregate(cbind(x1, x2) ~ Cluster, df, mean)
centers <- cbind.data.frame(centers, noProjects = c(unname(table(df$Cluster))))

# Define UI for application that draws a histogram
ui <- basicPage(
    titlePanel("Clustering of EUvsVirus Projects"),
    p("In order to select a cluster use the lasso or the box selection tool."),
    plotlyOutput("plot1"),
    verbatimTextOutput("help"),
    sidebarLayout(
      sidebarPanel(
        h3("Selected clusters"),
        DT::dataTableOutput("table1")
      ),
      mainPanel(
        DT::dataTableOutput("table2"),
        h3("Description of projects selected in the above table"),
        DT::dataTableOutput("abstracts"),
        h3("Select clusters in the left side bar table to visualize word clouds."),
        p("After selecting the clusters, press the action button."),
        actionButton("goButton", "Go!"),
        plotOutput("plot3")#, width = "750px", height="500px")
      )
    )
)

server <- function(input, output) {
    ## Plot the cluster centers based on the dimensionality reduction
    output$plot1 <- renderPlotly({
      p <- ggplot(data = centers, aes(x = x1, y = x2, label = Cluster)) +
        geom_point(aes(size = noProjects, alpha=.02)) +  
        scale_size(range = c(1.4, 10), name = "Number of projects") +
        xlab("") + ylab("")
      ggplotly(p, source = "selected_clusters", tooltip=c("noProjects", "Cluster"))  %>% layout(dragmode = "lasso")      
    })  
    
    get_selected_clusters_from_plot <- reactive({
       d <- event_data("plotly_selected", source = "selected_clusters")
      if (!is.null(d) & length(d) != 0) d[d$curveNumber == 0, "pointNumber"] + 1
    })

    selected_clusters_table <- reactive({
        id <- get_selected_clusters_from_plot()
        if (is.null(id)) id <- 1:nrow(centers)
        centers[id,  c("Cluster", "noProjects")]
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
        d <- data_selected_clusters()
        s <- input$table2_rows_selected
        d[s, c("Title", "Abstract")]
    })

    ## Print the text of the documents in the selected clusters
    output$abstracts <- DT::renderDataTable({# renderText({
        data_selected_titles()
    })


    ## generate and plot the word cloud for the selected clusters max 6 groups
    output$plot3 <-  renderPlot({
      input$goButton
      
      # Use isolate() to avoid dependency on input$obs
      isolate({
      s <- input$table1_rows_selected
      ncl <- length(s)
      if (!is.null(s)) {
        max_words <- 25
        sel_cluster <- selected_clusters_2()
        d <- long[long$Cluster %in% sel_cluster$Cluster, ]
        id <- c(sapply(which(!duplicated(d$Cluster)), function(i) i + 0:(max_words - 1)))
        d <- d[id, ]
 
        p <- ggplot(d, aes(label = word, size = freq, angle = angle, col = freq)) + 
          geom_text_wordcloud(rm_outside = TRUE, max_steps = 1,
                              grid_size = 1, eccentricity = .9)+
          scale_color_gradient(low="green3", high="violetred", 
                               trans = "log10",
                               guide = guide_colourbar(direction = "horizontal",
                                                       title.position ="top")) +
          
          scale_size_area(max_size = 24) +
          theme_minimal() + 
          facet_wrap(~ Cluster, nrow = ceiling(ncl/3),
                     ncol = min(ncl, 3))
        p
      }
    })
    })
}

shinyApp(ui, server)