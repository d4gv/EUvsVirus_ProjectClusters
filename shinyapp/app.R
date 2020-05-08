#
# This is a Shiny web application for visualizing the clustering results of the EUvsVirus Hackathon projects
# To generate the app press the 'Run App' button above.
#


library(shiny)
if (!require("DT")) install.packages("DT"); library("DT") 

if (!require("tm")) install.packages("tm"); library("tm") 
if (!require("movMF")) install.packages("movMF"); library("movMF") 
if (!require("tsne")) install.packages("tsne"); library("tsne") 
if (!require("slam")) install.packages("slam"); library("slam") 
if (!require("ggplot2")) install.packages("ggplot2"); library("ggplot2") 
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
    p("In order to select a cluster, click or brush over the graph below."),
    plotOutput("plot1",
               click = "plot_click",
               dblclick = "plot_dblclick",
               hover = "plot_hover",
               brush = "plot_brush"),
    sidebarLayout(
        #2("Clusters and project titles in the selected clusters"),
        sidebarPanel(
               h3("Selected clusters"),
               dataTableOutput("table1")
        ),
        mainPanel(
               dataTableOutput("table2"),
               h3("Description of projects selected in the above table"),
               dataTableOutput("abstracts"),
               h3("Select maximum 6 clusters to visualize word clouds."),
               plotOutput("plot2", width = "750px", height="500px")
        )
    )
)

server <- function(input, output) {
    ## Plot the cluster centers based on the dimensionality reduction
    output$plot1 <- renderPlot({
        ggplot(centers, aes(x = x1, y = x2, size = noProjects))  +
            geom_point(alpha = 0.7) +
            scale_size(range = c(1.4, 10), name = "Number of projects") + 
            theme(legend.position="none")
    })
        
    ## Selected clusters
    selected_clusters_in_plot <- reactive({
        tmpclick   <- nearPoints(centers, input$plot_click, 
                                 threshold = 10, maxpoints = 1, addDist = TRUE)
        tmpbrushed <- brushedPoints(centers, input$plot_brush, xvar = "x1", 
                                    yvar = "x2")
        if (nrow(tmpbrushed) > 0) tmpbrushed else (tmpclick)
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