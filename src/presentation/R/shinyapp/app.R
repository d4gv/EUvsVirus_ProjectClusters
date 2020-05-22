#
# This is a Shiny web application for visualizing the clustering results of the EUvsVirus Hackathon projects
# To generate the app press the 'Run App' button above.
#
# This app is published on: https://rania.shinyapps.io/euvsvirus_collab/
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
if (!require("ggplot2")) install.packages("ggplot2"); library("ggplot2") 
if (!require("plotly")) install.packages("plotly"); library("plotly") 
if (!require("ggwordcloud")) install.packages("ggwordcloud"); library("ggwordcloud") 
if (!require("RColorBrewer")) install.packages("RColorBrewer"); library("RColorBrewer") 


load(file = "data_shiny_app.rda")

# add the clickable project URL
df$TitleLink <- paste0('<a href = "', df$ID , '", target = "blank">', df$Title, '</a>')

# Define UI for application that draws a histogram
ui <- basicPage(
  titlePanel("Clustering of EUvsVirus Projects"),
  helpText(p("This app and the models presented in it are the result of a group effort by Jillian Augustine, Thomas Treml, Laura Vana, and Rania Wazir, of ",
             a("VDSG/data4good", href = "https://www.vdsg.at/data4good", target = "blank"), ", and ",
             a("R-Ladies Vienna.", href = "https://www.meetup.com/rladies-vienna/", target = "blank"))),
  br(),
  helpText("Select a clustering method. Each method delivers a different grouping of the projects."),
  selectInput("method", "Select method:",
              c("Topic models" = "tm",
                "Hierarchical clustering (1)" = "hc1",
                "Hierarchical clustering (2)" = "hc2",
                "Hierarchical clustering (3)" = "hc3")),
  hr(),
  br(),
  textOutput("selected_method"),
  plotlyOutput("plot1"),
  # verbatimTextOutput("help"),
  fluidRow(
    column(3, offset = 1, style = "background-color:#F8F8FF;",
      h3("Clusters"),
      helpText("Clusters can also be selected in this table by clicking on the respective rows."),
      DT::dataTableOutput("table1")
    ),
    column(8,
      h3("Visualization of common words"),
      helpText("Select clusters in the left side bar table to visualize word clouds. 
               After selecting the clusters in the side bar table, press the action button."),
      actionButton("goButton", "Go!"),
      plotOutput("plot3", height = "600px") #width = "750px", height="500px"
      )
    ),
  hr(),
  fluidRow(
    column(10, offset = 1, style = "background-color:#DCDCDC;",
    h3("Projects"),
    helpText("This table contains the title of all projects in the clusters in the table above. 
               If no cluster is selected this table contains all projects.  
               The search bar can be used for searching terms in the project title and project description."),
    DT::dataTableOutput("table2")
    )
  )
)


server <- function(input, output) {
  ## Plot the cluster centers based on the dimensionality reduction
  output$selected_method <- renderText({
    if (!is.null(input$method)) {
      method <- switch(input$method,
                       "tm" = "Topic models",
                       "hc1"=  "Hierarchical clustering (1)",
                       "hc2"=  "Hierarchical clustering (2)",
                       "hc3"=  "Hierarchical clustering (3)")      
      
      sprintf("You have selected the method %s. The method delivers %i clusters or groups. For this grouping, you can now select clusters for inspection in the figure below by using the lasso or the box selection tool. The size of the points is proportional to the number of projects in each cluster.",
              method, nlevels( factor(df[, grepl(input$method, colnames(df))])))
    }
  })
  
  get_data_selected_method <- reactive({
    df$Cluster <- df[, grepl(input$method, colnames(df))]
    df <- df[, !grepl("method", colnames(df))]
    df[order(df$Cluster), ]
  })
  
  agg_cluster_project_DTM <- reactive({
    df_word_clouds[grepl(input$method, names(df_word_clouds))][[1]]
  })
  output$help <- renderPrint({
    str(agg_cluster_project_DTM())
    
  })
  get_centers <- reactive({
    d <-   get_data_selected_method()
    centers <- aggregate(cbind(x1, x2) ~ Cluster, d, mean)
    centers <- cbind.data.frame(centers, noProjects = c(unname(table(d$Cluster))))
    centers
  })
  
  output$plot1 <- renderPlotly({
    p <- ggplot(data =  get_centers(), aes(x = x1, y = x2, label = Cluster)) +
      geom_point(aes(size = noProjects, alpha=.02), color = "blue") +
      scale_size(range = c(1.4, 10), name = "Number of projects") +
      xlab("") + ylab("")

    ggplotly(p, source = "selected_clusters", tooltip=c("size", "label"))  %>% 
      layout(dragmode = "lasso")
  })
  

  get_selected_clusters_from_plot <- reactive({
    d <- event_data("plotly_selected", source = "selected_clusters")
    if (!is.null(d) & length(d) != 0) d[d$curveNumber == 0, "pointNumber"] + 1
  })
  
  selected_clusters_table <- reactive({
    centers <- get_centers()
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
  
  
  #  highlight selected rows and print them
  output$table2 <- DT::renderDataTable({
    d <- selected_clusters_2()
    df <- get_data_selected_method()
    tmp <- df[df$Cluster %in% d$Cluster, c("Cluster", "TitleLink", "Description", "Challenge", "SubChallenge")]
  },
  escape = c(TRUE, TRUE, FALSE, TRUE, TRUE, TRUE),
  extensions = 'RowGroup',
  options = list(rowGroup = list(dataSrc = 1), 
                 pageLength = 10,
                 lengthMenu = list(c(10, 20, 50, -1), c("10", "20", "50", "All")),
                 columnDefs = list(list(visible = FALSE, targets = c(0, 1, 3)))))
  
  ## Documents in the selected clusters
  data_selected_clusters <- reactive({
    df <- get_data_selected_method()
    sel_cluster <- selected_clusters_2()
    subset(df, Cluster %in% sel_cluster$Cluster)
  })
  
  
  ## generate and plot the word cloud for the selected clusters max 6 groups
  output$plot3 <-  renderPlot({
    input$goButton
    # Use isolate() to avoid dependency
    isolate({
      s <- input$table1_rows_selected
      ncl <- length(s)
      if (!is.null(s)) {
        sel_cluster <- selected_clusters_2()
        long <- agg_cluster_project_DTM()
        d <- long[long$Cluster %in% sel_cluster$Cluster, ]
        
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
        suppressWarnings(p)
      }
    })
  })
}

shinyApp(ui, server)