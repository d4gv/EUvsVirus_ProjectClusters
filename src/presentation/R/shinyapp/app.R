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
if (!require("tidyverse")) install.packages("tidyverse"); library("tidyverse") 
if (!require("plotly")) install.packages("plotly"); library("plotly") 
if (!require("ggwordcloud")) install.packages("ggwordcloud"); library("ggwordcloud") 
if (!require("RColorBrewer")) install.packages("RColorBrewer"); library("RColorBrewer") 
if (!require("shinycssloaders")) install.packages("shinycssloaders"); library("shinycssloaders")
if (!require("rintrojs")) install.packages("rintrojs"); library("rintrojs")

load(file = "data_shiny_app.rda")

# add the clickable project URL
df$ProjectLink <- paste0('<a href = "', df$ID , '", target = "blank">', df$ID, '</a>')

# Define UI for application that draws a histogram
ui <- basicPage(
  introjsUI(),
  titlePanel("Clustering of EUvsVirus Projects"),
  helpText(p("This app and the models presented in it are the result of a group effort by Jillian Augustine, Thomas Treml, Laura Vana, and Rania Wazir, of ",
             a("VDSG/data4good", href = "https://www.vdsg.at/data4good", target = "blank"), ", and ",
             a("R-Ladies Vienna.", href = "https://www.meetup.com/rladies-vienna/", target = "blank"))),
  br(),
  helpText("Select a clustering method. Each method delivers a different grouping of the projects."),
  helpText("NOTE: Currently, Models (3) and (4) are the same.  A new model will replace (4) shortly."),
  fluidRow(
    column(width = 4,
           introBox(
             selectInput("method", "Select method:",
                         c("Model 1 (LDA + hierarchical clustering)" = "tm",
                           "Model 2 (Hierarchical clustering)" = "hc1",
                           "Model 3 (Hierarchical clustering)" = "hc2",
                           "Model 4" = "hc3")),
             data.step = 2,
             data.intro = "If you've read the 'What do these clusters mean?' document, you'll know that
             different methods lead to different project groupings.  
             Select one grouping method (= Model) to explore here",
             data.position = "right"
           )
    ),
    column(width = 3, offset = 1,
           br(),
           introBox(
             actionButton("helpTM", "What do these clusters mean?", class = "btn-warning"),  # classes: warning, info, success, danger, primary
             data.step = 1,
             data.intro = "Click here for a brief description of topic models, and what they are being used for in this project."
           )
    ),
    column(width = 3, offset = 1,
           br(),
           actionButton("help", "Help! How does this app work?", class = "btn-info")
    )
  ),
  hr(),
  br(),
  textOutput("selected_method"),
  br(),
  introBox(withSpinner(
    plotlyOutput("plot1"),     
    type = 6),
    data.step = 3,
    data.intro = "The project groupings will be displayed here.  
             If you place your mouse anywhere inside the plot, you will see a tool box appear in the upper right hand corner.
             Use either the 'Lasso Select', or the 'Box Select' tools in the upper right hand corner to select the 
             clusters you want to investigate - they will appear in the table below on the left.  Notice also that, as you
              hover over any point in the plot, a pop-up will tell you the Cluster Number, 
              and how many projects are in that cluster.",
    data.position = "top"
  ), 
  fluidRow(
    column(3, offset = 1, style = "background-color:#F8F8FF;",
           h3("Clusters"),
           helpText("Clusters can also be selected in this table by clicking on the respective rows.  
                    Clusters selected in this table will be marked with a red dot in the plot above."),
           introBox(
             DT::dataTableOutput("table1"),
             data.step = 4,
             data.intro = "Any clusters selected in the plot above will be visible here.  
      If no clusters were selected in above, all clusters will appear here.  
      You can filter clusters by using the slider input under the column heading 'Cluster', 
      or by simply typing in the numeric range you wish to select, 
      as for example '3 ... 8' would select clusters 3 through 8",
             data.position = "top"
           )
    ),
    column(8,
           h3("Visualization of common words"),
           helpText("Select clusters in the left side table to visualize the corresponding word clouds. 
               After selecting the clusters in the side table, press the action button."),
           introBox(
             actionButton("goButton", "Go!"),
             data.step = 5,
             data.intro = "To see the word clouds associated with particular clusters, make sure to select the desired
      clusters in the table on the left, and then press the button 'Go'.  The word clouds are just visualizations of
             the most frequently occurring words within each cluster of projects."
           ),
           plotOutput("plot3") %>%
             withSpinner(type = 4) #width = "750px", height="500px" , height = "600px"
    )
  ),
  hr(),
  fluidRow(
    column(10, offset = 1, style = "background-color:#DCDCDC;",
           h3("Projects"),
           fluidRow(
             column(3,
                    helpText("This table contains information on all projects in the clusters selected in the table above. 
               If no cluster is selected above, then this table will contain all projects."),
                    helpText("Please note that the download button only downloads the data currently visible on the screen.  
                    To download all data, make sure 'Show entries' is set to 'All'."),
                    br()
             ),  
             column(8, offset = 1,
                    helpText("The search bar can be used for searching terms in the project title and project description."),
                    helpText("What if you want to search for more than one word? Typing in the expression 'open source' will look
                    for projects that have the two words 'open' and 'source' anywhere in the project title or description 
                    (not necessarily right next to each other).  Typing in 'open|source' will search for projects that
                     contain either 'open' or 'source'.  You can use the search windows under each column heading to 
                    do more specific searches eg. type in the word 'resource' in the 'Top 5' column to find 
                    projects where 'resource' is one of the 5 most frequently occurring words."),
                    helpText("For those who want to do more advanced searches, the search bar accepts JavaScript-based regular expressions."),
                    br()
             )
           ),
           introBox(DT::dataTableOutput("table2") %>%
                      withSpinner(type = 6),
                    data.step = 6,
                    data.intro = "This table contains information on the projects in the clusters selected in the
                      table above.  Rather than first selecting clusters in the plot at the top of the page, you 
                      could also begin your whole investigation here.  Is there one particular
                      project that you would like to start with, and explore what other projects might be related?  Then
                      search for its name in the 'Title' column, figure out what cluster it belongs to, select the cluster
                      in the table above, and come back and explore all other projects in the same cluster.  Or perhaps you
                      are interested in a particular concept or word.  Search for it in the general search box 
                      on the right - this will find all projects that contain the word anywhere in title or description. 
                      Or else make a search among the Top 5 or Top 10 words - this will pull up all projects that contain 
                      this word in their list of most important words.")
    )
  )
)


server <- function(input, output, session) {
  
  ## Global Variables:  
  markersAdded <- FALSE  # Keep track of when markers have been added to plot, so they can be updated with plotlyProxy
  
  NumGroups <- reactive({  # how many clusterings are there?
    nlevels( factor(df[, grepl(input$method, colnames(df))]))
  })
  
  ## Plot the cluster centers based on the dimensionality reduction
  output$selected_method <- renderText({
    if (!is.null(input$method)) {
      method <- switch(input$method,
                       "tm" = "Topic models",
                       "hc1"=  "Hierarchical clustering (1)",
                       "hc2"=  "Hierarchical clustering (2)",
                       "hc3"=  "Hierarchical clustering (3)")      
      
      sprintf("The %s method delivers %i clusters or groups. For this grouping, you can now select clusters for inspection in the figure below by using the lasso or the box selection tool. Clusters 
              are represented by points: The size of the points is proportional to the number of projects in each cluster; the distance between points is an indicator of how 
              similar the clusters are to each other.",
              method, NumGroups())
    }
  })
  
  get_data_selected_method <- reactive({
    df$ClusterName <- df[, grepl(input$method, colnames(df))]
    df <- df[, !grepl("method", colnames(df))]
    df <- df %>%
      mutate(Cluster = str_extract(ClusterName, "\\d+") %>%
               as.integer())
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
    centers <- aggregate(cbind(x1, x2) ~ Cluster + ClusterName, d, mean)
    centers <- cbind.data.frame(centers, NumberOfProjects = c(unname(table(d$Cluster))))
    centers
  })
  
  plot1Obj <- reactive({
    p <- ggplot(data =  get_centers(), aes(x = x1, y = x2, label = Cluster)) +
      geom_point(aes(size = NumberOfProjects, alpha=.02), color = "blue") +
      scale_size(range = c(1.4, 10), name = "Number of projects") +
      xlab("") + ylab("")
    
    ggplotly(p, source = "selected_clusters", tooltip=c("size", "label"))  %>% 
      layout(dragmode = "lasso")
  })
  
  output$plot1 <- renderPlotly({
    p <- plot1Obj()
  })
  
  observeEvent(selected_clusters_2(), {
    lenTable <- nrow(selected_clusters_table())
    d <- selected_clusters_2()

    s <- input$table1_rows_selected
    if (is.null(s)) noneSelected <- TRUE else noneSelected <- FALSE
    if (nrow(d) > 0 & !noneSelected) {
      
      centers <- get_centers() %>%
        filter(Cluster %in% d$Cluster)
      
     if (nrow(centers) == 1) centers <- bind_rows(centers, centers) # because plotProxy has trouble handling only one point

      # delete the old traces, if they exist  
     if (markersAdded == TRUE) {
       plotlyProxy("plot1", session, deferUntilFlush = FALSE) %>%
         plotlyProxyInvoke("deleteTraces", list(-1))
     }

      
      plotlyProxy("plot1", session, deferUntilFlush = FALSE) %>%
        plotlyProxyInvoke("addTraces", 
                          list(x = centers$x1, # need at least 2 points
                               y = centers$x2,
                               type = "scatter",
                               mode = "markers",
                               marker = list(size = centers$NumberOfProjects, 
                                             sizeref = 0.5, 
                                             sizemode = "area", 
                                             symbol = 0, 
                                             opacity = 0.8,
                                             color = "red")

                          )
        )
      
      markersAdded <<- TRUE
    } else if (markersAdded == TRUE) {
      plotlyProxy("plot1", session, deferUntilFlush = FALSE) %>%
        plotlyProxyInvoke("deleteTraces", list(-1))
      markersAdded <<- FALSE
    }
    
  })
  
  get_selected_clusters_from_plot <- reactive({
    req(plot1Obj())
    d <- event_data("plotly_selected", source = "selected_clusters")
    if (!is.null(d) & length(d) != 0) d[d$curveNumber == 0, "pointNumber"] + 1
  })
  
  selected_clusters_table <- reactive({
    centers <- get_centers()
    id <- get_selected_clusters_from_plot()
    if (is.null(id)) id <- 1:nrow(centers)
    centers[id,  c("ClusterName", "Cluster", "NumberOfProjects")]
  })
  
  output$table1 <- DT::renderDataTable({
    selected_clusters_table()
  },
  filter = "top",
  options = list(dom = "ltipr",
                 pageLength = 10,
                 lengthMenu = list(c(10, 20, 50, -1), c("10", "20", "50", "All")),
                 columnDefs = list(list(visible = FALSE, targets = c(0, 1))))
  )
  
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
    tmp <- df[df$Cluster %in% d$Cluster, c("ClusterName", "Title", "ProjectLink", "Description", "Challenge", "SubChallenge", "Top 5", "Top 10")]
  },
  filter = "top",
  escape = c(TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE),
  extensions = c("Buttons", 'RowGroup'),
  options = list(rowGroup = list(dataSrc = 1), 
                 dom = "Bfltipr",
                 search = list(regex = TRUE),
                 buttons = list(list(extend = 'copy', exportOptions = list(columns = ':visible')), # this way, we only download visible columns
                                list(extend = 'csv', exportOptions = list(columns = ':visible')),
                                list(extend = 'excel', exportOptions = list(columns = ':visible')),
                                list(extend = 'pdf', exportOptions = list(columns = ':visible'))
                                ),
                 exportOptions = list(columns = ':visible'),
                 pageLength = 10,
                 lengthMenu = list(c(10, 20, 50, -1), c("10", "20", "50", "All")),
                 columnDefs = list(list(visible = FALSE, targets = c(0, 1, 4)))))
  
  
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
        d <- long[long$Cluster %in% sel_cluster$ClusterName, ]
        
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
  
  # start a modal Dialog (to describe briefly what the topic models are) when helpTM button is pressed
  observeEvent(input$helpTM, {
    showModal(modalDialog(
      title = "What do these clusters mean?",
      h3("Introduction"),
      p("The purpose of this visualization is to help detect similarities between projects.  The approach used here
      is based on a combination of pre-processing, topic modeling and clustering of the projects from their 
        descriptions (as posted on devpost)."),
      h3("Pre-processing in brief:"),
      p("We first take the descriptions, and consider them simply as a 'bag of words' - meaning just a
      set of words, without paying attention to order.  These 'bags' are 'cleaned' - this includes getting rid of
      punctuation, one-letter words, words that occur too frequently across all projects, or words that occur 
      in only a few contexts.  I am being purposely vague - exactly which words are 'cleaned' in this process, what is 
      meant by 'frequently' or 'too few', are parameters that can vary from model to model. 
      We can also assign weights to the words, depending on how frequently a word occurs occur within 
      one project description, versus how frequently it occurs across all projects.  (To understand why this can be
      important, consider words such as 'corona' or 'EU' which will probably occur very frequently across all projects, and
      will therefore not be very useful in distinguishing between project topics).  Whether we choose to weight words, and
        exactly how we do it, is something else that can vary between models."),  
      h3("The essence of Topic Modeling:"),
      p("After the text descriptions have been cleaned, we consider the co-occurrences of words across projects.
      What's a co-occurrence? Two words co-occur if they both appear in the same document (project description).
      The basic assumption of topic modeling is that words that frequently co-occur across various project descriptions 
      actually belong to the same 'topic'.  We therefore try to convert information about word co-occurrences 
      into a list of topics: each topic is essentially just a list of words that appeared together 
      frequently.  There are different procedures
      for extracting topic information from word co-occurrences, and this leads to different topic configurations.
      Whatever the procedure adopted for finding the topics, once we have them, 
      we go back to the project descriptions, and determine 
      which topics belong to each project description - we will call this the topic distribution of the projects."),
      h3("Clustering made easy:"),
      p("The final step in the modeling process is to group the projects by similarities in their topic distributions.  
      There are many ways to define and measure 'similarity', so again, depending on the choices made, the 
      final results will vary.
      Each group of projects thus determined to be 'similar' is called a cluster - and these are the points that 
      can be seen in the plot."),
      h3("Why is there more than one model to choose from?"),
      p("Pre-processing, topic modeling and clustering are fundamentally mathematical/statistical procedures.  In order
        to transform text into objects that can be manipulated mathematically, many choices and assumptions have to be made.  
        I have tried to point out some of the sources of these choices above.
        Different assumptions and transformations result in different models - and since it is not a priori
        clear which is the 'best', we present here several possibilities, for you to explore the ones that will work
        best for you."),
      h3("How should I use this app?"),
      p("This is meant as a tool for exploration of the EUvsVirus projects.  If you already have a project or 
        specific terms that you want to explore, a 'bottom-up' approach works best.  
        Scroll down to the project table at the bottom, search for 
        the project or term, and determine the clusters they belong to.  From there, you can proceed to
        explore the clusters, and those nearest to them (as determined by the graph).  Try this with 
        different models, as they will each deliver different clusters, and some will work better than others."),
      p("Alternatively, you can start with the plot, and seek out moderately-sized clusters (this is subjective - how 
        many projects would you like to look at in one shot? 5? 10? 50?), or groups of 
        clusters - to see what projects they contain, and if a significant number of them are related.  
        Once you have familiarized yourself with the tool, you will certainly come up with your own way 
        to explore projects with it!"),
      size = "l",
      easyClose = TRUE
    ))
  })
  
  # start introjs when help button is pressed with custom options and events
  observeEvent(input$help,
               introjs(session, options = list("nextLabel" = "Next",
                                               "prevLabel" = "Previous",
                                               "skipLabel" = "Done"),
                       events = list("oncomplete" = I('alert("Tour of app complete")'))
               )
  )
  
}

shinyApp(ui, server)