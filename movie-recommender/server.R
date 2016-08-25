# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyServer(function(input, output) {
  
  getRecommendationTable <- reactive({
    ## We'll need the movie titles
    titles <- movies %>%
      mutate(movie_id = id) %>%
      select(movie_id, title)
    
    ## This takes into consideration the genre filter
    include <- plot.data() %>%
      filter(stroke ==1)

    ## Ranks are computed based on a weighted average
    ## Reviewers like the app user have more weight
    ranking.data <- data %>%
      filter(movie_id %in% include$movie_id) %>%
      merge(., reviewers.like.me()) %>%
      mutate(rating = rating * similarity) %>%
      group_by(movie_id) %>%
      summarise(rating = sum(rating), count=n()) %>%
      mutate(score = rating / count) %>%
      select(movie_id, score, count) %>%
      merge(., titles) %>%
      arrange(-score, -count)
    
    ## Remove films seen by shiny app user
    if(!input$movie_id.1 == 'Pick a Movie'){
      ranking.data <- ranking.data[ranking.data$movie_id != input$movie_id.1,]
    }
    if(!input$movie_id.2 == 'Pick a Movie'){
      ranking.data <- ranking.data[ranking.data$movie_id != input$movie_id.2,]
    }
    if(!input$movie_id.3 == 'Pick a Movie'){
      ranking.data <- ranking.data[ranking.data$movie_id != input$movie_id.3,]
    }
    
    ## Shorten the list if needed
    if(nrow(ranking.data)> 500){
      ranking.data <- ranking.data[1:500,]
    }
    
    ## Format data for table
    ranking.data <- ranking.data %>%
      select(title)
    names(ranking.data) <- c('Movie Title')
    ranking.data
  })
  
  reviewers.like.me <- reactive({
    ## This function returns a user id and a number that indicates how similar 
    ## the reviewer is to the shiny app user
    rlm <- data %>%
      select(user_id) %>%
      unique() %>%
      arrange(user_id) %>%
      mutate(similarity = 1)
    if(!input$movie_id.1 == 'Pick a Movie'){
      temp <- data %>%
        filter(movie_id == input$movie_id.1) %>%
        filter(rating == input$rating.1) %>%
        select(user_id)
      rlm[rlm$user_id %in% temp$user_id,]$similarity <- rlm[rlm$user_id %in% temp$user_id,]$similarity * 10
    }
    if(!input$movie_id.2 == 'Pick a Movie'){
      temp <- data %>%
        filter(movie_id == input$movie_id.2) %>%
        filter(rating == input$rating.2) %>%
        select(user_id)
      rlm[rlm$user_id %in% temp$user_id,]$similarity <- rlm[rlm$user_id %in% temp$user_id,]$similarity * 10
    }
    if(!input$movie_id.3 == 'Pick a Movie'){
      temp <- data %>%
        filter(movie_id == input$movie_id.3) %>%
        filter(rating == input$rating.3) %>%
        select(user_id)
      rlm[rlm$user_id %in% temp$user_id,]$similarity <- rlm[rlm$user_id %in% temp$user_id,]$similarity * 10
    }
    
    rlm
  })
  
  output$recommendation.table <- renderDataTable(
    getRecommendationTable(),
    options = list(
      paging=FALSE,
      searching = FALSE,
      scrollY= 400
    )
  )

  ## Thanks to the folks at RStudio for the inspiration for this visualizion!
  movie_tooltip <- function(x) {
    if (is.null(x) || is.null(x$movie_id)) return(NULL)
    # Pick out the movie with this ID
    all_movies <- isolate(movies)
    movie <- all_movies[all_movies$id == x$movie_id, ]
    paste0("<b>", movie$title, "</b>")
  }

  plot.data <- reactive({
    plot.data <- data %>%
      group_by(movie_id) %>%
      summarise(score=sum(rating), reviews=n()) %>%
      mutate(avg.rating=score/reviews) %>%
      mutate(stroke=1)
    if(!input$genre.filter == 'Anything'){
      plot.data$stroke <- movies[,names(movies) == input$genre.filter]
    }
    plot.data
  })

  vis <- reactive({
    plot.data() %>%
      ggvis(x=~avg.rating, y=~reviews) %>%
      layer_points(size:=50, size.hover:=200, fillOpacity:=~stroke, stroke = ~stroke, key:=~movie_id) %>%
      add_tooltip(movie_tooltip, "hover") %>%
      add_axis('x', title = 'Average Rating') %>%
      add_axis('y', title = 'Number of Reviews') %>%
      hide_legend('stroke')%>%
      set_options(width = 500, height = 500)
    }) %>%
  bind_shiny("plot1")

})
