# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(
  titlePanel("Movie Recommender"),
  fluidRow(
    column(3,
           wellPanel(
             selectInput('genre.filter', "I'm in the mood for", c('Anything', genres))
           ),
           wellPanel(
             h4("I've Seen"),
             selectInput('movie_id.1', 'Movie 1:', c('Pick a Movie',dropdown)),
             sliderInput('rating.1', "Rate It:", min=1, max=5, step=1, value=5),
             selectInput('movie_id.2', 'Movie 2:', c('Pick a Movie',dropdown)), 
             sliderInput('rating.2', "Rate It:", min=1, max=5, step=1, value=5),
             selectInput('movie_id.3', 'Movie 3:', c('Pick a Movie',dropdown)), 
             sliderInput('rating.3', "Rate It:", min=1, max=5, step=1, value=5)
           )
    ),
    column(9, 
           tabsetPanel(type = 'tabs', 
                       tabPanel('Directions',
                                h3('Introduction'),
                                p("This app gives you the best possible movie recommendations in seconds with minimal effort on your part."),
                                p("Select a couple of movies you have seen and rate them on a 1 to 5 star scale (5 means you love it)."),
                                p("Using our proprietary algorithm we will search through our database of close to ½ million reviews of over 1,000 films to find the best recommendations.  You can filter the movie recommendations by genre at any time.")
                       ),
                       tabPanel('We Recommend You See', dataTableOutput('recommendation.table')),
                       tabPanel("Reviewer's Ratings", ggvisOutput('plot1'))
                      )
           )
  )
))
