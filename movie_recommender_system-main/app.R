# server.R
library(shiny)
library(recommenderlab)

# Load the movie and rating data
movie_data <- read.csv("movies.csv",stringsAsFactors=FALSE)
rating_data <- read.csv("ratings.csv")

# Create a rating matrix
ratingMatrix <- dcast(rating_data, userId~movieId, value.var = "rating", na.rm=FALSE)
ratingMatrix <- as.matrix(ratingMatrix[,-1]) #remove userIds
ratingMatrix <- as(ratingMatrix, "realRatingMatrix")

# Train a recommender system
recommen_model <- Recommender(data = ratingMatrix,
                             method = "IBCF", parameter = list(k = 30))

# Define a function to generate recommendations for a user
get_recommendations <- function(user_id) {
  # Predict ratings for all items
  predicted_ratings <- predict(recommen_model, newdata = ratingMatrix[user_id,], n = 10)

  # Get the top 10 recommendations
  top_recommendations <- predicted_ratings@items[[1]]

  # Return the recommended movies
  recommended_movies <- as.character(subset(movie_data,
   movie_data$movieId == top_recommendations)$title)

  return(recommended_movies)
}

# Define a shiny app
ui <- fluidPage(
  titlePanel("Movie Recommender"),
  sidebarLayout(
    sidebarPanel(
      textInput("user_id", "User ID:"),
      actionButton("submit", "Get Recommendations")
    ),
    mainPanel(
      uiOutput("recommendations")
    )
  )
)

server <- function(input, output) {
  # Get the recommended movies for the selected user
  output$recommendations <- renderUI({
    user_id <- input$user_id

    recommended_movies <- get_recommendations(user_id)

    # Create a list item for each recommended movie
    list_items <- list.items(recommended_movies)

    # Return the list items
    return(list_items)
  })
}

shinyApp(ui, server)