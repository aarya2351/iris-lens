library(shiny)

shinyUI(fluidPage(
  tags$head(tags$style(HTML("\n+    body { background: #f6f8fb; color: #193047; }\n+    .jumbotron { background: #163a5f; color: white; border-radius: 12px; padding: 24px 30px; margin-top: 15px; }\n+    .jumbotron h1 { font-size: 36px; margin-top: 0; }\n+    .well, .panel { border-radius: 10px; }\n+    .prediction { font-size: 25px; font-weight: 700; color: #116568; }\n+  "))),
  div(class = "jumbotron",
      h1("Iris Lens"),
      p("Identify an iris flower from two simple petal measurements")),
  sidebarLayout(
    sidebarPanel(
      h3("Enter measurements"),
      sliderInput("length", "Petal length (cm)", min = 1.0, max = 7.0,
                  value = 4.5, step = 0.1),
      sliderInput("width", "Petal width (cm)", min = 0.1, max = 2.5,
                  value = 1.5, step = 0.1),
      selectInput("k", "Number of nearby flowers", choices = c(3, 5, 7, 9),
                  selected = 5),
      hr(),
      h4("How to use this app"),
      p("1. Measure the length and width of one iris petal in centimetres."),
      p("2. Move the two sliders to those measurements. You can also try the default values."),
      p("3. Read the predicted species and the neighbour counts on the right."),
      p("4. Change the number of nearby flowers to see whether the answer changes."),
      p("The chart shows the 150 reference flowers. The black star is your input; black circles mark its nearest neighbours."),
      p("This is an educational demonstration using R's built-in iris dataset. It is not a definitive botanical identification.")
    ),
    mainPanel(
      h3("Prediction"),
      div(class = "prediction", textOutput("species")),
      textOutput("explanation"),
      plotOutput("flowerPlot", height = "420px"),
      h4("Votes among the nearest flowers"),
      tableOutput("votes"),
      h4("How the result is calculated"),
      p("Each measurement is scaled by the spread of that measurement in the reference data. The app finds the closest flowers using the two scaled measurements and selects the species with the most votes. If votes tie, the tied species with the closest neighbour wins. The vote fraction is a neighbour share, not a calibrated probability.")
    )
  )
))
