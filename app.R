library(shiny)

ui <- shinyUI(fluidPage(
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

server <- shinyServer(function(input, output, session) {
  flowers <- iris
  petal_sd <- apply(flowers[, c("Petal.Length", "Petal.Width")], 2, sd)
  species_names <- levels(flowers$Species)
  species_colours <- c(setosa = "#2a9d8f", versicolor = "#e9a23b",
                       virginica = "#8965bb")

  result <- reactive({
    measurements <- c(input$length, input$width)
    distances <- sqrt(
      ((flowers$Petal.Length - measurements[1]) / petal_sd[1])^2 +
      ((flowers$Petal.Width - measurements[2]) / petal_sd[2])^2
    )
    nearby <- order(distances)[seq_len(as.integer(input$k))]
    counts <- table(factor(flowers$Species[nearby], levels = species_names))
    leaders <- names(counts)[counts == max(counts)]
    if (length(leaders) == 1L) {
      winner <- leaders
    } else {
      # Resolve vote ties by the nearest individual among tied species.
      winner <- as.character(flowers$Species[
        nearby[which(as.character(flowers$Species[nearby]) %in% leaders)[1]]
      ])
    }
    list(nearby = nearby, counts = counts, winner = winner)
  })

  output$species <- renderText({
    paste("Likely species:", tools::toTitleCase(result()$winner))
  })

  output$explanation <- renderText({
    selected <- result()
    paste0(unname(selected$counts[selected$winner]), " of ", input$k,
           " nearest reference flowers are ", selected$winner, ".")
  })

  output$votes <- renderTable({
    data.frame(Species = tools::toTitleCase(species_names),
               Neighbours = as.integer(result()$counts),
               Share = paste0(round(100 * as.integer(result()$counts) /
                                      as.integer(input$k)), "%"),
               check.names = FALSE)
  }, striped = TRUE, bordered = TRUE)

  output$flowerPlot <- renderPlot({
    selected <- result()
    plot(flowers$Petal.Length, flowers$Petal.Width,
         col = species_colours[as.character(flowers$Species)], pch = 16,
         xlab = "Petal length (cm)", ylab = "Petal width (cm)",
         main = "Your flower among the reference flowers",
         xlim = c(0.8, 7.1), ylim = c(0, 2.7))
    points(flowers$Petal.Length[selected$nearby],
           flowers$Petal.Width[selected$nearby], pch = 1, cex = 1.8,
           lwd = 2, col = "black")
    points(input$length, input$width, pch = 8, cex = 2.2,
           lwd = 2.5, col = "black")
    legend("topleft", legend = c(species_names, "Your flower"),
           col = c(unname(species_colours), "black"),
           pch = c(16, 16, 16, 8), bty = "n", cex = 0.83)
  })
})

shinyApp(ui, server)
