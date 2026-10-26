library(shiny)

shinyServer(function(input, output, session) {
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
