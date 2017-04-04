library(ggplot2)
library(dplyr)

function(input, output, session) {
  fProj <- reactive(read.csv(paste0("Data/", input$site1, "/projections.csv"), stringsAsFactors = F))
  
  output$mtime1 <- renderText(as.character(file.info(paste0("Data/", input$site1, "/projections.csv"))$mtime))
  
  proj <- reactive({
    if (input$position1 == "Batters") {
      fProj() %>% filter(Pos %in% c("C", "1B", "2B", "3B", "SS", "OF"))
    } else {
      if (input$site1 == "FD") {
        fProj() %>% filter(Pos == input$position1)
      } else {
        fProj() %>% filter(Pos == input$position1 | Pos2 == input$position1)
      }
    }
  })
  
  output$projPlot <- renderPlot({
    ggplot(proj(), aes(x = Salary, y = Projection, label = Label)) +
      geom_jitter() +
      geom_smooth(method = "lm") +
      geom_text(size = 4, vjust = -0.75, fontface = "bold") +
      # ggtitle(paste(input$site, as.character(Sys.Date()), "All Positions", sep = " :: "),
      #         subtitle = "@wetlkfntsysprts") +
      theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +
      theme(text = element_text(size = 20))
  })
  
  output$projTable <- renderDataTable({
    proj <- proj()
    if (input$position1 == "Batters") {
      if (input$site1 == "DK") {
        proj <- proj[!is.na(proj$Pos) & !is.na(proj$Pos2), ]
        proj$Pos[proj$Pos2 != ""] <- paste0(proj$Pos[proj$Pos2 != ""], "/",
                                            proj$Pos2[proj$Pos2 != ""])
      }
      proj[, c("Player", "Pos", "Team", "Opp", "Hand", "Order", "Salary", "Projection")]
    } else {
      if (input$position1 == "P") {
        proj[, c("Player", "Team", "Opp", "Hand", "Salary", "Projection")]
      } else {
        proj[, c("Player", "Team", "Opp", "Hand", "Order", "Salary", "Projection")]
      }
    }
  })

###############################################################################  
    
  fResults <- reactive(read.csv(paste0("Data/", input$site2, "/results.csv"), stringsAsFactors = F))

  output$mtime2 <- renderText(as.character(file.info(paste0("Data/", input$site2, "/results.csv"))$mtime))

  results <- reactive({
    if (input$position2 == "Batters") {
      fResults() %>% filter(Pos %in% c("C", "1B", "2B", "3B", "SS", "OF"))
    } else {
      if (input$site2 == "FD") {
        fResults() %>% filter(Pos == input$position2)
      } else {
        fResults() %>% filter(Pos == input$position2 | Pos2 == input$position2)
      }
    }
  })

  output$resultsPlot <- renderPlot({
    ggplot(results(), aes(x = Projection, y = Actual, label = Label, color = Value)) +
      geom_jitter() +
      scale_color_gradient(low = "blue", high = "red", name = "Proj.\nValue") +
      geom_smooth(method = "lm", color = "black") +
      geom_text(size = 4, vjust = -0.75, fontface = "bold") +
      theme(text = element_text(size = 20))
  })

  output$resultsTable <- renderDataTable({
    temp <- results()
    names(temp)[names(temp) == "Value"] <- "Proj. Value"
    names(temp)[names(temp) == "actValue"] <- "Actual Value"
    temp[, c("Proj. Value", "Actual Value")] <- round(temp[, c("Proj. Value", "Actual Value")], 2)
    if (input$position2 == "Batters") {
      if (input$site2 == "DK") {
        temp <- temp[!is.na(temp$Pos) & !is.na(temp$Pos2), ]
        temp$Pos[temp$Pos2 != ""] <- paste0(temp$Pos[temp$Pos2 != ""], "/",
                                            temp$Pos2[temp$Pos2 != ""])
      }
      temp[, c("Player", "Pos", "Team", "Opp", "Salary", "Projection", "Actual", "Proj. Value",
                    "Actual Value")]
    } else {
      temp[, c("Player", "Team", "Opp", "Salary", "Projection", "Actual", "Proj. Value", "Actual Value")]
    }
  })
  
  session$onSessionEnded(stopApp)
}