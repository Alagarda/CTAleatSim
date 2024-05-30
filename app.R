library(shiny)
source("./utils/global.R")  # Source the external file containing auxiliary functions

ui <- fluidPage(
  titlePanel("Clinical Trial Randomization and Simulation"),
  sidebarLayout(
    sidebarPanel(
      textInput("center_ids", "Center IDs (comma separated):", "2342"),
      textInput("treatments", "Treatments (comma separated):", "0,1"),
      textInput("block_size", "Block Size (1-4 or two integers from 1-4):", "1,2"),
      numericInput("num_patients", "Number of Patients:", value = 100, min = 1),
      textInput("stratum", "Stratum:", ""),
      sliderInput("num_sims", "Number of Simulations:", min = 500, max = 10000, value = 1000),
      downloadButton("download_pdf", "Download PDF Report")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Randomization List", tableOutput("rand_list")),
        tabPanel("Simulations", plotOutput("sim_plot",height = "1200px"))
      )
    )
  )
)

server <- function(input, output) {
  randomization <- reactive({
    centers <- unlist(strsplit(input$center_ids, ","))
    treatments <- unlist(strsplit(input$treatments, ","))
    treatments <- ifelse(treatments %in% c("0", "1"), as.numeric(treatments), seq_along(treatments))
    block_size <- as.numeric(unlist(strsplit(input$block_size, ",")))
    #block_size <- ifelse(length(block_size) > 1, block_size, sample(block_size, 1))
    
    num_patients_per_center <- ceiling(input$num_patients / length(centers))
    
    rand_list <- do.call(rbind, lapply(centers, function(center) {
      rand_center <- blockrand::blockrand(
        n = num_patients_per_center, 
        block.sizes = block_size, 
        levels=treatments,
        num.levels = length(unique(treatments))
      )
      data.frame(center_id = center, rand_center)
    }))
    
    rand_list <- rand_list[1:input$num_patients, ]  # Ensure the final data frame has exactly num_patients rows
    rand_list
  })
  
  output$rand_list <- renderTable({
    randomization()
  })
  
  output$sim_plot <- renderPlot({
    rand_list <- randomization()
    num_sims <- input$num_sims
    
    sim_results_50 <- perform_simulations(rand_list, num_sims, 0.5, 0.15)
    sim_results_70 <- perform_simulations(rand_list, num_sims, 0.7, 0.15)
    sim_results_100 <- perform_simulations(rand_list, num_sims, 0.95, 0.05)
    
    plot_simulation_results(sim_results_50, sim_results_70, sim_results_100)
  })
  
  output$download_pdf <- downloadHandler(
    filename = function() {
      paste("clinical_trial_report", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      temp_rmd <- tempfile(fileext = ".Rmd")
      temp_pdf <- tempfile(fileext = ".pdf")
      
      rmd_content <- generate_rmd_content(input, randomization())
      writeLines(rmd_content, temp_rmd)
      
      rmarkdown::render(temp_rmd, output_file = temp_pdf)
      file.copy(temp_pdf, file)
    }
  )
}

shinyApp(ui = ui, server = server)
