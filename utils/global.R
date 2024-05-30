library(blockrand)
library(ggplot2)
library(rmarkdown)
library(reshape2)

generate_rmd_content <- function(input, rand_list) {
  rmd_content <- paste0(
    "---\n",
    "title: \"Clinical Trial Randomization and Simulation\"\n",
    "author: \"Data Wizard\"\n",
    "output: pdf_document\n",
    "---\n\n",
    "## User Inputs\n\n",
    "Center IDs: ", input$center_ids, "\n\n",
    "Treatments: ", input$treatments, "\n\n",
    "Block Size: ", input$block_size, "\n\n",
    "Number of Patients: ", input$num_patients, "\n\n",
    "Stratum: ", input$stratum, "\n\n",
    "Number of Simulations: ", input$num_sims, "\n\n",
    "## Randomization List\n\n",
    "```{r, echo=FALSE}\n",
    "rand_list <- ", deparse(substitute(rand_list)), "\n",
    "print(rand_list)\n",
    "```\n\n",
    "## Simulation Results (50%)\n\n",
    "```{r, echo=FALSE}\n",
    "sim_results_50 <- perform_simulations(rand_list, ", input$num_sims, ", 0.5, 0.15)\n",
    "plot_simulation_results_single(sim_results_50, \"50% Simulation Results\")\n",
    "```\n\n",
    "## Simulation Results (70%)\n\n",
    "```{r, echo=FALSE}\n",
    "sim_results_70 <- perform_simulations(rand_list, ", input$num_sims, ", 0.7, 0.15)\n",
    "plot_simulation_results_single(sim_results_70, \"70% Simulation Results\")\n",
    "```\n\n",
    "## Simulation Results (100%)\n\n",
    "```{r, echo=FALSE}\n",
    "sim_results_100 <- perform_simulations(rand_list, ", input$num_sims, ", 0.95, 0.05)\n",
    "plot_simulation_results_single(sim_results_100, \"100% Simulation Results\")\n",
    "```\n"
  )
  return(rmd_content)
}

perform_simulations <- function(rand_list, num_sims, base_fraction, variation) {
  replicate(num_sims, {
    sample_fraction <- runif(1, base_fraction - variation, base_fraction + variation)
    sampled <- rand_list[sample(nrow(rand_list), size = round(sample_fraction * nrow(rand_list))), ]
    table(sampled$treatment)
  }, simplify = FALSE)
}

plot_simulation_results <- function(sim_results_50, sim_results_70, sim_results_100) {
  sim_results_df_50 <- as.data.frame(do.call(rbind, sim_results_50))
  sim_results_df_70 <- as.data.frame(do.call(rbind, sim_results_70))
  sim_results_df_100 <- as.data.frame(do.call(rbind, sim_results_100))
  
  colnames(sim_results_df_50) <- paste0("Treatment_", colnames(sim_results_df_50))
  colnames(sim_results_df_70) <- paste0("Treatment_", colnames(sim_results_df_70))
  colnames(sim_results_df_100) <- paste0("Treatment_", colnames(sim_results_df_100))
  
  sim_results_df_50 <- melt(sim_results_df_50, variable.name = "Var2")
  sim_results_df_70 <- melt(sim_results_df_70, variable.name = "Var2")
  sim_results_df_100 <- melt(sim_results_df_100, variable.name = "Var2")
  
  sim_results_df_50$simulation <- "50%"
  sim_results_df_70$simulation <- "70%"
  sim_results_df_100$simulation <- "100%"
  
  all_sim_results <- rbind(sim_results_df_50, sim_results_df_70, sim_results_df_100)
  
  # ggplot(all_sim_results, aes(x = value, fill = as.factor(Var2))) +
  #   geom_histogram(binwidth = 1, alpha = 0.5, position = "identity") +
  #   facet_wrap(~simulation, scales = "free_y") +
  #   labs(title = "Simulation Results", x = "Number of Patients", y = "Frequency", fill = "Treatment") +
  #   theme_minimal()
  p1 <- ggplot(sim_results_df_50, aes(x = value, fill = as.factor(Var2))) +
    geom_histogram(binwidth = 1,color = "black") +
    labs(title = "50% Simulation Results", x = "Number of Patients", y = "Frequency")+theme_bw()
  
  p2 <- ggplot(sim_results_df_70, aes(x = value, fill = as.factor(Var2))) +
    geom_histogram(binwidth = 1, color = "black") +
    labs(title = "70% Simulation Results", x = "Number of Patients", y = "Frequency")+theme_bw()
  
  p3 <- ggplot(sim_results_df_100, aes(x = value, fill = as.factor(Var2))) +
    geom_histogram(binwidth = 1, color = "black") +
    labs(title = "100% Simulation Results", x = "Number of Patients", y = "Frequency")+theme_bw()
  
  gridExtra::grid.arrange(p1, p2, p3, ncol = 1)
  
}

plot_simulation_results_single <- function(sim_results, title) {
  sim_results_df <- as.data.frame(do.call(rbind, sim_results))
  colnames(sim_results_df) <- paste0("Treatment_", colnames(sim_results_df))
  sim_results_df <- melt(sim_results_df, variable.name = "Var2")
  
  ggplot(sim_results_df, aes(x = value, fill = Var2)) +
    geom_histogram(binwidth = 1, alpha = 0.5, position = "identity") +
    labs(title = title, x = "Number of Patients", y = "Frequency", fill = "Treatment") +
    theme_minimal()
}
