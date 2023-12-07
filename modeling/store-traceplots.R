library(rethinking)
library(ggplot2)
library(gridExtra)

# Assuming 'ordinal_model' is your fitted model object
param_names <- names(extract.samples(ordinal_model))

# Function to generate and save trace plot for a parameter
save_trace_plot <- function(param_name, model, save_path) {
  # Extract parameter data and convert it to long format
  trace_data <- as.data.frame(extract.samples(model, pars=param_name)[[1]])
  trace_data_long <- melt(trace_data, variable.name = "Chain", value.name = "Value")
  
  # Create the plot
  p <- ggplot(trace_data_long, aes(x=1:nrow(trace_data_long), y=Value, group=Chain)) +
    geom_line() +
    labs(title=paste("Trace plot for", param_name), x="Iteration", y="Value")
  
  # Save the plot
  ggsave(filename=paste0(save_path, param_name, "_trace_plot.png"), plot=p, width=10, height=6)
}

# Directory to save plots
save_path <- "figures/modeling/ordinal-model/traceplots"

# Generate and save trace plots for each parameter
for (param_name in param_names) {
  save_trace_plot(param_name, ordinal_model, save_path)
}

ordinal_precis <- precis(ordinal_model, depth = 2)
cutpoints_inv_logit <- format(inv_logit(coef(ordinal_model)), digits = 3)

ordinal_posterior <- extract.samples(ordinal_model)