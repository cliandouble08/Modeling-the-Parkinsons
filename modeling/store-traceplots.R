# Number of pages to plot - adjust this as per your model's requirements
n_pages <- 114 # You might need to change this number based on your model's parameters

# File path for saving the images
file_path <- "figures/modeling/ordinal-model/traceplots" # Update this with your actual file path

# Generate and save trace plots
for (i in 1:n_pages) {
  png(filename = paste0(file_path, "/trace_plot_", i, ".png"))
  traceplot(ordinal_model, page = i)
  dev.off()
}
