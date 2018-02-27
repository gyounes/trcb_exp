# main function
main <- function() {
  # draw!
  metrics_dir <- "processed"
  ylabel <- "Transmission (B)"
  xlabel <- "Time (s)"
  logy <- FALSE
  logx <- TRUE
  key1 <- "tcbcast_ack"
  key2 <- "tcbcast"

  # list of simulations
  simulations <- list.files(metrics_dir)

  for(i in 1:length(simulations)) {
    simulation <- simulations[[i]]
    dir <- paste(metrics_dir, simulation, sep="/")

    # source("boxplot.R")
    # source("linesplot.R")
    source("cdfplot.R")

    output_file <- paste(simulation, "_", key1, "_line", ".png", sep="")
    splot(dir, simulation, key1, output_file, ylabel, logx)
    # splot(dir, simulation, key1, output_file, ylabel, xlabel, logy)

    output_file <- paste(simulation, "_", key2, "_line", ".png", sep="")
    splot(dir, simulation, key2, output_file, ylabel, logx)
    # splot(dir, simulation, key2, output_file, ylabel, xlabel, logy)

#    source("barplot.R")
#    output_file <- paste(simulation, "_", key, "_bar", ".pdf", sep="")
#    splot(dir, simulation, key, output_file, ylabel)
  }
}

main()
