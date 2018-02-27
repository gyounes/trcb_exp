# main function
main <- function() {
    source("cdfplot.R")
  # source("linesplot.R")
  # source("boxplot.R")
  # draw!
  metrics_dir <- "processed"
  label <- "Processing time (ms)"
  xlabel1 <- "Number of Sent messages"
  xlabel2 <- "Number of Received messages"
   logx <- TRUE
    # logy <- FALSE

  # list of simulations
  simulations <- list.files(metrics_dir)
  
  for(i in 1:length(simulations)) {
    simulation <- simulations[[i]]
    dir <- paste(metrics_dir, simulation, sep="/")
  
    # latency local
    key <- "latency_local"
    output_file <- paste(simulation, "_", key, ".png", sep="")
    splot(dir, simulation, key, output_file, label, logx)
  	# splot(dir, simulation, key, output_file, label, xlabel1, logy)

    # latency remote
    key <- "latency_remote"
    output_file <- paste(simulation, "_", key, ".png", sep="")
    splot(dir, simulation, key, output_file, label, logx)
  	# splot(dir, simulation, key, output_file, label, xlabel2, logy)
  }
}

main()
