# main function
main <- function() {
    # source("cdfplot.R")
  source("linesplot.R")
  # source("boxplot.R")
  # draw!
  metrics_dir <- "processed"
  ylabel1 <- "Sending time (ms)"
  ylabel2 <- "Delivering time (ms)"
  xlabel1 <- "Number of Sent messages"
  xlabel2 <- "Number of delivered messages"
  logx <- TRUE
  logy <- FALSE

  # list of simulations
  simulations <- list.files(metrics_dir)
  
  for(i in 1:length(simulations)) {
    simulation <- simulations[[i]]
    dir <- paste(metrics_dir, simulation, sep="/")
  
    # latency local
    key <- "send"
    output_file <- paste(simulation, "_", key, ".png", sep="")
    # splot(dir, simulation, key, output_file, ylabel1, logx)
  	splot(dir, simulation, key, output_file, ylabel1, xlabel1, logy)

    # latency remote
    key <- "deliver"
    output_file <- paste(simulation, "_", key, ".png", sep="")
    # splot(dir, simulation, key, output_file, ylabel2, logx)
  	splot(dir, simulation, key, output_file, ylabel2, xlabel2, logy)
  }
}

main()
