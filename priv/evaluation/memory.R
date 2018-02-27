# main function
main <- function() {
     # source("boxplot.R")
  source("linesplot.R")
    source("cdfplot.R")
  
# draw!
  metrics_dir <- "processed"
  ylabel <- "Memory (B)"
  xlabel <- "Time (s)"
  logy <- FALSE
  logx <- TRUE

  # list of simulations
  simulations <- list.files(metrics_dir)

  for(i in 1:length(simulations)) {
    simulation <- simulations[[i]]
    dir <- paste(metrics_dir, simulation, sep="/")
    
    # memory crdt
    #    key <- "memory_crdt"
    #    output_file <- paste(simulation, "_", key, ".pdf", sep="")
    #    splot(dir, simulation, key, output_file, ylabel, logx)

    # memory algorithm
    key <- "memory_algorithm"
    output_file <- paste(simulation, "_", key, ".png", sep="")
    splot(dir, simulation, key, output_file, ylabel, logx)
    # splot(dir, simulation, key, output_file, ylabel, xlabel, logy)
  }
}

main()
