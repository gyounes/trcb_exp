# Given a package name,
# install it (if not already installed) and load it.
load <- function(package) {
  mirror <- "http://cran.us.r-project.org"

  if(!require(package, character.only=TRUE)) {
    install.packages(package, repos=mirror, dependencies=TRUE)
    require(package, character.only=TRUE)
  }
}

# Load a list of dependencies.
load_dependencies <- function(packages) {
  Map(load, packages)
}

# given the vector of subpaths,
# return the json file
json <- function(v) {
  load("jsonlite")
  file_path <- paste(v, collapse="/")
  fromJSON(file_path)
}

# compute label name given key.
get_labels <- function(keys) {
  labels = list()
  labels[["base"]] = "VV-based"
  labels[["dots"]] = "Dot-based"
  labels[["2"]] = " [Nodes:"
  labels[["3"]] = ", Rate:"
  labels[["4"]] = "ms, Latency:"
  labels[["5"]] = "ms, MsgPerNode:"

  lapply(
    keys,
    function(key) {
      parts <-  strsplit(key, "~")[[1]]
      Mode <- parts[c(1)]
      paste (
        labels[[Mode]],
        labels[["2"]],
        parts[c(2)],
        labels[["3"]],
        parts[c(3)],
        labels[["4"]],
        parts[c(4)],
        labels[["5"]],
        parts[c(5)],
        "]",
        sep=""
      )
    }
  )
}

# get the plot title
get_title <- function(key) {
  titles = list()
  titles[["base"]] = "VV-based"
  titles[["dots"]] = "Dot-based"
  
  titles[[key]]
}
