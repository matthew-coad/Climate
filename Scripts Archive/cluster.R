
library(doParallel)

if (!exists("climate_cluster")) {
    climate_cluster <- NULL
}

start_cluster <- function() {
    if (is.null(climate_cluster)) {
        climate_cluster <<- parallel::makeCluster(6)
        doParallel::registerDoParallel(climate_cluster)
    } else {
        message("Cluster is already running")
    }
}

stop_cluster <- function() {
    if (!is.null(climate_cluster)) {
        parallel::stopCluster(climate_cluster)
        climate_cluster <<- NULL
    }
}
