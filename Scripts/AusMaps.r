
aus_map <- function(gg) {
    
    
    map <- withr::with_namespace(
        "maps", 
        ggplot2::borders("world", regions = "Australia",  fill = "grey90", colour = "black")
    )
    
    gg + map  + xlim(112, 155) + ylim(-44, -10)  # Set y axis limits
}

aus_dark_map <- function(gg) {
    
    map <- withr::with_namespace(
        "maps", 
        ggplot2::borders("world", regions = "Australia",  fill = "DarkGrey", colour = "black")
    )
    
    gg + map  + xlim(112, 155) + ylim(-44, -10)  # Set y axis limits
}

