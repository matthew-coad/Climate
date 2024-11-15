## ---------------
## Exploration
## ---------------

# Get data relevant to exploration
m_get_exploration_data <- function(m) {

    stopifnot(m_is_task(m))
    stopifnot(!m_is_multi(m))
    stopifnot(!m_is_dirty(m))
    
    if (!is.null(m$pre_processed_data)) {
        data <- m$pre_processed_data
    }
    else if (!is.null(m$engineered_data)) {
        data <- m$engineered_data
    }
    else if (!is.null(m$training_data)) {
        data <- m$training_data
    }
    else {
        data <- m$input_data
    }
    stopifnot(!is.null(data))
    data
}

m_get_data_statistics <- function(m) {
    
    name <- lazyeval::expr_text(m)
    if (is.data.frame(m)) {
        m <- m_task(name) %>% m_input_data(m)
    }

    l <- m %>% m_run() %>% m_as_task_list()
    builder <- function(l_i) {
        df <- m_get_exploration_data(l_i) %>% data_statistics()
        names <- list(name = rep(m_name(l_i), nrow(df)))
        bind_cols(names, df)
    }

    base <- l %>% m_query()
    df <- l %>% map_df(builder)
    base %>% dplyr::inner_join(df, by = "name")
}

m_get_variable_distribution <- function(m) {
    
    name <- lazyeval::expr_text(m)
    if (is.data.frame(m)) {
        m <- m_task(name) %>% m_input_data(m)
    }
    
    l <- m %>% m_run() %>% m_as_task_list()
    builder <- function(l_i) {
        df <- m_get_exploration_data(l_i) %>% variable_distribution()
        names <- list(name = rep(m_name(l_i), nrow(df)))
        bind_cols(names, df)
    }
    
    base <- l %>% m_query()
    df <- l %>% map_df(builder)
    base %>% dplyr::inner_join(df, by = "name")
}

m_get_variable_degeneracy <- function(m) {
    
    name <- lazyeval::expr_text(m)
    if (is.data.frame(m)) {
        m <- m_task(name) %>% m_input_data(m)
    }
    
    l <- m %>% m_run() %>% m_as_task_list()
    builder <- function(l_i) {
        df <- m_get_exploration_data(l_i) %>% variable_degeneracy()
        names <- list(name = rep(m_name(l_i), nrow(df)))
        bind_cols(names, df)
    }
    
    base <- l %>% m_query()
    df <- l %>% map_df(builder)
    base %>% dplyr::inner_join(df, by = "name")
}

m_get_level_statistics <- function(m) {
    
    name <- lazyeval::expr_text(m)
    if (is.data.frame(m)) {
        m <- m_task(name) %>% m_input_data(m)
    }
    
    l <- m %>% m_run() %>% m_as_task_list()
    builder <- function(l_i) {
        df <- m_get_exploration_data(l_i) %>% level_statistics()
        names <- list(name = rep(m_name(l_i), nrow(df)))
        bind_cols(names, df)
    }
    
    base <- l %>% m_query()
    df <- l %>% map_df(builder)
    base %>% dplyr::inner_join(df, by = "name")
}
