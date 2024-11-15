
## -- Task --

m_task <- function(name, ...) {
    r <- list()
    class(r) <- c(m_task_class, class(r))
    r <- m_named(r, name)
    r <- m_set(r, local_cache = memory_cache())
    r <- m_set(r, ...)
    r
}

m_as_task <- function(m) {
    name <- lazyeval::expr_text(m)
    if (is.data.frame(m)) {
        r <- m_task(name, input_data = as_tibble(m))
    }
    else
        r <- m
    r
}

m_task_class <- "m_task"

m_is_task <- function(x) m_task_class %in% class(x)

## -- Multi Task --

m_multi_class <- "m_multi"

m_is_multi <- function(x) m_multi_class %in% class(x)

## -- Task Lists --

# Task lists are a list of tasks
# It sometimes easier to perform processing on tasks lists rather than tasks

m_task_list_class <- "m_task_list"

m_is_task_list <- function(x) m_task_list_class %in% class(x)

m_as_task_list <- function(m) {
    if (m_is_multi(m)) {
        l <- append(list(), m)
    } else if (m_is_task(m)) {
        l <- list(m)
    } else if(is.list(m)) {
        l <- m
    } else {
        stop("m is not convertible to a task list")
    }
        
    class(l) <- c(m_task_list_class, class(l))
    l
}


## -- Dirty --

m_as_dirty <- function(x) {
    attr(x, "dirty") <- TRUE
    x
}

m_as_clean <- function(x) {
    attr(x, "dirty") <- NULL
    x
}

m_is_dirty <- function(x)!is.null(attr(x, "dirty")) && attr(x, "dirty")

## -- Name --

m_named <- function(x, name) {
    attr(x, "name") <- name
    x
}

m_name <- function(x) attr(x, "name")

## -- Formula Value --

m_col_formula <- function(x) colnames(x) %>% map(~as.formula(paste0("~", .)))

## -- Set Arg Value --

m_set <- function(m, .name = NULL, .value = NULL, .overwrite = TRUE, ...) {
    args <- list(...)
    arg_names <- c(names(args))
    if (!is.null(.name)) {
        if (is.null(.value)) {
            stop("Arg value not provided")
        }
        args[[.name]] <- .value
        arg_names <- c(arg_names, .name)
    }
    if (length(args) == 0) return(m)

    for (i in 1:length(args)) {
        arg_name <- arg_names[i]
        if (is.null(arg_name)) {
            stop("Arg name not provided")
        }
    }
    r <- m
    for (i in 1:length(args)) {
        arg_name <- arg_names[i]
        arg_value <- args[[i]]
        if (!m_is_multi(r)) {
            if (is.null(m[[arg_name]]) || .overwrite) {
                r[[arg_name]] <- arg_value
                r <- m_as_dirty(r)
            }
        }
        else {
            for (j in 1:length(r)) {
                if (is.null(m[[j]][[arg_name]]) || .overwrite) {
                    r[[j]][[arg_name]] <- arg_value
                    r[[j]] <- m_as_dirty(r[[j]])
                    r <- m_as_dirty(r)
                }
            }
        }
    }
    r
}

## -- Set arg default value --

m_default <- function(m, .name = NULL, .value = NULL, ...) {
    m_set(m, .name = .name, .value = .value, .overwrite = FALSE, ...)
}

## -- Arg Types --

m_types <- function(m) {
    types <- attr(m, "types")
    if (is.null(types)) {
        types <- character(0)
    }
    types
}

m_type <- function(m, .name = NULL, .value = NULL, ...) {
    args <- list(...)
    arg_names <- c(names(args))
    if (!is.null(.name)) {
        if (is.null(.value)) {
            stop("Type value not provided")
        }
        args[[.name]] <- .value
        arg_names <- c(arg_names, .name)
    }
    if (length(args) == 0) return(m)
    
    for (i in 1:length(args)) {
        arg_name <- arg_names[i]
        if (is.null(arg_name)) {
            stop("Arg name not provided")
        }
    }
    r <- m
    for (i in 1:length(args)) {
        arg_name <- arg_names[i]
        arg_value <- args[[i]]
        if (!m_is_multi(r)) {
            types <- attr(r, "types")
            types[arg_name] <- arg_value
            attr(r, "types") <- types
            r <- m_as_dirty(r)
        }
        else {
            for (j in 1:length(r)) {
                types <- attr(r[[j]], "types")
                types[arg_name] <- arg_value
                attr(r[[j]], "types") <- types
                r[[j]] <- m_as_dirty(r[[j]])
                r <- m_as_dirty(r)
            }
        }
    }
    r
}


## -- Formulas --

# Set a formula value
m_set_formula <- function(m, .name = NULL, .value = NULL, .overwrite = TRUE, ...) {
    
    args <- list(...)
    arg_types <- c(names(args), .name)
    if (length(arg_types) == 0) return(m)
    
    if (!is.null(.name) && is.null(.value)) {
        .name <- NULL
    }
    m <- m_set(m, .name, .value, .overwrite, ...)
    m <- arg_types %>% purrr::reduce(function(m, arg_type) m_type(m, arg_type, "formula"), .init = m)
    m
}

## -- Parameters --

# Set a parameter value
# Setting a parameter without a name can be used to convert an existing arg into a parameter
m_set_parameter <- function(m, .name = NULL, .value = NULL, .overwrite = TRUE, ...) {
    
    args <- list(...)
    arg_types <- c(names(args), .name)
    if (length(arg_types) == 0) return(m)
    
    if (!is.null(.name) && is.null(.value)) {
        .name <- NULL
    }
    m <- m_set(m, .name, .value, .overwrite, ...)
    m <- arg_types %>% purrr::reduce(function(m, arg_type) m_type(m, arg_type, "parameter"), .init = m)
    m
}

## -- Cross Join Pipes ---

m_cross <- function(m, ..., .parameter = NULL) {
    args <- list(...)
    arg_names <- names(args)

    if (length(args) == 0) return(m)
    
    name_m <- m_name(m)
    if (!m_is_multi(m)) {
        m <- list(m)
    }

    r <- list()
    for (i in 1:length(m)) {
        l_i <- m[[i]]
        l_name = m_name(l_i)

        r_i <- list()
        for (j in 1:length(args)) {
            arg_formula <- args[[j]]
            arg_name <- arg_names[j]
            if (is.null(arg_name)) {
                arg_name <- sprintf("%s", j)
            }
            j_name <-  sprintf("%s.%s", l_name, arg_name)
            r_j <- l_i %>% m_named(j_name)
            r_j <- f_eval(f_interp(arg_formula), data = list(m = r_j, .m = r_j))
            stopifnot(m_is_task(r_j))
            if (!is.null(.parameter)) {
                r_j <- m_set_parameter(r_j, .parameter, arg_name)
            }
            r_i <- append(r_i, list(r_j))
            r_i <- m_as_dirty(r_i)
        }
        r <- append(r, r_i)
    }
    if (length(r) == 1) {
        r <- r[[1]]
    }
    else {
        names(r) <- r %>% map_chr(m_name)
        class(r) <- c(m_multi_class, m_task_class, class(r))
    }
    r <- r %>% m_named(name_m) %>% m_as_dirty()
    r
}

## -- Cross Apply Pipes ---

m_cross_apply <- function(m, x, f, .parameter = NULL) {

    stopifnot(is.vector(x) || is.list(x))
    
    if (is.vector(x)) {
        args <- as.list(x)
    }
    else {
        args <-x
    }
    arg_names <- names(args)
    
    if (length(args) == 0) return(m)
    
    name_m <- m_name(m)
    
    if (!m_is_multi(m)) {
        m <- list(m)
    }
    
    r <- list()
    for (i in 1:length(m)) {
        l_i <- m[[i]]
        l_name = m_name(l_i)
        
        r_i <- list()
        for (j in 1:length(args)) {
            arg_j <- args[[j]]
            arg_name_j <- arg_names[j]
            if (is.null(arg_name_j)) {
                arg_name_j <- sprintf("%s", j)
            }
            j_name <-  sprintf("%s.%s", l_name, arg_name_j)
            r_j <- l_i %>% m_named(j_name)
            r_j <- f_eval(f_interp(f), data = list(m = r_j, .m = r_j, .x = arg_j))
            stopifnot(m_is_task(r_j))
            if (!is.null(.parameter)) {
                r_j <- m_set_parameter(r_j, .parameter, arg_name_j)
            }
            r_i <- append(r_i, list(r_j))
            r_i <- m_as_dirty(r_i)
        }
        r <- append(r, r_i)
    }
    if (length(r) == 1) {
        r <- r[[1]]
    }
    else {
        names(r) <- r %>% map_chr(m_name)
        class(r) <- c(m_multi_class, m_task_class, class(r))
    }
    r <- r %>% m_named(name_m) %>% m_as_dirty()
    r
}


## Run task implementation
m_run_task <- function(m, diagnostics = FALSE) {

    eval_value <- function(x, args) {
        if (lazyeval::is_formula(x)) {
            r <- lazyeval::f_eval(x, args)
        }
        else if (is.function(x)) {
            r <- m_do_call(x, args = args)
        }
        else {
            r <- x
        }
        r
    }

    eval_formula <- function(x, args) {
        if (lazyeval::is_formula(x)) {
            r <- x
        }
        else if (is.function(x)) {
            r <- m_do_call(x, args = args)
        }
        else {
            r <- x
        }
        r
    }

    eval_stage <- function(pipe, stage_index, prev_state) {
        pipe_name <- attr(pipe, "name")
        stage_name <- names(pipe)[stage_index]

        if (diagnostics) {
            cat("Pipe: ", pipe_name, "Stage:", stage_name, "\n");
        }

        value <- pipe[[stage_index]]
        if (is.null(value)) {
            return(prev_state)
        }
        type <- m_types(pipe)[stage_name]
        is_m_formula <- !is.na(type) && type == "formula"
        state <- list()
        for (i in 1:length(prev_state)) {
            state_name_i <- names(prev_state)[i]
            eval_args <- append(list(name = state_name_i), prev_state[[state_name_i]])
            if (!is_m_formula) {
                evaluated <- eval_value(value, eval_args)
            }
            else {
                evaluated <- eval_formula(value, eval_args)
            }
            state[[state_name_i]] <- prev_state[[state_name_i]]
            state[[state_name_i]][stage_name] <- list(evaluated)
            
            types <- m_types(prev_state[[state_name_i]])
            types[stage_name] <- type
            attr(state[[state_name_i]], "types") <- types
        }
        state
    }
    
    stopifnot(m_is_task(m))
    task_name <- attr(m, "name")
    if (diagnostics) {
      cat(">>> Pipe:", task_name, "\n")
    }
    state <- list()
    state[[task_name]] <- list()
    if (length(m) > 0) {
      for (i in 1:length(m)) {
        state <- eval_stage(m, i, state)
      }
    }
    if (diagnostics) {
      cat("<<<\n")
    }
    state
}

m_run <- function(m, diagnostics = FALSE, cache = TRUE) {
    if (!m_is_dirty(m))
        return(m)
    
    cluster <- start_cluster(null_cluster())
    check_cluster <- function(m) {
        cluster_i <- m$cluster
        if (!is.null(cluster_i)) {
            if (cluster$id != cluster_i$id) {
                stop_cluster(cluster)
                cluster <<- start_cluster(cluster_i)
            }
        }
    }
    
    if (m_is_multi(m)) {
        r <- list()
        for (i in 1:length(m)) {
            check_cluster(m[[i]])
            r <- append(r, m_run_task(m[[i]], diagnostics))
        }
    }
    else {
        check_cluster(m)
        r <- m_run_task(m, diagnostics)
    }
    stop_cluster(cluster)
    
    if (length(r) == 1) {
        r <- r[[1]]
        attr(r, "name") <- attr(m, "name")
        class(r) <- c(m_task_class, class(r))
    }
    else {
        r_names <- names(r)
        for (i in 1:length(r)) {
            attr(r[[i]], "name") <- r_names[i]
            class(r[[i]]) <- c(m_task_class, "list")
        }
        attr(r, "name") <- attr(m, "name")
        class(r) <- c(m_multi_class, m_task_class, class(r))
    }
    r
    
}

# Query a task or a task list
# Converts the task into a parameised data frame
# Accepts any number of functions that converts each task into a list of column values that will be bound into each row
m_query <- function(m, ..., parameters = TRUE) {
    stopifnot(!m_is_dirty(m))
    stopifnot(m_is_task(m) || m_is_task_list(m))
    
    l <- m_as_task_list(m)
    name_builder <- function(l_i) {
        list(name = m_name(l_i))
    }
    parameter_builder <- function(l_i) {
        parameter_names <- m_types(l_i) %>% purrr::keep(~ . == "parameter") %>% names()
        parameters <- parameter_names %>% purrr::map(~ l_i[[.]]) %>% setNames(parameter_names)
        parameters
    }
    builders <- list(name_builder)
    if (parameters) {
        builders <- append(builders, parameter_builder)
    }
    builders <- append(builders, list(...))
    builder <- function(l_i) {
        builders %>% purrr::map(~ .(l_i)) %>% bind_cols()
    }
    l %>% purrr::map(builder) %>% bind_rows()
}

m_do_call <- function(.fcn, ..., args = NULL, alwaysArgs = NULL, .functions = list(.fcn), .ignoreUnusedArgs = TRUE, envir = parent.frame()) {

    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    # Validate arguments
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    # Argument '.fcn':
    if (is.function(.fcn)) {
    } else if (is.character(.fcn)) {
    } else {
        stop("Argument '.fcn' must be a character string: ", mode(.fcn));
    }

    # Argument '.functions':
    # Backward compatibility. /HB 2014-01-27
    if (is.character(.functions)) {
        .functions <- as.list(.functions);
    }
    if (!is.list(.functions)) {
        stop("Argument '.functions' must be a list: ", mode(.functions));
    }
    for (kk in seq_along(.functions)) {
        fcn <- .functions[[kk]];
        if (is.function(fcn)) next;
        if (!exists(fcn, mode = "function")) {
            stop("Argument '.functions' specifies a non-existing function: ", fcn);
        }
        fcn <- get(fcn, mode = "function");
        .functions[[kk]] <- fcn;
    }

    # Argument 'envir':
    stopifnot(is.environment(envir));


    # Put all arguments in a list.
    args <- c(list(...), args);

    # Keep only arguments part accepted by a set of known functions
    if (.ignoreUnusedArgs && length(.functions) > 0L) {
        fcnArgs <- lapply(.functions, FUN = function(fcn) {
            names(formals(fcn));
        })
        fcnArgs <- unlist(fcnArgs, use.names = FALSE);
        keep <- intersect(names(args), fcnArgs);
        args <- args[keep];
    }

    args <- c(args, alwaysArgs);
    do.call(.fcn, args = args, envir = envir);
}

## -- Cache --

# Define a cluster that does nothing
null_cluster <- function() {
    list(
        new_system_cluster = function() NULL,
        id = "null_cluster",
        type = "null_cluster"
    )
}

local_cluster <- function(cores) {
    list(
        new_system_cluster = function() parallel::makeCluster(cores),
        id = sprintf("local_cluster:cores=%s", cores),
        type = "local_cluster"
    )
}

start_cluster <- function(cluster) {
    stopifnot(cluster$status != "running")
    system_cluster <- cluster$new_system_cluster()
    if (!is.null(system_cluster)) {
        doParallel::registerDoParallel(system_cluster)
    }
    cluster$system_cluster <- system_cluster
    cluster$status <- "running"
    cluster
}

stop_cluster <- function(cluster) {
    stopifnot(cluster$status == "running")
    system_cluster <- cluster$system_cluster
    if (!is.null(system_cluster)) {
        parallel::stopCluster(system_cluster)
    }
    cluster$system_cluster <- NULL
    cluster$status <- "stopped"
    cluster
}


# Define the cluster for an m-task
m_cluster <- function(m, cluster = NULL) {
    if (!is.null(cluster))
        m_set(m, cluster = cluster)
    else
        m_default(m, cluster = null_cluster())
}

## -- Cache --

# Create an in memory cache
memory_cache <- function() memoise::cache_memory()

# Create a file system cache
filesystem_cache <- function(path) memoise::cache_filesystem(path)

# Define the cache for an M-task.
m_cache <- function(m, cache = NULL) {
    m_set(m, cache = cache)
}

## -- Recording --

# Call a function, with the given parameters, recording all messages, warnings and errors along the way
# Optionally using a result cache
# Returns an evaluation object that contains the result, error, execution times and the replay log
# By default environments are executed in the globalenv to accidently prevent accidentlal use of closures which
# are not correctly hashed when using cachine
# All information needed to execute the function should be passed as parameters 
record_value <- function(operation, evaluator, ..., cache = NULL, envir = .GlobalEnv, silent = FALSE) {
    
    args <- list(...)
    cache_key <- list(operation = operation, args = args)
    
    if (!is.null(cache)) {
        cache_key <- cache$digest(list(operation = operation, args = args))
        if (cache$has_key(cache_key)) {
            output <- cache$get(cache_key)
            return (output)
        }
    }
    
    result <- NULL
    succeded <- FALSE
    started <- lubridate::now()
    handler <- evaluate::new_output_handler(
        value = function(v) {
            result <<- v
            succeded <<- TRUE
        }
    )
    expr_envir <- new.env(parent = envir)
    assign("operation", operation, envir = envir)
    assign("evaluator", evaluator, envir = envir)
    assign("args", args, envir = envir)
    expr <- function() {
        message("-- '", operation, "' --")
        r <- do.call(evaluator, args)
        evaluate::flush_console()
        r
    }
    
    output <- evaluate::evaluate(expr, output_handler = handler, stop_on_error = 1, envir = envir)
    finished <- lubridate::now()
    messages <- output %>% purrr::keep(evaluate::is.message) %>% length() - 1
    warnings <- output %>% purrr::keep(evaluate::is.warning) %>% length()
    errors <- output %>% purrr::keep(evaluate::is.error) %>% length()
    error <- output %>% purrr::keep(evaluate::is.error) %>% first()
    recording <- list(
        operation = operation,
        result = result,
        succeded = succeded,
        log = output,
        error = error,
        errors = errors,
        messages = messages,
        warnings = warnings,
        problems = warnings + errors,
        started = started,
        finished = finished
    )
    if (!is.null(cache)) {
        cache$set(cache_key, recording)
    }
    if (!silent){
        replay(recording)
    }
    recording
}

# Replay an evaluation, redisplaying any messages, warnings or 
replay <- function(recording) {
    log <- recording$log %>% purrr::discard(evaluate::is.source)
    invisible(evaluate::replay(log))
}
