## -- Pre Processing --

m_no_transform <- function(m) m %>% m_set(pre_processors = c(m$pre_processors, "none"))

m_center_transform <- function(m) m %>% m_set(pre_processors = c(m$pre_processors, "center"))

m_scale_transform <- function(m) m %>% m_set(pre_processors = c(m$pre_processors, "scale"))

m_standardize_transform <- function(m) m %>% m_center_transform() %>% m_scale_transform()

m_range_transform <- function(m) m %>% m_set(pre_processors = c(m$pre_processors, "range"))

m_boxcox_transform <- function(m) m %>% m_set(pre_processors = c(m$pre_processors, "BoxCox"))

m_yeojohnson_transform <- function(m) m %>% m_set(pre_processors = c(m$pre_processors, "YeoJohnson"))

m_pca_transform <- function(m) m %>% m_set(pre_processors = c(m$pre_processors, "pca"))

m_ica_transform <- function(m, n.comp) {
    m %>%
        m_set(pre_processors = c(m$pre_processors, "ica")) %>%
        m_set(pre_processor_args = append(m$pre_processor_args, list(ica = list(n.comp = n.comp))))
}

m_spatial_sign_transform <- function(m) m %>% m_set(pre_processors = c(m$pre_processors, "spatialSign"))

m_knn_impute <- function(m) m %>% m_set(pre_processors = c(m$pre_processors, "knnImpute"))

m_median_impute <- function(m) m %>% m_set(pre_processors = c(m$pre_processors, "medianImpute"))

m_bag_impute <- function(m) m %>% m_set(pre_processors = c(m$pre_processors, "bagImpute"))

m_pre_process_param <- function(m) {

    build <- function(training_data, seed, response = NULL, pre_processors = NULL, pre_processor_args = NULL, engineered_data = NULL) {
        
        if (length(pre_processors) == 0) {
            return (NULL)
        }
        
        pre_processors <- pre_processors %>% purrr::discard(. == "none")

        if (length(pre_processors) == 0) {
            return (NULL)
        }
        
        if (!is.null(pre_processor_args)) {
            pre_processor_args <- pre_processor_args %>% flatten()
        }
        
        if (is.null(engineered_data)) {
            engineered_data <- training_data
        }
        divide <- m_divide(engineered_data, response)

        set.seed(seed)
        pre_process_args <- append(list(x = divide$x, method = pre_processors), pre_processor_args)
        safely(function() do.call(caret::preProcess, pre_process_args))()
    }

    m %>%
        m_seed() %>%
        m_split() %>%
        m_engineer() %>%
        m_default(pre_process_param = build)
}

m_pre_process <- function(data, response, pre_process_param) {
    if (is.null(pre_process_param)) {
        return(data)
    }
    divide <- m_divide(data, response)
    x <- predict(pre_process_param, newdata = as.data.frame(divide$x)) %>% as_tibble()
    y <- divide$y
    bind_cols(x, y)
}

m_pre_processed_data <- function(m) {

    build <- function(training_data, response = NULL, engineered_data = NULL, pre_process_param = NULL) {
        if (is.null(pre_process_param)) {
            return(NULL)
        }
        if (!is.null(pre_process_param$error)) {
            return(NULL)
        }
        if (is.null(engineered_data)) {
            engineered_data <- training_data
        }
        m_pre_process(engineered_data, response, pre_process_param$result)
    }

    m %>%
        m_split() %>%
        m_engineer() %>%
        m_pre_process_param() %>%
        m_default(pre_processed_data = build)
}

