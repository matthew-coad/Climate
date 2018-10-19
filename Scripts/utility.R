

# Excecutes the given expression, ensuring that the requested libraries
with_nameSpace <- function(.library, .expr) {
    if (suppressPackageStartupMessages(require(.library, character.only = TRUE))) {
        result <- rlang::eval_tidy(.expr)
        base::unloadNamespace(.library)
    }
    result
}