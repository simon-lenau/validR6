cat("Running man-roxygen/_roxygen_setup.R\n")

Sys.setenv(
    "__DOC_DATA_DIR__" = tools::file_path_as_absolute(file.path(
        "man-roxygen",
        "data"
    ))
)

options("__DOC_HYPERTARGETS__" = character(0))

# Load all documentation functions in dependency order
doc_functions_dir <- file.path("man-roxygen", "doc_functions")
doc_files <- c(
    ".DOC_DECLARE_REF",
    ".doc_ref",
    ".doc_arg",
    ".doc_data",
    ".doc_fmt",
    ".doc_method",
    ".doc_phrase",
    ".doc_r6",
    ".doc_section",
    ".doc_tag",
    ".doc_tbl",
    ".doc_topic",
    ".doc_type",
    ".doc_value"
)
for (file in doc_files) {
    source(file.path(doc_functions_dir, paste0(file, ".R")))
}


knitr::knit_hooks$set(
    doc_tag = function(before, options, envir, name, ...) {
        if (before) {
            .doc_tag(
                tag = options[[name]]
            )
        }
    }
)
