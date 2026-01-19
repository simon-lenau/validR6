


# ============================ > .doc_data_load < ============================ #
.doc_data_load <-
    function(filename) {
        file <-
            .doc_data_path(filename)
        if (length(file) > 1) {
            stop(paste0("FILE:\n", paste0(file, collapse = "\n")))
        }

        if (!file.exists(file)) {
            "NULL"
        } else {
            env <-
                new.env()
            output <-
                get(
                    load(
                        file = file,
                        envir = env
                    )[1],
                    envir = env,
                    inherits = FALSE
                )

            return(output[[1]])
        }
    }
# ────────────────────────────────── <end> ─────────────────────────────────── #

# ============================ > .doc_data_path < ============================ #
.doc_data_path <-
    function(filename) {
        file.path(Sys.getenv("__DOC_DATA_DIR__"), paste0(filename, ".RData"))
    }
# ────────────────────────────────── <end> ─────────────────────────────────── #
# ============================ > .doc_data_save < ============================ #

.doc_data_save <-
    function(..., filename) {
        file <-
            .doc_data_path(filename)
        if (!dir.exists(dirname(file))) {
            dir.create(
                dirname(file),
                showWarnings = FALSE,
                recursive = TRUE
            )
        }
        content <-
            list(...)
        save(
            content,
            file = file
        )
    }

# ────────────────────────────────── <end> ─────────────────────────────────── #
# tag <-
#     "sec-R6Member-details"
# print(.doc_data_load(file.path("hypertargets", tag)))

# testfun <-
#     function(...){
#         print(lapply(...,quote))
#     }

#     testfun(a,b,c)
