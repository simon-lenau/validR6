# =========================== > .DOC_DECLARE_REF < =========================== #
.DOC_DECLARE_REF <-
    function(...) {
        fun_args <-
            as.list(substitute(list(...)))[-1L]

        unsupported_args <-
            names(fun_args)[
                !names(fun_args) %in%
                    names(formals(.doc_ref))
            ]

        if (length(unsupported_args) > 0) {
            stop(paste0(
                "Unsupported arguments:",
                "\n",
                paste0(" - ", unsupported_args, collapse = "\n"),
                "\n",
                "valid arguments are:",
                "\n",
                paste0(" - ", names(formals(.doc_ref)), collapse = "\n")
            ))
        }

        fun_args <-
            c(
                fun_args,
                formals(.doc_ref)[
                    !names(formals(.doc_ref)) %in% names(fun_args)
                ]
            )

        fun <-
            function() {}

        formals(fun) <-
            fun_args

        body(fun) <- expression({
            do.call(
                ".doc_ref",
                as.list(environment())
            )
        })

        # environment(fun) <-
        #     baseenv()
        fun
    }
# ────────────────────────────────── <end> ─────────────────────────────────── #
