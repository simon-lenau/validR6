# =========================== > .doc_fmt_cslist < ============================ #

.doc_fmt_cslist <-
    function(..., sep = ", ", last = " and") {
        items <-
            c(...)
        if (length(items) == 1) {
            items[[1]]
        } else {
            return(
                paste0(
                    items,
                    c(rep(sep, length(items) - 2), last, ""),
                    collapse = " "
                )
            )
        }
    }
# ────────────────────────────────── <end> ─────────────────────────────────── #

# ========================== > .doc_fmt_describe < =========================== #

.doc_fmt_describe <-
    function(
        ...,
        list_char = "    
            - "
    ) {
        knitr::asis_output(
            .doc_tbl(
                cbind(
                    .doc_fmt_pad(
                        ...names(),
                        width = max(nchar(...names())) + 3
                    ),
                    .doc_fmt_pad(vapply(list(...), as.character, character(1)))
                )
            )
        )
    }
# ────────────────────────────────── <end> ─────────────────────────────────── #

# ========================== > .doc_fmt_highlight < ========================== #

.doc_fmt_highlight <-
    function(text) {
        text <-
            paste0(text, collapse = " ")
        paste0("<br>", "_", text, "_")
    }
# ────────────────────────────────── <end> ─────────────────────────────────── #
# ============================= > .doc_fmt_pad < ============================= #

.doc_fmt_pad <-
    function(content, width = NULL, pad_char = "&nbsp;", before = FALSE) {
        tryCatch(
            {
                if (is.null(width) || width <= 0 || is.na(width)) {
                    width <-
                        max(nchar(content))
                }
            },
            error = function(e) {
                stop("Table width is: ", width, "\n")
            }
        )
        padding <-
            vapply(
                content,
                function(x) {
                    paste0(
                        rep(
                            pad_char,
                            max(c((width - nchar(x)) / nchar(pad_char), 0))
                        ),
                        collapse = ""
                    )
                },
                character(1)
            )
        if (is.character(pad_char)) {
            padding <-
                paste0("", padding, "")
        }

        if (before) {
            return(paste0("", padding, content, ""))
        } else {
            return(paste0("", content, padding, ""))
        }
    }
# ────────────────────────────────── <end> ─────────────────────────────────── #
# ========================= > .doc_fmt_print_what < ========================== #

.doc_fmt_print_what <-
    function(...) {
        paste(
            "print",
            if (...length() == 0) {
                "nothing"
            } else {
                .doc_fmt_cslist(...)
            }
        )
    }
# ────────────────────────────────── <end> ─────────────────────────────────── #
