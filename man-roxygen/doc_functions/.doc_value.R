# ================================ > .doc_value < ============================ #
.doc_value <-
    function(value, deparse = TRUE) {
        if (deparse && !is.character(value)) {
            value <-
                deparse(substitute(value))
        }
        if (is.null(value)) {
            value <-
                "NULL"
        }
        value <-
            paste0(value, collapse = "")
        helper_name <-
            paste0(".doc_value_", value)
        if (exists(helper_name)) {
            get(helper_name)(brackets = FALSE)
        } else {
            paste0("\\code{", value, "}")
        }
    }
# ────────────────────────────────── <end> ─────────────────────────────────── #

# =========================== > .doc_value_NULL < ============================ #
.doc_value_NULL <-
    function(prefix = "", suffix = "", brackets = FALSE, text = NULL) {
        .doc_ref(
            namespace = "base",
            topic = "NULL",
            prefix = prefix,
            suffix = suffix,
            brackets = brackets,
            text = text
        )
    }
# ────────────────────────────────── <end> ─────────────────────────────────── #


