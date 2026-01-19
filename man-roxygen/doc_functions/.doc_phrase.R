# ======================= > .doc_phrase_constructor < ======================== #

.doc_phrase_constructor <-
    function() {
        paste("Construct a new", .doc_ref_self(), "object")
    }
# ────────────────────────────────── <end> ─────────────────────────────────── #

# ======================= > .doc_phrase_r6declaring < ======================== #

.doc_phrase_r6declaring <-
    function() {
        c("To be used when declaring", .doc_r6class())
    }
# ────────────────────────────────── <end> ─────────────────────────────────── #

# =================== > .doc_phrase_restriction_example < ==================== #

.doc_phrase_restriction_example <-
    function(
        attr = "I",
        link = "base:AsIs",
        rhs = NULL,
        brackets = TRUE,
        value = .doc_phrase_value()
    ) {
        paste0(
            if (brackets) "(" else NULL,
            "\\code{",
            "\\link[",
            link,
            "]{",
            attr,
            "}",
            "(",
            gsub("`", "", gsub("\\code\\{(.*?)\\}", "\\1", value)),
            ")",
            if (!is.null(rhs)) {
                gsub("\\s*$", "", paste0(" ", gsub("^\\s*", "", rhs)))
            },
            "}",
            if (brackets) ")" else NULL
        )
    }
# ────────────────────────────────── <end> ─────────────────────────────────── #
# ========================== > .doc_phrase_value < =========================== #
.doc_phrase_value <-
    function() {
        "`value`"
    }
# ────────────────────────────────── <end> ─────────────────────────────────── #