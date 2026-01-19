# ================================= > .doc_type < ============================= #
.doc_type <-
    function(type, brackets = FALSE) {
        helper_name <-
            paste0(".doc_", type)
        if (exists(helper_name)) {
            type <-
                get(helper_name)(brackets = brackets)
        } else {
            type <-
                paste0("", type, "")
        }
        return(type)
    }
# ────────────────────────────────── <end> ─────────────────────────────────── #

# ========================= > .doc_type_character < ========================== #
.doc_type_character <-
    .DOC_DECLARE_REF(
        namespace = "base",
        topic = "character"
    )
# ────────────────────────────────── <end> ─────────────────────────────────── #

# ================================ > .doc_error < ============================= #
.doc_error <-
    .DOC_DECLARE_REF(
        namespace = "base",
        topic = "stop",
        text = "error"
    )
# ────────────────────────────────── <end> ─────────────────────────────────── #

# ========================= > .doc_type_expression < ========================= #
.doc_type_expression <-
    .DOC_DECLARE_REF(
        namespace = "base",
        topic = "expression"
    )
# ────────────────────────────────── <end> ─────────────────────────────────── #

# ========================== > .doc_type_function < ========================== #
.doc_type_function <-
    .DOC_DECLARE_REF(
        namespace = "base",
        topic = "function"
    )
# ────────────────────────────────── <end> ─────────────────────────────────── #

# ============================ > .doc_type_list < ============================ #
.doc_type_list <-
    .DOC_DECLARE_REF(
        namespace = "base",
        topic = "list"
    )
# ────────────────────────────────── <end> ─────────────────────────────────── #

# ========================== > .doc_type_numeric < =========================== #
.doc_type_numeric <-
    .DOC_DECLARE_REF(
        namespace = "base",
        topic = "numeric"
    )
# ────────────────────────────────── <end> ─────────────────────────────────── #

# ========================== > .doc_type_logical < =========================== #
.doc_type_logical <-
    .DOC_DECLARE_REF(
        namespace = "base",
        topic = "logical"
    )
# ────────────────────────────────── <end> ─────────────────────────────────── #
