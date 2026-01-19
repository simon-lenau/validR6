# ============================ > .doc_r6class < ============================= #
.doc_r6class <-
    .DOC_DECLARE_REF(
        namespace = "R6",
        topic = "R6Class",
        suffix = "es"
    )
# ────────────────────────────────── <end> ─────────────────────────────────── #

# =========================== > .doc_r6class_public < ======================== #
.doc_r6class_public <-
    .DOC_DECLARE_REF(
        namespace = "R6",
        topic = "R6Class",
        text = "public"
    )
# ────────────────────────────────── <end> ─────────────────────────────────── #

# ========================== > .doc_r6class_private < ======================== #
.doc_r6class_private <-
    .DOC_DECLARE_REF(
        namespace = "R6",
        topic = "R6Class",
        text = "private"
    )
# ────────────────────────────────── <end> ─────────────────────────────────── #

# ============================ > .doc_r6class_method < ======================== #
.doc_r6class_method <-
    .DOC_DECLARE_REF(
        namespace = "R6",
        topic = "R6Class",
        text = "method"
    )
# ────────────────────────────────── <end> ─────────────────────────────────── #

# ========================= > .doc_r6class_active < ========================== #

.doc_r6class_active <-
    .DOC_DECLARE_REF(
        namespace = "R6",
        topic = "R6Class",
        text = "active"
    )
# ────────────────────────────────── <end> ─────────────────────────────────── #

# ======================== > .doc_r6class_activeBinding < ===================== #
.doc_r6class_activeBinding <-
    .DOC_DECLARE_REF(
        namespace = "R6",
        topic = "R6Class",
        suffix = " binding",
        text = "\\code{active}",
        code = FALSE
    )
# ────────────────────────────────── <end> ─────────────────────────────────── #

# ============================== > .doc_r6member < ============================ #
.doc_r6member <-
    .DOC_DECLARE_REF(
        topic = "R6Member"
    )
# ────────────────────────────────── <end> ─────────────────────────────────── #

# ======================== > .doc_r6member_activeBinding < ==================== #
.doc_r6member_activeBinding <-
    .DOC_DECLARE_REF(
        topic = "R6Member",
        tag = "active",
        suffix = " binding"
    )
# ────────────────────────────────── <end> ─────────────────────────────────── #

# ====================== > .doc_r6member_restrictions < ====================== #

.doc_r6member_restrictions <-
    .DOC_DECLARE_REF(
        topic = "R6Member",
        tag = "restrictions"
    )
# ────────────────────────────────── <end> ─────────────────────────────────── #

# ========================== > .doc_r6memberlist < =========================== #

.doc_r6memberlist <-
    .DOC_DECLARE_REF(
        topic = "R6MemberList"
    )
# ────────────────────────────────── <end> ─────────────────────────────────── #
# =================== > .doc_r6memberlist_outputMethods < ==================== #
.doc_r6memberlist_outputMethods <-
    function(method, ref_help = TRUE) {
        r6_ref <-
            function(prefix = "'", suffix = "'") {
                .doc_ref(
                    tag = method,
                    topic = "R6Class",
                    namespace = "R6",
                    text = paste0(
                        "\\code{",
                        method,
                        "}"
                    ),
                    prefix = prefix,
                    suffix = suffix,
                    code = FALSE
                )
            }
        paste(
            "named",
            .doc_type_list(),
            "that represent the",
            switch(
                method,
                "active" = "active bindings",
                "public" = "public fields",
                "private" = "private fields"
            ),
            "provided by the stored",
            paste0(.doc_r6member(suffix = "s"), "."),
            "<br>",
            "To be used as argument",
            r6_ref(),
            "in",
            .doc_ref(topic = "R6Class", namespace = "R6", suffix = "()"),
            "definition.",
            if (ref_help) {
                .doc_fmt_highlight(paste(
                    "See also the documentation of argument",
                    r6_ref(prefix = "'", suffix = "'"),
                    "in",
                    .doc_ref_help(topic = "r6class"),
                    "."
                ))
            }
        )
    }
# ────────────────────────────────── <end> ─────────────────────────────────── #
# ======================= > .doc_r6memberrestriction < ======================= #

.doc_r6memberrestriction <-
    .DOC_DECLARE_REF(
        topic = "R6MemberRestriction",
        link = TRUE
    )
# ────────────────────────────────── <end> ─────────────────────────────────── #

# ================= > .doc_r6memberrestriction_restriction < ================= #

.doc_r6memberrestriction_restriction <-
    .DOC_DECLARE_REF(
        topic = "R6MemberRestriction",
        tag = "restriction"
    )
# ────────────────────────────────── <end> ─────────────────────────────────── #


