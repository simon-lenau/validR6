# ======================= > .doc_in_current_package < ======================== #

.doc_ref_is_in_package <-
    function(namespace) {
        pkg_namespace <-
            tryCatch(
                getNamespaceName(environment(sys.function())),
                error = function(e) NULL
            )
        return(
            (is.null(namespace) ||
                (all(
                    all.equal(
                        namespace,
                        pkg_namespace
                    ) ==
                        TRUE
                )))
        )
    }
# ────────────────────────────────── <end> ─────────────────────────────────── #
