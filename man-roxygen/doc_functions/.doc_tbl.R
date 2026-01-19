# =============================== > .doc_tbl < =============================== #

.doc_tbl <-
    function(table, alignment = NULL, width = NULL) {
        col_names <-
            if (is.null(colnames(table))) {
                rep(" ", ncol(table))
            } else {
                colnames(table)
            }

        if (is.null(alignment)) {
            alignment <-
                rep("l", ncol(table))
        }
        if (is.null(width)) {
            col_widths <-
                rep(0, ncol(table))
        } else {
            col_widths <-
                rep(width, length.out = ncol(table))
        }

        if (any(col_widths <= 0)) {
            pos <-
                which(col_widths <= 0)
            col_widths[pos] <-
                vapply(
                    pos,
                    function(j) max(nchar(table[, j])),
                    integer(1)
                )
        }

        alignment <-
            rep(alignment, length.out = ncol(table))

        # Format table columns to fixed width
        #   (pad width spaces)
        for (j in 1:ncol(table)) {
            table[, j] <-
                if (alignment[j] %in% c("l", "r")) {
                    .doc_fmt_pad(
                        table[, j],
                        width = width[j],
                        before = (alignment[j] == "r")
                    )
                } else {
                    .doc_fmt_pad(
                        .doc_fmt_pad(
                            table[, j],
                            width = floor(width[j] / 2),
                            before = FALSE
                        ),
                        width = ceiling(width[j] / 2),
                        before = TRUE
                    )
                }
        }

        knitr::asis_output(
            paste0(
                c(
                    .doc_tbl_row(col_names),
                    .doc_tbl_alignment(alignment),
                    vapply(
                        seq_len(nrow(table)),
                        function(i) {
                            .doc_tbl_row(table[i, ])
                        },
                        character(1)
                    )
                ),
                collapse = "\n"
            )
        )
    }
# ────────────────────────────────── <end> ─────────────────────────────────── #
# ========================== > .doc_tbl_alignment < ========================== #

.doc_tbl_alignment <-
    function(alignments) {
        .doc_tbl_row(
            vapply(
                alignments,
                function(alignment) {
                    if (alignment == "l") {
                        ":---"
                    } else if (alignment == "c") {
                        ":---:"
                    } else if (alignment == "r") {
                        "---:"
                    }
                },
                character(1)
            )
        )
    }
# ────────────────────────────────── <end> ─────────────────────────────────── #
# ============================= > .doc_tbl_row < ============================= #

.doc_tbl_row <-
    function(col_entries) {
        paste0(
            "| ",
            paste0(col_entries, collapse = " | "),
            " |"
        )
    }
# ────────────────────────────────── <end> ─────────────────────────────────── #
