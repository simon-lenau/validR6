# ============================= > .doc_section < ============================= #

.doc_section <-
    function(section, level = 1, tag = NULL) {
        tag <-
            paste0(
                "sec-",
                .doc_topic(),
                "-",
                tolower(
                    gsub(
                        "\\s",
                        "--",
                        if (is.null(tag)) {
                            section
                        } else {
                            tag
                        }
                    )
                )
            )
        output <-
            paste0(
                .doc_tag(
                    tag = tag,
                    text = section
                ),
                if (level <= 1) {
                    ":"
                }
            )

        knitr::asis_output(output)
    }
# ────────────────────────────────── <end> ─────────────────────────────────── #