#' @include internals-.doc_in_current_package.R
# ============================ > .doc_ref_link < ============================ #
.doc_ref_link <-
    function(topic, tag, text, namespace = NULL, link = TRUE) {
        if (missing(tag)) {
            tag <-
                NULL
        }
        if (missing(topic)) {
            topic <-
                .doc_topic()
        }

        if (
            # If link is disabled or would simply lead to the beginning of the current page:
            #   don't create any link
            (link %in% FALSE) ||
                (topic == .doc_topic() && is.null(tag))
        ) {
            return(text)
        }

        topic_tag_link <-
            .doc_tag_anchor(
                tag = tag,
                topic = topic
            )

        topic_link <-
            paste0(
                gsub(
                    "#.*$",
                    "",
                    topic_tag_link
                )
            )

        tag_link <-
            paste0(
                "#",
                gsub(
                    ".*?#",
                    "",
                    topic_tag_link
                )
            )

        namespace_link <-
            paste0(namespace, ":", topic_tag_link)

        knitr::asis_output(
            paste0(
                "\\ifelse{html}{",
                # ============================== > HTML Links < ============================== #
                # ┌┌────────────────────────────────────────────────────────────────────────┐┐ #
                # ││ This part constructs the HTML links.                                   ││ #
                # ││ - Linking to sections works only within the same topic                 ││ #
                # ││ - Linking to topics in other packages is possible                      ││ #
                # └└────────────────────────────────────────────────────────────────────────┘┘ #
                if (any(c("true", "html") %in% tolower(link))) {
                    if (.doc_ref_is_in_package(namespace)) {
                        if (topic == .doc_topic()) {
                            paste0(
                                "\\href{",
                                # "#",
                                tag_link,
                                "}{",
                                text,
                                "}"
                            )
                        } else {
                            paste0(
                                "\\link[=",
                                topic_link,
                                "]{",
                                text,
                                "}"
                            )
                        }
                    } else {
                        paste0(
                            "\\link[",
                            namespace_link,
                            "]{",
                            text,
                            "}"
                        )
                    }
                } else {
                    text
                },
                # ────────────────────────────────── <end> ─────────────────────────────────── #
                "}{",
                "\\ifelse{latex}{",
                # ============================= > Latex Links < ============================== #
                # ┌┌────────────────────────────────────────────────────────────────────────┐┐ #
                # ││ This part constructs the Latex links.                                  ││ #
                # ││ - Linking to sections works between topics                             ││ #
                # ││ - Linking to topics in other packages not possible                     ││ #
                # └└────────────────────────────────────────────────────────────────────────┘┘ #
                if (
                    any(c("true", "latex") %in% tolower(link)) &&
                        .doc_ref_is_in_package(namespace)
                ) {
                    paste0(
                        "\\href{",
                        tag_link,
                        "}",
                        "{",
                        text,
                        "}"
                    )
                } else {
                    text
                },
                # ────────────────────────────────── <end> ─────────────────────────────────── #
                "}{",
                # ========================= > Other Formats' Links < ========================= #

                # ┌┌────────────────────────────────────────────────────────────────────────┐┐ #
                # ││ This part constructs the links for non-latex / html formats            ││ #
                # ││ - unclear if this is ever needed                                       ││ #
                # └└────────────────────────────────────────────────────────────────────────┘┘ #
                paste0(
                    "\\link[",
                    if (.doc_ref_is_in_package(namespace)) {
                        paste0(
                            "=",
                            if (.doc_topic() == topic) {
                                .doc_topic()
                            } else {
                                topic_link
                            }
                        )
                    } else {
                        paste0(
                            namespace,
                            ":",
                            topic
                        )
                    },
                    "]{",
                    text,
                    "}",
                    # ────────────────────────────────── <end> ─────────────────────────────────── #
                    "}",
                    "}"
                )
            )
        )
    }

# ────────────────────────────────── <end> ─────────────────────────────────── #
