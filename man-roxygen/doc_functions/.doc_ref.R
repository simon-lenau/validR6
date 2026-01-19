# =============================== > .doc_ref < =============================== #
.doc_ref <-
    function(
        namespace = NULL,
        topic = .doc_topic(),
        tag,
        prefix = "",
        suffix = "",
        brackets = FALSE,
        text = NULL,
        link = TRUE,
        code = TRUE
    ) {
        if (brackets) {
            prefix <-
                paste0("(", prefix)
            suffix <-
                paste0(suffix, ")")
        }

        pkg_namespace <-
            tryCatch(
                getNamespaceName(environment(sys.function())),
                error = function(e) NULL
            )

        if (
            is.null(namespace) ||
                (all(all.equal(namespace, pkg_namespace) == TRUE))
        ) {
            if (missing(tag)) {
                tag <-
                    NULL
            }
            text <-
                if (is.null(text)) {
                    if (topic == .doc_topic()) {
                        if (!is.null(tag)) {
                            gsub("^sec-", "", tag)
                        } else {
                            topic
                        }
                    } else {
                        paste0(
                            topic,
                            if (!missing(tag)) {
                                if (!is.null(tag)) paste0(".", tag)
                            }
                        )
                    }
                } else {
                    paste0(text, collapse = " ")
                }

            .doc_ref_internal(
                tag = tag,
                topic = topic,
                text = paste0(prefix, text, suffix),
                link = link,
                code = code
            )
        } else {
            text <-
                if (!is.null(text)) {
                    paste0(prefix, text, suffix)
                } else if (!missing(tag) && !is.null(tag)) {
                    paste0(prefix, tag, suffix)
                } else if (!is.null(topic)) {
                    paste0(prefix, topic, suffix)
                }
            .doc_ref_external(
                topic = topic,
                namespace = namespace,
                text = text,
                link = link,
                code = code
            )
        }
    }
# ────────────────────────────────── <end> ─────────────────────────────────── #

# ========================== > .doc_ref_external < =========================== #
.doc_ref_external <-
    function(topic, namespace, text = topic, link = TRUE, code = TRUE) {
        if (is.null(text) || grepl("^\\s*$", text, perl = TRUE)) {
            stop("Text must not be NULL or empty string")
        }
        if (code) {
            text <-
                paste0("\\code{", text, "}")
        }
        .doc_ref_link(
            namespace = namespace,
            topic = topic,
            text = text,
            link = link
        )
    }
# ────────────────────────────────── <end> ─────────────────────────────────── #
# ============================ > .doc_ref_field < ============================ #
.doc_ref_field <-
    .DOC_DECLARE_REF(
        text = paste0(
            c(if (topic != .doc_topic()) topic, c("$", tag)),
            collapse = ""
        )
    )
# ────────────────────────────────── <end> ─────────────────────────────────── #
# ==================== > .doc_ref_first_section_element < ==================== #

.doc_ref_first_section_element <-
    function(class_name, section) {
        if (!exists(class_name)) {
            return(invisible(NULL))
        }
        class_obj <-
            get(class_name)

        switch(
            section,
            "method" = (names(class_obj$public_methods))[1],
            "active" = (names(class_obj$active))[1],
            "attribute" = (names(class_obj$public_fields))[1],
            NULL
        )
    }
# ────────────────────────────────── <end> ─────────────────────────────────── #
# ============================ > .doc_ref_help < ============================= #

.doc_ref_help <-
    function(
        namespace = NULL,
        topic = .doc_topic(),
        link = TRUE
    ) {
        ref_name <-
            paste0(".doc_", tolower(topic))
        if (!exists(ref_name)) {
            .doc_ref(
                namespace = namespace,
                topic = topic,
                link = link,
                prefix = "?",
                code = TRUE
            )
        } else {
            get(ref_name)(prefix = "?", suffix = "", link = FALSE)
        }
    }
# ────────────────────────────────── <end> ─────────────────────────────────── #

# ========================== > .doc_ref_internal < =========================== #
.doc_ref_internal <-
    function(
        tag,
        topic = .doc_topic(),
        text = paste0(topic, ".", tag),
        link = TRUE,
        code = TRUE
    ) {
        if (missing(tag)) {
            tag <-
                NULL
        }
        text <-
            paste0(text, collapse = "")
        if (is.null(text) || grepl("^\\s*$", text, perl = TRUE)) {
            stop("Text must not be NULL or empty string")
        }
        if (code) {
            text <-
                paste0(
                    "\\code{",
                    gsub("(')", "}\\1\\\\code{", text, perl = TRUE),
                    "}"
                )
        }

        output <-
            .doc_ref_link(
                topic = topic,
                tag = tag,
                text = text,
                namespace = NULL,
                link = link
            )
        return(output)
    }
# ────────────────────────────────── <end> ─────────────────────────────────── #
# ======================== > .doc_ref_is_in_package < ======================== #

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
# ============================ > .doc_ref_link < ============================= #

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
# =========================== > .doc_ref_method < ============================ #
.doc_ref_method <-
    .DOC_DECLARE_REF(
        text = paste0(
            c(if (topic != .doc_topic()) topic, c("$", tag, "()")),
            collapse = ""
        )
    )
# ────────────────────────────────── <end> ─────────────────────────────────── #

# =========================== > .doc_ref_section < =========================== #
.doc_ref_section <-
    function(
        section,
        topic = .doc_topic(),
        text = if (topic == .doc_topic()) {
            section
        },
        # else {
        #     paste0("section '", section, "' in ", .doc_ref_help(topic = topic))
        # },
        tag
    ) {
        # ToDo: Store & access section names if possible
        sectionx <-
            switch(
                tolower(section),
                "active bindings" = "active",
                "methods" = "method",
                "attributes" = "attribute",
                tolower(section)
            )
        if (!sectionx %in% c("active", "method", "attribute")) {
            tag <-
                paste0("sec-", topic, "-", sectionx)
            .doc_ref(
                topic = topic,
                tag = tag,
                text = if (!missing(text)) {
                    paste(text, collapse = " ")
                } else {
                    gsub(
                        "^([[:upper:]])",
                        "\\L\\1",
                        .doc_data_load(file.path("hypertargets", tag)),
                        perl = TRUE
                    )
                    # .doc_data_load(file.path("hypertargets", tag))
                },
                code = FALSE
            )
        } else {
            .doc_ref(
                topic = topic,
                tag = if (missing(tag)) {
                    .doc_ref_first_section_element(topic, sectionx)
                } else {
                    tag
                },
                text = if (!missing(text)) {
                    paste(text, collapse = " ")
                } else {
                    section
                },
                code = FALSE
            )
        }
    }
# ────────────────────────────────── <end> ─────────────────────────────────── #

# ============================ > .doc_ref_self < ============================= #
.doc_ref_self <-
    function(
        tag,
        prefix = "",
        suffix = "",
        brackets = FALSE,
        text = NULL,
        code = TRUE
    ) {
        args <- mget(names(formals()), envir = environment())

        do.call(
            ".doc_ref",
            c(
                args,
                list(link = !(missing(tag) || is.null(tag)))
            )
        )
    }
# ────────────────────────────────── <end> ─────────────────────────────────── #
