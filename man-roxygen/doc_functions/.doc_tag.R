# =============================== > .doc_tag < =============================== #
.doc_tag <-
    function(tag, topic = .doc_topic(), text = "") {
        .doc_tag_hypertarget(
            anchor = .doc_tag_anchor(
                tag = tag,
                topic = topic
            ),
            text = text
        )
    }
# ────────────────────────────────── <end> ─────────────────────────────────── #

# =========================== > .doc_tag_anchor < ============================ #
.doc_tag_anchor <-
    function(tag, topic = .doc_topic()) {
        if (missing(topic) || is.null(topic)) {
            topic <-
                NULL
        }

        tag_group <-
            .doc_tag_group(tag = tag, topic = topic)
        anchor <-
            paste0(
                c(
                    tag_group,
                    # In case tag is "initialize", it needs to be switched.
                    #   This is due to the  `initialize` method being
                    #   wrapped in the `new` method
                    #   by default R6Class behaviour,
                    if (!missing(tag) && !is.null(tag)) {
                        switch(
                            tag,
                            "initialize" = "new",
                            tag
                        )
                    }
                    # "="
                ),
                collapse = "-"
            )

        if (topic != .doc_topic()) {
            anchor <-
                paste0(c(topic, anchor), collapse = "#")
        }
        return(
            .doc_tag_anchor_sanitize(anchor)
        )
    }
# ────────────────────────────────── <end> ─────────────────────────────────── #

# ======================= > .doc_tag_anchor_sanitize < ======================= #

.doc_tag_anchor_sanitize <-
    function(anchor) {
        gsub("\\#\\s*$", "", anchor)
    }
# ────────────────────────────────── <end> ─────────────────────────────────── #

# ============================ > .doc_tag_group < ============================ #

.doc_tag_group <-
    function(tag, topic = .doc_topic()) {
        if (is.null(tag) || missing(tag)) {
            return(NULL)
        }
        if (exists(topic)) {
            topic_obj <-
                get(topic)
            tag_group <-
                if (
                    "R6ClassGenerator" %in%
                        class(topic_obj) &&
                        !is.null(tag) &&
                        !missing(tag)
                ) {
                    if (tag %in% names(topic_obj$public_methods)) {
                        "method"
                    } else if (tag %in% names(topic_obj$active)) {
                        "active"
                    } else if (tag %in% names(topic_obj$public_fields)) {
                        "attribute"
                    } else {
                        NULL
                    }
                } else {
                    NULL
                }
        } else {
            tag_group <-
                NULL
        }
        if (!is.null(tag_group)) {
            tag_group <-
                paste0(tag_group, "-", topic)
        }
    }
# ────────────────────────────────── <end> ─────────────────────────────────── #
# ========================= > .doc_tag_hypertarget < ========================= #

.doc_tag_hypertarget <-
    function(anchor, text = "") {
        opt_name <-
            paste0("target_", anchor, "_")

        last_index <-
            .doc_data_load(
                filename = file.path("hypertargets", opt_name)
            )
        if (last_index == "NULL") {
            last_index <-
                0
        }
        current_index <-
            getOption(opt_name) + 1

        if (is.null(current_index) || length(current_index) == 0) {
            current_index <-
                1
        }

        options(
            setNames(
                list(current_index),
                opt_name
            )
        )

        if (current_index > last_index) {
            .doc_data_save(
                current_index,
                filename = file.path("hypertargets", opt_name)
            )
        } else if (current_index < last_index) {
            return(text)
        }

        # if (anchor %in% getOption()) {
        #     return(text)
        # } else {
        #     options(
        #         "__DOC_HYPERTARGETS__" = c(
        #             getOption("__DOC_HYPERTARGETS__"),
        #             anchor
        #         )
        #     )
        # }

        if (is.character(text) && text != "") {
            .doc_data_save(
                text,
                filename = file.path("hypertargets", anchor)
            )
        }

        paste0(
            c(
                paste0(
                    "\\if{html}{\\out{<a id=\"",
                    anchor,
                    "\">",
                    text,
                    "</a>}}"
                ),
                paste0(
                    "\\if{latex}{",
                    "\\out{",
                    "\\hypertarget{",
                    anchor,
                    "}{",
                    text,
                    "}",
                    "}",
                    "}"
                )
            ),
            collapse = ""
        )
    }
# ────────────────────────────────── <end> ─────────────────────────────────── #
