# ================================ > .doc_topic < ============================= #
.doc_topic <-
    function() {
        Sys.getenv("__DOC_TOPIC__")
    }
# ────────────────────────────────── <end> ─────────────────────────────────── #

# ============================ > .doc_topic_set < ============================ #
.doc_topic_set <-
    function(topic, check_doc_function = TRUE) {
        doc_function <-
            paste0(".doc_", tolower(topic))
        if (check_doc_function && (!exists(doc_function, mode = "function"))) {
            stop(paste0(
                "Topic '",
                topic,
                ": missing doc function `",
                doc_function,
                "`"
            ))
        }
        Sys.setenv("__DOC_TOPIC__" = topic)
        .doc_tag_hypertarget(anchor = topic)
    }
# ────────────────────────────────── <end> ─────────────────────────────────── #