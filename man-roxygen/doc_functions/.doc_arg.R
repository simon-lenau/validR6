# =============================== > .doc_arg < =============================== #

.doc_arg <-
    function(
        type,
        default,
        description,
        affected_methods,
        class = .doc_topic(),
        arg_name
    ) {
        if (!missing(default)) {
            default <-
                deparse(substitute(default))

            if (grepl("^eval", default[1])) {
                default <-
                    paste0(eval(parse(text = paste0(default, collapse = "\n"))))
                if (is.null(default) || length(default) == 0) {
                    default <-
                        "NULL"
                }
            }
        } else {
            if (
                missing(arg_name) &&
                    !is.null(knitr::opts_current$get(".doc_arg_name"))
            ) {
                arg_name <-
                    knitr::opts_current$get(".doc_arg_name")
            }

            if (
                missing(class) &&
                    !is.null(knitr::opts_current$get(
                        ".doc_class_name"
                    ))
            ) {
                class <-
                    knitr::opts_current$get(".doc_class_name")
            }
            if (
                !missing(arg_name) &&
                    !missing(class) &&
                    !is.null(class) &&
                    !is.null(arg_name)
            ) {
                default_vals <-
                    .doc_arg_default_from_class(
                        arg = arg_name,
                        class_name = class
                    )
                if (length(default_vals) > 0) {
                    default <-
                        default_vals[[1]]
                }
            }
        }

        if (
            !(missing(type) &&
                missing(default) &&
                missing(affected_methods) &&
                missing(description))
        ) {
            if (
                !(missing(type) &&
                    missing(default) &&
                    missing(affected_methods))
            ) {
                tbl <-
                    rbind(
                        if (!missing(type)) {
                            c("Type:", .doc_type(type))
                        },
                        if (!missing(default)) {
                            c("Default:", .doc_value(default, deparse = FALSE))
                        },
                        if (!missing(affected_methods)) {
                            .doc_arg_affected_methods(
                                class = class,
                                methods = affected_methods,
                                label = "Affected methods"
                            )
                        }
                    )

                # Format table output
                tbl <-
                    c(
                        .doc_tbl(
                            tbl,
                            alignment = c("l", "l"),
                            width = c(
                                nchar("Affected methods:"),
                                80 - nchar("Affected methods:")
                            )
                        ),
                        "<br>"
                    )
            } else {
                tbl <-
                    NULL
            }
            return(
                knitr::asis_output(paste0(
                    c(
                        if (!missing(description)) {
                            c(.doc_arg_description(description), "\n\n")
                        },
                        tbl
                    ),
                    collapse = "\n"
                ))
            )
        }
    }
# ────────────────────────────────── <end> ─────────────────────────────────── #

# ====================== > .doc_arg_affected_methods < ======================= #

.doc_arg_affected_methods <-
    function(
        class,
        methods,
        namespace = NULL,
        prefix = "",
        suffix = "",
        brackets = FALSE,
        label="Affected Methods"
    ) {
        method_names <-
            grep(
                paste0("(", paste0(methods, collapse = "|"), ")"),
                names(get(class)$public_methods),
                value = TRUE
            )
        output <-
            cbind(
                "",
                vapply(
                    method_names,
                    function(method) {
                        .doc_ref(
                            topic = class,
                            tag = method,
                            code = TRUE
                        )
                    },
                    character(1)
                )
            )
        output[1, 1] <-
            paste0(label,":")
        return(output)
    }

# ────────────────────────────────── <end> ─────────────────────────────────── #

# ========================= > .doc_arg_description < ========================= #

.doc_arg_description <-
    function(description) {
        if (length(description) > 1) {
            description <-
                paste0(
                    description,
                    ifelse(
                        c(
                            # No whitespace AFTER (
                            grepl(
                                "[\\(]$",
                                gsub(
                                    "\\n",
                                    "<br>",
                                    description[-length(description)],
                                    perl = TRUE
                                ),
                                perl = TRUE
                            ) |
                                # No whitespace BEFORE .;;)
                                grepl(
                                    "^[\\.\\,\\;\\)]",
                                    gsub(
                                        "\\n",
                                        "<br>",
                                        description[-1],
                                        perl = TRUE
                                    ),
                                    perl = TRUE
                                ),
                            TRUE
                        ),
                        "",
                        "\n"
                    )
                )
        }
        paste0(description, collapse = "")
    }
# ────────────────────────────────── <end> ─────────────────────────────────── #

# ===================== > .doc_arg_default_from_class < ====================== #


.doc_arg_default_from_class <-
    function(
        arg,
        members,
        class_name = .doc_topic()
    ) {
        if (
            length(class_name) != 1 ||
                !is.character(class_name) ||
                !exists(class_name) ||
                !"R6ClassGenerator" %in%
                    class(
                        class_gen <-
                            get(class_name)
                    )
        ) {
            stop(
                paste0(
                    "Argument `class_name` (",
                    .capture(class_name),
                    ") is not the name of a R6 class!"
                )
            )
        }

        if (missing(members)) {
            members <-
                c(
                    names(class_gen$public_fields),
                    names(class_gen$public_methods),
                    names(class_gen$active),
                    names(class_gen$private_fields),
                    names(class_gen$private_methods)
                )
        }

        default_vals <-
            list()

        for (member_list_name in c(
            grep("^public_", names(class_gen), value = TRUE),
            grep("^private_", names(class_gen), value = TRUE)
        )) {
            member_list <-
                class_gen[[member_list_name]]

            member_names <-
                grep(
                    paste0(
                        "(",
                        paste0(members, collapse = "|"),
                        ")"
                    ),
                    names(member_list),
                    value = TRUE,
                    perl = TRUE
                )

            for (member_name in member_names) {
                member <-
                    member_list[[member_name]]
                if (is.function(member)) {
                    if (arg %in% names(formals(member))) {
                        if (
                            !identical(
                                formals(member)[[arg]],
                                quote(expr = )
                            )
                        ) {
                            default_vals[paste0(
                                member_list_name,
                                "$",
                                member_name
                            )] <-
                                list(formals(member)[[arg]])
                        }
                    }
                }
            }
        }
        default_vals
    }
# ────────────────────────────────── <end> ─────────────────────────────────── #
