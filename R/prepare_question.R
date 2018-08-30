prepare_question <- function(question, answers, default, exitable = FALSE) {
  list_answers <- !is.null(answers)
  answers_strings <- NULL

  if (list_answers) {

    if (is.null(names(answers))) {
      names(answers) <- 1:length(answers)
    }

    answers_strings <- lapply(seq_along(answers), function(a) {
      paste0(names(answers)[a], ": ", answers[a])
    })

    if (!is.null(default)) {
      if (!default %in% names(answers)) {
        stop("Default value not in acceptable answers", call. = FALSE)
      }
      answers_strings[length(answers) + 1] <- paste0("default: ", default)
    }
    answers_strings <- paste0(answers_strings, collapse = "\n")
  } else {
    if (!is.null(default)) {
      question <- paste0(question, " (default: ", default, ")")
    }
  }

  if (exitable) {
    question <- paste0(question, " (type q to exit)\n")
  } else {
    question <- paste0(question, "\n")
  }

  answered <- FALSE

  function(preview = FALSE) {

    if (preview) {
      cat(question, answers_strings, sep = "")
    } else {

      cat(question, answers_strings, sep = "")

      while (!answered) {

        answer <- readline()
        if (exitable & answer == "q") {
          cat("Cancelled by user")
          answer <- "cancelled"
          answered <- TRUE

        } else {
          if (list_answers) {
            if (answer == "" & !is.null(default)) {
              answer <- default
            }

            if (!answer %in% names(answers)) {
              cat("Not understood your answer, please choose a answer in the liste above.\n")
            } else {
              answered <- TRUE
            }
          } else {
            if (!is.null(default) & answer == "") {
              answer <- default
            }
            answered <- TRUE
          }
        }
      }
      answer
    }
  }
}
