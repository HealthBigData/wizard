Wizard <- R6::R6Class(
  "Wizard",
  private = list(
    questions = NULL,
    questions_f = NULL,
    answers = NULL,
    cancelled = NULL,

    prepare_question = function(name) {

      private$questions_f[[name]] <- wizard::prepare_question(
        question = private$questions[[name]][["question"]],
        answers  = private$questions[[name]][["answers"]],
        default  = private$questions[[name]][["default"]],
        exitable = private$questions[[name]][["exitable"]]
      )

      private$answers[[name]] <- ""

    },

    prepare_all_questions = function() {

      lapply(names(private$questions), function(q) {
        private$prepare_question(q)
      })

    },

    is.prepared = function(name) {
      name %in% names(private$questions_f)
    }
  ),

  public = list(

    initialize = function(questions = NULL) {

      private$questions_f <- vector("list")
      private$answers <- vector("list")
      private$cancelled <- FALSE


      if (is.list(questions)) {
        private$questions <- questions
        private$prepare_all_questions()

      } else if (is.character(questions)) {
        if (!stringr::str_detect(questions, ".yml?")) {
          stop("file question must be in yaml format.")
        }

        if (!file.exists(questions)) {
          stop("file not found.")
        }

        private$questions <- yaml::read_yaml(questions)
        private$prepare_all_questions()

      } else{
        private$questions <- vector("list")
      }
    },

    set_questions = function(questions) {
      private$questions <- questions
      private$prepare_all_questions()
    },

    get_questions = function() {
      private$questions
    },

    add_question = function(name,
                            question,
                            answers = NULL,
                            default = NULL,
                            exitable = TRUE) {

      item <- list(
        question = question,
        answers = answers,
        default = default,
        exitable = exitable
      )

      private$questions[[name]] <- item
      private$prepare_question(name)
    },

    preview = function(name) {
      if (!private$is.prepared(name)) {
        stop("Question is not executable.")
      }
      private$questions_f[[name]](preview = TRUE)
    },

    is.cancelled = function() {
      private$cancelled
    },

    ask = function(name) {
      if (!private$is.prepared(name)) {
        stop("Question is not executable.")
      }

      if (!private$cancelled) {
        private$answers[[name]] <- private$questions_f[[name]]()
        if (private$answers[[name]] == "cancelled") {
          private$cancelled <- TRUE
        }
      }
    },

    get_answer_to = function(name) {
      if (!name %in% names(private$answers)) {
        stop(paste0("No answer for question ", name))
      }
      private$answers[[name]]
    },

    read_questions = function(file) {
      private$questions <- yaml::read_yaml(file)
      private$prepare_all_questions()
    },

    save_questions = function(file) {
      yaml::write_yaml(private$questions, file)
    }
  )
)
