#' Validate Fireworks API key is available
#'
#' Checks if FIREWORKS_API_KEY environment variable is set and provides
#' helpful setup instructions if not available.
#'
#' @return Invisible NULL if valid, stops with error otherwise
#' @keywords internal
validate_fireworks_available <- function() {
  api_key <- Sys.getenv("FIREWORKS_API_KEY")

  if (api_key == "") {
    cli_abort(c(
      "{.envvar FIREWORKS_API_KEY} not found.",
      "i" = "Please set up your Fireworks API key:",
      " " = "1. Sign up at {.url https://fireworks.ai}",
      " " = "2. Get your API key from {.url https://fireworks.ai/api-keys}",
      " " = "3. Add to .Renviron: {.code usethis::edit_r_environ()}",
      " " = "4. Add line: {.code FIREWORKS_API_KEY=fw_...}",
      " " = "5. Restart R session"
    ))
  }

  invisible(NULL)
}

#' Define the feedback tool for Fireworks LLM evaluation
#'
#' Creates the tool definition in OpenAI/Fireworks format for structured feedback.
#'
#' @return List containing tool definition for Fireworks API
#' @keywords internal
define_fireworks_feedback_tool <- function() {
  list(
    type = "function",
    "function" = list(
      name = "respond_with_feedback",
      description = "Provide structured feedback on student code",
      parameters = list(
        type = "object",
        required = c("is_correct", "feedback_message"),
        properties = list(
          is_correct = list(
            type = "boolean",
            description = "TRUE if code meets requirements, FALSE otherwise"
          ),
          feedback_message = list(
            type = "string",
            description = "Brief, encouraging feedback (2-3 sentences)"
          )
        )
      )
    )
  )
}

#' Build an httr2 request for the Fireworks chat completions API
#'
#' @param api_key Character string API key
#' @param model Character string of Fireworks model name
#' @param prompt Character string containing the full prompt
#' @param tools List of tool definitions
#' @return An httr2 request object
#' @keywords internal
build_fireworks_request <- function(api_key, model, prompt, tools) {
  messages <- list(
    list(
      role = "system",
      content = paste0(
        "You MUST call the respond_with_feedback tool exactly once. ",
        "Pass ONLY these two arguments and no others:\n",
        "  - is_correct: true or false\n",
        "  - feedback_message: a short string (2-3 sentences)\n",
        "Do not add extra arguments. Do not output JSON manually. Use the tool."
      )
    ),
    list(role = "user", content = prompt)
  )

  httr2::request("https://api.fireworks.ai/inference/v1/chat/completions") |>
    httr2::req_headers(
      "Content-Type" = "application/json",
      "Authorization" = paste0("Bearer ", api_key)
    ) |>
    httr2::req_body_json(list(
      model = model,
      messages = messages,
      tools = tools
    )) |>
    httr2::req_retry(max_tries = 3) |>
    httr2::req_timeout(30)
}

#' Execute a Fireworks API request with classified error handling
#'
#' @param req An httr2 request object
#' @param model Character string model name (used in error messages)
#' @return An httr2 response object
#' @keywords internal
perform_fireworks_request <- function(req, model) {
  tryCatch(
    httr2::req_perform(req),
    error = function(e) {
      if (grepl("401|403", e$message)) {
        cli_abort(c(
          "Authentication failed.",
          "i" = "Please check your {.envvar FIREWORKS_API_KEY}.",
          "i" = "Get your key from: {.url https://fireworks.ai/api-keys}"
        ))
      } else if (grepl("404", e$message, fixed = TRUE)) {
        cli_abort(c(
          "Model {.val {model}} not found.",
          "i" = "Check available models at: {.url https://fireworks.ai/models}"
        ))
      } else if (grepl("429", e$message, fixed = TRUE)) {
        cli_abort("Rate limit exceeded. Please wait a moment and try again.")
      } else {
        cli_abort(c(
          "Error calling Fireworks API.",
          "x" = e$message
        ))
      }
    }
  )
}

#' Extract feedback from a text response when no tool call was made
#'
#' @param body Parsed JSON response body
#' @return List with is_correct and feedback, or NULL if no text content
#' @keywords internal
extract_text_fallback <- function(body) {
  content <- trimws(body$choices[[1]]$message$content %||% "")
  if (nchar(content) == 0) {
    return(NULL)
  }

  is_correct <- grepl("correct", content, ignore.case = TRUE) &&
    !grepl("incorrect|not correct|does not", content, ignore.case = TRUE)
  list(is_correct = is_correct, feedback = content)
}

#' Call Fireworks API with tool calling support
#'
#' Sends a chat completion request to Fireworks AI with tool calling
#' for structured feedback extraction.
#'
#' @param model Character string of Fireworks model name
#' @param prompt Character string containing the full prompt
#' @return List with is_correct (logical) and feedback (character)
#' @keywords internal
call_fireworks_with_tools <- function(model, prompt) {
  validate_fireworks_available()

  api_key <- Sys.getenv("FIREWORKS_API_KEY")
  tools <- list(define_fireworks_feedback_tool())

  req <- build_fireworks_request(api_key, model, prompt, tools)
  resp <- perform_fireworks_request(req, model)
  body <- httr2::resp_body_json(resp)

  result <- parse_fireworks_tool_response(body)
  if (!is.null(result)) {
    return(result)
  }

  extract_text_fallback(body) %||%
    list(
      is_correct = FALSE,
      feedback = "Unable to parse LLM response. Please try again."
    )
}

#' Extract the first respond_with_feedback tool call from a Fireworks response
#'
#' @param body Parsed JSON response from Fireworks API
#' @param debug Logical; if TRUE, print debug info
#' @return The raw tool_call list element, or NULL if none found
#' @keywords internal
extract_tool_call <- function(body, debug = FALSE) {
  if (is.null(body$choices) || length(body$choices) == 0) {
    if (debug) {
      cat("[DEBUG] No choices in response\n")
    }
    return(NULL)
  }

  message <- body$choices[[1]]$message
  if (is.null(message$tool_calls) || length(message$tool_calls) == 0) {
    if (debug) {
      cat("[DEBUG] No tool_calls in message\n")
      cat("[DEBUG] Message content:", message$content, "\n")
    }
    return(NULL)
  }

  if (debug) {
    cat("[DEBUG] Tool calls found:", length(message$tool_calls), "\n")
    cat("[DEBUG] Tool call structure:\n")
    str(message$tool_calls[[1]])
  }

  tool_call <- message$tool_calls[[1]]
  if (
    is.null(tool_call[["function"]]) ||
      tool_call[["function"]]$name != "respond_with_feedback"
  ) {
    if (debug) {
      cat("[DEBUG] Tool function name:", tool_call[["function"]]$name, "\n")
    }
    return(NULL)
  }

  tool_call
}

#' Parse is_correct and feedback_message from a tool call's JSON arguments
#'
#' @param tool_call A tool_call list element with a `function$arguments` JSON string
#' @param debug Logical; if TRUE, print debug info
#' @return List with is_correct (logical) and feedback (character), or NULL on parse failure
#' @keywords internal
parse_feedback_arguments <- function(tool_call, debug = FALSE) {
  if (debug) {
    cat("[DEBUG] Function arguments (raw):\n")
    cat(tool_call[["function"]]$arguments, "\n")
  }

  args <- tryCatch(
    jsonlite::fromJSON(tool_call[["function"]]$arguments),
    error = function(e) {
      if (debug) {
        cat("[DEBUG] Error parsing arguments:", conditionMessage(e), "\n")
      }
      NULL
    }
  )
  if (is.null(args)) {
    return(NULL)
  }

  if (debug) {
    cat("[DEBUG] Parsed arguments:\n")
    str(args)
  }

  feedback <- if (is.character(args$feedback_message)) {
    args$feedback_message[1]
  } else if (
    is.list(args$feedback_message) &&
      !is.null(args$feedback_message$description)
  ) {
    as.character(args$feedback_message$description)
  } else {
    ""
  }

  if (debug) {
    cat("[DEBUG] Extracted is_correct:", args$is_correct, "\n")
    cat("[DEBUG] Extracted feedback:", feedback, "\n")
  }

  list(
    is_correct = isTRUE(as.logical(args$is_correct)),
    feedback = feedback
  )
}

#' Parse tool response from Fireworks API
#'
#' Extracts is_correct and feedback_message from the Fireworks API response.
#' Fireworks uses OpenAI-compatible format.
#'
#' @param body Parsed JSON response from Fireworks API
#' @return List with is_correct and feedback, or NULL if no valid tool call
#' @keywords internal
parse_fireworks_tool_response <- function(body) {
  debug <- getOption("blendtutor.debug", FALSE)

  tool_call <- extract_tool_call(body, debug)
  if (is.null(tool_call)) {
    return(NULL)
  }

  parse_feedback_arguments(tool_call, debug)
}

#' Evaluate student code using LLM
#'
#' Sends student code to Fireworks AI for evaluation and feedback using
#' structured tool calling.
#'
#' @param student_code Character string of the code submitted by the student
#' @param exercise_prompt Character string containing the exercise evaluation prompt
#' @param model Character string specifying which Fireworks model to use
#' @return List with is_correct (logical) and feedback (character)
#' @keywords internal
evaluate_with_llm <- function(student_code, exercise_prompt, model = NULL) {
  if (is.null(model)) {
    model <- "accounts/fireworks/models/qwen3-vl-30b-a3b-instruct"
  }
  full_prompt <- glue::glue(exercise_prompt)
  call_fireworks_with_tools(model, full_prompt)
}
