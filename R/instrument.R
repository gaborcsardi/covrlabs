#' Trace calls in a function
#'
#' @param x Function to trace.
#' @param compile Logical scalar, whether to byte-compile the traced
#' function.
#' @return The traced function
#'
#' @export

trace_calls <- function(x, compile = TRUE) {
  trace_calls_(x, compile = compile)
}

trace_calls_ <- function(
    x,
    parent_functions = NULL,
    parent_ref = NULL,
    compile = TRUE) {
  if (is.null(parent_functions)) {
    parent_functions <- deparse(substitute(x))
  }

  recurse <- function(y) {
    lapply(
      y,
      trace_calls_,
      parent_functions = parent_functions,
      compile = compile
    )
  }

  if (is.atomic(x) || is.name(x) || is.null(x)) {
    if (is.null(parent_ref)) {
      x
    } else {
      if (is_brace(x)) {
        x
      } else {
        key <- new_counter(parent_ref, parent_functions)
        count_call(key, x)
      }
    }
  } else if (is.call(x)) {
    src_ref <- attr(x, "srcref") %||% impute_srcref(x, parent_ref)
    if (is_assignment(x)) {
      parent_functions <- c(parent_functions, as.character(x[[2]]))
    }

    if (is_curly_curly(x)) {
      as.call(x)
    } else if (!is.null(src_ref)) {
      as.call(Map(
        trace_calls,
        x,
        src_ref,
        MoreArgs = list(
          parent_functions = parent_functions,
          compile = compile
        )
      ))
    } else if (!is.null(parent_ref)) {
      key <- new_counter(parent_ref, parent_functions)
      count_call(key, as.call(recurse(x)))
    } else {
      as.call(recurse(x))
    }
  } else if (is.function(x)) {
    # We cannot trace primitive functions
    if (is.primitive(x)) {
      return(x)
    }

    fun_body <- body(x)

    if (is_curly_fn(x, fun_body)) {
      src_ref <- attr(x, "srcref")
      key <- new_counter(src_ref, parent_functions)
      fun_body <- count_call(
        key,
        trace_calls(fun_body, parent_functions, compile = compile)
      )
    } else {
      fun_body <- trace_calls(fun_body, parent_functions)
    }

    new_formals <- trace_calls(
      formals(x),
      parent_functions,
      compile = compile
    )
    if (is.null(new_formals)) new_formals <- list()
    formals(x) <- new_formals
    body(x) <- fun_body

    if (compile) {
      try_compile(x)
    } else {
      x
    }
  } else if (is.pairlist(x)) {
    as.pairlist(recurse(x))
  } else if (is.expression(x)) {
    as.expression(recurse(x))
  } else if (is.list(x)) {
    recurse(x)
  } else {
    message(
      "Unknown language class: ",
      paste(class(x), collapse = "/"),
      ". Leaving untouched.",
      call. = FALSE
    )
    x
  }
}

# Construct the calls by hand to avoid a NOTE from R CMD check
count_call <- function(key, val) {
  call(
    "if", TRUE,
    call(
      "{",
      as.call(list(
        call(":::", as.symbol("covrlabs"), as.symbol("count")),
        key
      )),
      val
    )
  )
}

is_brace <- function(x) {
  is.symbol(x) && as.character(x) == "{"
}

is_curly_curly <- function(x) {
  identical(x[[1]], as.name("{")) &&
    length(x) == 2 &&
    is.call(x[[2]]) &&
    identical(x[[2]][[1]], as.name("{"))
}

is_curly_fn <- function(x, fun_body) {
  !is.null(attr(x, "srcref")) &&
    (is.symbol(fun_body) || !identical(fun_body[[1]], as.name("{")))
}

is_assignment <- function(x) {
  (identical(x[[1]], as.name("<-")) || identical(x[[1]], as.name("="))) &&
    is.call(x[[3]]) &&
    identical(x[[3]][[1]], as.name("function"))
}
