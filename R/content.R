#' Content
#'
#' An R6 class that represents content
#'
#' @family R6 classes
#' @export
Content <- R6::R6Class(
  "Content",
  public = list(
    #' @field connect An R6 Connect object
    connect = NULL,
    #' @field content The content details from RStudio Connect
    content = NULL,

    initialize = function(connect, content) {
      validate_R6_class(connect, "Connect")
      self$connect <- connect
      # TODO: need to check that content has
      # at least guid, url, title to be functional
      self$content <- content
    },
    get_connect = function() {
      self$connect
    },
    get_content = function() {
      self$content
    },
    get_content_remote = function() {
      new_content_details <- self$get_connect()$content(self$get_content()$guid)
      self$content <- new_content_details
      self$get_content()
    },
    get_bundles = function(page_number = 1) {
      url <- glue::glue("v1/experimental/content/{self$get_content()$guid}/bundles?page_number={page_number}")
      self$get_connect()$GET(url)
    },
    update = function(...) {
      params <- rlang::list2(...)
      url <- glue::glue("v1/experimental/content/{self$get_content()$guid}")
      res <- self$get_connect()$POST(
        url,
        params
      )
      return(self)
    },
    runas = function(run_as, run_as_current_user = FALSE) {
      warn_experimental("content_runas")
      params <- list(
        run_as = run_as,
        run_as_current_user = run_as_current_user
      )
      url <- glue::glue("applications/{self$get_content()$guid}/runas")
      res <- self$get_connect()$POST(
        url,
        params
      )
      return(res)
    },
    get_url = function() {
      self$get_content()$url
    },
    get_dashboard_url = function(pane = "") {
      dashboard_url_chr(self$connect$host, self$content$guid, pane = pane)
    },
    get_jobs = function() {
      lifecycle::deprecate_warn("0.1.0.9005", what = "get_jobs()", with = "jobs()")
      self$jobs()
    },
    get_job = function(key) {
      lifecycle::deprecate_warn("0.1.0.9005", "get_job()", "job()")
      self$job(key)
    },
    jobs = function() {
      warn_experimental("jobs")
      url <- glue::glue("applications/{self$get_content()$guid}/jobs")
      res <- self$get_connect()$GET(url)
    },
    job = function(key) {
      warn_experimental("job")
      url <- glue::glue("applications/{self$get_content()$guid}/job/{key}")
      res <- self$get_connect()$GET(url)

      content_guid <- self$get_content()$guid
      purrr::map(
        list(res),
        ~ purrr::list_modify(.x, app_guid = content_guid)
      )[[1]]
    },
    variants = function() {
      warn_experimental("variants")
      url <- glue::glue("applications/{self$get_content()$guid}/variants")
      self$get_connect()$GET(url)
    },
    tag_set = function(id) {
      warn_experimental("tag_set")
      url <- glue::glue("applications/{self$get_content()$guid}/tags")
      self$get_connect()$POST(
        url,
        body = list(
          id = id
        )
      )
    },
    tag_delete = function(id) {
      # note that deleting the parent tag deletes all children
      warn_experimental("tag_delete")
      url <- glue::glue("applications/{self$get_content()$guid}/tags/{id}")
      invisible(self$get_connect()$DELETE(url))
    },
    tags = function() {
      warn_experimental("tags")
      url <- glue::glue("applications/{self$get_content()$guid}/tags")
      self$get_connect()$GET(url)
    },
    environment = function() {
      warn_experimental("environment")
      url <- glue::glue("applications/{self$get_content()$guid}/environment")
      res <- self$get_connect()$GET(url)
      # update values to be NA, which is how we preserve them
      res$values <- purrr::map(
        res$values,
        ~ NA_character_
      )
      return(res)
    },
    environment_set = function(..., .version = 0) {
      warn_experimental("environment_set")
      url <- glue::glue("applications/{self$get_content()$guid}/environment")
      # post with
      # key = NA to retain
      # post without a variable/key to remove
      # bump version number each time
      vals <- rlang::list2(...)

      # TODO: evaluate whether we should be coercing to character or erroring
      vals <- purrr::map(vals, as.character)
      body <- list(
        values = vals,
        version = .version,
        app_guid = self$get_content()$guid
      )
      self$get_connect()$POST(
        path = url,
        body = body
      )
      invisible()
    },
    print = function(...) {
      cat("RStudio Connect Content: \n")
      cat("  Content GUID: ", self$get_content()$guid, "\n", sep = "")
      cat("  Content URL: ", self$get_content()$url, "\n", sep = "")
      cat("  Content Title: ", self$get_content()$title, "\n", sep = "")
      cat("\n")
      cat('content_item(client, guid = "', self$get_content()$guid, '")', "\n", sep = "")
      cat("\n")
      invisible(self)
    }
  )
)

#' Environment
#'
#' An R6 class that represents a Content's Environment Variables
#'
#' @export
Environment <- R6::R6Class(
  "Environment",
  inherit = Content,
  public = list(
    env_version = NULL,
    env_raw = NULL,
    env_vars = NULL,
    initialize = function(connect, content) {
      super$initialize(connect = connect, content = content)
      self$env_refresh()
    },
    env_refresh = function() {
      # mutates the existing instance, so future
      # references have the right version
      self$env_raw <- self$environment()
      self$env_version <- self$env_raw$version
      self$env_vars <- self$env_raw$values
      return(self)
    },
    print = function(...) {
      super$print(...)
      cat("Environment Variables:\n")
      cat("  vctrs::vec_c(\n")
      purrr::imap(self$env_vars, ~ cat(paste0("    ", .y, " = ", .x, ",\n")))
      cat("  )\n")
      cat("\n")
      invisible(self)
    }
  )
)
