library(vcr)
library(keyring)

vcr_dir <- vcr::vcr_test_path("fixtures")

# API key: use OPENAI_API_KEY or the key stored in the keyring, without asking for one
api_key <- Sys.getenv("OPENAI_API_KEY")
if (!nzchar(api_key)) {
  api_key <- tryCatch(keyring::key_get(service = "openai", username = "user"),
                      error = function(e) "")
}

if (!nzchar(api_key)) {
  if (dir.exists(vcr_dir)) {
    # Fake key for the package: the recorded answers (cassettes) are replayed, so no real key is needed.
    # It is removed again after the tests, so it does not affect the rest of your R session.
    withr::local_envvar(OPENAI_API_KEY = "foobar", .local_envir = testthat::teardown_env())
  } else {
    # If there's no mock files nor API token, impossible to run tests
    stop("No API key nor cassettes, tests cannot be run.", call. = FALSE)
  }
}

# Configure vcr
invisible(vcr::vcr_configure(
  dir = vcr_dir,
  record = "once",
  filter_request_headers = list(Authorization = "My bearer token is safe", "User-Agent"),
  filter_response_headers = list("Set-Cookie", "Date", "openai-organization", "openai-project"),
  serialize_with = "yaml",
  preserve_exact_body_bytes = FALSE,
  match_requests_on = c("method", "uri")
))
