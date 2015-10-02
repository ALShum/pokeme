#' Get Twilio login credentials.
#'
#' @return Twilio account SID and auth token
#' @export 
#' @examples
#' \dontrun{
#'   get_twilio_credentials()
#' }
get_twilio_credentials = function() {
  sid = Sys.getenv('twilio_account_sid')
  auth = Sys.getenv('twilio_account_auth')
  if(
    !identical(sid, "") & 
    !identical(auth, "")
  ) return(list(account_sid = sid, account_auth = auth))
  
  if(!interactive()) {
    stop("Please set env var twilio_account_sid and twilio_account_auth to your account SID and account auth", call. = FALSE)
  }
  message("Please enter your account sid and press enter:")
  sid = readline(": ")
  
  if(identical(sid, "")) {
    stop("Invalid sid!", call. = FALSE)
  }
  message("Updating twilio_account_sid env var.")
  Sys.setenv(twilio_account_sid = sid)
  
  message("Please enter your account auth and press enter:")
  auth = readline(": ")

  if(identical(auth, "")) {
    stop("Invalid auth!", call. = FALSE)
  }
  message("Updating twilio_account_auth env var.")
  Sys.setenv(twilio_account_auth = auth)

  return(list(account_sid = sid, account_auth = auth))
}

#' Sets the Twilio login credentials
#'
#' @param account_sid Account SID for Twilio
#' @param account_auth Account auth for Twilio
#' @return Twilio account SID and auth token
#' @export 
#' @examples
#' \dontrun{
#'   set_twilio_credentials("account_sid", "account_auth")
#' }
set_twilio_credentials = function(account_sid, account_auth) {
  if(identical(account_sid, "")) {
    stop("Invalid account_sid!", call. = FALSE)
  }
  if(identical(account_auth, "")) {
    sotp("Invalid account_auth!", call. = FALSE)
  }
  Sys.setenv(twilio_account_sid = account_sid)
  Sys.setenv(twilio_account_auth = account_auth)
  
  return(
    list(
      account_sid = Sys.getenv("twilio_account_sid"),
      account_auth = Sys.getenv("twilio_account_auth")
    )
  )
}

#' Detects if Twilio login credentials are set
#'
#' @return TRUE if account sid and auth set, otherwise FALSE
#' @export
#' @examples
#' \dontrun{
#'   has_twilio_credentials()
#' }
has_twilio_credentials = function() {
  cred = get_twilio_credentials()
  !identical(cred$account_sid, "") & !identical(cred$account_auth, "")
}