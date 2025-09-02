#' Check if on inbo domain
#'
#' @param domain character string containing the dns domain
#'
#' @returns logical TRUE if on domain, FALSE else
#' @export
#'
is_on_inbo_domain <- function(domain = "inbo.be") { 
  # Sys.getenv() retrieves environment variables. USERDNSDOMAIN is a standard
  # variable on domain-joined Windows machines.
  user_domain <- Sys.getenv("USERDNSDOMAIN")
  
  # We compare the retrieved domain with the expected domain.
  # toupper() makes the comparison case-insensitive, which is more robust.
  # The `nzchar()` check ensures the environment variable is not empty.
  is_on_domain <- nzchar(user_domain) && (toupper(user_domain) == toupper(domain))
  
  return(is_on_domain)
}
