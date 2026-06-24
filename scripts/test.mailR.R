# test_email.R
library(mailR)

# Test with mailR (recommended for SMTP with auth)
send.mail(
  from = "your-valid-email@biochem.mpg.de", # Must be a valid domain for this server
  to = "your-test-email@gmail.com",
  subject = "Test Email from Shiny Server",
  body = "This is a test email",
  smtp = list(
    host.name = "msx.biochem.mpg.de",
    port = 25,
    ssl = FALSE,
    tls = FALSE
  ),
  authenticate = FALSE,
  send = TRUE
)
