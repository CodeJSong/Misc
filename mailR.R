# code from mailR package instruction

# send.mail : Send an e-mail in R   // after time-consuming job
# JAVA SE is required

library(mailR) 
from = "Jun.Mac@gmail.com"
to = "Jun <songjun0525@gmail.com>"
subject = "Work done"
body = "It's done! you can check"

send.mail(from = from,
          to = to,
          subject = subject,
          body = body,
          smtp = list(host.name = "aspmx.l.google.com", port = 25),
          authenticate = FALSE,
          send = TRUE)
