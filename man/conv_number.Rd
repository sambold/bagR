% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/conv_number.R
\name{conv_number}
\alias{conv_number}
\title{conv_number: wandelt Zahlenformat um}
\usage{
conv_number(x, to = "R", ...)
}
\arguments{
\item{x}{String/Numeric; Wert, der umgewandelt werden soll}

\item{to}{String, Standardwert = "R"; gibt an in welches Format umgewandelt 
werden soll:
\itemize{
    \item R: Umwandlung in R-spezifischen, numerischen Wert
    \item US: Umwandlung in amerikan. Zahlenformat
    \item DE: Umwandlung in europ. Zahlenformat
}}

\item{...}{weitere Parameter fuer format}
}
\value{
Numeric (to=R), String (to=US/DE)
}
\description{
Wandelt Zahlen-Strings in europaeischem Format in R-Zahlenwerte (ohne 
Gruppierungstrenner, nur mit Punkt als Komma) um. Zudem koennen R-Zahlenwerte
fuer die Ausgabe in Strings umgewandelt werden, die eine entsprechende 
Formatierung besitzen (US/DE):
\itemize{
    \item US: Gruppierungstrennzeichen="," - Komma="."
    \item DE: Gruppierungstrennzeichen="." - Komma=","
}
Die Auswahl von US/DE gibt einen String, die Auswahl R einen numerischen Wert
zurueck.
}
\examples{
conv_number(x="1234,56")
conv_number(x="1.234,56")
conv_number(x=1234.56,to="US")
conv_number(x=1234.56,to="DE")
conv_number(x="1234.56",to="DE")
conv_number(x=123456789,99,to="DE",digits=15)
}
