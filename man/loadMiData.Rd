% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/loadMiData.R
\name{loadMiData}
\alias{loadMiData}
\title{Load MiBand data}
\usage{
loadMiData(path, user_id)
}
\arguments{
\item{path}{a character string specifing the "databases" directory which is copied from android system /data/data/com.xiaomi.hm.health. Example: If folder databases is in home directory, then path should be "~/databases".}

\item{user_id}{a caracter string specifying your id. User id can be accessed in the user info page in MiBand android app. Or you can find it as a part of file name specifying the origin_db file. For example, you may find a file named as "origin_db_123456" in the databases directory, then parse character string "123456" as user_id.}
}
\value{
return a list with element "cleandata" as a data frame with missing value and "data_week" as a data frame with missing data substitued.
}
\description{
Load in MiBand data from databases directory. Extract steps, slight sleep and deep sleep data as a data frame. Fill in missing data with average values of the same day in a week.
}
\author{
Borg
}

