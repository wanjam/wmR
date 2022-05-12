# wmR::tla.parser parses output data from Nils Lüschow's Android Time Logger App
#     Copyright (C) 2022 Wanja Mössing
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

#' Load the output logfile that Nils Lüschow's Android Time Logger App produces,
#' and parses it to data science friendly long-table format.
#'
#' Note: This function was designed to work with V1.1.1 of the app. Since the
#'       app's output might change at any time, the function may stop working
#'       with higher versions.
#'
#' check https://github.com/IXP-Team/exptool_Time-Logger/ to get a release apk.
#'
#' \code{tla.parser} parses output data from Nils Lüschow's Android Time Logger App
#'
#' @param rawpath rawpath to a file or a folder with files (mandatory)
#' @param fpattern if path is a folder and regex is not NULL, will compare filenames in path against regex. Default is to match "^timelogger_data.*\\.txt$", which should match all non-renamed files.
#' @param outpath path, incl. filename, with output csv (optional)
#' @param Hz pseudo-sampling rate of output (default 1000Hz)
#'
#' @return a single data.table in long format with information of all files.
#'
#' @examples
#'
#' \dontrun{
#' DT = tla.parser("raw/", "^f")
#' }
#'
#' @author Wanja Mössing
#' @name tla.parser
#' @export tla.parser
#'
#' @import data.table
#' @import stringr
#' @import assertthat
#' @importFrom lubridate milliseconds

tla.parser <- function(rawpath,
                       fpattern="^timelogger_data.*\\.txt$",
                       outpath = NA_character_,
                       Hz = 1000) {

  # check if rawpath is a file or a path
  isFilePath = any(stringr::str_ends(rawpath, c(".csv", ".txt", ".log")))

  # if not a file, check for files in path
  if (!isFilePath){
    # get all matching filenames
    fnames = list.files(path = rawpath, pattern = fpattern, full.names = T)
  } else {
    fnames = list(rawpath)
  }

  # parse all files and rbind
  print("Now parsing...")
  outdat = rbindlist(lapply(fnames, .tla.parser.singlefile, SR=Hz), fill = T)

  # store if desired
  if (!is.na(outpath)){
    fwrite(outdat, outpath)
  }


  # return
  return(outdat)
}

.tla.parser.singlefile <- function(fname, SR=1000){

  # read & transpose the lengthy string
  one.log = melt(fread(fname, header = F), id.vars = 1)
  one.log = rbind(one.log[, .(value = unique(V1))], one.log[, .(value)])

  # extract the part containing the marker information
  one.log[, entry := str_extract(value, "(?<=^Code:\\s{1}).*(?=:\\s{1}\\d{4})")]

  # take care of start and end of session
  one.log[value %like% "^Measurement started:\\s",
          ':='(entry = "Measurement started",
               type = "started",
               marker_name = "start recording",
               marker_code = "start recording")]
  one.log[value %like% "^Measurement stopped:\\s",
          ':='(entry = "Measurement stopped",
               type = "stopped",
               marker_name = "stop recording",
               marker_code = "stop recording")]

  # extract the type of event (i.e. start or stop)
  one.log[entry %like% "^.*\\s\\-\\s.*",
          type := str_extract(entry, "(?<=\\s\\-\\s)(started|stopped)$")]

  # do so for instantaneous (i.e. "non-toggleable" markers as well)
  one.log[entry %like% "^nt\\s{1}\\-\\s{1}.*", type := "instantaneous"]

  # extract the marker name and code for non-instantaneous markers
  one.log[entry %like% "^.*\\s\\-\\s.*" & type != "instantaneous",
          ':='(marker_name = str_extract(
            entry, "(?<=\\s\\-\\s).*(?=\\s{1}\\-\\s{1}(started|stopped)$)"),
            marker_code = str_extract(entry, "^.*?(?=\\s{1}\\-\\s{1})"))]

  # extract the marker name and code for instantaneous markers
  one.log[entry %like% "^nt.*\\s\\-\\s.*" & type == "instantaneous",
          ':='(
            marker_name = str_extract(
              entry, "(?<=[^(nt)]\\s{1}\\-\\s{1}).*$"),
            marker_code = str_extract(
              entry,"(?<=nt\\s{1}\\-\\s{1}).*(?=\\s{1}\\-\\s{1})"))
  ]

  # warn if "-" have been used
  if (any(one.log[, str_detect(marker_name, "\\s{1}\\-\\s{1}")], na.rm=T) ||
      any(one.log[, str_detect(marker_code, "\\s{1}\\-\\s{1}")], na.rm=T)){
    warning(paste("You included `-` in your marker names, which is used as",
                  "separator between marker names, codes, and other",
                  "information.\nConsequently, the result may now be wrong. In",
                  "the future, please try to avoid `-` in your marker names",
                  "and codes."))
  }

  # extract timestamps
  one.log[, timestamp := str_extract(value, paste0("(?<=", entry, ":\\s{1}).*$"))]


  # remove "entry" & "value" columns
  one.log[, entry := NULL][, value := NULL]

  # convert time column to POSIXct
  one.log[, timestamp := as.POSIXct(timestamp, format="%Y-%m-%dT%H:%M:%OS")]

  # now create a running count of events
  one.log[, Running_Event_Count := 1:.N, by = .(type, marker_code)]



  # based on this, create a time-series long format table
  ## check how many milliseconds the session lasted and create a table that long
  ms_tot = as.numeric(
    difftime(one.log[marker_name == "stop recording", timestamp],
             one.log[marker_name == "start recording", timestamp],
             units = "secs")) * 1000
  freq = 1000/SR
  one.log.ts = data.table(ID = NA,
                          Instructor = NA,
                          Age = NaN,
                          Sex = NA,
                          Height_cm = NaN,
                          Weight_kg = NaN,
                          Filename = str_extract(fname, "(?<=/)[^/]*$"),
                          Rel_time_ms = seq(0, ms_tot, freq),
                          CET_timestamp = as.POSIXct(NA))

  # add a CET timeseries variable
  cet_start = one.log[marker_code == "start recording", timestamp]
  cet_stop = one.log[marker_code == "stop recording", timestamp]

  one.log.ts[, CET_timestamp := cet_start + lubridate::milliseconds(Rel_time_ms)]

  # assert that the error is smaller than one millisecond
  assert_that((as.numeric(
    difftime(one.log.ts[.N, CET_timestamp],
             cet_stop, units = "secs")) / 1000) < 1,
    msg = "Error in interpolating between start and end time")

  # merge the smaller table into the timeseries version
  # adaot names of smaller table
  setnames(one.log, c("type", "marker_code", "marker_name", "timestamp"),
           c("Event_Start_Stop", "Marker_Code", "Marker_Name", "CET_timestamp"),
           skip_absent = TRUE)

  # create merger index for closest match using rolling join
  one.log = cbind(one.log,
                  one.log.ts[one.log, .(Rel_time_ms),on = .(CET_timestamp),
                             mult="first", roll="nearest"])

  # merge
  one.log.ts <- merge(one.log.ts, one.log, by="Rel_time_ms", all.x = T)

  # remove redundant columns
  one.log.ts[, CET_timestamp.y:=NULL]
  setnames(one.log.ts, "CET_timestamp.x", "CET_timestamp")

  # create one column per marker code and set to true and false depending on whether card is active
  mkrnames = one.log.ts[!(is.na(Marker_Name)), unique(Marker_Name)]

  # mkrnames = str_replace_all(mkrnames, "\\s", "_")
  one.log.ts[, (mkrnames) := NA,]
  timestamps.mkr = one.log.ts[, .(min = min(CET_timestamp, na.rm = T),
                               max = max(CET_timestamp, na.rm = T)),
                           by = .(Marker_Name, Running_Event_Count)]
  for (imkr in mkrnames) {
    one.log.ts[, (imkr) := FALSE]
    for (icntr in timestamps.mkr[Marker_Name == imkr,
                                 unique(Running_Event_Count)]){
      one.log.ts[CET_timestamp %between% timestamps.mkr[
        Marker_Name == imkr & Running_Event_Count == icntr, c(min, max)],
                 (imkr) := TRUE]
    }
  }

  return(one.log.ts)
}

