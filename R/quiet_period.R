#' dqp_sub_noreset
#'
#' recursive function for applying quiet period
#'
#' @param d vector of \code{POSIX} event dates
#' @param qp quiet period
#' @param included indexes of elements of \code{d} already identified for
#'   inclusion
#'
#' @return indices of elements in \code{d} that should be included based on
#'   quiet period \code{qp}
#'
#' @keywords internal
#'
dqp_sub_noreset <- function(d, qp, included) {

  # get the last included date
  last_in <- included[length(included)]


  # add the quiet period to the last included
  min_date <- d[last_in] + qp

  if (min_date < max(d)) {

    # find next included date
    next_in <- min(which(d >= min_date))

    # repeat
    dqp_sub_noreset(d, qp, c(included, next_in))

  } else {

    # no more eligible dates, return
    included

  }

}

#' dqp_sub_reset
#'
#' function for applying quiet period that resets on subsequent events
#'
#' @param d vector of \code{POSIX} event dates
#' @param qp quiet period
#'
#' @return indices of elements in \code{d} that should be included based on
#'   quiet period \code{qp}
#'
#' @keywords internal
#'
dqp_sub_reset <- function(d, qp) {

  which(c(TRUE, diff(d) >= qp))

}

#' dynamic_quiet_period
#'
#' Clusters irregular time series data using a quiet period defined by the
#'   time elapsed since the last event (\code{rolling = TRUE}) or the
#'   start of the previous cluster (\code{rolling = False}).
#'
#' @param event_dates a vector \code{POSIX} timestamps when the event of
#'   interest occurred
#' @param quiet_period the interval used for clustering, preferably defined
#'   with a function from the \code{lubridate} package (e.g.
#'   \code{lubridate::hours} or \code{lubridate::minutes})
#' @param rolling a logical scalar determining how the quiet period is applied,
#'   see details section
#' @param batch a logical scalar controlling the return type, see return section
#'
#' @return if \code{batch = TRUE} \code{dynamic_quiet_period} returns an integer
#'   vector suitable for additional grouping/summary, otherwise
#'   \code{dynamic_quiet_period} returns a logical vector indicating which
#'   elements of \code{event_dates} satisfy the criteria defined by the
#'   \code{quiet_period} argument
#'
#' @export
#'
#' @examples
#'
#' if (requireNamespace("lubridate", quietly = TRUE)) {
#'   # cleaner with `lubridate`
#'   start <- lubridate::ymd_hms("2025-01-01 12:00:00")
#'   elapsed_min <- lubridate::minutes(
#'     c(5, 11, 31, 32, 35, 60, 62, 63, 150, 155)
#'   )
#'   events <- data.frame(
#'     event_date = start + elapsed_min
#'   )
#'   qp <- lubridate::minutes(30)
#' } else {
#'   # but it can be done with base R
#'   start <- as.POSIXct("2025-01-01 12:00:00")
#'   elapsed_min <- c(5, 11, 31, 32, 35, 60, 62, 63, 150, 155)
#'   events <- data.frame(
#'     event_date = start + elapsed_min * 60
#'   )
#'   qp <- 30 * 60 # 30 minutes in seconds
#' }
#'
#' if (requireNamespace("dplyr", quietly = TRUE)) {
#'   # same goes for `dplyr`
#'   events  |>
#'     dplyr::mutate(
#'       include_roll = dynamic_quiet_period(event_date, qp, batch = FALSE),
#'       batch_roll = dynamic_quiet_period(event_date, qp),
#'       include_noroll = dynamic_quiet_period(
#'         event_date,
#'         qp,
#'         rolling = FALSE,
#'         batch = FALSE
#'       ),
#'       batch_noroll = dynamic_quiet_period(event_date, qp, rolling = FALSE)
#'     )
#' } else {
#'   events$include_roll <- dynamic_quiet_period(event_date, qp, batch = FALSE)
#'   events$batch_roll <- dynamic_quiet_period(event_date, qp)
#'   events$include_noroll <- dynamic_quiet_period(
#'     event_date,
#'     qp,
#'     rolling = FALSE,
#'     batch = FALSE
#'   )
#'   events$batch_roll <- dynamic_quiet_period(event_date, qp, rolling = FALSE)
#'   events
#' }
#'
dynamic_quiet_period <- function(event_dates,
                                 quiet_period,
                                 rolling = TRUE,
                                 batch = TRUE) {

  # check inputs
  stopifnot(
    `event_dates must be a vector POSIX datetime values` =
      inherits(event_dates, "POSIXt"),
    `quiet_period incompatible with event_dates` =
      !inherits(try(event_dates[1] + quiet_period, silent = TRUE), "try-error"),
    `quiet_period must be a single value` = length(quiet_period) == 1,
    `rolling must be a single logical value (TRUE or FALSE)` =
      class(rolling) == "logical" && length(rolling) == 1,
    `batch must be a single logical value (TRUE or FALSE)` =
      class(batch) == "logical" && length(batch) == 1
  )

  if (length(event_dates)) {

    # determine chronological order
    date_order <- order(event_dates)

    # apply quiet period (returns indices of included values in sorted vector)
    if (rolling) {
      hits <- dqp_sub_reset(event_dates[date_order], quiet_period)
    } else {
      hits <- dqp_sub_noreset(event_dates[date_order], quiet_period, 1L)
    }

    if (batch) {
      # assign batch number to each sorted date for grouping
      batches <- cumsum(seq_along(date_order) %in% hits)
      # match to original input
      batches[order(date_order)]
    } else {
      # translate to logical vector matching original (unsorted) input
      seq_along(event_dates) %in% date_order[hits]
    }

  } else {

    ifelse(batch, integer(0), logical(0))

  }

}
