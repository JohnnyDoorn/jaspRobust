#
# Copyright (C) 2013-2026 University of Amsterdam
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

# Shared helpers for robust descriptive statistics tables.

# Returns a list with n, mean (trimmed), median, winsorSd, mad for a numeric vector.
# tr is the trimming proportion in [0, 0.5).
.robustSummary <- function(x, tr) {
  x <- x[!is.na(x)]
  n <- length(x)
  list(
    n        = n,
    mean     = if (n > 0) mean(x, trim = tr)             else NA_real_,
    median   = if (n > 0) stats::median(x)               else NA_real_,
    winsorSd = if (n > 1) sqrt(WRS2::winvar(x, tr = tr)) else NA_real_,
    mad      = if (n > 1) stats::mad(x)                  else NA_real_
  )
}

# Adds the standard robust descriptive columns to a jaspTable.
.addRobustDescColumns <- function(table) {
  table$addColumnInfo(name = "n",        title = gettext("n"),             type = "integer")
  table$addColumnInfo(name = "mean",     title = gettext("Trimmed Mean"),  type = "number")
  table$addColumnInfo(name = "median",   title = gettext("Median"),        type = "number")
  table$addColumnInfo(name = "winsorSd", title = gettext("Winsorized SD"), type = "number")
  table$addColumnInfo(name = "mad",      title = gettext("MAD"),           type = "number")
}

# Standard footnote describing the trimming used.
.robustDescFootnote <- function(table, tr, isMedianMethod = FALSE) {
  if (isMedianMethod) {
    table$addFootnote(gettext("Trimmed Mean shown without trimming because the medians method is selected."))
  } else {
    table$addFootnote(gettextf("Trimmed Mean and Winsorized SD use %.0f%% trimming.", tr * 100))
  }
}
