# wmR::baysCO16ported is an R port of Paul Bays' CO16 Matlab code.
# Copyright (C) 2017 Paul Bays, Port 2019 Wanja Mössing
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

##' @title CO16_FIT
##' @description This is a ported version of Paul Bays' Matlab code published at http://www.paulbays.com/code/CO16/
##'   ----------------------------------
##'   Original documentation
##'   ----------------------------------
##'
##'   This code is released under a GNU General Public License:
##'   feel free to use and adapt these functions as you like, but credit should be given to Paul Bays if they contribute to your work, by citing:
##'   Schneegans S & Bays PM. No fixed item limit in visuospatial working memory. Cortex 83: 181-193 (2016)
##'
##'   Returns maximum likelihood parameters B (res[1]) for a mixture model describing
##'   recall responses X in terms of target TT, non-target NT, and uniform
##'   responses. Inputs should be in radians, -\code{pi} <= X < \code{pi}. Fitting is based
##'   on an EM algorithm with multiple starting parameters.
##'
##'   \code{CO16_fit(X, TT, NT)} returns a three-element list with:
##'   1. vector \code{B c(K, pT, pN, pU)}, where K is
##'     the concentration parameter of a Von Mises distribution capturing
##'     response variability, pT is the probability of responding with the
##'     target value, pN the probability of responding with a non-target
##'     value, and pU the probability of responding "randomly".
##'
##'   2. vector \code{LL} additionally returns the log likelihood LL.
##'
##'   3. vector \code{W} additionally returns a weight matrix of
##'     trial-by-trial posterior probabilities that responses come from each of
##'     the three mixture components. Each row of W corresponds to a separate
##'     trial and is of the form \code{c(wT, wN, wU)}, corresponding to the probability
##'     the response comes from the target, non-target or uniform response
##'     distributions, respectively.
##'
##' @references Schneegans S & Bays PM. No fixed item limit in visuospatial working
##'   memory. Cortex 83: 181-193 (2016),
##'   Bays PM, Catalao RFG & Husain M. The precision of visual working
##'   memory is set by allocation of a shared resource. Journal of Vision
##'   9(10): 7, 1-11 (2009)
##'
##' @author Paul Bays, R port by Wanja Moessing
##' @param X [n*1,1] vector of responses **or** data.table containing columns
##' with responses, targets and non-targets
##' @param TT [n*1,1] column vector of Target orientations
##' @param NT [n*1, m] matrix of non-target values
##' @param ... optional input arguments passed to \code{DT2CO16}, in case
##' \code{X} is a \code{data.table}.
##' @seealso DT2CO16
##' @name  CO16_fit
##' @export CO16_fit
##' @importFrom pracma size repmat ones zeros
##' @importFrom CircStats A1inv
##' @import data.table
##' @importFrom data.table data.table
##'
CO16_fit <- function(X, TT = NULL, NT=NULL, ...) {
  # allow to input a data.table; at some point, the whole thing should be ported to data.table sytax
  if (is.data.table(X)) {
    res = DT2CO16(X, ...)
    X = res$X
    TT = res$TT
    NT = res$NT
    urTrial = res$Trial
  }
  n = size(X, 1)
  if (is.null(TT)) { TT = zeros(n, 1)}

  if  (is.null(TT) || size(X, 2) > 1 || size(TT, 2) > 1 ||
       size(X, 1) != size(TT, 1) || !is.null(NT) &&
       (size(NT, 1) != size(X, 1) || size(NT, 1) != size(TT, 1))) {
    stop('Input is not correctly dimensioned.\n',
         'Did you use row instead of column vectors?\n',
         'Try X = matrix(X,length(X),1)')
  }

  if (is.null(NT)) {
    NT = matrix(NaNs(n),n,1)
    nn = 0
  } else {
    nn = size(NT, 2)
  }

  # Starting parameters
  K = c(1,    10,  100)
  N = c(0.01, 0.1, 0.4)
  U = c(0.01, 0.1, 0.4)

  if (nn == 0) {N = 0}

  LL = -Inf; B = c(NaN, NaN, NaN, NaN); W = NaN;

  #warning('off','JV10_function:MaxIter');

  # Parameter estimates
  for (i in 1:length(K)) {
    for (j in 1:length(N)) {
      for (k in 1:length(U)) {
        res <- CO16_function(X, TT, NT, c(K[i], 1 - N[j] - U[k], N[j], U[k]))
        b <- res[1][[1]]; ll <- res[2][[1]]; w <- res[3][[1]]; res <- NULL
        if (ll > LL) {
          LL = ll; B = b; W = w;
        }
      }
    }
  }
  #warning('on','JV10_function:MaxIter');
  names(B) = c('K', 'pT', 'pN', 'pU')
  names(LL) = 'Log-likelihood'
  W = data.table(W)
  names(W) = c('wT', 'wN', 'wU')
  if (length(urTrial) == W[, .N]) {
    W[, Trial := urTrial]
  }
  res = list(B, LL, W)
  names(res) = c('B','LL','W')
  return(res)
}

#' @title CO16_function
#' @description sub-function of CO16_fit
#' @author Paul Bays, R port by Wanja Moessing
#' @param X = [n*1,1] vector of responses
#' @param TT = [n*1,1] column vector of Target orientations
#' @param NT = [n*1, m] matrix of non-target values
#' @param B_start starting values
#' @importFrom pracma size repmat ones zeros isempty
#' @importFrom CircStats A1inv
#' @export CO16_function
CO16_function <- function(X, TT=NULL, NT=NULL, B_start=NULL) {
  # --> www.paulbays.com | R-port github.com/wanjam

  if (isempty(NT)){NT <- NULL}

  if  (is.null(TT) || size(X, 2) > 1 || size(TT, 2) > 1 ||
       size(X, 1) != size(TT, 1) || !is.null(NT) && (size(NT, 1) != size(X, 1) ||
                                                     size(NT, 1) != size(TT, 1))) {
    stop('Input is not correctly dimensioned')
  }

  # Check if data are in radians
  if (any(as.logical(lapply(list(X, TT, NT), function(k) any(k > 2*pi))))) {
    stop('Values in input are larger than 2*pi! Please convert to radians!')
  }

  if (is.null(B_start) &&
      (B_start[1] < 0 || any(B_start[2:4] < 0) || any(B_start[2:4] > 1) ||
       abs(sum(B_start[2:4]) - 1) > 10^-6)) {
    stop('Invalid model parameters')
  }

  MaxIter = 10^4; MaxdLL = 10^-4

  n = size(X, 1)

  if (is.null(NT) || all(is.na(NT))) {
    NT = matrix(NaNs(n),n,1)
    nn = 0
  } else {
    nn = size(NT, 2)
  }

  # Default starting parameters
  if (is.null(B_start)) {
    K = 5
    Pt = 0.5
    Pn = ifelse(nn > 0, 0.3, 0)
    Pu = 1 - Pt - Pn
  } else {
    K = B_start[1]
    Pt = B_start[2]
    Pn = B_start[3]
    Pu = B_start[4]
  }

  E  = X - TT
  E = ((E + pi) %% (2 * pi)) - pi
  if (nn != 0) {
    NE = repmat(X, 1, nn) - NT
    NE = ((NE + pi) %% (2 * pi)) - pi
  }

  LL = NaN; dLL = NaN; iter = 0

  while (TRUE) {
    iter = iter + 1
    Wt = Pt * vonmisespdf(E, 0, K)
    Wg = Pu * ones(n, 1) / (2 * pi)

    if (nn == 0) {
      Wn = matrix(zeros(length(NT),1))
    } else {
      Wn = Pn / nn * wmR::vonmisespdf(NE, 0, K)
    }

    W = t(t(rowSums(cbind(Wt, Wn, Wg))))

    dLL = LL - sum(log(W))
    LL = sum(log(W))

    # had to split that up, as in Matlab NaN<float is TRUE, in R is NA
    if (!is.nan(dLL)) {
      if (abs(dLL) < MaxdLL) {break}
    }
    if (iter > MaxIter) {break}


    Pt = sum(Wt / W) / n
    Pn = sum(rowSums(Wn) / W) / n
    Pu = sum(Wg / W) / n

    if (nn == 0) {
      rw = Wt / W
      S = sin(E)
      C = cos(E)
    }else{
      rw = cbind((Wt / W), (Wn / repmat(W, 1, nn)))
      S = cbind(sin(E), sin(NE))
      C = cbind(cos(E), cos(NE))
    }

    r = cbind(sum(sum(S * rw)), sum(sum(C * rw)))

    if (sum(sum(rw)) == 0) {
      K = 0
    } else {
      R = sqrt(sum(r^2)) / sum(sum(rw))
      K = A1inv(R)
    }

    if (n <= 15) {
      if (K < 2) {
        K = max(K - 2 / (n * K), 0)
      } else {
        K = K * (n - 1)^3 / (n^3 + n)
      }
    }
  }
  if (iter > MaxIter) {
    warning('JV10_function:MaxIter: ','Maximum iteration limit exceeded.')
    B = c(NaN, NaN, NaN, NaN)
    LL = NaN
    W = NaN
  } else {
    B = c(K, Pt, Pn, Pu)
    W <- cbind(Wt / W, t(t(rowSums(Wn, 2))) / W, Wg / W)
  }
  return(list(B, LL, W))
}

#' @title vonmisespdf
#' @description Von Mises probability density function (pdf)
#'  \code{vonmisespdf(x, mu, K)} returns the pdf of the Von Mises
#'   distribution with mean MU and concentration parameter K,
#'   evaluated at the values in THETA (given in radians).
#' @param x Theta
#' @param mu mu
#' @param K Kappa
#' @author Wanja Mössing
#' @export vonmisespdf
vonmisespdf <- function(x, mu, K) {
  p = exp( K * cos( x - mu) - log(2 * pi) - besseliln(0, K));
  return(p)
}

#' @title besseliln
#' @description log of base::besselI
#' @param nu numeric; The order (maybe fractional!) of the corresponding Bessel function.
#' @param z numeric, ≥ 0.
#' @author Wanja Mössing
#' @name besseliln
#' @export besseliln
besseliln <- function(nu, z){
  w = log(besselI(z, nu, 1)) + abs(Re(z))
  return(w)
}

#' @title DT2C016
#' @description Currently, the ported CO16 functions require matrices instead of
#' \code{data.tables} (or \code{data.frames}). This function takes care of
#' extracting the right columns and converts them to matrices. This function is
#' called internally in \code{CO16_fit}.
#' @param DT a \code{data.table} containg one column with X (responses) data,
#' none or one with TT (target) data, and none or multiple with NT (non-target)
#' data. Data should be in radians (default) or degree (see \code{type})
#' @param dt.X pattern that will match the column containing responses. Default
#' matches columns starting with 'X'
#' @param dt.TT pattern that will match the column containing targets. Default
#' matches columns starting with 'TT'
#' @param dt.NT pattern that will match the column containing distractors. Default
#' matches columns starting with 'NT'
#' @param dt.Trial pattern that will match the column containing Trials. Default
#' matches columns starting with 'Trial'. Ignored if missing.
#' @param type Are the input data in degree ('deg') or in radian ('rad',
#' default). If 'deg' is specified, data will be converted to radian.
#' @author Wanja Mössing
#' @name DT2CO16
#' @export DT2CO16
#' @import data.table
#' @importFrom pracma deg2rad
DT2CO16 <- function(DT, dt.X='^X', dt.TT='^TT', dt.NT='^NT', dt.Trial='^Trial',
                    type='rad'){
  ptrns = c(dt.X, dt.TT, dt.NT, dt.Trial)
  names(ptrns) = c('X','TT','NT','Trial')
  if (type == 'deg') {
    circcols <- names(DT)
    circcols <- circcols[circcols %like% paste(ptrns[c('X', 'TT', 'NT')],
                                               collapse = '|')]
    DT[, (circcols) := lapply(.SD, deg2rad), .SDcols = circcols]
  } else {
    if (any(sapply(res[c('X','TT','NT')], function(k) abs(k) > 2 * pi))) {
      stop(paste0('Specified data type is radians, but values exceed 2 * pi.',
                  '\nConsider using type = "deg" instead.'))
    }
  }
  res = sapply(ptrns, function(x) .DT2CO16sub(x, DT),
               simplify = FALSE, USE.NAMES = TRUE)
  return(res)
}

#' @title .DT2CO16sub
#' @description internal function of DT2CO16
#' @author Wanja Mössing
#' @name .DT2CO16sub
#' @import data.table
#' @keywords internal
.DT2CO16sub = function(ptrn, DT){
  if (any(names(DT) %like% ptrn)) {
    return(as.matrix(DT[, .SD, .SDcols = patterns(ptrn)]))
  } else {
    return(NULL)
  }
}

# A1inv is included in CircStats
# A1inv <- function(R) {
#   if (0 <= R & R < 0.53) {
#     K = 2 * R + R^3 + (5 * R^5) / 6
#   } else if (R < 0.85) {
#     K = -0.4 + 1.39 * R + 0.43 / (1 - R)
#   } else {
#     K = 1 / (R^3 - 4 * R^2 + 3 * R)
#   }
#   return(K)
# }

