
library(gsl)
library(numDeriv)
library(polynom)

setClass("BaseCurve", representation(TimeToMaturity = "numeric",
                                     PriceYield     = "numeric",
                                     Poly.Order     = "integer",
                                     BaseData       = "data.frame",
                                     n.obs          = "numeric",
                                     n.segments     = "numeric",
                                     n.interior     = "numeric",
                                     n.param        = "numeric",
                                     coef           = "numeric",
                                     "VIRTUAL"))

setValidity("BaseCurve",
            function(object){
              if(anyDuplicated(object@TimeToMaturity) != 0) return("Duplicated Values!")
              if(length(object@PriceYield) != length(object@TimeToMaturity)) return("Dimension between Price/Yield and Maturity mismatch.")
              maturitys <- sort(object@TimeToMaturity)
              if(any(object@TimeToMaturity < 0)) return("Time to maturity must be positive.")
              if(maturitys[1] != 0)              return("There must be a 0 value in the time to maturity data!")
              if(object@Poly.Order < 1)          return("Degree of the polinomial must be greater than 1.")
              return(TRUE)
                            }  
           )

setMethod(
  f = "[",
  signature = "BaseCurve",
  definition = function(x, i, j, drop){
    if(i == "coef"){
      N <- x@n.segments
      degree <- x@Poly.Order + 1
      param <- matrix(data = NA, nrow = N, ncol = degree)
      rownames(param ) <- paste("S", 1:N, sep = "")
      colnames(param) <- c("constant", paste("x^", 1:x@Poly.Order, sep = ""))
      for(id in 1:N){
      param[id,] <- x@coef[(degree*(id-1) + 1):(degree*id)]
      }
      return(param)}else{return(NULL)}
  }
)

setMethod(f = "initialize",  signature  = "BaseCurve",
  definition = function(.Object, Time_Maturity, Price_Yield, Poly_degree, ...){
    
    input.data <- data.frame(Time_Maturity, Price_Yield)
    input.data <- input.data[order(Time_Maturity),]
   
    .Object@BaseData   <- data.frame(Maturity = input.data[, 1] , Yields = input.data[, 2])
    
    .Object@coef           <- 9999L
    .Object@TimeToMaturity <- Time_Maturity
    .Object@PriceYield     <- Price_Yield
    .Object@Poly.Order     <- as.integer(Poly_degree)
    .Object@n.obs          <- nrow(.Object@BaseData)
    .Object@n.segments     <- .Object@n.obs - 1
    .Object@n.interior     <- .Object@n.obs - 2
    .Object@n.param        <- (.Object@Poly.Order + 1) * (.Object@n.obs - 1)
    
    Discount <- exp(-input.data[, 2] * input.data[, 1]/100)
    
    .Object@BaseData <-cbind(.Object@BaseData, Discount)
    
    k <- 1:.Object@n.segments
    Forward.Rate <- log(.Object@BaseData[k, 3]/.Object@BaseData[k + 1, 3])
    Forward.Rate <- c(NA, Forward.Rate)
    
    .Object@BaseData <- cbind(.Object@BaseData , Forward.Rate)
    
    names(.Object@BaseData) <- c("Maturity", "Yields",  "Discount", "Forward.Rate")
    
    validObject(.Object)
    
    return(.Object)
  }
)

setGeneric("eval_poly",function(object, tau){standardGeneric("eval_poly")})
setMethod("eval_poly", "BaseCurve", function(object, tau){
  
  func <- function(time.mat){
    if(time.mat >= object@BaseData[object@n.obs, "Maturity"]){id <- object@n.segments}else if(time.mat < object@BaseData[1, "Maturity"]){id <- 1}else{
      flag <- time.mat >= object@BaseData[, "Maturity"]
      id <- max(which(flag))
    }
    degree <- object@Poly.Order + 1
    fwrd <- gsl::gsl_poly(c_gsl = object@coef[(degree*(id - 1) + 1):(degree * id)], x = time.mat)
    return(fwrd)
  }
  
  dim(tau) <- c(length(tau), 1)
  ys <- apply(tau, 1, func)  
  return(ys)
}
)

setClass("MaxSmooth", contains = "BaseCurve")

setGeneric("zcoef",function(.Object, X1, X2){standardGeneric("zcoef")})
setMethod(f = zcoef, signature = "MaxSmooth",
          definition = function(.Object, X1, X2){
            
            ###y(0) = f(0) constrain
            block0 <- function(){
              block <- matrix(data = 0, nrow = 1, ncol = 5*.Object@n.segments)
              block[1, 1] <- 1
              return(block)  
            }
            
            ### Equality of level
            block1 <- function(){
              block <- matrix(data = 0, nrow = .Object@n.interior, ncol = 10)
              
              for(k in 1:.Object@n.interior){
                v1 <- c(1, .Object@BaseData[k + 1, "Maturity"], .Object@BaseData[k + 1, "Maturity"]^2,
                        .Object@BaseData[k + 1, "Maturity"]^3, .Object@BaseData[k + 1, "Maturity"]^4)
                v <- c(v1, -v1)
                block[k,] <- v
              }
              
              bloco <- matrix(data = 0, nrow = .Object@n.interior, ncol = 5*.Object@n.segments)
              for(k in 1:.Object@n.interior){
                bloco[k, (5*k - 4):(5*k - 4 + 9)] <- block[k,]
              }
              
              return(bloco)
            }
            
            ### Equality of first derivative
            block2 <- function(){
              block <- matrix(data = 0, nrow = .Object@n.interior, ncol = 10)
              
              for(k in 1:.Object@n.interior){
                v1 <- c(0, 1, 2*.Object@BaseData[k + 1, "Maturity"], 3*.Object@BaseData[k + 1, "Maturity"]^2,
                        4*.Object@BaseData[k + 1, "Maturity"]^3)
                v <- c(v1, -v1)
                block[k,] <- v
              }
              
              bloco <- matrix(data = 0, nrow = .Object@n.interior, ncol = 5*.Object@n.segments)
              for(k in 1:.Object@n.interior){
                bloco[k, (5*k - 4):(5*k - 4 + 9)] <- block[k,]
              }
              
              return(bloco)
            }
            
            ### Equality of second derivative
            block3 <- function(){
              block <- matrix(data = 0, nrow = .Object@n.interior, ncol = 10)
              
              for(k in 1:.Object@n.interior){
                v1 <- c(0, 0, 2, 6*.Object@BaseData[k + 1, "Maturity"], 12*.Object@BaseData[k + 1, "Maturity"]^2)
                v <- c(v1, -v1)
                block[k,] <- v
              }
              
              bloco <- matrix(data = 0, nrow = .Object@n.interior, ncol = 5*.Object@n.segments)
              for(k in 1:.Object@n.interior){
                bloco[k, (5*k - 4):(5*k - 4 + 9)] <- block[k,]
              }
              
              return(bloco)
            }
            
            ### Equality of third derivative
            block4 <- function(){
              block <- matrix(data = 0, nrow = .Object@n.interior, ncol = 10)
              
              for(k in 1:.Object@n.interior){
                v1 <- c(0, 0, 0, 6, 24*.Object@BaseData[k + 1, "Maturity"])
                v <- c(v1, -v1)
                block[k,] <- v
              }
              
              bloco <- matrix(data = 0, nrow = .Object@n.interior, ncol = 5*.Object@n.segments)
              for(k in 1:.Object@n.interior){
                bloco[k, (5*k - 4):(5*k - 4 + 9)] <- block[k,]
              }
              
              return(bloco)
            }
            
            ### Matching vales of the market forward rate
            block5 <- function(){
              block <- matrix(data = 0, nrow = .Object@n.segments, ncol = 5)
              
              for(k in 2:.Object@n.obs){
                v1 <- c(               .Object@BaseData[k, "Maturity"]   - .Object@BaseData[k - 1, "Maturity"],
                                0.5  *(.Object@BaseData[k, "Maturity"]^2 - .Object@BaseData[k - 1, "Maturity"]^2), 
                                (1/3)*(.Object@BaseData[k, "Maturity"]^3 - .Object@BaseData[k - 1, "Maturity"]^3), 
                                0.25 *(.Object@BaseData[k, "Maturity"]^4 - .Object@BaseData[k - 1, "Maturity"]^4),
                                0.2  *(.Object@BaseData[k, "Maturity"]^5 - .Object@BaseData[k - 1, "Maturity"]^5) )
                
                block[k - 1,] <- v1
              }
              
              bloco <- matrix(data = 0, nrow = .Object@n.segments, ncol = 5*.Object@n.segments)
              for(k in 1:.Object@n.segments){
                bloco[k, (5*k - 4):(5*k)] <- block[k,]
              }
              
              return(bloco)
            }
            
            ### second derivative of the first segment at t = 0 set at x1
            block6 <- function(){
              block <- matrix(data = 0, nrow = 1, ncol = 5*.Object@n.segments)
              block[1, 3] <- 2
              return(block)
            }
            
            ### first derivative of the last segment at t = n set at 0
            block7 <- function(){
              block <- matrix(data = 0, nrow = 1, ncol = 5*.Object@n.segments)
              
              v1 <- c(0, 1, 2*.Object@BaseData[.Object@n.obs, "Maturity"], 3*.Object@BaseData[.Object@n.obs, "Maturity"]^2,
                      4*.Object@BaseData[.Object@n.obs, "Maturity"]^3)
              block[1, (5*(.Object@n.segments - 1)+ 1):(5*.Object@n.segments)] <- v1
              
              return(block)
            }
            
            ### second derivative of the last segment at t = n set at x2
            block8 <- function(){
              block <- matrix(data = 0, nrow = 1, ncol = 5*.Object@n.segments)
              
              v1 <- c(0, 0, 2, 6*.Object@BaseData[.Object@n.obs, "Maturity"], 12*.Object@BaseData[.Object@n.obs, "Maturity"]^2)
              block[1, (5*(.Object@n.segments - 1)+ 1):(5*.Object@n.segments)] <- v1
              
              return(block)
            }
            
            ### vector of match
            y.vector <- function(x1 = 0, x2 = 0){
              
              block <- matrix(data = 0, nrow = 5*.Object@n.segments , ncol = 1)
              
              block[1, 1] <- .Object@BaseData[1,"Yields"]/100
              
              block[5*.Object@n.segments - 2, 1] <- x1
              block[5*.Object@n.segments    , 1] <- x2
              
              linha <- 1 + 4 * .Object@n.interior + 1
              block[linha:(linha + .Object@n.interior), 1] <- .Object@BaseData[2:.Object@n.obs, "Forward.Rate"]
              return(block)
            }
            
            Xm <- rbind(block0(), block1(), block2(), block3(), block4(), block5(), block6(), block7(), block8())
            
            coef <- solve(Xm, y.vector(x1 = X1, x2 = X2))
            
            return(coef)
          }
          )

setMethod(f = "initialize", signature = "MaxSmooth",
          definition = function(.Object, Time_Maturity, Price_Yield, Poly_degree, X_1, X_2, ...){
            .Object <- callNextMethod(.Object, Time_Maturity, Price_Yield, 4)
            param <-  zcoef(.Object, X1 = X_1, X2 = X_2)
            .Object@coef <- as.numeric(param)
            .Object@Poly.Order <- 4L
            validObject(.Object)
            return(.Object)
          })

setGeneric("frwd_rate", function(tau, obj){standardGeneric("frwd_rate")})
setMethod(f = frwd_rate, signature(tau = "numeric", "MaxSmooth"),
          definition = function(tau, obj){eval_poly(object = obj, tau = tau)})

# Yield curve construction from forward rate.
# Slow function, using the general integrate function.
################################################################################
setGeneric("yield_curve", function(tau, obj){standardGeneric("yield_curve")})
setMethod(f = yield_curve, signature(tau = "numeric", "MaxSmooth"),
          definition = function(tau, obj){ 
            
          dim(tau) <- c(length(tau), 1)
          
          func_rate <- function(tau, obj){
            if(tau > 0){return(stats::integrate(f = frwd_rate, lower = 0, upper = tau, obj, 
                                                subdivisions = 300L, rel.tol = 0.000001)$value/tau)
            }else{
              return(frwd_rate(0, obj))
            }
          }
          
          apply(tau, 1, func_rate, obj = obj)})

# Optimization objective function
################################################################################
target <- function(Xx, obj){
  
  obj@coef <- as.numeric(zcoef(.Object = obj, X1 = Xx[1], X2 = Xx[2]))
  
  func1 <- function(tau){ v <- hessian(func = frwd_rate, x = tau,  method = "Richardson", obj = obj)
                          return(v*v)
                        }
  
  func2 <- function(tau){
                          dim(tau) <- c(length(tau), 1)
                          apply(tau, 1, func1)
                        }
  
  interval <- c(0, obj@BaseData[obj@n.obs,"Maturity"])
  goal <- stats::integrate(f = func2, lower = interval[1], upper = interval[2], subdivisions = 300L, rel.tol = 0.00001)$value
  
  return(goal)
}

target2 <- function(Xx = c(0, 0), obj){
  obj@coef <- as.numeric(zcoef(.Object = obj, X1 = Xx[1], X2 = Xx[2]))
  
  temp <- dxI_poly(obj["coef"], func = dpx)
  hessian_matrix <- dxI_poly(temp, func = dpx)
  
  hessian_matrix_sqr <- matrix(NA, nrow = nrow(hessian_matrix), ncol = 2*(ncol(hessian_matrix) - 1) + 1)
  
  for(k in 1:nrow(hessian_matrix)){
    p  <- polynom::polynomial(hessian_matrix[k,])
    pp <- matrix(as.numeric(p*p), nrow = 1)
    hessian_matrix_sqr[k,] <- pp
  }
  
  intervalo <- cbind(obj@BaseData[1:obj@n.segments ,"Maturity"], obj@BaseData[2:obj@n.obs ,"Maturity"] )
  
  goal <- func_int_poly(coefs = hessian_matrix_sqr, intervalo = intervalo) #*(intervalo[nrow(intervalo), 2] - intervalo[1, 1])
  return(goal)
}

################################################################################

# Intergration of function represented by segmented polinomial
################################################################################

func_int_poly <- function(coefs, intervalo){
# faster evaluation of  func_int_poly2 for the particular case that 
# the integration is done over the interval represented by "intervalo"
  
  Icoefs <- dxI_poly(coefs = coefs, func = Ipx)
  s <- 0
  for(k in 1:nrow(Icoefs)){ 
    y <- gsl::gsl_poly(c_gsl =  Icoefs[k, ], x = intervalo[k, 1:2])
    y <- y[2] - y[1]
    s <- s + y
  }
  return(s)
}

func_integrate_poly <- function(tau, coefs, intervalo){
  
  func_int_poly2 <- function(tau, coefs, intervalo){
    # Calculates a numeric integral of a function represented by a segmented polimonial.
    # The polinomials coeficientes are set in "coefs", a numerical matrix [a x b]
    # where "a" is the number of segments and "b" the order of the polinomial.  
    # The polinomial evaluation function always considers that the constant is included.  
    # The segments is represented by a numerical matrix "intervalo" of dimension [a x 2]
    # look out that the numbers of rows of "intervalo" and "coef" must be the same, but it is not verified!
    # the first argument, "tau", represents the point which the intergral must be taken.
    # the integration interval is from intervalo[1, 1] to "tau" 
    
    flag <- intervalo[, 2] >= tau  
    
    if(any(flag)){id <- min(which(flag))}else{id <- nrow(coefs)} 
    
    Icoefs <- dxI_poly(coefs = coefs, func = Ipx)
    
    if(tau == 0){return(gsl::gsl_poly(c_gsl =  coefs[1, ], x = 0))}
    
    s <- 0
    if(id == 1){  
      y <- gsl::gsl_poly(c_gsl =  Icoefs[1, ], x = c(intervalo[1, 1], tau))
      y <- (y[2] - y[1])
      s <- s + y}else{
        for(k in 1:(id - 1)){ 
          y <- gsl::gsl_poly(c_gsl =  Icoefs[k, ], x = intervalo[k, 1:2])
          y <- (y[2] - y[1])
          s <- s + y
        }
        y <- gsl::gsl_poly(c_gsl =  Icoefs[id, ], x = c(intervalo[id, 1], tau))
        y <- (y[2] - y[1])
        s <- s + y        
      }
    
    return(s)
  }
  
  # vectorize func_int_poly2  
  dim(tau) <- c(length(tau), 1)
  apply(X = tau, MARGIN = 1, FUN = func_int_poly2, coefs = coefs, intervalo = intervalo)
}

################################################################################

#Polinomial evaluation
################################################################################

# Derivative
dpx <- function(poly.coefs){
  order <- length(poly.coefs) # constant included
  dx <- rep(0, order - 1)
  
  for(k in 1:(order - 1)){
    dx[k] <- k*poly.coefs[k + 1]
  }
  
  nomes <-  list("", c("constant", paste("x^", 1:(order - 2), sep = "")))
  dx <- matrix(data = dx, nrow = 1, ncol = order - 1, dimnames = nomes)
  return(dx)
}

# Integrate
Ipx <- function(poly.coefs){
  order <- length(poly.coefs) # constant included
  Ix <- rep(0, order + 1)
  
  for(k in 2:(order + 1)){
    Ix[k] <- poly.coefs[k - 1]/(k - 1)
  }
  
  nomes <-  list("", c("constant", paste("x^", 1:order, sep = "")))
  Ix <- matrix(data = Ix, nrow = 1, ncol = order + 1, dimnames = nomes)
  return(Ix)
}

dxI_poly <- function(coefs, func = dpx){
  y <- apply(coefs, 1, func)
  y <- t(y)
  rownames(y) <- NULL
  colnames(y) <- c("constant", paste("x^", 1:(ncol(y) - 1), sep = ""))
  return(y)
}
################################################################################

#Faster evaluation of yield_curve
setGeneric("ycurve", function(tau, obj){standardGeneric("ycurve")})
setMethod(f = ycurve, signature(tau = "numeric", "MaxSmooth"),
          definition = function(tau, obj){
            
            intervalo  <- cbind(obj@BaseData[1:obj@n.segments,  "Maturity"], obj@BaseData[2:obj@n.obs,  "Maturity"])
            yc  <- func_integrate_poly(tau = tau, coefs =  obj["coef"],  intervalo = intervalo)
            tau[tau == 0] <- 1
            
            yc <- yc/tau
            
            return(yc)
            
          })


MaxSmooth <- function(time_to_maturity, yield, x1 = 0, x2 = 0){
  
  MS <- new("MaxSmooth", Time_Maturity = time_to_maturity, Price_Yield = yield, Poly_degree = 4, X_1 = x1, X_2 = x2)
}


###############################################

setClass("PolySqr", contains = "BaseCurve")

setMethod(f = "initialize", signature = "PolySqr",
          definition = function(.Object, Time_Maturity, Price_Yield, Poly_degree, X_1, ...){
            .Object <- callNextMethod(.Object, Time_Maturity, Price_Yield, 2)
            param <-  coefunc(.Object, X1 = X_1)
            .Object@coef <- as.numeric(param)
            .Object@Poly.Order <- 2L
            validObject(.Object)
            return(.Object)
          })

setGeneric("coefunc",function(.Object, X1, ...){standardGeneric("coefunc")})
setMethod(f = coefunc, signature = c("PolySqr", "numeric"),
          definition = function(.Object, X1, ...){
            
            ### Equality of level
            block1 <- function(){
              block <- matrix(data = 0, nrow = 2 * .Object@n.segments, ncol = 3)
              
              for(k in 1:.Object@n.segments){
                v1 <- c(1, .Object@BaseData[k    , "Maturity"], .Object@BaseData[k    , "Maturity"]^2)
                v2 <- c(1, .Object@BaseData[k + 1, "Maturity"], .Object@BaseData[k + 1, "Maturity"]^2)
                
                block[(2 * k) - 1    ,] <- v1
                block[(2 * k) - 1 + 1,] <- v2
              }
              
              bloco <- matrix(data = 0, nrow = 2 * .Object@n.segments, ncol = 3 * .Object@n.segments)
              for(k in 1:.Object@n.segments){
                
                bloco[(2 * k) - 1    , (3*k - 2):(3*k)] <- block[(2 * k) - 1    , ]
                bloco[(2 * k) - 1 + 1, (3*k - 2):(3*k)] <- block[(2 * k) - 1 + 1, ]
              }
              
              return(bloco)
            }
            
            ### Equality of first derivatives
            block2 <- function(){
              block <- matrix(data = 0, nrow = .Object@n.interior, ncol = 3*2)
              
              for(k in 1:.Object@n.interior){
                v1 <- c(0, 1, 2*.Object@BaseData[k + 1, "Maturity"])
                v <- c(v1, -v1)
                block[k,] <- v
              }
              
              bloco <- matrix(data = 0, nrow = .Object@n.interior, ncol = 3*.Object@n.segments)
              for(k in 1:.Object@n.interior){
                bloco[k, (3*k - 2):(3*k - 2 + 5)] <- block[k,]
              }
              
              return(bloco)
            }
            
            ### Fisrt derivative at T = 0
            block3 <- function(){
              block <- matrix(data = 0, nrow = 1, ncol = 3*.Object@n.segments)
              
              block[1, 3*.Object@n.segments - 1] <- 1
              block[1, 3*.Object@n.segments] <- 2*.Object@BaseData[.Object@n.obs, "Maturity"]
              
              return(block)
            }
            
            ### vector to match
            y.vector <- function(x1 = 0){
              
              block <- matrix(data = 0, nrow = 3*.Object@n.segments , ncol = 1)
              
              block[1, 1] <- .Object@BaseData[1, "Yields"]/100
              block[2 * .Object@n.segments, 1] <- .Object@BaseData[.Object@n.obs, "Yields"]/100
              
              block[3 * .Object@n.segments, 1] <- x1
              
              for(k in 1:.Object@n.interior){
                block[2*k    , 1] <- .Object@BaseData[k + 1, "Yields"]/100
                block[2*k + 1, 1] <- .Object@BaseData[k + 1, "Yields"]/100
              }
              
              return(block)
            }
            
            Xm <- rbind(block1(), block2(), block3())
            
            coef <- solve(Xm, y.vector(x1 = X1))
            
            return(coef)
          }
)


