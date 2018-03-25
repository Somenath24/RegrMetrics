#' MAPE Function
#'
#' This function allows you to calculate Mean Absolute Percentage Error (MAPE)
#' @param actual Actual value
#' @param pred  Predicted value
#' @param overadj Logical variable. Default is True. It will adjust MAPE to 100 if it is >100
#' @return Mape value
#' @export
#' @examples
#' data(mtcars)
#' reg <- lm(mpg ~ ., data = mtcars)
#' MAPE(actual = mtcars$mpg,pred=reg$fitted.values)
#'

MAPE=function(actual,pred,overadj=T){
  actual=as.numeric(actual)
  pred=as.numeric(pred)
  mapes <- 100*abs(actual - pred)/actual
  if(overadj){
    mapes[is.infinite(mapes)] <- 100
    mapes[is.nan(mapes)] <- 100
    mapes[mapes>100] <- 100
  }

  Mape=mean(mapes)
  return(Mape)
}

#' MedianAPE Function
#'
#' This function allows you to calculate Median Absolute Percentage Error (MedianAPE)
#' @param actual Actual value
#' @param pred  Predicted value
#' @param overadj Logical variable. Default is True. It will adjust MAPE to 100 if it is >100
#' @return MedianAPE value
#' @importFrom stats median
#' @export
#' @examples
#' data(mtcars)
#' reg <- lm(mpg ~ ., data = mtcars)
#' MedianAPE(actual = mtcars$mpg,pred=reg$fitted.values)
#'

MedianAPE=function(actual,pred,overadj=T){
  actual=as.numeric(actual)
  pred=as.numeric(pred)
  mapes <- 100*abs(actual - pred)/actual
  if(overadj){
    mapes[is.infinite(mapes)] <- 100
    mapes[is.nan(mapes)] <- 100
    mapes[mapes>100] <- 100
  }

  MedianAPE=stats::median(mapes)
  return(MedianAPE)
}


#' NMAPE Function
#'
#' This function allows you to calculate Normalized Mean Absolute Percentage Error (NMAPE), where denominator is maximum of actual and predicted
#' @param actual Actual value
#' @param pred  Predicted value
#' @param overadj Logical variable. Default is True. It will adjust MAPE to 100 if it is >100
#' @return NMape value
#' @export
#' @examples
#' data(mtcars)
#' reg <- lm(mpg ~ ., data = mtcars)
#' NMAPE(actual = mtcars$mpg,pred=reg$fitted.values)
#'

NMAPE=function(actual,pred,overadj=T){
  actual=as.numeric(actual)
  pred=as.numeric(pred)
  den=pmax(actual,pred)
  mapes <- 100*abs(actual - pred)/den
  if(overadj){
    mapes[is.infinite(mapes)] <- 100
    mapes[is.nan(mapes)] <- 100
    mapes[mapes>100] <- 100
  }

  Mape=mean(mapes)
  return(Mape)
}

#' SMAPE Function
#'
#' This function allows you to calculate Symmetric Mean Absolute Percentage Error (SMAPE)
#' @param actual Actual value
#' @param pred  Predicted value
#' @param overadj Logical variable. Default is True. It will adjust MAPE to 100 if it is >100
#' @return SMAPE value
#' @export
#' @examples
#' data(mtcars)
#' reg <- lm(mpg ~ ., data = mtcars)
#' SMAPE(actual = mtcars$mpg,pred=reg$fitted.values)
#'

SMAPE=function(actual,pred,overadj=T){
  actual=as.numeric(actual)
  pred=as.numeric(pred)
  mapes=100*(mean(abs(actual-pred)/((abs(actual)+abs(pred))/2)))

  if(overadj){
    mapes[is.infinite(mapes)] <- 100
    mapes[is.nan(mapes)] <- 100
    mapes[mapes>100] <- 100
  }

  mapes <- mapes[!is.na(mapes)]
  Mape=mean(mapes)
  return(Mape)
}

#' MADP Function
#'
#' This function allows you to calculate Mean Absolute Deviation Percent (MADP)
#' @param actual Actual value
#' @param pred  Predicted value
#' @return MADP value
#' @export
#' @examples
#' data(mtcars)
#' reg <- lm(mpg ~ ., data = mtcars)
#' MADP(actual = mtcars$mpg,pred=reg$fitted.values)
#'


MADP=function(actual,pred){
  actual=as.numeric(actual)
  pred=as.numeric(pred)

  Mape=100*(mean(abs(actual-pred))/mean(actual))
  if(is.na(Mape)){
    Mape=0
  }

  return(Mape)
}


#' AlterMAPE Function
#'
#' This function allows you to calculate Alternative Mean Absolute Percent Error (AlterMAPE). This is basically error percentage from actual mean line.
#' @param actual Actual value
#' @param pred  Predicted value
#' @param overadj Logical variable. Default is True. It will adjust MAPE to 100 if it is >100
#' @return AlterMAPE value
#' @export
#' @examples
#' data(mtcars)
#' reg <- lm(mpg ~ ., data = mtcars)
#' AlterMAPE(actual = mtcars$mpg,pred=reg$fitted.values)
#'

AlterMAPE=function(actual,pred,overadj=T){
  actual=as.numeric(actual)
  pred=as.numeric(pred)

  mapes <- 100*abs(mean(actual) - pred)/mean(actual)

  if(overadj){
    mapes[is.infinite(mapes)] <- 100
    mapes[is.nan(mapes)] <- 100
    mapes[mapes>100] <- 100
  }

  mapes <- mapes[!is.na(mapes)]
  Mape=mean(mapes)
  return(Mape)
}

#' MDA Function
#'
#' This function allows you to calculate Mean Directional Accuracy (MDA) which helps to find out how much prediction is following the trend of actual
#' @param actual Actual value
#' @param pred  Predicted value
#' @return MDA value
#' @importFrom stats na.omit
#' @export
#' @examples
#' data(mtcars)
#' reg <- lm(mpg ~ ., data = mtcars)
#' MDA(actual = mtcars$mpg,pred=reg$fitted.values)
#'

MDA=function(actual,pred){
  actual=as.numeric(actual)
  pred=as.numeric(pred)

  Actual=actual
  Actual_1=data.table::shift(actual,n=1)
  Actual_sign=as.numeric(stats::na.omit(sign(Actual-Actual_1)))

  Predicted=pred
  Predicted_1=data.table::shift(Predicted,n=1)
  Predicted_sign=as.numeric(stats::na.omit(sign(Predicted-Predicted_1)))
  acc=mean(Actual_sign==Predicted_sign)*100

  return(acc)
}

#' RMSE Function
#'
#' This function allows you to calculate Root Mean Square Error (RMSE)
#' @param actual Actual value
#' @param pred  Predicted value
#' @return RMSE value
#' @export
#' @examples
#' data(mtcars)
#' reg <- lm(mpg ~ ., data = mtcars)
#' RMSE(actual = mtcars$mpg,pred=reg$fitted.values)
#'

RMSE=function(actual,pred){
  actual=as.numeric(actual)
  pred=as.numeric(pred)
  RMSE=sqrt(mean((actual-pred)^2))
  return(RMSE)
}

#' MSE Function
#'
#' This function allows you to calculate Mean Square Error (MSE)
#' @param actual Actual value
#' @param pred  Predicted value
#' @return MSE value
#' @export
#' @examples
#' data(mtcars)
#' reg <- lm(mpg ~ ., data = mtcars)
#' MSE(actual = mtcars$mpg,pred=reg$fitted.values)
#'

MSE=function(actual,pred){
  actual=as.numeric(actual)
  pred=as.numeric(pred)
  MSE=mean((actual-pred)^2)
  return(MSE)
}

#' MAE Function
#'
#' This function allows you to calculate Mean Absolute Error (MAE)
#' @param actual Actual value
#' @param pred  Predicted value
#' @return MAE value
#' @export
#' @examples
#' data(mtcars)
#' reg <- lm(mpg ~ ., data = mtcars)
#' MAE(actual = mtcars$mpg,pred=reg$fitted.values)
#'

MAE=function(actual,pred){
  actual=as.numeric(actual)
  pred=as.numeric(pred)
  MAE=mean(abs(actual-pred))
  return(MAE)
}

#' R_sqr Function
#'
#' This function allows you to calculate R-Square value
#' @param actual Actual value
#' @param pred  Predicted value
#' @return R-square value
#' @export
#' @examples
#' data(mtcars)
#' reg <- lm(mpg ~ ., data = mtcars)
#' R_sqr(actual = mtcars$mpg,pred=reg$fitted.values)
#'

R_sqr=function(actual, pred)
{
  R_sqr <- 1 - sum((actual - pred)^2)/sum((actual - mean(actual))^2)
  return(R_sqr)
}
