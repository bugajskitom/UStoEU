#' @title Converts US units to European and vice versa
#' @author Olga Chwedoruk, Tomasz Bugajski
#' @description The package converts American units such as 'ft, 'in', 'lbs' to European units: 'm', 'cm', 'kg' and vice versa. 'ft' can also be written as 'foot' and 'feet'. 'in' can also be written as 'inch' and 'inches'. 'm' can also be written as 'meter' and 'metre'. 'cm' can also be written as 'centimeter' and 'centimetre'.
#' @param x numeric value
#' @param from possible values to choose from: 'ft, 'in', 'm', 'cm', 'lbs'
#' @param to possible values to choose from: 'ft, 'in', 'm', 'cm', 'lbs'
#' @param return possible values to choose from: 'value', 'ft&in'
#' @param ... other parameters
#' @return Function returns a measure in the selected unit
#' @rdname UStoEU
#' @export
UStoEU <- function(x, from, to, return = 'value', ...) UseMethod("UStoEU")

#' @title UStoEU
#' @author Olga Chwedoruk, Tomasz Bugajski
#' @description The package converts American units such as 'ft, 'in', 'lbs' to European units: 'm', 'cm', 'kg' and vice versa. 'ft' can also be written as 'foot' and 'feet'. 'in' can also be written as 'inch' and 'inches'. 'm' can also be written as 'meter' and 'metre'. 'cm' can also be written as 'centimeter' and 'centimetre'.
#' @param x numeric value
#' @param from possible values to choose from: 'ft, 'in', 'm', 'cm', 'lbs'
#' @param to possible values to choose from: 'ft, 'in', 'm', 'cm', 'lbs'
#' @param return possible values to choose from: 'value', 'ft&in'
#' @param ... other parameters
#' @return Function returns a measure in the selected unit
#' @rdname UStoEU.default
#' @export
UStoEU.default = function(x, from , to , return = 'value', ...){
  # Check if given value is numeric
  if(is.numeric(x) == FALSE){
    stop("Non-numeric argument")
  }
  # Check if argument in return is correct
  if(!(return %in% c('value', 'ft&in'))){
    stop("Wrong return argument. Try: 'value', 'ft&in'")
  }
  # Conversion from meters to foots and inches
  if(from %in% c('m', 'meter', 'metre') & to %in% c('ft', 'foot', 'feet') & return == 'ft&in'){
    value = x / 1
    me <- list(result = paste(as.character(floor(value*(100/2.54/12))),"ft", as.character(round((value*(100/2.54/12)-floor(value*(100/2.54/12)))*12, 1)), 'in'), unit ="")
    class(me)<-append(class(me),'UStoEU')
    return(me)
  }
  # Conversion from centimeters to foots and inches
  if(from %in% c('cm', 'centimetre', 'centimeter') & to %in% c('ft', 'foot', 'feet') & return == 'ft&in'){
    value = x / 100
    me <- list(result = paste(as.character(floor(value*(100/2.54/12))),"ft", as.character(round((value*(100/2.54/12)-floor(value*(100/2.54/12)))*12, 1)), 'in'), unit ="")
    class(me)<-append(class(me),'UStoEU')
    return(me)
  }
  # Conversion from foots to meters
  if(from %in% c('ft', 'foot', 'feet') & to %in% c('m', 'meter', 'metre')){
    value = x / (100/2.54/12)
    me <- list(result = value*1, unit = to)
    class(me)<-append(class(me),'UStoEU')
    return(me)
  }
  # Conversion from foots to centimeters
  if(from %in% c('ft', 'foot', 'feet') & to %in% c('cm', 'centimetre', 'centimeter')){
    value = x / (100/2.54/12)
    me <- list(result = value*100, unit = to)
    class(me)<-append(class(me),'UStoEU')
    return(me)
  }
  # Conversion from meters to foots
  if(from %in% c('m', 'meter', 'metre') & to %in% c('ft', 'foot', 'feet')){
    value = x / 1
    me <- list(result = value*(100/2.54/12), unit = to)
    class(me)<-append(class(me),'UStoEU')
    return(me)
  }
  # Conversion from centimeters to foots
  if(from %in% c('cm', 'centimetre', 'centimeter') & to %in% c('ft', 'foot', 'feet')){
    value = x / 100
    me <- list(result = value*(100/2.54/12), unit = to)
    class(me)<-append(class(me),'UStoEU')
    return(me)
  }
  # Conversion from inches to meters
  if(from %in% c('in', 'inch', 'inches') & to %in% c('m', 'meter', 'metre')){
    value = x / (100/2.54)
    me <- list(result = value*1, unit = to)
    class(me)<-append(class(me),'UStoEU')
    return(me)
  }
  # Conversion from inches to centimeters
  if(from %in% c('in', 'inch', 'inches') & to %in% c('cm', 'centimetre', 'centimeter')){
    value = x / (100/2.54)
    me <- list(result = value*100, unit = to)
    class(me)<-append(class(me),'UStoEU')
    return(me)
  }
  # Conversion from meters to inches
  if(from %in% c('m', 'meter', 'metre') & to %in% c('in', 'inch', 'inches')){
    value = x / 1
    me <- list(result = value*(100/2.54), unit = to)
    class(me)<-append(class(me),'UStoEU')
    return(me)
  }
  # Conversion from centimeters to inches
  if(from %in% c('cm', 'centimetre', 'centimeter') & to %in% c('in', 'inch', 'inches')){
    value = x / 100
    me <- list(result = value*(100/2.54), unit = to)
    class(me)<-append(class(me),'UStoEU')
    return(me)
  }
  # Conversion from lbs to kg
  if(from %in% c('lbs') & to %in% c('kg')){
    value = x / 2.20462234e-3
    me <- list(result = value*1e-3, unit = to)
    class(me)<-append(class(me),'UStoEU')
    return(me)
  }
  # Conversion from kg to lbs
  if(from %in% c('kg') & to %in% c('lbs')){
    value = x / 1e-3
    me <- list(result = value*2.20462234e-3, unit = to)
    class(me)<-append(class(me),'UStoEU')
    return(me)
  }
  # Check if 'from' argument is correct
  if(!(from %in% c('m', 'meter', 'metre', 'ft', 'foot', 'feet', 'cm', 'centimetre', 'centimeter', 'in', 'inch', 'inches', 'lbs', 'kg'))){
    stop(" Wrong unit: '", from, "'. Try: 'm', 'meter', 'metre', 'ft', 'foot', 'feet', 'cm', 'centimetre', 'centimeter', 'in', 'inch', 'inches', 'lbs', 'kg'")
  }
  # Check if 'to' argument is correct
  if(!(to %in% c('m', 'meter', 'metre', 'ft', 'foot', 'feet', 'cm', 'centimetre', 'centimeter', 'in', 'inch', 'inches', 'lbs', 'kg'))){
    stop(" Wrong unit: '", to, "'. Try: 'm', 'meter', 'metre', 'ft', 'foot', 'feet', 'cm', 'centimetre', 'centimeter', 'in', 'inch', 'inches', 'lbs', 'kg'")
  }
  # Check if 'from' and 'to' arguments are from the same dimension
  else{
    stop("These units cannot be converted because they are of different dimensions")
  }

}

#' @title Print UStoEU
#' @author Olga Chwedoruk, Tomasz Bugajski
#' @description Print the results
#' @param x numeric value
#' @param from possible values to choose from: 'ft, 'in', 'm', 'cm', 'lbs'
#' @param to possible values to choose from: 'ft, 'in', 'm', 'cm', 'lbs'
#' @param return possible values to choose from: 'value', 'ft&in'
#' @param ... other parameters
#' @rdname print.UStoEU
#' @export
print.UStoEU <- function(x, from, to, return = 'value', ...){
  # Print the results with appropriate unit
  cat("You have", x$result, x$unit, "\n")
}

#' @title Summarise UStoEU
#' @author Olga Chwedoruk, Tomasz Bugajski
#' @description Summarise the results
#' @param object numeric value
#' @param ... Other parameters
#' @rdname summary.UStoEU
#' @export
summary.UStoEU <- function(object, ...){
  # Print structure of the argument x
  str(object)
  # Printing out the summary of result
  summary(object$result)
  # Printing out the summary of unit
  summary(object$unit)
  # Printing out the execution time
  cat("\nExecution time:\n")
  system.time(object)
}
