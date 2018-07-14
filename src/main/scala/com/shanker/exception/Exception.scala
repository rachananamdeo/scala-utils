package com.shanker.exception

trait EException

case class ValidationException(exceptionStr: String) extends EException

case class ConversionException(exceptionStr: String) extends EException

case class ElementnotFoundException(exceptionStr: String) extends EException

case class GenericException(exceptionStr: String) extends EException