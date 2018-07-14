
package com.shanker

import java.time.Instant
import java.time.ZoneId
// import com.shanker.exception.ValidationException
import java.time.ZonedDateTime
import java.util.Calendar
import java.util.Date

import scala.util.Try

import com.shanker.exception.ConversionException
import com.shanker.exception.EException
import com.shanker.exception.ValidationException

package object DateUtils {

  /**
   * Converts the Date in string format to Date object
   * 1. String Validation
   * 2. Date Object
   */
  implicit class stringToDate(val dateString: String) {

    def dateFromInstant: Either[Date, EException] = {
      Try(Left(Date.from(Instant.parse(dateString))))
        .getOrElse(Right(ValidationException(s"${dateString} isn't in valid instant string")))
    }
    
    // TODO for other Date string Formats

  }

  /**
   * Returns the individual components
   */
  implicit class individualComponentsFromDate(val date: Date) {

    private def calendarObj = {
      val cal = Calendar.getInstance()
      cal.setTime(date)
      cal
    }

    @inline def year = calendarObj.get(Calendar.YEAR)

    @inline def month = calendarObj.get(Calendar.MONTH)

    @inline def dayOfWeek = calendarObj.get(Calendar.DAY_OF_WEEK)

    @inline def dayOfYear = calendarObj.get(Calendar.DAY_OF_YEAR)

    @inline def dayOfMonth = calendarObj.get(Calendar.DAY_OF_MONTH)

    @inline def hour = calendarObj.get(Calendar.HOUR) + 1

    @inline def hourOfDay = calendarObj.get(Calendar.HOUR_OF_DAY)

    @inline def minute = calendarObj.get(Calendar.MINUTE)

    @inline def second = calendarObj.get(Calendar.SECOND)

    @inline def millisecond = calendarObj.get(Calendar.MILLISECOND)

  }

  /**
   * Rounds off the date to the nearest formats
   */
  implicit class roundedDate(val date: Date) {

    private def calendarObj = {
      val cal = Calendar.getInstance()
      cal.setTime(date)
      cal
    }

    def roundedToMinute = {
      calendarObj.set(Calendar.SECOND, 0)
      calendarObj.set(Calendar.MILLISECOND, 0)
      calendarObj.getTime
    }

    def roundedToTenMinute = {
      val modten = calendarObj.get(Calendar.MINUTE) % 10
      calendarObj.add(Calendar.MINUTE, if (modten < 10) -modten else 10 - modten)
      calendarObj.set(Calendar.SECOND, 0)
      calendarObj.set(Calendar.MILLISECOND, 0)
      calendarObj.getTime
    }

    def roundedToTwentyMinute = {
      val modtwenty = calendarObj.get(Calendar.MINUTE) % 20
      calendarObj.add(Calendar.MINUTE, if (modtwenty < 20) -modtwenty else 20 - modtwenty)
      calendarObj.set(Calendar.SECOND, 0)
      calendarObj.set(Calendar.MILLISECOND, 0)
      calendarObj.getTime
    }

    def roundedToHour = {
      calendarObj.add(Calendar.MINUTE, 0)
      calendarObj.set(Calendar.SECOND, 0)
      calendarObj.set(Calendar.MILLISECOND, 0)
      calendarObj.getTime
    }

    def roundedToDay = {
      calendarObj.set(Calendar.HOUR_OF_DAY, 0)
      calendarObj.add(Calendar.MINUTE, 0)
      calendarObj.set(Calendar.SECOND, 0)
      calendarObj.set(Calendar.MILLISECOND, 0)
      calendarObj.getTime
    }
  }

  /**
   * Conversion of the to different formats i.e into Time Zone conversion
   */
  implicit class convertDate(date: Date) {

    def inUTC: Date = {
      Date.from(ZonedDateTime.ofInstant(date.toInstant(), ZoneId.of("UTC")).toInstant())
    }

    def inAnotherZone(anotherZone: String): Either[Date, EException] = {
      Try(Left(Date.from(ZonedDateTime.ofInstant(date.toInstant(), ZoneId.of(anotherZone)).toInstant())))
        .getOrElse(Right(ConversionException(s"${anotherZone} is not a valid")))
    }
  }
}