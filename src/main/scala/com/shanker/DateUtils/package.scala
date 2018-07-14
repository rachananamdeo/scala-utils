
package com.shanker

import java.time.Instant
import java.time.ZoneId
// import com.shanker.exception.ValidationException
import java.time.ZonedDateTime
import java.util.Calendar
import java.util.Date

import scala.util.control.Breaks._
import scala.util.Try

import com.shanker.exception.EException
import com.shanker.exception.ConversionException
import com.shanker.exception.ValidationException
import com.shanker.exception.PatternFoundException
import java.text.SimpleDateFormat

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

    private def generalMethod(bool: Boolean, pattern: String) =
      {
        if (bool) {
          Left(new SimpleDateFormat(pattern).parse(dateString))
        } else {
          Right(PatternFoundException(s"$dateString has no standard Pattern"))
        }
      }

    /**
     * a generic Method which tries to match the given string
     * with pre-defined date pattern
     * if found valid, then converts to date
     * else @PatternFoundException is thrown
     *
     * Available Patterns are
     *
     * "MM/dd/yyyy",
     * "dd-MMM-yyyy",
     * "MM dd, yyyy",
     * "E, MMM dd yyyy",
     * "dd-M-yyyy hh:mm:ss",
     * "dd MMMM yyyy",
     * "dd MMMM yyyy zzzz",
     * "E, dd MMM yyyy HH:mm:ss z",
     * "E, MMM dd yyyy HH:mm:ss"
     */
    def toDate: Either[Date, EException] = {

      val patternsDefined = new UnmodifiableSeq(List(
        "MM/dd/yyyy",
        "dd-MMM-yyyy",
        "MM dd, yyyy",
        "E, MMM dd yyyy",
        "dd-M-yyyy hh:mm:ss",
        "dd MMMM yyyy",
        "dd MMMM yyyy zzzz",
        "E, dd MMM yyyy HH:mm:ss z",
        "E, MMM dd yyyy HH:mm:ss"))

      var foundele = false
      var pattern = ""

      breakable {
        patternsDefined.foreach(pat => {
          if (pat.r.pattern.matcher(dateString).matches()) {
            foundele = true
            pattern = pat
            break
          }
        })
      }
      generalMethod(foundele, pattern)
    }

    /**
     * Use this method for this pattern
     * for MM/dd/yyyy
     */
    @inline def MMddyyyy = generalMethod(true, "MM/dd/yyyy")
    
    /**
     * Use this method for this pattern
     * for dd-M-yyyy hh:mm:ss
     */
    @inline def ddMyyyyhhmmss = generalMethod(true, "dd-M-yyyy hh:mm:ss")
    
    /**
     * Use this method for this pattern
     * dd MMMM yyyy
     */
    @inline def ddMMMMyyyy = generalMethod(true, "dd MMMM yyyy")
    
    /**
     * Use this Method for this pattern
     * dd MMMM yyyy zzzz
     */
    @inline def ddMMMMyyyyzzzz = generalMethod(true, "dd MMMM yyyy zzzz")
    
    /**
     * Use this Method for this pattern
     * E, dd MMM yyyy HH:mm:ss z
     */
    @inline def EddMMMyyyyHHmmssz = generalMethod(true, "E, dd MMM yyyy HH:mm:ss z")

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