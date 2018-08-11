
package com.shanker

import java.text.SimpleDateFormat
import java.time.Instant
import java.time.ZoneId
import java.time.ZonedDateTime
import java.util.Calendar
import java.util.Date
import java.util.TimeZone

import scala.util.Try
import scala.util.control.Breaks.break
import scala.util.control.Breaks.breakable

import org.joda.time.DateTime
import org.joda.time.Days
import org.joda.time.Minutes
import org.joda.time.Months
import org.joda.time.Seconds
import org.joda.time.Years

import com.shanker.exception.ConversionException
import com.shanker.exception.EException
import com.shanker.exception.ElementnotFoundException
import com.shanker.exception.PatternFoundException
import com.shanker.exception.ValidationException
import com.shanker.annotations.Since

// TODO: Add the check style configuration
package object DateUtils {

  /**
   * Converts the Date in string format to Date object
   * 1. String Validation
   * 2. Date Object
   */
  @Since("Starting")
  implicit class StringToDate(val dateString: String) {

    def dateFromInstant: Either[Date, EException] = {
      Try(Left(Date.from(Instant.parse(dateString))))
        .getOrElse(Right(ValidationException(s"${dateString} isn't in valid instant string")))
    }

    private[this] def generalMethod(bool: Boolean, pattern: String) =
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
     *
     * TODO: To add more date patterns
     *
     * Thought:
     * How about taking the date and dissemble it and return the date
     * like first check the pattern if found ok else
     * dissecct the given date string and try to form the
     * best possible date pattern and return the pattern
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
  @Since("Starting")
  implicit class IndividualComponentsFromDate(val date: Date) {

    private[this] def calendarObj = {
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

    /**
     * TODO:
     * 1. getTImeZone
     */
  }

  /**
   * Rounds off the date to the nearest formats
   */
  @Since("Starting")
  implicit class RoundedDate(val date: Date) {

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

    // TODO def toNearest Sunday

  }

  /**
   * Conversion of the to different formats i.e into Time Zone conversion
   */
  @Since("Starting")
  implicit class ConvertDate(date: Date) {

    def inUTC: Date = {
      Date.from(ZonedDateTime.ofInstant(date.toInstant(), ZoneId.of("UTC")).toInstant())
    }

    def inAnotherZone(anotherZone: String): Either[Date, EException] = {
      TimeZone.getAvailableIDs.contains(anotherZone)
      Try({
        if (TimeZone.getAvailableIDs.contains(anotherZone)) {
          Left(Date.from(ZonedDateTime.ofInstant(date.toInstant(), ZoneId.of(anotherZone)).toInstant()))
        } else {
          Right(ElementnotFoundException(s"${anotherZone} is not a valid Zone ID"))
        }
      }).getOrElse(Right(ConversionException(s"${anotherZone} is not a valid")))
    }

  }

  /**
   * TODO:
   * 1. isFutureDate
   * 2. Same Day in the nearestFuture on the same date
   * 3. Same Day in the nearestPast on the same date
   */
  @Since("0.1")
  implicit class FutureDatesOpr(val date: Date) {

    private def calendarObj = {
      val cal = Calendar.getInstance()
      cal.setTime(date)
      cal
    }

    def isFutureDate(dateOp: Option[Date] = None) = {
      val _anotherDate = if (dateOp.isDefined) new DateTime(date) else new DateTime()
      if (new DateTime(date).getMillis() - _anotherDate.getMillis() > 0) true
      //else if(new DateTime(date).getMillis()-_anotherDate.getMillis()<0) false
      else false
    }

    def getDay = {
      calendarObj.get(Calendar.DAY_OF_WEEK)
    }
  }

  /**
   * TODO:
   * 1. Get all the dates of Mentioned Day for the past year from given date
   * 2. Get all the dates of Mentioned Day for the future year from given date
   * 3. Number of SUnDAY/any day in a year, remaining from current date, remaining from day
   * 4. current Day in another Year, returns the Day s
   */
  @Since("0.1")
  implicit class DaysOpr(val date: Date) {

    def calendarObj(date: Date) = {
      val cal = Calendar.getInstance()
      cal.setTime(date)
      cal
    }

    def sundaysCount(dateOp: Option[Date] = None) = {
      val _anotherDate = if (dateOp.isDefined) new DateTime(date) else new DateTime()
      var sunCount = 0
      val (sDate, eDate) = if (new DateTime(date).getMillis() - _anotherDate.getMillis() > 0) { (_anotherDate.toDate(), date) } else { (date, _anotherDate.toDate()) }
      val (startDate, endStart) = (calendarObj(sDate), calendarObj(eDate))

      while (endStart.after(startDate)) {
        if (startDate.get(Calendar.DAY_OF_WEEK) == Calendar.SUNDAY)
          sunCount + 1
        startDate.add(Calendar.DATE, 1)
      }
    }

    def datesOfDay(dateOp: Option[Date] = None) = {
      val _anotherDate = if (dateOp.isDefined) new DateTime(date) else new DateTime()
      var dates = List[Date]()
      val (sDate, eDate) = if (new DateTime(date).getMillis() - _anotherDate.getMillis() > 0) { (_anotherDate.toDate(), date) } else { (date, _anotherDate.toDate()) }
      val (startDate, endStart) = (calendarObj(sDate), calendarObj(eDate))

      while (endStart.after(startDate)) {
        if (startDate.get(Calendar.DAY_OF_WEEK) == Calendar.SUNDAY)
          dates ::= startDate.getTime()
        startDate.add(Calendar.DATE, 7)
      }
    }

    def isLeapYear = {
      if (calendarObj(date).getTime().toInstant().atZone(ZoneId.systemDefault()).toLocalDate().isLeapYear()) true
      else false
    }
  }

  // TODO: Add the comments for scala Doc
  @Since("Starting")
  implicit class DifferenceofDuration(val date: Date) {

    def diffInYears(dateOp: Option[Date] = None) = {
      val _anotherDate = if (dateOp.isDefined) new DateTime(date) else new DateTime()
      Years.yearsBetween(new DateTime(date), _anotherDate).getYears
    }

    def diffInMonths(dateOp: Option[Date] = None) = {
      val _anotherDate = if (dateOp.isDefined) new DateTime(date) else new DateTime()
      Months.monthsBetween(new DateTime(date), _anotherDate).getMonths
    }

    def diffInDays(dateOp: Option[Date] = None) = {
      val _anotherDate = if (dateOp.isDefined) new DateTime(date) else new DateTime()
      Days.daysBetween(new DateTime(date), _anotherDate).getDays
    }
    def diffInMinutes(dateOp: Option[Date] = None) = {
      val _anotherDate = if (dateOp.isDefined) new DateTime(date) else new DateTime()
      Minutes.minutesBetween(new DateTime(date), _anotherDate).getMinutes
    }
    def diffInSeconds(dateOp: Option[Date] = None) = {
      val _anotherDate = if (dateOp.isDefined) new DateTime(date) else new DateTime()
      Seconds.secondsBetween(new DateTime(date), _anotherDate).getSeconds
    }

  }

}