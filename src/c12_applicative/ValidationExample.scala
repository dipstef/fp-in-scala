package c12_applicative

import java.util.Date
import c12_applicative.Validation.validationApplicative

object ValidationExample {

  case class WebForm(name: String, birthdate: Date, phoneNumber: String)

  def validName(name: String): Validation[String, String] =
    if (name != "") Success(name)
    else Failure("Name cannot be empty")

  def validBirthdate(birthdate: String): Validation[String, Date] =
    try {
      import java.text._
      Success(new SimpleDateFormat("yyyy-MM-dd").parse(birthdate))
    } catch {
      case _: Throwable => Failure("Birthdate must be in the form yyyy-MM-dd")
    }

  def validPhone(phoneNumber: String): Validation[String, String] =
    if (phoneNumber.matches("[0-9]{10}"))
      Success(phoneNumber)
    else Failure("Phone number must be 10 digits")

  def validWebForm(name: String,
                    birthdate: String,
                    phone: String): Validation[String, WebForm] =

    validationApplicative.map3(
      validName(name),
      validBirthdate(birthdate),
      validPhone(phone))(
      WebForm)


  def main(args: Array[String]): Unit = {
    println(s"Validation = ${validWebForm("", "Foo", "Bar")}")
    println(s"Validation = ${validWebForm("Name", "1981-07-02", "1234567890")}")
  }

}
