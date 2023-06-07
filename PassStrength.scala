object PassStrength {
  def main(args: Array[String]): Unit = {
    println("Enter a password:")
    val password = scala.io.StdIn.readLine()

    val (strength, score) = checkPasswordStrength(password)

    strength match {
      case PasswordStrength.Weak => println(s"Weak password! Score: $score%")
      case PasswordStrength.Medium => println(s"Medium password. Score: $score%")
      case PasswordStrength.Strong => println(s"Strong password! Score: $score%")
    }
  }

  def checkPasswordStrength(password: String): (PasswordStrength.Value, Int) = {
    val lengthScore = calculateLengthScore(password)
    val characterScore = calculateCharacterScore(password)
    val score = (lengthScore + characterScore) / 2

    if (score < 40) {
      (PasswordStrength.Weak, score)
    } else if (score < 70) {
      (PasswordStrength.Medium, score)
    } else {
      (PasswordStrength.Strong, score)
    }
  }

  def calculateLengthScore(password: String): Int = {
    val minLength = 8
    val maxLength = 12
    val passLen = password.length

    if (passLen < minLength) {
      0
    } else if (passLen > maxLength) {
      100
    } else {
      val lengthRange = maxLength - minLength
      val lengthScore = ((passLen - minLength) / lengthRange.toDouble) * 100
      lengthScore.toInt
    }
  }

  def calculateCharacterScore(password: String): Int = {
    val uppercaseScore = calculateUppercaseScore(password)
    val lowercaseScore = calculateLowercaseScore(password)
    val digitScore = calculateDigitScore(password)
    val specialCharScore = calculateSpecialCharScore(password)
    val characterScore = (uppercaseScore + lowercaseScore + digitScore + specialCharScore) / 4
    characterScore.toInt
  }

  def calculateUppercaseScore(password: String): Int = {
    val uppercaseCount = password.count(_.isUpper)
    val score = (uppercaseCount.toDouble / password.length) * 100
    score.toInt
  }

  def calculateLowercaseScore(password: String): Int = {
    val lowercaseCount = password.count(_.isLower)
    val score = (lowercaseCount.toDouble / password.length) * 100
    score.toInt
  }

  def calculateDigitScore(password: String): Int = {
    val digitCount = password.count(_.isDigit)
    val score = (digitCount.toDouble / password.length) * 100
    score.toInt
  }

  def calculateSpecialCharScore(password: String): Int = {
    val specialCharCount = password.count(!_.isLetterOrDigit)
    val score = (specialCharCount.toDouble / password.length) * 100
    score.toInt
  }

  object PasswordStrength extends Enumeration {
    val Weak, Medium, Strong = Value
  }
}
