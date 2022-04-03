package am.scalalearn

import com.amazonaws.thirdparty.joda.time.DateTime
import com.amazonaws.thirdparty.joda.time.format.DateTimeFormat.forPattern


class ProgramArgumentParser(var args: Array[String]) {
  val DateFormat = "yyyy-MM-dd"
  val KeyRegex = "--.*";
  val params = args.foldLeft(Map[String, String]()) {
    (params, arg) =>
      val key :: value :: _ = arg.split("=").toList
      if (key.matches(KeyRegex)) {
        params + (key.replace("--", "") -> value)
      } else {
        params
      }
  }

  def get(param: String, default: String) = params.getOrElse(param, default);
  def parseInt(param: String, default: Int) = params.getOrElse(param, default+"").toInt;
  def parseBoolean(param: String, default: Boolean) = params.getOrElse(param, default+"").toBoolean;
  def parseDouble(param: String, default:Double) = params.getOrElse(param, default+"").toDouble;
  def parseDate(param: String, default: DateTime, format: String = DateFormat) = {
    val value = params.getOrElse(param, null)
    if(value != null) {
      forPattern(format).parseDateTime(value).withTimeAtStartOfDay()
    } else default
  }
}
