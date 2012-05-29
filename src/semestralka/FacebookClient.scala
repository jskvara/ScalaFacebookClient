package semestralka

import java.text.SimpleDateFormat
import java.util.Date
import net.liftweb.json._

case class FacebookException(message: String) extends Exception

object DateUtil {
  val format = new SimpleDateFormat("dd.MM.yyyy HH:mm:ss")

  def dateToString(d:Date):String = {
	format.format(d)
  }
}

case class User(id: String, name: String) {
  def toStr:String = {
	return name
  }
}
case class Users(data: List[User])
case class Event(id: String, name: String, rsvp_status: String,
				 location: Option[String], start_time: Date, end_time: Date) {
  def toStr = {
	val ret = new StringBuilder
	ret.append("[")
	ret.append(DateUtil.dateToString(start_time))
	ret.append("-")
	ret.append(DateUtil.dateToString(end_time))
	ret.append("] ")
	ret.append(name)
	ret.append(": ")
	if (location.isDefined) {
	  ret.append(location.get)
	  ret.append(", ")
	}
	ret.append(rsvp_status)

	ret.toString
  }
}
case class Events(data: List[Event])
case class Likes(count: Int, data: Option[Users])
case class Comment(id: Option[String], from: Option[User], message: Option[String],
				   created_time: Option[Date], likes: Option[Int]) {
  def this() = this(None, None, None, None, None)
}
case class CommentsData(data: List[Comment])
case class Comments(count: Int/*, data: Option[CommentsData]*/)
case class Place(id: String, name: String)
case class Status(id: String, from: User, `type`: String,
				  created_time: Date, updated_time: Date, likes: Option[Likes],
				  comments: Option[Comments], message: Option[String], story: Option[String],
				  picture: Option[String], link: Option[String], name: Option[String],
				  caption: Option[String], description: Option[String], place: Option[Place]/*,
to: Option[Users]*/) {
  def getMessage:String = {
	if (this.message.isDefined) {
	  return this.message.get
	}

	if (this.story.isDefined) {
	  return this.story.get
	}

	if (this.name.isDefined) {
	  val ret = this.name.get

	  if (this.description.isDefined) {
		ret.addString(new StringBuilder(" - ").append(this.description.get))
	  }
	  return ret
	}

	return ""
  }
  def toStr():String = {
	val ret = new StringBuilder()
	ret.append(DateUtil.dateToString(updated_time))
	ret.append(" - ")
	ret.append(from.name)
	ret.append(": ")
	ret.append(getMessage)
	if (place.isDefined) {
	  ret.append(", at: ")
	  ret.append(place)
	}

	ret.toString
  }
}
case class Feed(data: List[Status])


class FacebookClient(val accessToken:String) {

  implicit val formats = new DefaultFormats {
	override def dateFormatter = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss")
  }

  def fetchObject[T](url:String)(implicit mf: Manifest[T]):T = {
	val content = FacebookClient.fetchObject(accessToken, url)

	val json = parse(content)
	val obj = json.extract[T]

	return obj
  }

  def fetchMe():User = {
	val content = FacebookClient.fetchObject(accessToken, "me")

	val json = parse(content)
	val user = json.extract[User]

	return user
  }

  def fetchFriends():Users = {
	val content = FacebookClient.fetchObject(accessToken, "me/friends")

	val json = parse(content)
	val friends = json.extract[Users]

	friends
  }

  def fetchEvents(params:Map[String, Any] = Map.empty):Events = {
	val content = FacebookClient.fetchObject(accessToken, "me/events", params)

	val json = parse(content)
	val events = json.extract[Events]

	events
  }

  def fetchFeed(params:Map[String, Any] = Map.empty):Feed = {
	val content = FacebookClient.fetchObject(accessToken, "me/home", params)

	val json = parse(content)
	val feed = json.extract[Feed]

	feed
  }
}

object FacebookClient {
  import java.net.{URL,HttpURLConnection}
  import java.net.URLEncoder.encode
  import java.io.{OutputStreamWriter,IOException}
  import scala.io.Source.fromInputStream

  val PERMISSIONS = "read_stream,user_events";
  val FB_URL = "https://graph.facebook.com/%s?access_token=%s"

  def fetchObject(accessToken:String, url:String, params:Map[String, Any] = Map.empty):String = {
	val u = FB_URL.format(url, accessToken) + buildParams(params)
	val content = get(u)

	content
  }

  def buildParams(params:Map[String, Any]):String = {
	if (params.isEmpty) {
	  return ""
	}

	val p = new StringBuilder
	params.foreach {
	  case (key, value) =>
		p.append("&")
		p.append(key)
		p.append("=")
		p.append(value.toString)
	}

	return p.toString
  }

  def get(url:String):String = {
	val u = new URL(url)
	val conn = u.openConnection.asInstanceOf[HttpURLConnection]
	conn.connect
	try {
	  fromInputStream(conn.getInputStream).getLines.mkString("\n")
	} catch {
	  case ioe:IOException => {
		  val error = fromInputStream(conn.getErrorStream).getLines.mkString("\n")

		  throw new FacebookException("Error: " + error + " in URL: " + url)
		}
	  case e:Exception => throw new FacebookException("Error: " + e)
	}
  }

  def post(url:String, data:Map[String, String]):String = {
	val u = new URL(url)
	val conn = u.openConnection.asInstanceOf[HttpURLConnection]
	conn.setRequestMethod("POST")
	conn.setDoOutput(true)
	conn.connect
	val wr = new OutputStreamWriter(conn.getOutputStream())
	wr.write(encodePostData(data))
	wr.flush
	wr.close

	try {
	  fromInputStream(conn.getInputStream).getLines.mkString("\n")
	} catch {
	  case ioe:IOException => {
		  val error = fromInputStream(conn.getErrorStream).getLines.mkString("\n")

		  throw new FacebookException("Error: " + error + " in URL: " + url)
		}
	  case e:Exception => throw new FacebookException("Error: " + e)
	}
  }

  private def encodePostData(data: Map[String, String]) = {
	(for ((name, value) <- data)
	  yield encode(name, "UTF-8") + "=" + encode(value, "UTF-8"))
	    .mkString("&")
  }
}