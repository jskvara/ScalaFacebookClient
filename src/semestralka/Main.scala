package semestralka

import java.util.Date

object Main {
  def main(args: Array[String]): Unit = {
	println("Paste your access token from: https://developers.facebook.com/tools/explorer/ (Permissions: read_stream,user_events)")
	val accessToken = readLine()
	if (accessToken == null) {
	  return
	}

	val fc = new FacebookClient(accessToken)

	try {
	  val user = fc.fetchMe
	  println("User: " + user.name)

	  println("Friends")
	  val friends = fc.fetchFriends
	  friends.data.sortBy(_.name).foreach(
		f => println(f.toStr)
	  )

	  println("Events")
	  val now = new Date().getTime / 1000
	  val events = fc.fetchEvents(Map("until" -> (now + 7 * 24 * 3600)))
	  events.data.foreach(
		e => println(e.toStr)
	  )

	  println("Feed")
	  val feed = fc.fetchFeed()
	  feed.data.foreach(
		s => {
//		  println(s)
		  println(s.toStr)
		}
	  )

	  val mutual:scala.collection.mutable.Map[String, Int] =
		scala.collection.mutable.Map.empty
	  println("Mutual Friends - sorted (takes more time)")
	  friends.data.foreach(
		f => {
		  val mf = fc.fetchObject[MutualFriends]("me/mutualfriends/" + f.id)
		  mutual.put(f.name, mf.data.size)
//		  println(f.name + ": " + mf.data.size)
		}
	  )
	  mutual.toList.sortBy(-_._2).foreach(
		mf => println(mf._1 + ": " + mf._2)
	  )

	} catch {
	  case fe:FacebookException => println(fe.message)
	  case e:Exception => println("Error: " + e)
	}
  }
}

case class MutualFriends(data: List[User])
