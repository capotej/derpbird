package org.sully.derpbird

import twitter4j.{Status, TwitterFactory}
import org.pircbotx.hooks.events.MessageEvent
import org.pircbotx.PircBotX
import scala.actors.Actor

case class TwitterMessageFetchForUser(event: MessageEvent[_ <: PircBotX], username: String)
case class TwitterMessageFetchForId(event: MessageEvent[_ <: PircBotX], id: String)

object TwitterFetch extends Actor {
  val twitter = new TwitterFactory().getInstance

  // Regexes to match twitter input from IRC.
  val sup_match = """^sup\s+(\S+).*""".r
  val ext_match = """.*twitter\.com/.+?/status?e?s/(\d+)/?$""".r

  def formatStatus(status: Status) = {
    "<" + status.getUser.getScreenName + "> " + status.getText.replaceAll("[\\r\\n]", " ")
  }

  def fetchTimeLineForUser(event: MessageEvent[_ <: PircBotX], username: String) {
    try {
      event.getBot.sendMessage(event.getChannel, formatStatus(twitter.getUserTimeline(username).get(0)))

    } catch {
      case e: Exception => {
        println("Failed to get timeline: " + e.getMessage)
      }
    }
  }

  def fetchTweet(event: MessageEvent[_ <: PircBotX], id: String) {
    try {
      event.getBot.sendMessage(event.getChannel, formatStatus(twitter.showStatus(id.toLong)))

    } catch {
      case e: Exception => {
        println("Failed to get timeline: " + e.getMessage)
      }
    }
  }

  def act() {
    twitter.verifyCredentials

    loop {
      react {
        case TwitterMessageFetchForUser(event, username) => fetchTimeLineForUser(event, username)
        case TwitterMessageFetchForId(event, tweetId) => fetchTweet(event, tweetId)
        case _ => null
      }
    }
  }
}
