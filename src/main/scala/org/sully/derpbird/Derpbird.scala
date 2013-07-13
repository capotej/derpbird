package org.sully.derpbird

import java.io.IOException

import actors.Actor
import collection.mutable.HashMap

import twitter4j.{Status, TwitterException, TwitterFactory}

import org.pircbotx.{MultiBotManager, PircBotX, UtilSSLSocketFactory}
import org.pircbotx.exception.{IrcException, NickAlreadyInUseException}
import org.pircbotx.hooks.{Listener, ListenerAdapter}
import org.pircbotx.hooks.events._
import org.pircbotx.hooks.managers.ThreadedListenerManager
import grizzled.config.Configuration
import io.Source
import org.apache.http.impl.client.DefaultHttpClient
import org.apache.http.client.methods.{HttpUriRequest, HttpGet}
import org.apache.http.util.EntityUtils
import org.apache.http.params.BasicHttpParams
import org.apache.http.client.params.HttpClientParams
import org.apache.http.client.{RedirectStrategy, RedirectHandler}
import org.apache.http.protocol.HttpContext
import org.apache.http.{HttpRequest, HttpResponse}
import java.net.{URLDecoder, URLEncoder}

class Derpbird[T <: PircBotX] extends ListenerAdapter[T] with Listener[T] {

  var configs = new HashMap[String, Configuration]

  // Regexes to match twitter input from IRC.
  val sup_match = """^sup\s+(\S+).*""".r
  val ext_match = """.*twitter\.com/.+?/status?e?s/(\d+)/?$""".r

  // Regexes to match rdio input from IRC.
  // http://rd.io/x/QB1eK37z9A/
  val rdio_match = """.*rd\.io/x/(.*)/""".r


  // Keep the config around so we can use it to rejoin channels on reconnect.
  def addConfig(server: String, config: Configuration) {
    configs += server -> config
  }

  def joinChannels(event: ConnectEvent[T]) {

    val server = event.getBot.getServer
    val config = configs.get((server)).get

    config.getAsList(server, "channels") match {

      case Some(names : List[String]) => {

        for (channelName <- names) {

          val channel  = config.get(channelName.trim, "channel").get
          val password = config.getOrElse(channelName.trim, "password", "")

          println("Joining channel: " + channel + " on: " + server + ":" + event.getBot.getPort)
          event.getBot.joinChannel(channel, password)
        }
      }

      case _ => {
        println("No channels found in config!")
      }
    }
  }

  override def onConnect(event: ConnectEvent[T]) {
    println("Connected to: " + event.getBot.getServer + ":" + event.getBot.getPort)
    joinChannels(event)
  }

  // Re-connect to the server if we've been dropped.
  override def onDisconnect(event: DisconnectEvent[T]) {
    println("Disconnected from: " + event.getBot.getServer + ":" + event.getBot.getPort + ", attempting to reconnect in 5 seconds!")

    var attempts = 0

    Thread.sleep(5 * 1000)

    while (!event.getBot.isConnected && attempts < 15) {
      try {
        attempts += 1
        event.getBot.reconnect()
      } catch {
        case e: Exception => {
          Thread.sleep(10 * 1000)
        }
      }
    }
  }

  override def onKick(event: KickEvent[T]) {

    if (event.getRecipient.getNick == event.getBot.getNick) {
      println("Kicked from channel: " + event.getChannel.getName + ", sleeping for 5 seconds before rejoining..")
      Thread.sleep(5 * 1000)
      event.getBot.joinChannel(event.getChannel.getName)
    }
  }

  override def onMessage(event: MessageEvent[T]) {

    try {

      // Strip trailing white space before matching.
      event.getMessage.trim match {
        case sup_match(username) => TwitterFetch ! TwitterMessageFetchForUser(event, username)
        case ext_match(tweetId)  => TwitterFetch ! TwitterMessageFetchForId(event, tweetId)
        case rdio_match(rdioId)  => RdioFetch ! RdioFetchForId(event, rdioId)
        case _ => null
      }

    } catch {
      case e: Exception => {
        e.printStackTrace()
      }
    }
  }
}

case class RdioFetchForId(event: MessageEvent[_ <: PircBotX], id: String)

object RdioFetch extends Actor {

  class DontRedirectStrategy extends RedirectStrategy {
    def isRedirected(p1: HttpRequest, p2: HttpResponse, p3: HttpContext): Boolean = false
    def getRedirect(p1: HttpRequest, p2: HttpResponse, p3: HttpContext): HttpUriRequest = null
  }

  val httpClient = new DefaultHttpClient()
  httpClient.setRedirectStrategy(new DontRedirectStrategy)

  val urlRegex = """https://www.rdio.com/artist/(.*)/album/.*/track/(.*)/""".r

  def findOnYoutube(event: MessageEvent[_ <: PircBotX], artist: String, track: String) {
    //decode and clean the rdio track url
    val cleanedTitle = URLDecoder.decode("%s %s".format(artist, track), "UTF-8").replace('_', ' ')
    val encodedQuery = URLEncoder.encode(cleanedTitle, "UTF-8")

    val httpGet = new HttpGet("https://gdata.youtube.com/feeds/api/videos?q=%s&max-results=1&alt=json".format(encodedQuery))
    val response = httpClient.execute(httpGet)

    try {
      val body = EntityUtils.toByteArray(response.getEntity)
      val json = new String(body)
      httpGet.releaseConnection()

      //parse the response with json, rofl
      val roflParse = """media\$player":\[\{"url":"(.*)"}],""".r

      roflParse.findFirstMatchIn(json) match {
        case Some(foundMatch) if foundMatch.subgroups.nonEmpty =>
          val url = foundMatch.subgroups.head
          //unsure why the regex captures more than needed, doing this to fix
          val trailingPart = """"}],"""
          val splitUrl = url.split(trailingPart).head

          event.getBot.sendMessage(event.getChannel, splitUrl)
      }

    } finally {
      httpGet.releaseConnection()
    }
  }

  def fetchRdioAndConvertToYoutube(event: MessageEvent[_ <: PircBotX], rdioId: String) {
    try {
      val httpGet = new HttpGet("https://www.rdio.com/x/%s/".format(rdioId))
      val response = httpClient.execute(httpGet)

      try {
        val fullUrl = response.getFirstHeader("Location").getValue
        httpGet.releaseConnection()
        fullUrl match {
          case urlRegex(artist, track) => findOnYoutube(event, artist, track)
          case _ => println("Couldnt extract track from Rdio link")
        }

        // do something useful with the response body
        // and ensure it is fully consumed
        EntityUtils.consume(response.getEntity)
      } finally {
        httpGet.releaseConnection()
      }
    } catch {
      case e: Exception => {
        println("Failed get Rdio Link: " + e.getMessage)
      }
    }
  }

  def act() {
    loop {
      react {
        case RdioFetchForId(event, rdioId) => fetchRdioAndConvertToYoutube(event, rdioId)
        case _ => null
      }
    }
  }
}

// Actor matching.
case class TwitterMessageFetchForUser(event: MessageEvent[_ <: PircBotX], username: String)
case class TwitterMessageFetchForId(event: MessageEvent[_ <: PircBotX], id: String)

object TwitterFetch extends Actor {

  val twitter = new TwitterFactory().getInstance

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

object Main {

  def main(args: Array[String]) {

    if (args.length == 0) {
      println("Usage: <derpbird> config.ini")
      System.exit(1)
    }

    println("Derpbird starting up..")

    // Fire up the Actor that will send requests to Twitter.
    TwitterFetch.start()
    RdioFetch.start()

    // Allow # and . in section names. Allow ; in comments.
    val validSection   = """([a-zA-Z0-9_\.\/#]+)""".r
    val commentSection = """^\s*([;#].*)$""".r
    val emptyMap       = Map.empty[String, Map[String, String]]

    val config   = new Configuration(emptyMap, validSection, commentSection).load(Source.fromFile(args(0)))
    val multi    = new MultiBotManager("Derpbird")
    val manager  = new ThreadedListenerManager
    val listener = new Derpbird

    // Associate our Derpbird class with the PircBotX manager.
    manager.addListener(listener)

    multi.setEncoding("UTF-8")
    multi.setListenerManager(manager)

    println("Processing config.")

    config.getAsList("global", "servers") match {

      case Some(servers : List[String]) => {

        for (server <- servers) {
          println("Adding config for: " + server)

          val host     = config.get(server, "host")
          val port     = config.getIntOrElse(server, "port", 6667)
          val pass     = config.getOrElse(server, "password", "")
          val socket   = if (config.getBooleanOrElse(server, "ssl", false)) new UtilSSLSocketFactory else null

          val bot      = multi.createBot(host.get, port, pass, socket)

          bot.setName(config.getOrElse(server, "nick", "derpbird"))
          bot.setLogin(config.getOrElse(server, "nick", "derpbird"))
          bot.setFinger(config.getOrElse(server, "finger", "herp derp"))
          bot.setVersion(config.getOrElse(server, "version", "Derpbird"))
          bot.setVerbose(config.getBooleanOrElse(server, "verbose", true))

          // My identification? You don't need to see my identification.
          bot.identify(config.getOrElse(server, "nickpass", ""))

          listener.addConfig(server, config)
        }
      }

      case _ => {
        println("Server list is empty!")
      }
    }

    println("Connecting to IRC servers..")

    try {
      multi.connectAll()
    } catch {
      case e: (IOException, IrcException, NickAlreadyInUseException) => {
        println("Couldn't connect to all servers: " + e.toString)
      }
    }
  }
}
