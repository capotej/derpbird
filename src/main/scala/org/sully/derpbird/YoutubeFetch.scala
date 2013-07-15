package org.sully.derpbird

import scala.actors.Actor
import org.pircbotx.hooks.events.MessageEvent
import org.pircbotx.PircBotX
import java.net.{URLEncoder, URLDecoder}
import org.apache.http.client.methods.HttpGet
import org.apache.http.util.EntityUtils
import org.apache.http.impl.client.DefaultHttpClient

case class YoutubeFetchWithArtistTitle(event: MessageEvent[_ <: PircBotX], artist: String, track: String)
case class YoutubeFetchWithQuery(event: MessageEvent[_ <: PircBotX], query: String)

object YoutubeFetch extends Actor {
  val httpClient = new DefaultHttpClient()
  val youtube_match = """find (.*)""".r

  def findOnYoutube(event: MessageEvent[_ <: PircBotX], artist: String, title: String) {
    findOnYoutube(event, artist + " " + title)
  }

  def findOnYoutube(event: MessageEvent[_ <: PircBotX], query: String) {
    //decode and clean the rdio track url
    val cleanedTitle = URLDecoder.decode(query, "UTF-8").replace('_', ' ')
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

  def act() {
    loop {
      react {
        case YoutubeFetchWithArtistTitle(event, artist, title) => findOnYoutube(event, artist, title)
        case YoutubeFetchWithQuery(event, query) => findOnYoutube(event, query)
        case _ => null
      }
    }
  }

}
