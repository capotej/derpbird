package org.sully.derpbird

import scala.actors.Actor
import org.pircbotx.hooks.events.MessageEvent
import org.pircbotx.PircBotX
import org.apache.http.impl.client.DefaultHttpClient
import org.apache.http.client.methods.HttpGet
import org.apache.http.util.EntityUtils

case class SpotifyFetchForId(event: MessageEvent[_ <: PircBotX], id: String)

object SpotifyFetch extends Actor {

  val spotify_match = """http://open\.spotify\.com/track/(.*)/?""".r
  val httpClient = new DefaultHttpClient()

  def findTrackFromId(event: MessageEvent[_ <: PircBotX], id: String) {
    val httpGet = new HttpGet("http://open.spotify.com/track/%s".format(id))
    val response = httpClient.execute(httpGet)

    val titleRegex = """<title>(.*)</title>""".r

    try {
      val body = EntityUtils.toByteArray(response.getEntity)
      val html = new String(body)
      httpGet.releaseConnection()

      titleRegex.findFirstMatchIn(html) match {
        case Some(foundMatch) if foundMatch.subgroups.nonEmpty =>
          val title = foundMatch.subgroups.head
          val cleanedTitle = title.replaceAll("on Spotify", "").split(" by ")
          YoutubeFetch ! YoutubeFetchWithArtistTitle(event, cleanedTitle.head, cleanedTitle.last)
      }
    } finally {
      httpGet.releaseConnection()
    }
  }

  def act() {
    loop {
      react {
        case SpotifyFetchForId(event, id) => findTrackFromId(event, id)
        case _ => null
      }
    }
  }
}
