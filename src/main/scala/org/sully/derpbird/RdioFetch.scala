package org.sully.derpbird

import org.apache.http.client.RedirectStrategy
import org.apache.http.{HttpResponse, HttpRequest}
import org.apache.http.protocol.HttpContext
import org.apache.http.client.methods.{HttpGet, HttpUriRequest}
import org.apache.http.impl.client.DefaultHttpClient
import org.pircbotx.hooks.events.MessageEvent
import org.pircbotx.PircBotX
import java.net.{URLEncoder, URLDecoder}
import org.apache.http.util.EntityUtils
import scala.actors.Actor

case class RdioFetchForId(event: MessageEvent[_ <: PircBotX], id: String)

object RdioFetch extends Actor {

  class DontRedirectStrategy extends RedirectStrategy {
    def isRedirected(p1: HttpRequest, p2: HttpResponse, p3: HttpContext): Boolean = false
    def getRedirect(p1: HttpRequest, p2: HttpResponse, p3: HttpContext): HttpUriRequest = null
  }

  // Regexes to match rdio input from IRC.
  // http://rd.io/x/QB1eK37z9A/
  val rdio_match = """.*rd\.io/x/(.*)/""".r

  val httpClient = new DefaultHttpClient()
  httpClient.setRedirectStrategy(new DontRedirectStrategy)

  val urlRegex = """https://www.rdio.com/artist/(.*)/album/.*/track/(.*)/""".r

  def fetchRdioAndConvertToYoutube(event: MessageEvent[_ <: PircBotX], rdioId: String) {
    try {
      val httpGet = new HttpGet("https://www.rdio.com/x/%s/".format(rdioId))
      val response = httpClient.execute(httpGet)

      try {
        val fullUrl = response.getFirstHeader("Location").getValue
        httpGet.releaseConnection()
        fullUrl match {
          case urlRegex(artist, track) =>
            YoutubeFetch ! YoutubeFetchWithArtistTitle(event, artist, track)
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
