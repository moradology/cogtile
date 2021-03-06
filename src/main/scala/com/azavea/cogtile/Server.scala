package com.azavea.cogtile

import cats.effect._
import fs2._
import fs2.StreamApp.ExitCode
import org.http4s._
import org.http4s.dsl.Http4sDsl
import org.http4s.server.blaze.BlazeBuilder
import org.http4s.server.HttpMiddleware
import org.http4s.server.middleware.{GZip, CORS, CORSConfig}
import geotrellis.raster._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import java.net.URI

object Server extends StreamApp[IO] with Http4sDsl[IO] {
  private val corsConfig = CORSConfig(
    anyOrigin = true,
    anyMethod = false,
    allowedMethods = Some(Set("GET")),
    allowCredentials = true,
    maxAge = 1.day.toSeconds
  )

  def middleware: HttpMiddleware[IO] = { (routes: HttpService[IO]) =>
    GZip(routes)
  }.compose { (routes: HttpService[IO]) =>
    CORS(routes)
  }

  object UriQueryParamMatcher extends QueryParamDecoderMatcher[URI]("uri")

  implicit val uriQueryParamDecoder: QueryParamDecoder[URI] =
    QueryParamDecoder[String].map(URI.create)

  val emptyTile = IntConstantNoDataArrayTile(Array(0), 1, 1).renderPng()

  val service: HttpService[IO] = HttpService[IO] {
    case GET -> Root =>
      Ok("testing - root")
    case GET -> Root / IntVar(zoom) / IntVar(x) / IntVar(y) :? UriQueryParamMatcher(uri) =>
      Ok(CogView.fetchCroppedTile(uri, zoom, x, y) match {
        case Some(png) => png.bytes
        case None => emptyTile.bytes
      })
  }

  def stream(args: List[String], requestShutdown: IO[Unit]): Stream[IO, ExitCode] =
    BlazeBuilder[IO]
      .enableHttp2(true)
      .bindHttp(8080, "0.0.0.0")
      .mountService(middleware(service))
      .serve
}
