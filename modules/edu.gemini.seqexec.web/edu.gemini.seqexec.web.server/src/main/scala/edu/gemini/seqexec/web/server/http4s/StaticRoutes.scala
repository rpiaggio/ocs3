package edu.gemini.seqexec.web.server.http4s

import java.io.File

import org.http4s.MediaType._
import org.http4s.dsl._
import org.http4s.headers.`Content-Type`
import org.http4s.server.middleware.GZip
import org.http4s._

import scalaz.concurrent.Task

class StaticRoutes(devMode: Boolean) {
  val index = {
    val style = """
                  |   @media screen and (-webkit-min-device-pixel-ratio:0) {
                  |        select,
                  |        textarea,
                  |        input {
                  |          font-size: 16px !important;
                  |        }
                  |      }
                  |"""
    val deps = if (devMode) "edu_gemini_seqexec_web_client-jsdeps.js" else "edu_gemini_seqexec_web_client-jsdeps.min.js"
    val seqexecScript = if (devMode) "seqexec.js" else "seqexec-opt.js"
    val xml = <html lang="en">
      <head>
        <meta charset="utf-8"/>
        <meta http-equiv="X-UA-Compatible" content="IE=edge"/>
        <meta name="viewport" content="width=device-width, initial-scale=1"/>
        <!-- The above 3 meta tags *must* come first in the head; any other head content must come *after* these tags -->
        <meta name="description" content=""/>
        <meta name="author" content=""/>
        <link rel="icon" href="images/launcher.ico"/>

        <!-- Add to homescreen for Safari on iOS -->
        <meta name="apple-mobile-web-app-capable" content="yes"/>
        <meta name="apple-mobile-web-app-status-bar-style" content="black"/>
        <meta name="apple-mobile-web-app-title" content="Seqexec"/>
        <link rel="apple-touch-icon-precomposed" href="images/launcher.png"/>

        <title>Seqexec</title>

        <link rel="stylesheet" href="css/semantic.css"/>
        <style>{style.stripMargin}</style>
      </head>

      <body>

        <div id="content">
        </div>

        <script src={deps}></script>
        <script src={seqexecScript}></script>
        <script type="text/javascript">
          edu.gemini.seqexec.web.client.SeqexecApp().main();
        </script>
      </body>
    </html>
    s"<!DOCTYPE html>$xml"
  }

  // Get a resource from a local file, useful for development
  def localResource(base: String, path: String, req: Request):Option[Response] = StaticFile.fromFile(new File(base, path), Some(req))
  def localResource(path: String, req: Request):Option[Response] = StaticFile.fromResource(path, Some(req))
  // Get a resource from a local file, used in production
  def embeddedResource(path: String, req: Request):Option[Response] = {
    val url = Option(getClass.getResource(path))
    url.flatMap(StaticFile.fromURL(_, Some(req)))
  }

  implicit class ReqOps(req: Request) {
    def endsWith(exts: String*): Boolean = exts.exists(req.pathInfo.endsWith)

    def serve(path: String = req.pathInfo): Task[Response] = {
      // To find scala.js generated files we need to go into the dir below, hopefully this can be improved
      localResource(path, req).orElse(embeddedResource(path, req))
        .map(Task.now)
        .getOrElse(NotFound())
    }
  }

  val supportedExtension = List(".html", ".js", ".map", ".css", ".png", ".woff", ".woff2", ".ttf", ".mp3")

  val service = GZip { HttpService {
    case req if req.pathInfo == "/"                  => Ok(index).withContentType(Some(`Content-Type`(`text/html`, Charset.`UTF-8`)))
    case req if req.pathInfo == "/cli" && devMode    => req.serve("/cli-dev.html")
    case req if req.pathInfo == "/cli"               => req.serve("/cli.html")
    case req if req.endsWith(supportedExtension: _*) => req.serve()
  }}
}
