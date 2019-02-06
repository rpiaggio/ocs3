// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.keywords

import cats.tests.CatsSuite
import scala.xml.XML

@SuppressWarnings(Array("org.wartremover.warts.Throw"))
final class GdsClientSpec extends CatsSuite {
  test("GDSClient should reject bad responses") {
    val xml = XML.load(getClass.getResource("/gds-bad-resp.xml"))
    GdsClient
      .parseError(xml)
      .isLeft shouldEqual true
  }
}
