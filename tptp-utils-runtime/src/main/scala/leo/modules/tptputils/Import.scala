package leo.modules.tptputils

import leo.datastructures.TPTP
import leo.modules.tptputils

import java.io.{BufferedInputStream, InputStream}

object Import {
  final def apply(file: io.Source, from: ExternalLanguage): TPTP.Problem = {
    from match {
      case tptputils.LegalRuleML => LegalRuleMLImport(file)
    }
  }

  final object LegalRuleMLImport {
    import scala.xml

    def apply(file: io.Source): TPTP.Problem = {
      val input = xml.XML.load(new SourceInputStream(file))
      println(input.toString())
      ???
    }
  }

}
