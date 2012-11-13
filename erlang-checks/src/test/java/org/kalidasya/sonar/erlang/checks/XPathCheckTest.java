package org.kalidasya.sonar.erlang.checks;

import java.io.File;

import org.junit.Test;
import org.kalidasya.sonar.erlang.ErlangAstScanner;
import org.sonar.squid.api.SourceFile;

import com.sonar.sslr.squid.checks.CheckMessagesVerifier;

public class XPathCheckTest {

  @Test
  public void check() {
    XPathCheck check = new XPathCheck();
    check.xpathQuery = "//IDENTIFIER[@tokenValue = 'really_retain']";
    check.message = "Do not use name: really_retain";

    SourceFile file = ErlangAstScanner.scanSingleFile(new File("src/test/resources/checks/complexity.erl"), check);
    CheckMessagesVerifier.verify(file.getCheckMessages())
        .next().atLine(18).withMessage("Do not use name: really_retain")
        .noMore();
  }

}
