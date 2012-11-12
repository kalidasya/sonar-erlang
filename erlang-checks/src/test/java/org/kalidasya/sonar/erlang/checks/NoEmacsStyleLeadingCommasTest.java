package org.kalidasya.sonar.erlang.checks;

import java.io.File;

import org.junit.Test;
import org.kalidasya.sonar.erlang.ErlangAstScanner;
import org.sonar.squid.api.SourceFile;

import com.sonar.sslr.squid.checks.CheckMessagesVerifier;

public class NoEmacsStyleLeadingCommasTest {

	@Test
	public void test() {
		NoEmacsStyleLeadingCommasCheck check = new NoEmacsStyleLeadingCommasCheck();

		SourceFile file = ErlangAstScanner.scanSingleFile(new File(
				"src/test/resources/checks/noemacsstyle.erl"), check);
		CheckMessagesVerifier.verify(file.getCheckMessages())
		.next().atLine(8).withMessage("No Emacs-style leading commas.")
		.next().atLine(9)
		.next().atLine(17)
		.noMore();
	}
}
