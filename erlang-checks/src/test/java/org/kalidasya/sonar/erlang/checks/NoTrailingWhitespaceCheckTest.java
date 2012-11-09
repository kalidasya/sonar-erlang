package org.kalidasya.sonar.erlang.checks;

import java.io.File;

import org.junit.Test;
import org.kalidasya.sonar.erlang.ErlangAstScanner;
import org.sonar.squid.api.SourceFile;

import com.sonar.sslr.squid.checks.CheckMessagesVerifier;

public class NoTrailingWhitespaceCheckTest {

	@Test
	public void test() {
		NoTrailingWhitespace check = new NoTrailingWhitespace();

		SourceFile file = ErlangAstScanner.scanSingleFile(new File(
				"src/test/resources/checks/notrailingwhitespace.erl"), check);
		CheckMessagesVerifier.verify(file.getCheckMessages())
		.next().atLine(2).withMessage("No trailing white space.")
		.next().atLine(6).withMessage("No trailing white space.")
		.next().atLine(7).withMessage("No trailing white space.")
		.noMore();
	}
}
