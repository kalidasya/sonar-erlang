package org.kalidasya.sonar.erlang.checks;

import java.io.File;

import org.junit.Test;
import org.kalidasya.sonar.erlang.ErlangAstScanner;
import org.sonar.squid.api.SourceFile;

import com.sonar.sslr.squid.checks.CheckMessagesVerifier;

public class LineLengthCheckTest {

	@Test
	public void test() {
		LineLengthCheck check = new LineLengthCheck();

		SourceFile file = ErlangAstScanner.scanSingleFile(new File(
				"src/test/resources/checks/linelength.erl"), check);
		CheckMessagesVerifier.verify(file.getCheckMessages())
		.next().atLine(5)
		.next().atLine(7).withMessage("The line length is greater than 100 authorized.")
		.next().atLine(8)
		.noMore();
	}
}
