package org.kalidasya.sonar.erlang.checks;

import java.io.File;

import org.junit.Test;
import org.kalidasya.sonar.erlang.ErlangAstScanner;
import org.sonar.squid.api.SourceFile;

import com.sonar.sslr.squid.checks.CheckMessagesVerifier;

public class MultipleBlankLinesCheckTest {

	@Test
	public void test() {
		MultipleBlankLinesCheck check = new MultipleBlankLinesCheck();

		SourceFile file = ErlangAstScanner.scanSingleFile(new File(
				"src/test/resources/checks/multipleblankline.erl"), check);
		CheckMessagesVerifier.verify(file.getCheckMessages())
		.next().atLine(5).withMessage("Too many blank lines found, the threshold is 2.")
		.next().atLine(12).withMessage("Too many blank lines found, the threshold is 1.")
		.next().atLine(21).withMessage("Too many blank lines found, the threshold is 1.")
		.next().atLine(26).withMessage("Too many blank lines found, the threshold is 1.")
		.noMore();
	}
}
