package org.kalidasya.sonar.erlang.checks;

import java.io.File;

import org.junit.Test;
import org.kalidasya.sonar.erlang.ErlangAstScanner;
import org.sonar.squid.api.SourceFile;

import com.sonar.sslr.squid.checks.CheckMessagesVerifier;

public class NoSpaceAfterBeforeBracketsCheckTest {

	@Test
	public void test() {
		NoSpaceAfterBeforeBracketsCheck check = new NoSpaceAfterBeforeBracketsCheck();

		SourceFile file = ErlangAstScanner.scanSingleFile(new File(
				"src/test/resources/checks/nospacearoundbrackets.erl"), check);
		CheckMessagesVerifier.verify(file.getCheckMessages())
		.next().atLine(4).withMessage("Space after bracket in column: 9.")
		.next().atLine(5).withMessage("Space after bracket in column: 14.")
		.next().atLine(6).withMessage("Space after bracket in column: 9.")
		.next().atLine(7).withMessage("Space after bracket in column: 14.")
		.next().atLine(8).withMessage("Space after bracket in column: 9.")
		.next().atLine(9).withMessage("Space after bracket in column: 14.")
		.next().atLine(12).withMessage("Space after bracket in column: 15.")
		.next().atLine(13).withMessage("Space after bracket in column: 25.")
		.noMore();
	}
}
