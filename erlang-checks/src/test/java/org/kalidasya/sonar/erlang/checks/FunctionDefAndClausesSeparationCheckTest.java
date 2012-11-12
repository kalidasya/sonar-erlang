package org.kalidasya.sonar.erlang.checks;

import java.io.File;

import org.junit.Test;
import org.kalidasya.sonar.erlang.ErlangAstScanner;
import org.sonar.squid.api.SourceFile;

import com.sonar.sslr.squid.checks.CheckMessagesVerifier;

public class FunctionDefAndClausesSeparationCheckTest {

	@Test
	public void test() {
		FunctionDefAndClausesSeparationCheck check = new FunctionDefAndClausesSeparationCheck();

		SourceFile file = ErlangAstScanner.scanSingleFile(new File(
				"src/test/resources/checks/functionseparation.erl"), check);
		CheckMessagesVerifier.verify(file.getCheckMessages())
		.next().atLine(27).withMessage("The line has 0 precending blank line, the thresold is: 1.")
		.next().atLine(54).withMessage("The line has 1 precending blank line, the thresold is: 0.")
		.next().atLine(58).withMessage("The line has 2 precending blank line, the thresold is: 1.")
		.noMore();
	}
}
