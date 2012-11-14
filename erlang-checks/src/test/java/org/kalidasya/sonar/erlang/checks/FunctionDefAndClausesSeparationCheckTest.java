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
		.next().atLine(8).withMessage("The line has 0 precending blank line and it should be: 1.")
		.next().atLine(18).withMessage("The line has 1 precending blank line and it should be: 0.")
		.next().atLine(22).withMessage("The line has 2 precending blank line and it should be: 1.")
		.noMore();
	}
}
