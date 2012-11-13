package org.kalidasya.sonar.erlang.checks;

import java.io.File;

import org.junit.Test;
import org.kalidasya.sonar.erlang.ErlangAstScanner;
import org.sonar.squid.api.SourceFile;

import com.sonar.sslr.squid.checks.CheckMessagesVerifier;

public class SpaceAfterBeforeOperatorsCheckTest {

	@Test
	public void test() {
		SpaceAfterBeforeOperatorsCheck check = new SpaceAfterBeforeOperatorsCheck();

		SourceFile file = ErlangAstScanner.scanSingleFile(new File(
				"src/test/resources/checks/spaceafteroperator.erl"), check);
		CheckMessagesVerifier.verify(file.getCheckMessages())
		.next().atLine(4).withMessage("No space after operator in column: 9.")
		.next().atLine(5).withMessage("No space after operator in column: 12.")
		.next().atLine(6).withMessage("No space after operator in column: 5.")
		.next().atLine(6).withMessage("No space after operator in column: 11.")
		.next().atLine(7).withMessage("No space after operator in column: 8.")
		.noMore();
	}
}
