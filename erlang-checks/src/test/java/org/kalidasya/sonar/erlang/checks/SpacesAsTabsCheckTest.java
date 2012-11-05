package org.kalidasya.sonar.erlang.checks;

import java.io.File;

import org.junit.Test;
import org.kalidasya.sonar.erlang.ErlangAstScanner;
import org.sonar.squid.api.SourceFile;

import com.sonar.sslr.squid.checks.CheckMessagesVerifier;

public class SpacesAsTabsCheckTest {

	@Test
	public void test() {
		SpacesAsTabsCheck check = new SpacesAsTabsCheck();

		SourceFile file = ErlangAstScanner.scanSingleFile(new File(
				"src/test/resources/checks/spacesastabs.erl"), check);
		CheckMessagesVerifier.verify(file.getCheckMessages())
		.next().atLine(5)
		.next().atLine(9)
		.next().atLine(10)
		.next().atLine(11).withMessage("The line starts with 5 characters which is cannot be divided by 4.")
		.next().atLine(12).withMessage("The line starts with 1 characters which is cannot be divided by 4.")
		.next().atLine(15).withMessage("The line starts with 8 characters which is not 4 less or more than the previous line, which started at 0.")
		.noMore();
	}
}
