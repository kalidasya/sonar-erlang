package org.kalidasya.sonar.erlang.checks;

import java.io.File;

import org.junit.Test;
import org.kalidasya.sonar.erlang.ErlangAstScanner;
import org.sonar.squid.api.SourceFile;

import com.sonar.sslr.squid.checks.CheckMessagesVerifier;

public class DepthOfCasesCheckTest {

	@Test
	public void test() {
		DepthOfCasesCheck check = new DepthOfCasesCheck();

		SourceFile file = ErlangAstScanner.scanSingleFile(new File(
				"src/test/resources/checks/depthofcases.erl"), check);
		CheckMessagesVerifier.verify(file.getCheckMessages())
		.next().atLine(4).withMessage("Depth of case: 5 reached the threshold: 4.")
		.noMore();
	}
}
