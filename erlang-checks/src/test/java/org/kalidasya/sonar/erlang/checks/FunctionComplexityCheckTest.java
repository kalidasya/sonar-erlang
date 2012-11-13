package org.kalidasya.sonar.erlang.checks;

import java.io.File;

import org.junit.Test;
import org.kalidasya.sonar.erlang.ErlangAstScanner;
import org.sonar.squid.api.SourceFile;

import com.sonar.sslr.squid.checks.CheckMessagesVerifier;

public class FunctionComplexityCheckTest {

	@Test
	public void test() {
		FunctionComplexityCheck check = new FunctionComplexityCheck();

		SourceFile file = ErlangAstScanner.scanSingleFile(new File(
				"src/test/resources/checks/complexity.erl"), check);
		CheckMessagesVerifier.verify(file.getCheckMessages())
		.next().atLine(2).withMessage("Function has a complexity of 11 which is greater than 10 authorized.")
		.noMore();
	}
}
