package org.kalidasya.sonar.erlang.checks;

import java.io.File;

import org.junit.Test;
import org.kalidasya.sonar.erlang.ErlangAstScanner;
import org.sonar.squid.api.SourceFile;

import com.sonar.sslr.squid.checks.CheckMessagesVerifier;

public class NumberOfFunctionArgsCheckTest {

	@Test
	public void test() {
		NumberOfFunctionArgsCheck check = new NumberOfFunctionArgsCheck();

		SourceFile file = ErlangAstScanner.scanSingleFile(new File(
				"src/test/resources/checks/funargs.erl"), check);
		CheckMessagesVerifier.verify(file.getCheckMessages())
		.next().atLine(4).withMessage("Function has 7 arguments which is grater")
		.noMore();
	}
}
