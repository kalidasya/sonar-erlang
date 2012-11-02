package org.kalidasya.sonar.erlang.checks;

import java.io.File;

import org.junit.Test;
import org.kalidasya.sonar.erlang.ErlangAstScanner;
import org.sonar.squid.api.SourceFile;

import com.sonar.sslr.squid.checks.CheckMessagesVerifier;

public class UseEqualAsBlockSeparatorTest {

	@Test
	public void test() {
		UseEqualAsBlockSeparator check = new UseEqualAsBlockSeparator();

		SourceFile file = ErlangAstScanner.scanSingleFile(new File(
				"src/test/resources/checks/commentRegularExpression.js"), check);
		CheckMessagesVerifier.verify(file.getCheckMessages()).next().atLine(2).withMessage(
				"Avoid TODO").noMore();
	}

}
