package org.kalidasya.sonar.erlang.checks;

import java.io.File;

import org.junit.Test;
import org.kalidasya.sonar.erlang.ErlangAstScanner;
import org.sonar.squid.api.SourceFile;

import com.sonar.sslr.squid.checks.CheckMessagesVerifier;

public class NoTabsForIndentionCheckTest {

	@Test
	public void test() {
		NoTabsForIndention check = new NoTabsForIndention();

		SourceFile file = ErlangAstScanner.scanSingleFile(new File(
				"src/test/resources/checks/spacesastabs.erl"), check);
		CheckMessagesVerifier.verify(file.getCheckMessages())
		.next().atLine(6).withMessage("")
		.noMore();
	}
}
