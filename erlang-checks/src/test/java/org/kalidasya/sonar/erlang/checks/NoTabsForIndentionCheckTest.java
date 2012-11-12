package org.kalidasya.sonar.erlang.checks;

import java.io.File;

import org.junit.Test;
import org.kalidasya.sonar.erlang.ErlangAstScanner;
import org.sonar.squid.api.SourceFile;

import com.sonar.sslr.squid.checks.CheckMessagesVerifier;

public class NoTabsForIndentionCheckTest {

	@Test
	public void test() {
		NoTabsForIndentionCheck check = new NoTabsForIndentionCheck();

		SourceFile file = ErlangAstScanner.scanSingleFile(new File(
				"src/test/resources/checks/spacesastabs.erl"), check);
		CheckMessagesVerifier.verify(file.getCheckMessages())
		.next().atLine(6).withMessage("Line has tabs as indention.")
		.next().atLine(13)
		.noMore();
	}
}
