package org.kalidasya.sonar.erlang.checks;

import java.io.File;

import org.junit.Test;
import org.kalidasya.sonar.erlang.ErlangAstScanner;
import org.sonar.squid.api.SourceFile;

import com.sonar.sslr.squid.checks.CheckMessagesVerifier;

public class DoNotUseExportAllTest {

	@Test
	public void test() {
		DoNotUseExportAll check = new DoNotUseExportAll();

		SourceFile file = ErlangAstScanner.scanSingleFile(new File(
				"src/test/resources/checks/spacesastabs.erl"), check);
		CheckMessagesVerifier.verify(file.getCheckMessages()).noMore();
	}
	
	@Test
	public void test2() {
		DoNotUseExportAll check = new DoNotUseExportAll();
		SourceFile file = ErlangAstScanner.scanSingleFile(new File(
				"src/test/resources/checks/exportall.erl"), check);
		CheckMessagesVerifier.verify(file.getCheckMessages()).next().atLine(2).withMessage("Do not use export_all");
	}
}
