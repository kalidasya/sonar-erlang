package org.kalidasya.sonar.erlang.checks;

import java.io.File;

import org.junit.Test;
import org.kalidasya.sonar.erlang.ErlangAstScanner;
import org.sonar.squid.api.SourceFile;

import com.sonar.sslr.squid.checks.CheckMessagesVerifier;

public class BlockSeparatorCharacterCheckTest {

	@Test
	public void test() {
		BlockSeparatorCharacterCheck check = new BlockSeparatorCharacterCheck();

		SourceFile file = ErlangAstScanner.scanSingleFile(new File(
				"src/test/resources/checks/commentcheck.erl"), check);
		CheckMessagesVerifier.verify(file.getCheckMessages()).next().atLine(4).withMessage(
				"only use '=' sign(s) for block separators in comments (case sensitive)").noMore();
	}

	@Test
	public void testDifferentValue() {
		BlockSeparatorCharacterCheck check = new BlockSeparatorCharacterCheck();
		check.allowedChars = "-";
		SourceFile file = ErlangAstScanner.scanSingleFile(new File(
				"src/test/resources/checks/commentcheck.erl"), check);
		CheckMessagesVerifier.verify(file.getCheckMessages()).next().atLine(7).withMessage(
				"only use '-' sign(s) for block separators in comments (case sensitive)").noMore();
	}

	@Test
	public void testDifferentValue2() {
		BlockSeparatorCharacterCheck check = new BlockSeparatorCharacterCheck();
		check.allowedChars = "=-";
		SourceFile file = ErlangAstScanner.scanSingleFile(new File(
				"src/test/resources/checks/commentcheck.erl"), check);
		CheckMessagesVerifier.verify(file.getCheckMessages()).noMore();
	}
	
}
