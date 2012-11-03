package org.kalidasya.sonar.erlang.checks;

import java.io.File;

import org.junit.Test;
import org.kalidasya.sonar.erlang.ErlangAstScanner;
import org.sonar.squid.api.SourceFile;

import com.sonar.sslr.squid.checks.CheckMessagesVerifier;

public class CommentRegularExpressionCheckTest {

	@Test
	public void test() {
		CommentRegularExpressionCheck check = new CommentRegularExpressionCheck();

		SourceFile file = ErlangAstScanner.scanSingleFile(new File(
				"src/test/resources/checks/commentcheck.erl"), check);
		CheckMessagesVerifier.verify(file.getCheckMessages()).noMore();
	}

	@Test
	public void testDifferentValue() {
		CommentRegularExpressionCheck check = new CommentRegularExpressionCheck();
		check.regularExpression = "(?i).*TODO.*";
		SourceFile file = ErlangAstScanner.scanSingleFile(new File(
				"src/test/resources/checks/commentcheck.erl"), check);
		CheckMessagesVerifier.verify(file.getCheckMessages()).next().atLine(2).withMessage(
				"The regular expression matches this comment").next().atLine(9).withMessage(
						"The regular expression matches this comment");
	}

	@Test
	public void testDifferentValue2() {
		CommentRegularExpressionCheck check = new CommentRegularExpressionCheck();
		check.regularExpression = "(?i).*XXX.*";
		check.message = "XXX found";
		SourceFile file = ErlangAstScanner.scanSingleFile(new File(
				"src/test/resources/checks/commentcheck.erl"), check);
		CheckMessagesVerifier.verify(file.getCheckMessages()).next().atLine(8).withMessage(
				"XXX found").noMore();
	}
	
}
