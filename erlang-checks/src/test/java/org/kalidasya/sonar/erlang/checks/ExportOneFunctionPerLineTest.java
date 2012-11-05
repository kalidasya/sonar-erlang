package org.kalidasya.sonar.erlang.checks;

import java.io.File;

import org.junit.Test;
import org.kalidasya.sonar.erlang.ErlangAstScanner;
import org.sonar.squid.api.SourceFile;

import com.sonar.sslr.squid.checks.CheckMessagesVerifier;

public class ExportOneFunctionPerLineTest {

	@Test
	public void test() {
		ExportOneFunctionPerLine check = new ExportOneFunctionPerLine();

		SourceFile file = ErlangAstScanner.scanSingleFile(new File(
				"src/test/resources/checks/export_one_function_per_line.erl"), check);
		CheckMessagesVerifier.verify(file.getCheckMessages())
		.next().atLine(3).withMessage("The exported method with arity: kill/0 is in the same line, but it has different name than the previous arity: stop/0.")
		.next().atLine(5).withMessage("The exported method with arity: log/3 is in different line, but it has the same name as the previous arity: log/2.")
		.next().atLine(7).withMessage("The exported method with arity: refresh/2 is in the same line, but it has different name than the previous arity: update/2.").noMore();
	}
}
