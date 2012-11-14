package org.kalidasya.sonar.erlang.checks;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.util.Scanner;

import org.kalidasya.sonar.erlang.api.ErlangGrammar;
import org.sonar.check.BelongsToProfile;
import org.sonar.check.Cardinality;
import org.sonar.check.Priority;
import org.sonar.check.Rule;

import com.sonar.sslr.api.AstNode;
import com.sonar.sslr.squid.checks.SquidCheck;

@Rule(key = "NoTabsForIndention", priority = Priority.MAJOR, cardinality = Cardinality.SINGLE)
@BelongsToProfile(title = CheckList.SONAR_WAY_PROFILE, priority = Priority.MAJOR)
public class NoTabsForIndentionCheck extends SquidCheck<ErlangGrammar> {

	@Override
	public void visitFile(AstNode astNode) {
		try {
			checkFileIndention(getContext().getFile());
		} catch (FileNotFoundException e) {
		}
	}

	@Override
	public void leaveFile(AstNode astNode) {
	}

	private void checkFileIndention(File source) throws FileNotFoundException {
		Scanner scanner = new Scanner(new FileInputStream(source));
		try {
			int lineNumber = 1;
			while (scanner.hasNextLine()) {
				String line = scanner.nextLine();
				if (line.matches("^ *\t+.*")) {
					getContext().createLineViolation(this, "Line has tabs as indention.",
							lineNumber);
				}
				lineNumber++;
			}
		} finally {
			scanner.close();
		}
	}

}
