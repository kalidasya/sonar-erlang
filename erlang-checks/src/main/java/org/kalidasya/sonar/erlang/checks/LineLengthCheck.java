package org.kalidasya.sonar.erlang.checks;

import org.kalidasya.sonar.erlang.api.ErlangGrammar;
import org.sonar.api.utils.SonarException;
import org.sonar.check.BelongsToProfile;
import org.sonar.check.Cardinality;
import org.sonar.check.Priority;
import org.sonar.check.Rule;
import org.sonar.check.RuleProperty;

import com.sonar.sslr.api.AstAndTokenVisitor;
import com.sonar.sslr.api.AstNode;
import com.sonar.sslr.api.Token;
import com.sonar.sslr.api.Trivia;
import com.sonar.sslr.squid.checks.SquidCheck;

@Rule(key = "LineLength", priority = Priority.MAJOR, cardinality = Cardinality.SINGLE)
@BelongsToProfile(title = CheckList.SONAR_WAY_PROFILE, priority = Priority.MAJOR)
public class LineLengthCheck extends SquidCheck<ErlangGrammar> implements
		AstAndTokenVisitor {

	private static final int DEFAULT_MAXIMUM_LINE_LENHGTH = 100;

	private int lastIncorrectLine;

	@RuleProperty(key = "maximumLineLength", defaultValue = ""
			+ DEFAULT_MAXIMUM_LINE_LENHGTH)
	public int maximumLineLength = DEFAULT_MAXIMUM_LINE_LENHGTH;

	@Override
	public void init() {
		if (maximumLineLength <= 0) {
			throw new SonarException(
					"[AbstractLineLengthCheck] The maximal line length must be set to a value greater than 0 ("
							+ maximumLineLength + " given).");
		}
	}

	@Override
	public void visitFile(AstNode astNode) {
		lastIncorrectLine = -1;
	}

	public void visitToken(Token token) {
		if (!token.isGeneratedCode() && lastIncorrectLine != token.getLine()) {
			int incorrectLine = checkLine(token);

			if (incorrectLine > -1) {
				lastIncorrectLine = token.getLine();
				getContext()
						.createLineViolation(
								this,
								"The line length is greater than {0,number,integer} authorized.",
								incorrectLine, maximumLineLength);
			}
		}
	}

	private int checkLine(Token token) {
		int lineLength = token.getColumn() + token.getValue().length();
		if (lineLength > maximumLineLength) {
			return token.getLine();
		} else if (token.getTrivia().size() > 0) {
			for (Trivia trivia : token.getTrivia()) {
				if (trivia.isComment()
						&& trivia.getToken().getColumn()
								+ trivia.getToken().getValue().length() > maximumLineLength) {
					return trivia.getToken().getLine();
				}
			}
		}
		return -1;
	}

}
