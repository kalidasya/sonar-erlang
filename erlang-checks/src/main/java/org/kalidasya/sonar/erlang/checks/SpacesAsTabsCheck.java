package org.kalidasya.sonar.erlang.checks;

import org.kalidasya.sonar.erlang.api.ErlangGrammar;
import org.sonar.check.Cardinality;
import org.sonar.check.Priority;
import org.sonar.check.Rule;
import org.sonar.check.RuleProperty;

import com.sonar.sslr.api.AstAndTokenVisitor;
import com.sonar.sslr.api.AstNode;
import com.sonar.sslr.api.Token;
import com.sonar.sslr.squid.checks.SquidCheck;

@Rule(key = "SpacesAsTabs", priority = Priority.MAJOR, cardinality = Cardinality.SINGLE)
public class SpacesAsTabsCheck extends SquidCheck<ErlangGrammar> implements AstAndTokenVisitor {

	private Token previousToken;
	private int previousCol;

	@RuleProperty(key = "regularExpression", defaultValue = "4")
	public int numSpacesAsTab = 4;

	@Override
	public void visitFile(AstNode astNode) {
		previousToken = null;
		previousCol = 0;
	}

	@Override
	public void leaveFile(AstNode astNode) {
		previousToken = null;
		previousCol = 0;
	}

	@Override
	public void visitToken(Token token) {
		if (!token.isGeneratedCode()) {
			if (previousToken == null
					|| (previousToken != null && previousToken.getLine() != token.getLine())) {
				if (token.getColumn() % 4 != 0) {
					getContext()
							.createLineViolation(
									this,
									"The line starts with {0, number, integer} characters which is cannot be divided by {1, number, integer}.",
									token.getLine(), token.getColumn(), numSpacesAsTab);
				} else {
					int colDifference = (previousCol - token.getColumn())*-1;
					if (colDifference>0 && colDifference!= numSpacesAsTab	&& colDifference != 0) {
						getContext()
								.createLineViolation(
										this,
										"The line starts with {0, number, integer} characters which is not {1, number, integer} less or more than the previous line, which started at {2, number, integer}.",
										token.getLine(), token.getColumn(), numSpacesAsTab, previousCol);
					} else {
						previousCol = token.getColumn();	
					}
				}
				previousToken = token;
			}
		}
	}

}
