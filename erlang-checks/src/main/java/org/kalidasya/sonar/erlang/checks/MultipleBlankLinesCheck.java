package org.kalidasya.sonar.erlang.checks;

import org.kalidasya.sonar.erlang.api.ErlangGrammar;
import org.sonar.check.BelongsToProfile;
import org.sonar.check.Cardinality;
import org.sonar.check.Priority;
import org.sonar.check.Rule;
import org.sonar.check.RuleProperty;

import com.sonar.sslr.api.AstNode;
import com.sonar.sslr.api.GenericTokenType;
import com.sonar.sslr.api.Token;
import com.sonar.sslr.api.Trivia;
import com.sonar.sslr.squid.checks.SquidCheck;

@Rule(key = "MultipleBlankLines", priority = Priority.MAJOR, cardinality = Cardinality.SINGLE)
@BelongsToProfile(title = CheckList.SONAR_WAY_PROFILE, priority = Priority.MAJOR)
public class MultipleBlankLinesCheck extends SquidCheck<ErlangGrammar> {

	@RuleProperty(key = "maxBlankLinesInsideFunctions", defaultValue = "1")
	public int maxBlankLinesInsideFunctions = 1;

	@RuleProperty(key = "maxBlankLinesOutsideFunctions", defaultValue = "2")
	public int maxBlankLinesOutsideFunctions = 2;

	private Token previousToken;

	@Override
	public void init() {
		subscribeTo(getContext().getGrammar().primaryExpression,
				GenericTokenType.IDENTIFIER);
	}

	@Override
	public void visitFile(AstNode astNode) {
	}

	@Override
	public void leaveFile(AstNode astNode) {
	}

	@Override
	public void visitNode(AstNode ast) {
		if (!ast.getToken().isGeneratedCode()) {
			if (previousToken == null
					|| (previousToken != null && previousToken.getLine() != ast
							.getToken().getLine())) {
				int previousLine = (previousToken != null) ? previousToken
						.getLine() : 0;

				if (checkBlankLines(ast, previousLine)) {
					getContext()
							.createLineViolation(
									this,
									"Too many blank lines found, the threshold is {0}.",
									ast.getToken().getLine(),
									getMaxFor(ast));
				}
			}
			previousToken = ast.getToken();
		}

	}

	private boolean compare(int line1, int line2, int comp) {
		return (line1 - line2 - 1 > comp);
	}

	private int getMaxFor(AstNode ast) {
		if (ast.findFirstParent(getContext().getGrammar().clauseBody) != null) {
			return maxBlankLinesInsideFunctions;
		} else {
			return maxBlankLinesOutsideFunctions;
		}
	}

	private boolean checkTrivias(int previousLine, AstNode ast, int compTo) {
		int prevLine = previousLine;
		for (Trivia trivias : ast.getToken().getTrivia()) {
			if (compare(trivias.getToken().getLine(), prevLine, compTo)) {
				return true;
			}
			prevLine = trivias.getToken().getLine();
		}
		return compare(ast.getToken().getLine(), prevLine, compTo);
	}

	private boolean checkBlankLines(AstNode ast, int previousLine) {
		int compTo = getMaxFor(ast);

		boolean check = compare(ast.getToken().getLine(), previousLine, compTo);
		if (check) {
			if (ast.getToken().hasTrivia()) {
				return checkTrivias(previousLine, ast, compTo);
			}
		}
		return check;
	}

}
