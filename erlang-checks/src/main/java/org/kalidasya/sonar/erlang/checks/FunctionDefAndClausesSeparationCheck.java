package org.kalidasya.sonar.erlang.checks;

import java.util.Iterator;
import java.util.List;

import org.kalidasya.sonar.erlang.api.ErlangGrammar;
import org.sonar.check.BelongsToProfile;
import org.sonar.check.Cardinality;
import org.sonar.check.Priority;
import org.sonar.check.Rule;
import org.sonar.check.RuleProperty;

import com.sonar.sslr.api.AstNode;
import com.sonar.sslr.api.Token;
import com.sonar.sslr.api.Trivia;
import com.sonar.sslr.squid.checks.SquidCheck;

@Rule(key = "FunctionDefAndClausesSeparation", priority = Priority.MAJOR,
		cardinality = Cardinality.SINGLE)
@BelongsToProfile(title = CheckList.SONAR_WAY_PROFILE, priority = Priority.MAJOR)
public class FunctionDefAndClausesSeparationCheck extends SquidCheck<ErlangGrammar> {

	@RuleProperty(key = "allowedBlankLinesBetweenClauses", defaultValue = "0")
	public int allowedBlankLinesBetweenClauses = 0;

	@RuleProperty(key = "allowedBlankLinesBetweenDefinitions", defaultValue = "1")
	public int allowedBlankLinesBetweenDefinitions = 1;

	private AstNode previousDefinition;

	@Override
	public void init() {
		subscribeTo(getContext().getGrammar().functionDeclaration);
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
			/**
			 * Check the definition first
			 */
			if (ast.getType().equals(getContext().getGrammar().functionDeclaration)) {
				if (previousDefinition == null) {
					previousDefinition = ast;
				} else {
					check(ast, previousDefinition, allowedBlankLinesBetweenDefinitions);
					previousDefinition = ast;
				}
			}
			/**
			 * Check the clauses
			 */
			if (ast.findDirectChildren(getContext().getGrammar().functionClause).size() > 1) {
				List<AstNode> funcClauses = ast
						.findDirectChildren(getContext().getGrammar().functionClause);
				Iterator<AstNode> clauses = funcClauses.iterator();
				AstNode previousClause = clauses.next();
				while (clauses.hasNext()) {
					AstNode actClause = (AstNode) clauses.next();
					check(actClause, previousClause, allowedBlankLinesBetweenClauses);
					previousClause = actClause;

				}
			}
		}
	}

	private void check(AstNode ast, AstNode previous, int threshold) {
		if (diff(ast.getTokenLine(), previous.getLastToken().getLine(), threshold)) {
			boolean hasTrivias = ast.getToken().hasTrivia();
			if ((hasTrivias && checkTrivias(ast.getToken(), previous.getToken(), threshold))
					|| !hasTrivias) {
				getContext().createLineViolation(this,
						"The line has {0} precending blank line and it should be: {1}.",
						ast.getTokenLine(),
						(ast.getTokenLine() - previous.getLastToken().getLine() - 1), threshold);
			}
		}
	}

	private boolean diff(int a, int b, int threshold) {
		if (a - b - 1 != threshold) {
			return true;
		}
		return false;
	}

	private boolean checkTrivias(Token token, Token token2, int threshold) {
		int actLine = token2.getLine();
		for (Trivia trivia : token.getTrivia()) {
			if (actLine - trivia.getToken().getLine() - 1 > threshold) {
				return true;
			}
			actLine = trivia.getToken().getLine();
		}
		return false;
	}

}
