package org.kalidasya.sonar.erlang.checks;

import java.util.Iterator;
import java.util.List;

import org.kalidasya.sonar.erlang.api.ErlangGrammar;
import org.sonar.check.Cardinality;
import org.sonar.check.Priority;
import org.sonar.check.Rule;
import org.sonar.check.RuleProperty;

import com.sonar.sslr.api.AstNode;
import com.sonar.sslr.api.Token;
import com.sonar.sslr.squid.checks.SquidCheck;

@Rule(key = "FunctionDefAndClausesSeparation", priority = Priority.MAJOR,
		cardinality = Cardinality.SINGLE)
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
			if (ast.getType().equals(getContext().getGrammar().functionDeclaration)) {
				if (previousDefinition == null) {
					previousDefinition = ast;
				} else {
					check(ast, previousDefinition, allowedBlankLinesBetweenDefinitions);
					previousDefinition = ast;
				}
			}
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
		int difference = ast.getTokenLine() - previous.getLastToken().getLine() - 1;
		if (difference != threshold) {
			getContext().createLineViolation(this,
					"The line has {0} precending blank line, the thresold is: {1}.",
					ast.getTokenLine(), difference, threshold);
		}
	}

}
