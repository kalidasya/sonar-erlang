package org.kalidasya.sonar.erlang.metrics;

import java.util.List;

import org.kalidasya.sonar.erlang.api.ErlangGrammar;
import org.kalidasya.sonar.erlang.api.ErlangMetric;
import org.sonar.squid.api.SourceFunction;

import com.sonar.sslr.api.AstNode;
import com.sonar.sslr.api.GenericTokenType;
import com.sonar.sslr.squid.checks.SquidCheck;

public class BranchesOfRecursion extends SquidCheck<ErlangGrammar> {

	private ErlangGrammar grammar;

	@Override
	public void init() {
		grammar = getContext().getGrammar();
		subscribeTo(grammar.functionDeclaration);

	}

	@Override
	public void visitFile(AstNode astNode) {
	}

	@Override
	public void leaveFile(AstNode astNode) {
	}

	@Override
	public void leaveNode(AstNode ast) {
		List<AstNode> functionClauses = ast.findDirectChildren(grammar.functionClause);
		String functionName = ast.findFirstDirectChild(grammar.functionClause)
				.findFirstDirectChild(grammar.clauseHead).findFirstDirectChild(grammar.funcDecl)
				.findFirstDirectChild(GenericTokenType.IDENTIFIER).getTokenOriginalValue();
		int numOfRecursion = 0;
		for (AstNode functionClause : functionClauses) {
			List<AstNode> calls = functionClause.findChildren(grammar.callExpression);
			for (AstNode call : calls) {
				if (call.findFirstDirectChild(grammar.primaryExpression).getTokenOriginalValue()
						.equals(functionName)) {
					numOfRecursion++;
				}

			}
		}
		//getContext().popSourceCode()
		getContext().peekSourceCode().add(ErlangMetric.BRANCHES_OF_RECURSION, numOfRecursion);
	}
	
	@Override
	public void visitNode(AstNode ast) {

	}
}
