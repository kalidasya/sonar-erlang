package org.kalidasya.sonar.erlang.metrics;

import org.kalidasya.sonar.erlang.api.ErlangGrammar;
import org.kalidasya.sonar.erlang.api.ErlangMetric;

import com.sonar.sslr.api.AstNode;
import com.sonar.sslr.api.AstNodeType;
import com.sonar.sslr.squid.SquidAstVisitor;

/**
 * Erlang complexity calculation
 * +1 for every function clause over 1
 * +1 for every fun expression
 * +1 for every branch expression (if)
 * +1 for every pattern statement (case, try)
 * +1 for every catch pattern statement (catch)
 * @author Tamas Kende
 *
 */
public class ErlangComplexityVisitor extends SquidAstVisitor<ErlangGrammar> {

	private AstNodeType[] complexityAstNodeType;
	ErlangGrammar grammar;

	@Override
	public void init() {
		grammar = getContext().getGrammar();
		complexityAstNodeType = new AstNodeType[] {
				// Entry points
				grammar.functionDeclaration, grammar.funExpression,

				// Branching nodes
				grammar.branchExp, grammar.patternStatement, grammar.catchPatternStatement,
				

		// TODO: Expressions? when increase? if there are more than one guard and more than
		// one guard expression?
				//grammar.guardSequence
		};
		subscribeTo(complexityAstNodeType);
	}

	@Override
	public void visitNode(AstNode astNode) {
		if (astNode.getType().equals(grammar.functionDeclaration)) {
			countClauses(astNode);
		} else {
			increaseComplexity(1);
		}

	}

	private void increaseComplexity(int i) {
		getContext().peekSourceCode().add(ErlangMetric.COMPLEXITY, i);
	}

	/**
	 * Only increase complexity if the function has more than one function
	 * clause
	 * 
	 * @param functionDec
	 */
	private void countClauses(AstNode functionDec) {
		increaseComplexity((functionDec.findDirectChildren(grammar.functionClause).size() - 1));
	}

}