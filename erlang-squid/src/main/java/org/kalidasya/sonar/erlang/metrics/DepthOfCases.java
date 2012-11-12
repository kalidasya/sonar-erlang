package org.kalidasya.sonar.erlang.metrics;

import org.kalidasya.sonar.erlang.api.ErlangGrammar;
import org.kalidasya.sonar.erlang.api.ErlangMetric;

import com.sonar.sslr.api.AstNode;
import com.sonar.sslr.squid.SquidAstVisitor;

public class DepthOfCases extends SquidAstVisitor<ErlangGrammar> {
	
	@Override
	public void init() {
		subscribeTo(getContext().getGrammar().caseExpression);
	}
	
	@Override
	public void visitNode(AstNode astNode) {
		getContext().peekSourceCode().add(ErlangMetric.DEPTH_OF_CASES, countParent(astNode));
	}

	private int countParent(AstNode astNode){
		AstNode node = astNode;
		int count = 0;
		while(node.hasParents(getContext().getGrammar().caseExpression)){
			count ++;
			node = node.findFirstParent(getContext().getGrammar().caseExpression);
		}
		return count;
	}
	
}
