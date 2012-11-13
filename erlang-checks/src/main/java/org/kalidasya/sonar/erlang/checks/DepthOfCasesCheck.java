package org.kalidasya.sonar.erlang.checks;

import org.kalidasya.sonar.erlang.api.ErlangGrammar;
import org.sonar.check.Cardinality;
import org.sonar.check.Priority;
import org.sonar.check.Rule;
import org.sonar.check.RuleProperty;

import com.sonar.sslr.api.AstNode;
import com.sonar.sslr.squid.checks.SquidCheck;

@Rule(key = "DepthOfCases", priority = Priority.MAJOR, cardinality = Cardinality.SINGLE)
public class DepthOfCasesCheck extends SquidCheck<ErlangGrammar> {

	private static final int DEFAULT_MAXIMUM_CASE_DEPTH_THRESHOLD = 4;

	@RuleProperty(key = "maximumCaseDepthThreshold", defaultValue = ""
			+ DEFAULT_MAXIMUM_CASE_DEPTH_THRESHOLD)
	private int maximumCaseDepthThreshold = DEFAULT_MAXIMUM_CASE_DEPTH_THRESHOLD;

	private ErlangGrammar g;

	@Override
	public void init() {
		g = getContext().getGrammar();
		subscribeTo(g.caseExpression);
	}

	@Override
	public void visitNode(AstNode astNode) {
		if (isTopLevelCase(astNode)) {
			int depth = countChild(astNode);
			if (depth > maximumCaseDepthThreshold) {
				getContext().createLineViolation(this,
						"Depth of case: {0} reached the threshold: {1}.", astNode.getTokenLine(),
						depth, maximumCaseDepthThreshold);
			}
		}

	}

	private boolean isTopLevelCase(AstNode astNode) {
		return !astNode.hasParents(g.caseExpression);
	}

	private int countChild(AstNode astNode) {
		return astNode.findChildren(g.caseExpression).size()-1;
	}

}
