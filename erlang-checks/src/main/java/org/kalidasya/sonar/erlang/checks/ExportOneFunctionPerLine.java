package org.kalidasya.sonar.erlang.checks;

import java.util.List;

import org.kalidasya.sonar.erlang.api.ErlangGrammar;
import org.sonar.check.Priority;
import org.sonar.check.Rule;

import com.sonar.sslr.api.AstNode;
import com.sonar.sslr.squid.checks.SquidCheck;

@Rule(key = "ExportOneFunctionPerLine", priority = Priority.MINOR)
public class ExportOneFunctionPerLine extends SquidCheck<ErlangGrammar> {

	private int previousLineNum;
	private String previousFuncArity;

	@Override
	public void init() {
		subscribeTo(getContext().getGrammar().exportAttr);
		previousLineNum = 0;
		previousFuncArity = null;
	}

	@Override
	public void visitNode(AstNode node) {
		/**
		 * Get exported func arities in this export
		 */
		List<AstNode> funcArities = node.findChildren(getContext().getGrammar().funcArity);
		for (AstNode arityNode : funcArities) {
			String funcArity = getArity(arityNode);
			if (previousFuncArity != null) {
				/**
				 * If the exported arity is not in the same line but they has
				 * the same name
				 */
				if (previousLineNum != arityNode.getTokenLine()
						&& getFuncName(previousFuncArity).equals(getFuncName(funcArity))) {
					getContext()
							.createLineViolation(
									this,
									"The exported method with arity: {0} is in different line, but it has the same name as the previous arity: {1}.",
									arityNode.getTokenLine(), funcArity, previousFuncArity);
				}
				/**
				 * If exported arity is in the same line but has different name
				 */
				if (previousLineNum == arityNode.getTokenLine()
						&& !getFuncName(previousFuncArity).equals(getFuncName(funcArity))) {
					getContext()
							.createLineViolation(
									this,
									"The exported method with arity: {0} is in the same line, but it has different name than the previous arity: {1}.",
									arityNode.getTokenLine(), funcArity, previousFuncArity);
				}
			}
			previousFuncArity = funcArity;
			previousLineNum = arityNode.getTokenLine();
		}
	}

	private String getFuncName(String arity) {
		return arity.substring(0, arity.lastIndexOf("/"));
	}

	private String getArity(AstNode arityNode) {
		StringBuffer ret = new StringBuffer();
		for (AstNode arity : arityNode.getChildren()) {
			ret.append(arity.getTokenOriginalValue());
		}
		return ret.toString();
	}

}
