package org.kalidasya.sonar.erlang.checks;

import org.kalidasya.sonar.erlang.api.ErlangGrammar;
import org.sonar.check.Priority;
import org.sonar.check.Rule;

import com.sonar.sslr.api.AstNode;
import com.sonar.sslr.squid.checks.SquidCheck;

@Rule(key = "DoNotUseExportAll", priority = Priority.MINOR)
public class DoNotUseExportAllCheck extends SquidCheck<ErlangGrammar> {

	@Override
	public void init() {
		subscribeTo(getContext().getGrammar().compileAttr);
	}

	@Override
	public void visitNode(AstNode node) {
		if ("export_all".equalsIgnoreCase(node.getChild(3).getTokenOriginalValue())) {
			getContext().createLineViolation(this, "Do not use export_all", node);
		}
	}

}
