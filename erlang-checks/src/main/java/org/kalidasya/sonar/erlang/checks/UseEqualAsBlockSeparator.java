package org.kalidasya.sonar.erlang.checks;

import org.kalidasya.sonar.erlang.api.ErlangGrammar;

import com.sonar.sslr.squid.checks.AbstractCommentRegularExpressionCheck;

public class UseEqualAsBlockSeparator extends AbstractCommentRegularExpressionCheck<ErlangGrammar> {

	private static final String REGULAR_EXPRESSION = "^ *%% *([^x=X])\1+ *$";
	private static final String MESSAGE = "only use '===' signs for block separators in comments";

	@Override
	public String getMessage() {
		return MESSAGE;
	}

	@Override
	public String getRegularExpression() {
		return REGULAR_EXPRESSION;
	}

}
