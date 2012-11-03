package org.kalidasya.sonar.erlang.checks;

import org.kalidasya.sonar.erlang.api.ErlangGrammar;
import org.sonar.check.Cardinality;
import org.sonar.check.Priority;
import org.sonar.check.Rule;
import org.sonar.check.RuleProperty;

import com.sonar.sslr.squid.checks.AbstractCommentRegularExpressionCheck;

@Rule(key = "BlockSeparatorCharacter", priority = Priority.MAJOR, cardinality = Cardinality.SINGLE)
public class BlockSeparatorCharacterCheck extends
		AbstractCommentRegularExpressionCheck<ErlangGrammar> {

	private static final String REGULAR_EXPRESSION = "^%%+ *([^%s])\\1+ *$";
	private static final String DEFAULT_MESSAGE = "only use '%s' sign(s) for block separators in comments (case sensitive)";

	@RuleProperty(	key = "allowedChars", 
					defaultValue = "=")
	public String allowedChars = "=";

	@Override
	public String getMessage() {
		return String.format(DEFAULT_MESSAGE, allowedChars);
	}

	@Override
	public String getRegularExpression() {
		return String.format(REGULAR_EXPRESSION, allowedChars);
	}

}
