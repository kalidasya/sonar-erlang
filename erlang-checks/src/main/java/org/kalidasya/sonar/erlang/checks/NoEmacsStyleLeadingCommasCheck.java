package org.kalidasya.sonar.erlang.checks;

import org.kalidasya.sonar.erlang.api.ErlangGrammar;
import org.kalidasya.sonar.erlang.api.ErlangPunctuator;
import org.sonar.check.BelongsToProfile;
import org.sonar.check.Cardinality;
import org.sonar.check.Priority;
import org.sonar.check.Rule;

import com.sonar.sslr.api.AstAndTokenVisitor;
import com.sonar.sslr.api.AstNode;
import com.sonar.sslr.api.Token;
import com.sonar.sslr.squid.checks.SquidCheck;

@Rule(key = "NoEmacsStyleLeadingComma", priority = Priority.MAJOR, cardinality = Cardinality.SINGLE)
@BelongsToProfile(title = CheckList.SONAR_WAY_PROFILE, priority = Priority.MAJOR)
public class NoEmacsStyleLeadingCommasCheck extends SquidCheck<ErlangGrammar> implements AstAndTokenVisitor {

	private Token previousToken;


	@Override
	public void visitFile(AstNode astNode) {
	}

	@Override
	public void leaveFile(AstNode astNode) {
	}

	public void visitToken(Token token) {
		if(previousToken==null || (previousToken.getLine()!=token.getLine())){
			if(token.getType().equals(ErlangPunctuator.COMMA)){
				getContext()
				.createLineViolation(
						this,
						"No Emacs-style leading commas.",
						token.getLine());
			}
			previousToken = token;
		}
		
	}

}
