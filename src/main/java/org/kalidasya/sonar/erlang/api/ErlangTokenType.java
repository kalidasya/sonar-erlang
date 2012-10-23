package org.kalidasya.sonar.erlang.api;

import com.sonar.sslr.api.AstNode;
import com.sonar.sslr.api.TokenType;

public enum ErlangTokenType implements TokenType {
	NUMERIC_LITERAL,
	REGULAR_EXPRESSION_LITERAL, 
	ATOM;

	public String getName() {
		return name();
	}

	public String getValue() {
		return name();
	}

	public boolean hasToBeSkippedFromAst(AstNode node) {
		return false;
	}

}
