package org.kalidasya.sonar.erlang.api;

import com.sonar.sslr.api.AstNode;
import com.sonar.sslr.api.TokenType;

public enum ErlangKeyword implements TokenType {
	AFTER("after"), 
	AND("and"), 
	ANDALSO("andalso"), 
	BAND("band"), 
	BEGIN("begin"), 
	BNOT("bnot"), 
	BOR("bor"), 
	BSL("bsl"), 
	BSR("bsr"), 
	BXOR("bxor"), 
	CASE("case"), 
	CATCH("catch"), 
	COND("cond"), 
	DIV("div"), 
	END("end"), 
	FUN("fun"), 
	IF("if"), 
	LET("let"), 
	NOT("not"), 
	OF("of"), 
	OR("or"), 
	ORELSE("orelse"), 
	QUERY("query"), 
	RECEIVE("receive"), 
	REM("rem"), 
	TRY("try"), 
	WHEN("when"), 
	XOR("xor");

	private final String value;

	private ErlangKeyword(String value) {
		this.value = value;
	}

	public String getName() {
		return name();
	}

	public String getValue() {
		return value;
	}

	public boolean hasToBeSkippedFromAst(AstNode node) {
		return false;
	}

}
