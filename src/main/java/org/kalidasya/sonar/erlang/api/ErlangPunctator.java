package org.kalidasya.sonar.erlang.api;

import com.sonar.sslr.api.AstNode;
import com.sonar.sslr.api.TokenType;

public enum ErlangPunctator implements TokenType {
	ARROW("->"),
	ARROWBACK("<-"),
	LCURLYBRACE("{"),
	RCURLYBRACE("}"),
	LPARENTHESIS("("),
	RPARENTHESIS(")"),
	LBRACKET("["),
	RBRACKET("]"),
	DOT("."),
	SEMI(";"),
	COMMA(","),
	COLON(":"),
	MATCHOP("="),
	PLUS("+"),
	MINUS("-"),
	STAR("*"),
	DIV("/"),
	LT("<"),
	GT(">"),
	LE("=<"),
	GE(">="),
	EQUAL("=="),
	NOTEQUAL("/="),
	EQUAL2("=:="),
	NOTEQUAL2("=/="),
	BINSTART("<<"),
	BINEND(">>"),
	LISTCOMP("||"),
	PIPE("|"),
	DOLLAR("$"),
	APOSTROPHE("'")
	;
	
	private final String value;
	
	private ErlangPunctator(String word){
		this.value = word;
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
