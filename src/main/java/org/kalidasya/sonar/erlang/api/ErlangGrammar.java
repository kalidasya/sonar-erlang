package org.kalidasya.sonar.erlang.api;

import com.sonar.sslr.api.Grammar;
import com.sonar.sslr.api.Rule;

public class ErlangGrammar extends Grammar {

	public Rule eos;
	public Rule eosNoLb;

	public Rule identifierName;

	public Rule condition;

	// A.1 Lexical

	public Rule literal;
	public Rule nullLiteral;
	public Rule booleanLiteral;
	public Rule stringLiteral;
	public Rule regularExpressionLiteral;
	
	public Rule sourceElement;
	public Rule sourceElements;
	
	public Rule statement;
	public Rule functionDeclaration;
	public Rule functionClause;
	public Rule clauseHead;
	public Rule clauseBody;

	public Rule module;
	public Rule moduleAttribute;
	public Rule guardSequence;

	public Rule expression;
	
	@Override
	public Rule getRootRule() {
		return module;
	}

}
