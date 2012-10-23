package org.kalidasya.sonar.erlang.parser;

import java.io.EOFException;

import org.hibernate.annotations.OptimisticLock;
import org.kalidasya.sonar.erlang.api.ErlangGrammar;
import org.kalidasya.sonar.erlang.api.ErlangKeyword;
import org.kalidasya.sonar.erlang.api.ErlangPunctator;
import org.kalidasya.sonar.erlang.api.ErlangTokenType;

import static com.sonar.sslr.api.GenericTokenType.EOF;
import static com.sonar.sslr.api.GenericTokenType.IDENTIFIER;
import static com.sonar.sslr.api.GenericTokenType.LITERAL;
import static com.sonar.sslr.impl.matcher.GrammarFunctions.Predicate.next;
import static com.sonar.sslr.impl.matcher.GrammarFunctions.Predicate.not;
import static com.sonar.sslr.impl.matcher.GrammarFunctions.Standard.and;
import static com.sonar.sslr.impl.matcher.GrammarFunctions.Standard.o2n;
import static com.sonar.sslr.impl.matcher.GrammarFunctions.Standard.one2n;
import static com.sonar.sslr.impl.matcher.GrammarFunctions.Standard.opt;
import static com.sonar.sslr.impl.matcher.GrammarFunctions.Standard.or;

public class ErlangGrammarImpl extends ErlangGrammar {

	public ErlangGrammarImpl() {
		functions();
	}

	private void functions() {
		functionDeclaration.is(one2n(functionClause));
		functionClause.is(clauseHead, ErlangPunctator.ARROW, clauseBody);
		clauseHead.is(ErlangTokenType.ATOM, ErlangPunctator.LPARENTHESIS, o2n(LITERAL, ErlangPunctator.COMMA), opt(ErlangKeyword.WHEN, LITERAL), ErlangPunctator.ARROW);
		clauseBody.is();
		moduleAttribute.is(
				ErlangPunctator.MINUS, 
				LITERAL, 
				ErlangPunctator.LBRACKET, 
				or(
					LITERAL,
					one2n(
						and(
							ErlangPunctator.LBRACKET, 
							LITERAL, 
							ErlangPunctator.RBRACKET)
						)
					),
				ErlangPunctator.RPARENTHESIS, 
				ErlangPunctator.DOT);
		module.is(one2n(moduleAttribute), one2n(functionDeclaration), EOF);
	}
}
