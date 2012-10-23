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
		clauseHead.is(
			ErlangTokenType.ATOM, 
			ErlangPunctator.LPARENTHESIS, 
			o2n(
				or(IDENTIFIER, 
				ErlangPunctator.COMMA)
			), 
			ErlangPunctator.RPARENTHESIS,
			opt(
				ErlangKeyword.WHEN, 
				guardSequence
			), 
			ErlangPunctator.ARROW);
		clauseBody.is(o2n(or(expression, ErlangPunctator.COMMA)));
		guardSequence.is(one2n(or(guard,ErlangPunctator.SEMI)));
		guard.is(one2n(or(guardExpression, ErlangPunctator.COMMA)));
		guardExpression.is(or(term, IDENTIFIER),or(ErlangPunctator.PLUS), or(term, IDENTIFIER));
		term.is(or(LITERAL, ErlangTokenType.ATOM, ErlangTokenType.NUMERIC_LITERAL, list, tuple));
		tuple.is(ErlangPunctator.LCURLYBRACE, one2n(or(or(IDENTIFIER, ErlangPunctator.COMMA),tuple)),ErlangPunctator.RCURLYBRACE);
		list.is(ErlangPunctator.LBRACKET, one2n(or(or(IDENTIFIER, ErlangPunctator.COMMA),list)),ErlangPunctator.RBRACKET);
		//pattern.is();
		moduleAttribute.is(
				ErlangPunctator.MINUS, 
				ErlangTokenType.ATOM, 
				ErlangPunctator.LPARENTHESIS, 
				or(
					ErlangTokenType.ATOM,
					one2n(
						and(
							ErlangPunctator.LBRACKET, 
							one2n(
								or(
									ErlangTokenType.ATOM,
									ErlangPunctator.DIV,
									ErlangTokenType.NUMERIC_LITERAL,
									ErlangPunctator.COMMA)), 
							ErlangPunctator.RBRACKET)
						)
					),
				ErlangPunctator.RPARENTHESIS, 
				ErlangPunctator.DOT);
		module.is(one2n(moduleAttribute), one2n(functionDeclaration), EOF);
	}
}
