package org.kalidasya.sonar.erlang.parser;

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
			funcCall,
			opt(
				ErlangKeyword.WHEN, 
				guardSequence
			), 
			ErlangPunctator.ARROW);
		clauseBody.is(
			o2n(
				or(
					expression, 
					ErlangPunctator.COMMA
				)
			)
		);
		guardSequence.is(one2n(or(guard,ErlangPunctator.SEMI)));
		guard.is(one2n(or(guardExpression, ErlangPunctator.COMMA)));
		guardExpression.is(or(IDENTIFIER, termCompareExp, arithmeticExp, booleanExp, shortcircuitExp, funcCall));
		term.is(or(LITERAL, IDENTIFIER, ErlangTokenType.NUMERIC_LITERAL, list, tuple));
		tuple.is(ErlangPunctator.LCURLYBRACE, one2n(or(IDENTIFIER, ErlangPunctator.COMMA, tuple)),ErlangPunctator.RCURLYBRACE);
		list.is(
			ErlangPunctator.LBRACKET, 
			one2n(or(or(IDENTIFIER, ErlangPunctator.COMMA),list)),ErlangPunctator.RBRACKET
		);
		termCompOp.is(
			or(
				ErlangPunctator.EQUAL, 
				ErlangPunctator.EQUAL2, 
				ErlangPunctator.NOTEQUAL, 
				ErlangPunctator.NOTEQUAL2, 
				ErlangPunctator.LT, 
				ErlangPunctator.GT, 
				ErlangPunctator.LE, 
				ErlangPunctator.GE
			)
		);
		termCompareExp.is(term, termCompOp, term);
		arithmeticOp.is(or(
			ErlangPunctator.PLUS, 
			ErlangPunctator.MINUS, 
			ErlangPunctator.STAR, 
			ErlangPunctator.DIV, 
			ErlangKeyword.BNOT, 
			ErlangKeyword.DIV, 
			ErlangKeyword.REM, 
			ErlangKeyword.BAND, 
			ErlangKeyword.BOR,
			ErlangKeyword.BXOR, 
			ErlangKeyword.BSL, 
			ErlangKeyword.BSR
		));
		arithmeticExp.is(opt(ErlangTokenType.NUMERIC_LITERAL), arithmeticOp, ErlangTokenType.NUMERIC_LITERAL);
		booleanOp.is(or(ErlangKeyword.NOT, ErlangKeyword.AND, ErlangKeyword.OR, ErlangKeyword.XOR));
		booleanExp.is(opt(expression), booleanOp, expression);
		shortcircuitOp.is(or(ErlangKeyword.ANDALSO, ErlangKeyword.ORELSE));
		shortcircuitExp.is(opt(ErlangPunctator.LPARENTHESIS), expression, o2n(shortcircuitOp, shortcircuitExp), opt(ErlangPunctator.RPARENTHESIS));
		
		listOp.is(or(ErlangPunctator.PLUSPLUS, ErlangPunctator.MINUSMINUS));
		listExp.is(list, listOp, list);
		expression.is(or(funcCall, IDENTIFIER));
		//pattern.is();
		moduleAttribute.is(
				ErlangPunctator.MINUS, 
				IDENTIFIER, 
				ErlangPunctator.LPARENTHESIS, 
				or(
					IDENTIFIER,
					one2n(
						and(
							ErlangPunctator.LBRACKET, 
							one2n(
								or(
									funcArity,
									ErlangPunctator.COMMA)), 
							ErlangPunctator.RBRACKET)
						)
					),
				ErlangPunctator.RPARENTHESIS, 
				ErlangPunctator.DOT);
		funcArity.is(IDENTIFIER,
					ErlangPunctator.DIV,
					ErlangTokenType.NUMERIC_LITERAL);
		funcCall.is(IDENTIFIER, 
				ErlangPunctator.LPARENTHESIS, 
				o2n(
					or(IDENTIFIER, 
					ErlangPunctator.COMMA)
				), 
				ErlangPunctator.RPARENTHESIS);
		module.is(one2n(moduleAttribute), one2n(functionDeclaration), EOF);
	}
}
