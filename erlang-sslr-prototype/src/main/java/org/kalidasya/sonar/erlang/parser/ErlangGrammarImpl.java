package org.kalidasya.sonar.erlang.parser;

import org.kalidasya.sonar.erlang.api.ErlangGrammar;
import org.kalidasya.sonar.erlang.api.ErlangKeyword;
import org.kalidasya.sonar.erlang.api.ErlangPunctator;
import org.kalidasya.sonar.erlang.api.ErlangTokenType;

import com.sonar.sslr.api.GenericTokenType;

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
		module();
		functions();
		statements();
		dataTypes();
		operators();
		guards();
	}

	private void module() {
		module.is(
			one2n(moduleAttributes), 
			one2n(
				functionDeclaration
			), 
			EOF
		);
		
		moduleAttributes.is(
			or(
				moduleAttr,
				exportAttr,
				compileAttr,
				defineAttr,
				typeOrFunctionSpec,
				genericAttr
			)
		);
		
		moduleAttr.is(
			ErlangPunctator.MINUS,
			"module",
			ErlangPunctator.LPARENTHESIS, 
			IDENTIFIER,
			ErlangPunctator.RPARENTHESIS,
			ErlangPunctator.DOT
		);
		exportAttr.is(
			ErlangPunctator.MINUS,
			"export",
			ErlangPunctator.LPARENTHESIS, 
			funcExport,
			ErlangPunctator.RPARENTHESIS,
			ErlangPunctator.DOT
		);
		compileAttr.is(
			ErlangPunctator.MINUS,
			"compile",
			ErlangPunctator.LPARENTHESIS, 
			or(
				//listedTermsOrFunCalls,
				LITERAL,
				IDENTIFIER
			),
			ErlangPunctator.RPARENTHESIS, 
			ErlangPunctator.DOT
		);
		
		defineAttr.is(
			ErlangPunctator.MINUS,
			"define",
			ErlangPunctator.LPARENTHESIS, 
			or(
				and(IDENTIFIER, ErlangPunctator.COMMA, IDENTIFIER),
				and(funcCall, ErlangPunctator.COMMA, expression)
			),
			ErlangPunctator.RPARENTHESIS, 
			ErlangPunctator.DOT
		);
		
		genericAttr.is(
				ErlangPunctator.MINUS, 
				IDENTIFIER, 
				ErlangPunctator.LPARENTHESIS, 
				or(
			//		listedTermsOrFunCalls,
					LITERAL,
					IDENTIFIER
				),
				ErlangPunctator.RPARENTHESIS, 
			ErlangPunctator.DOT
		);
		typeOrFunctionSpec.is(
			ErlangPunctator.MINUS, 
			or(
				"type",
				"spec"
			),
			funcCall,
			or(
				and(
					ErlangPunctator.COLON,
					ErlangPunctator.COLON,
					funcCall,
					o2n(
						ErlangPunctator.PIPE,
						funcCall
					)
				),
				and(
					ErlangPunctator.ARROW,
					funcCall
				)
			),
			ErlangPunctator.DOT
		);
		//TODO: is it possible to have something like: -export().?
		funcExport.is(
			or(
				and(
					ErlangPunctator.LBRACKET,
					o2n(
						funcArity,
						o2n(
							ErlangPunctator.COMMA,
							funcArity
						)
					),
					ErlangPunctator.RBRACKET
				), 
				funcArity
			)
		);
	}

	private void statements(){
		arithmeticExp.is(
			additiveExp
		).skipIfOneChild();
		arithmeticPExp.is(
			or(
				and(
					ErlangPunctator.LPARENTHESIS,
					arithmeticExp,
					ErlangPunctator.RPARENTHESIS
				)
			)
		).skipIfOneChild();
		aExpression.is(
			additiveExp
		);
		additiveExp.is(
			multiplicativeExp, o2n(or(ErlangPunctator.PLUS, ErlangPunctator.MINUS), multiplicativeExp)
		).skipIfOneChild();
		multiplicativeExp.is(
			unaryExp, 
			o2n(
				or(
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
				),
			unaryExp)
		).skipIfOneChild();
		unaryExp.is(
			or(
				and(
					or(ErlangPunctator.PLUS, ErlangPunctator.MINUS), 
					unaryExp
				),
				primaryExp
			)
		).skipIfOneChild();
		primaryExp.is(
			or(
				ErlangTokenType.NUMERIC_LITERAL,
				funcCall,
				IDENTIFIER,
				arithmeticPExp
			)
		).skipIfOneChild();
		
		termCompareExp.is(termsOrFunCalls, termCompOp, termsOrFunCalls);
		
		booleanExp.is(termsOrFunCalls, booleanOp, termsOrFunCalls);
		
		shortcircuitExp.is(
			or(
				and(
					ErlangPunctator.LPARENTHESIS, 
					expression, 
					o2n(shortcircuitOp, shortcircuitExp), 
					ErlangPunctator.RPARENTHESIS
				),
				and(
					expression, 
					o2n(shortcircuitOp, shortcircuitExp) 
				)
			)
			
		);
		listExp.is(termsOrFunCalls, one2n(listOp, termsOrFunCalls));
		/**
		 * CANNOT ADD optional parenthesis here...
		 */
		expression.is(
		/*	possibleExpressions
		);
		possibleExpressions.is(*/
			opt(ErlangKeyword.CATCH), 
			or(
				matchExp,
				termCompareExp,
				flowExp,
			    funExpr,
				listExp,
				recordRef,
				arithmeticExp, 
				booleanExp, 
				comprehensionExps,
				macroExp,
				termsOrFunCalls 
			)
		);
		flowExp.is(or(ifExp, caseExp, receiveExp));
		caseExp.is(
			ErlangKeyword.CASE, 
			or(
				and(
					ErlangPunctator.LPARENTHESIS,
					expression,
					ErlangPunctator.RPARENTHESIS
				),
				expression
			),
			ErlangKeyword.OF, 
			casePattern,
			o2n(ErlangPunctator.SEMI, casePattern),
			ErlangKeyword.END
		);
		
		casePattern.is(
			pattern,
			opt(guardSequenceStart),
			ErlangPunctator.ARROW,
			expression,
			o2n(
				ErlangPunctator.COMMA,
				expression
			)
		);
		
		ifExp.is(
			ErlangKeyword.IF, 
			branchPatternExps,
			ErlangKeyword.END
		);
		
		receiveExp.is(ErlangKeyword.RECEIVE, branchPatternExps, ErlangKeyword.END);
		
		branchPatternExps.is(
			branchPatternExp,
			o2n(
				ErlangPunctator.SEMI,
				branchPatternExp
			)
		);
		
		branchPatternExp.is(
			guardSequence,
			ErlangPunctator.ARROW,
			expression,
			o2n(
				ErlangPunctator.COMMA,
				expression
			)
		);
		
		branchExpression.is(
			expression,
			o2n(
				ErlangPunctator.COMMA,
				expression
			)
		);
		
		pattern.is(
			or(
				matchExp,
				term
			)
		);
		
		
		macroExp.is(
			"?",
			IDENTIFIER,
			opt(funcArgs)
		);
		
		recordRef.is(
			or(
				recordSet,
				recordAcc
			)
		);
		
		recordSet.is(
			opt(IDENTIFIER),
			ErlangPunctator.NUMBERSIGN,
			IDENTIFIER,
			ErlangPunctator.LCURLYBRACE,
			matchExp, 
			o2n(
				ErlangPunctator.COMMA,
				matchExp
			),
			ErlangPunctator.RCURLYBRACE
		);
		matchExp.is(
			or(
				arithmeticExp,
				term
			),
			ErlangPunctator.MATCHOP,
			expression
		);
		recordAcc.is(
			opt(IDENTIFIER),
			ErlangPunctator.NUMBERSIGN,
			IDENTIFIER,
			o2n(
				ErlangPunctator.DOT,
				IDENTIFIER
			),
			opt(
				ErlangPunctator.LCURLYBRACE,	
				ErlangPunctator.RCURLYBRACE
			)
		);
	
		comprehensionExps.is(
			or(
				listComprehensionExp,
				binaryComprehensionExp
			)
		);
		
		listComprehensionExp.is(
			ErlangPunctator.LBRACKET,
			expression,
			ErlangPunctator.LISTCOMP,
			one2n(qualifier),
			ErlangPunctator.RBRACKET
		);
		
		binaryComprehensionExp.is(
			ErlangPunctator.BINSTART,
			binary,
			ErlangPunctator.LISTCOMP,
			one2n(qualifier),
			ErlangPunctator.BINEND
		);
		
		qualifier.is(
			or(
				and(
					expression,
					ErlangPunctator.ARROWBACK,
					expression
				),
				expression
			)
		);
	}
	
	private void guards(){
		guardSequenceStart.is(
				ErlangKeyword.WHEN, 
				guardSequence
			);
		guardSequence.is(
			guard,
			o2n(
				ErlangPunctator.SEMI,
				guard
			)
		);
		guard.is(
			guardExpression,
			o2n(
				ErlangPunctator.COMMA, 
				guardExpression
			)
		);
		guardExpression.is(
			or(
				matchExp,
				termCompareExp, 
				arithmeticExp, 
				booleanExp, 
				shortcircuitExp, 
				funcCall,
				IDENTIFIER
			)
		);
	}
	
	private void dataTypes(){
		term.is(
			or(
				LITERAL, 
				ErlangTokenType.NUMERIC_LITERAL,
				macroExp,
				binary,
				list, 
				tuple, 
				recordRef, 
				IDENTIFIER
			)
		);
		/**
		 * TODO: check can it be replaced with expression
		 */
		termsOrFunCalls.is(
			or(
				funcCall,
				term
			)
		);
		listedTermsOrFunCalls.is(
			and(
				termsOrFunCalls,
				o2n(
					ErlangPunctator.COMMA,
					termsOrFunCalls
				)
			)
		);
		listedExpressions.is(
			expression,
			o2n(
				ErlangPunctator.COMMA,
				expression
			)
		);
		tuple.is(
			ErlangPunctator.LCURLYBRACE, 
			opt(
				listedExpressions
			),
			ErlangPunctator.RCURLYBRACE
		);
		list.is(
			ErlangPunctator.LBRACKET, 
			o2n(
				listedExpressions,
				o2n(
					ErlangPunctator.PIPE, 
					listedExpressions
				)
			),
			ErlangPunctator.RBRACKET
		);
		
		binary.is(
			ErlangPunctator.BINSTART,
			bitSyntaxExpression,
			ErlangPunctator.BINEND
		);
		
		bitSyntaxExpression.is(
			opt(
				bitValue,
				o2n(
					ErlangPunctator.COMMA,
					bitValue
				)
			)
		);
		
		bitValue.is(
			or(
				LITERAL, 
				ErlangTokenType.NUMERIC_LITERAL, 
				binary,
				and(
					ErlangPunctator.LPARENTHESIS,
					expression,
					ErlangPunctator.RPARENTHESIS
				),
				IDENTIFIER
			),
			opt(
				ErlangPunctator.COLON,
				or(
					ErlangTokenType.NUMERIC_LITERAL,
					IDENTIFIER
				)
			),
			opt(
				ErlangPunctator.DIV,
				IDENTIFIER,
				o2n(
					ErlangPunctator.MINUS,
					IDENTIFIER
				)
			)
		);
	}
	
	private void operators(){
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
		booleanOp.is(or(
			ErlangKeyword.NOT, 
			ErlangKeyword.AND, 
			ErlangKeyword.OR, 
			ErlangKeyword.XOR
		));
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
			ErlangKeyword.BSR,
			ErlangPunctator.NUMBERSIGN
		));
		listOp.is(or(ErlangPunctator.PLUSPLUS, ErlangPunctator.MINUSMINUS));
		shortcircuitOp.is(or(ErlangKeyword.ANDALSO, ErlangKeyword.ORELSE));
	}
	
	private void functions() {
		functionDeclaration.is(
			functionClause, 
			o2n(
				ErlangPunctator.SEMI,
				functionClause
			), 
			
			ErlangPunctator.DOT
		);
		functionClause.is(clauseHead, ErlangPunctator.ARROW, clauseBody);
		clauseHead.is(
			funcDecl,
			opt(guardSequenceStart)
		);
		clauseBody.is(
			expression,
			o2n(
				ErlangPunctator.COMMA,
				expression
			)
		);
		
		funcArity.is(
			opt(
				IDENTIFIER,
				ErlangPunctator.COLON
			),
			IDENTIFIER,
			ErlangPunctator.DIV,
			ErlangTokenType.NUMERIC_LITERAL
		);
		/*
		 * What can be in a pattern?
		 */
		funcDecl.is(
			IDENTIFIER,
			ErlangPunctator.LPARENTHESIS, 
			opt(
				pattern,
				o2n(
					ErlangPunctator.COMMA,
					pattern
				)
			),
			ErlangPunctator.RPARENTHESIS
		);
		funcCall.is(
			opt(IDENTIFIER, ErlangPunctator.COLON),
			IDENTIFIER, 
			funcArgs 
		);
		funcArgs.is(
			ErlangPunctator.LPARENTHESIS, 
			opt(
				expression,
				o2n(
					ErlangPunctator.COMMA,
					expression
				)
			),
			ErlangPunctator.RPARENTHESIS
		);
		funExpr.is(
			ErlangKeyword.FUN,
			or(
				and(
					funcArgs,
					opt(guardSequenceStart),
					ErlangPunctator.ARROW,
					expression,
					o2n(
						ErlangPunctator.COMMA,
						expression
					),
					ErlangKeyword.END
				),
				funcArity
			)
		);
	}
}
