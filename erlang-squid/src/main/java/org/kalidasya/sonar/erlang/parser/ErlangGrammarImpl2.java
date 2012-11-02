package org.kalidasya.sonar.erlang.parser;

import static com.sonar.sslr.api.GenericTokenType.EOF;
import static com.sonar.sslr.api.GenericTokenType.IDENTIFIER;
import static com.sonar.sslr.api.GenericTokenType.LITERAL;
import static com.sonar.sslr.impl.matcher.GrammarFunctions.Predicate.next;
import static com.sonar.sslr.impl.matcher.GrammarFunctions.Standard.and;
import static com.sonar.sslr.impl.matcher.GrammarFunctions.Standard.o2n;
import static com.sonar.sslr.impl.matcher.GrammarFunctions.Standard.one2n;
import static com.sonar.sslr.impl.matcher.GrammarFunctions.Standard.opt;
import static com.sonar.sslr.impl.matcher.GrammarFunctions.Standard.or;
import static org.kalidasya.sonar.erlang.api.ErlangKeyword.AFTER;
import static org.kalidasya.sonar.erlang.api.ErlangKeyword.AND;
import static org.kalidasya.sonar.erlang.api.ErlangKeyword.ANDALSO;
import static org.kalidasya.sonar.erlang.api.ErlangKeyword.BAND;
import static org.kalidasya.sonar.erlang.api.ErlangKeyword.BEGIN;
import static org.kalidasya.sonar.erlang.api.ErlangKeyword.BOR;
import static org.kalidasya.sonar.erlang.api.ErlangKeyword.BSL;
import static org.kalidasya.sonar.erlang.api.ErlangKeyword.BSR;
import static org.kalidasya.sonar.erlang.api.ErlangKeyword.BXOR;
import static org.kalidasya.sonar.erlang.api.ErlangKeyword.CASE;
import static org.kalidasya.sonar.erlang.api.ErlangKeyword.CATCH;
import static org.kalidasya.sonar.erlang.api.ErlangKeyword.END;
import static org.kalidasya.sonar.erlang.api.ErlangKeyword.IF;
import static org.kalidasya.sonar.erlang.api.ErlangKeyword.NOT;
import static org.kalidasya.sonar.erlang.api.ErlangKeyword.OF;
import static org.kalidasya.sonar.erlang.api.ErlangKeyword.OR;
import static org.kalidasya.sonar.erlang.api.ErlangKeyword.ORELSE;
import static org.kalidasya.sonar.erlang.api.ErlangKeyword.RECEIVE;
import static org.kalidasya.sonar.erlang.api.ErlangKeyword.TRY;
import static org.kalidasya.sonar.erlang.api.ErlangKeyword.XOR;
import static org.kalidasya.sonar.erlang.api.ErlangKeyword.BNOT;
import static org.kalidasya.sonar.erlang.api.ErlangKeyword.REM;
import static org.kalidasya.sonar.erlang.api.ErlangPunctator.ARROW;
import static org.kalidasya.sonar.erlang.api.ErlangPunctator.BINEND;
import static org.kalidasya.sonar.erlang.api.ErlangPunctator.BINSTART;
import static org.kalidasya.sonar.erlang.api.ErlangPunctator.COLON;
import static org.kalidasya.sonar.erlang.api.ErlangPunctator.COMMA;
import static org.kalidasya.sonar.erlang.api.ErlangPunctator.DOT;
import static org.kalidasya.sonar.erlang.api.ErlangPunctator.EQUAL;
import static org.kalidasya.sonar.erlang.api.ErlangPunctator.EQUAL2;
import static org.kalidasya.sonar.erlang.api.ErlangPunctator.EXCLAMATION;
import static org.kalidasya.sonar.erlang.api.ErlangPunctator.GE;
import static org.kalidasya.sonar.erlang.api.ErlangPunctator.GT;
import static org.kalidasya.sonar.erlang.api.ErlangPunctator.LBRACKET;
import static org.kalidasya.sonar.erlang.api.ErlangPunctator.LCURLYBRACE;
import static org.kalidasya.sonar.erlang.api.ErlangPunctator.LE;
import static org.kalidasya.sonar.erlang.api.ErlangPunctator.LISTCOMP;
import static org.kalidasya.sonar.erlang.api.ErlangPunctator.LPARENTHESIS;
import static org.kalidasya.sonar.erlang.api.ErlangPunctator.LT;
import static org.kalidasya.sonar.erlang.api.ErlangPunctator.MATCHOP;
import static org.kalidasya.sonar.erlang.api.ErlangPunctator.MINUS;
import static org.kalidasya.sonar.erlang.api.ErlangPunctator.MINUSMINUS;
import static org.kalidasya.sonar.erlang.api.ErlangPunctator.NOTEQUAL;
import static org.kalidasya.sonar.erlang.api.ErlangPunctator.NOTEQUAL2;
import static org.kalidasya.sonar.erlang.api.ErlangPunctator.PIPE;
import static org.kalidasya.sonar.erlang.api.ErlangPunctator.PLUS;
import static org.kalidasya.sonar.erlang.api.ErlangPunctator.PLUSPLUS;
import static org.kalidasya.sonar.erlang.api.ErlangPunctator.RBRACKET;
import static org.kalidasya.sonar.erlang.api.ErlangPunctator.RCURLYBRACE;
import static org.kalidasya.sonar.erlang.api.ErlangPunctator.RPARENTHESIS;
import static org.kalidasya.sonar.erlang.api.ErlangPunctator.SEMI;
import static org.kalidasya.sonar.erlang.api.ErlangPunctator.STAR;
import static org.kalidasya.sonar.erlang.api.ErlangPunctator.QUESTIONMARK;
import static org.kalidasya.sonar.erlang.api.ErlangPunctator.DIV;
import static org.kalidasya.sonar.erlang.api.ErlangPunctator.ARROWBACK;
import static org.kalidasya.sonar.erlang.api.ErlangPunctator.NUMBERSIGN;
import static org.kalidasya.sonar.erlang.api.ErlangTokenType.NUMERIC_LITERAL;

import org.kalidasya.sonar.erlang.api.ErlangGrammar2;
import org.kalidasya.sonar.erlang.api.ErlangKeyword;
import org.kalidasya.sonar.erlang.api.ErlangPunctator;
import org.kalidasya.sonar.erlang.api.ErlangTokenType;

import com.sonar.sslr.impl.matcher.GrammarFunctions;

public class ErlangGrammarImpl2 extends ErlangGrammar2 {


	public ErlangGrammarImpl2() {
		expressions();
		statements();
		module();
		functions();
		GrammarFunctions.enableMemoizationOfMatchesForAllRules(this);
	}
	
	private void module() {
		module.is(
			moduleAttributes, 
			one2n(
				functionDeclaration
			), 
			EOF
		);
		
		moduleAttributes.is(
			one2n(
				or(
					moduleAttr,
					exportAttr,
					compileAttr,
					defineAttr,
					flowControlAttr,
					typeOrFunctionSpec,
					genericAttr
				)
			)
		);
		
		flowControlAttr.is(
				or(
					ifdefAttr,
					ifndefAttr
				),
				opt(elseAttr),
				endifAttr
			);

		ifdefAttr.is(
				MINUS,
				"ifdef",
				LPARENTHESIS,
				IDENTIFIER,
				RPARENTHESIS,
				DOT,
				moduleAttributes
			);
		
		ifndefAttr.is(
			MINUS,
			"ifndef",
			LPARENTHESIS,
			IDENTIFIER,
			RPARENTHESIS,
			DOT,
			moduleAttributes
		);
		
		elseAttr.is(
			MINUS,
			"else",
			DOT,
			moduleAttributes
		);
		
		endifAttr.is(
			MINUS,
			"endif",
			DOT
		);
		
		moduleAttr.is(
			MINUS,
			"module",
			LPARENTHESIS, 
			IDENTIFIER,
			RPARENTHESIS,
			DOT
		);
		exportAttr.is(
			MINUS,
			"export",
			LPARENTHESIS, 
			funcExport,
			RPARENTHESIS,
			DOT
		);
		compileAttr.is(
			MINUS,
			"compile",
			LPARENTHESIS, 
			or(
				LITERAL,
				IDENTIFIER
			),
			RPARENTHESIS, 
			DOT
		);
		
		defineAttr.is(
			MINUS,
			"define",
			LPARENTHESIS, 
			or(
				and(IDENTIFIER, COMMA, IDENTIFIER),
				and(funcDecl, COMMA, statement)
			),
			RPARENTHESIS, 
			DOT
		);
		
		genericAttr.is(
				MINUS, 
				IDENTIFIER, 
				LPARENTHESIS, 
				or(
					LITERAL,
					IDENTIFIER
				),
				RPARENTHESIS, 
			DOT
		);
		typeOrFunctionSpec.is(
			MINUS, 
			or(
				"type",
				"spec"
			),
			funcDecl,
			or(
				and(
					COLON,
					COLON,
					funcDecl,
					o2n(
						PIPE,
						funcDecl
					)
				),
				and(
					ARROW,
					funcDecl
				)
			),
			DOT
		);
		//TODO: is it possible to have something like: -export().?
		funcExport.is(
			or(
				and(
					LBRACKET,
					o2n(
						funcArity,
						o2n(
							COMMA,
							funcArity
						)
					),
					RBRACKET
				), 
				funcArity
			)
		);
	}
	
	
	private void functions() {
		functionDeclaration.is(
			functionClause, 
			o2n(
				SEMI,
				functionClause
			), 
			
			DOT
		);
		functionClause.is(clauseHead, ARROW, clauseBody);
		clauseHead.is(
			funcDecl,
			opt(guardSequenceStart)
		);
		clauseBody.is(
			statements
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

		funcDecl.is(
			IDENTIFIER,
			arguments
		);
	}
	
	private void expressions() {
		literal.is(
				or(
					NUMERIC_LITERAL,
					LITERAL
				)
			);
	    primaryExpression.is(or(
	        IDENTIFIER,
	        literal,
	        listLiteral,
	        tupleLiteral,
	        binaryLiteral,
	        and(LPARENTHESIS, expression, RPARENTHESIS)));
	    
	    listLiteral.is(
	    	LBRACKET,
	    	opt(
	    		or(
		    		and(
		    			assignmentExpression,
		    			LISTCOMP,
		    			one2n(qualifier)
		    		),
		    		and(
		    			assignmentExpression,
		    			o2n(or(COMMA, assignmentExpression)),
		    			opt(PIPE, assignmentExpression)
		    		)
	    		)
	    	),
	    	RBRACKET
	    );
	    qualifier.is(
	    	or(
	    		and(
   					primaryExpression,
   					ARROWBACK,
   					expression
   				)/*,
   				expression*/
   			)
	    );
	    recordLiteral.is(
		    	opt(IDENTIFIER),
	    		one2n(
	    			recordLiteralHead
	    		),
		    	opt(
		    		LCURLYBRACE,
		    		opt(
		    			assignmentExpression,
		    			o2n(COMMA,
		    				assignmentExpression
		    			)
		    		),
		    		RCURLYBRACE
		    	)
		    );
	    recordLiteralHead.is(
	    		NUMBERSIGN,
    			IDENTIFIER,
    			o2n(
    				DOT,
    				IDENTIFIER
    			)
	    );
	    
		macroLiteral.is(
	    	QUESTIONMARK,
	    	IDENTIFIER,
	    	opt(arguments)
	    );
	    tupleLiteral.is(LCURLYBRACE, o2n(or(COMMA, assignmentExpression)), RCURLYBRACE);
	    binaryLiteral.is(
	    	BINSTART,
	    	or(
	    		and(
    				and(
   		    			assignmentExpression,
  		    			LISTCOMP,
   		    			one2n(binaryQualifier)
   		    		),
   		    		o2n(or(COMMA, assignmentExpression))
	    		),
	    		o2n(or(COMMA, binaryElement))
	    	),
	    	BINEND
	    );
	    binaryQualifier.is(
		   	or(
		   		and(
	   				binaryLiteral,
	   				ErlangPunctator.DOUBLEARROWBACK,
	   				expression
	   			),
	   			and(
	   				primaryExpression,
	   				ARROWBACK,
	   				expression
	   			)/*,
	   			expression*/
	   		)
		);
	    
	    binaryElement.is(
		   	or(
		   		and(
					expression, 
					opt(
						COLON, 
						or(NUMERIC_LITERAL,	IDENTIFIER)
					), 
					opt(
						ErlangPunctator.DIV, 
						/*
						 * Hack for things like: 1024:32/little-float
						 */
						or(NUMERIC_LITERAL,and(IDENTIFIER, MINUS, IDENTIFIER), IDENTIFIER)
					)
				)
		   	)
		);
	    memberExpression.is(
	        or(
	        	recordLiteral,
	        	macroLiteral,
	        	ifExpression,
	        	funExpression,
	        	caseExpression,
	            primaryExpression
	        )).skipIfOneChild();
	    /**
	     * It can be a record ref (originaly a.b['a']) as well
	     */
	    callExpression.is(
	    	or(
	    		and(opt(IDENTIFIER, COLON), memberExpression, arguments),
	    		memberExpression
	    	)
	     ).skipIfOneChild();
	    arguments.is(LPARENTHESIS, opt(assignmentExpression, o2n(COMMA, assignmentExpression)), RPARENTHESIS);
	    unaryExpression.is(or(
	        callExpression,
	        and(NOT, unaryExpression)
	        )).skipIfOneChild();
	    otherArithmeticExpression.is(unaryExpression, o2n(or(BNOT, ErlangKeyword.DIV, REM), unaryExpression)).skipIfOneChild();
	    multiplicativeExpression.is(otherArithmeticExpression, o2n(or(STAR, DIV), otherArithmeticExpression)).skipIfOneChild();
	    additiveExpression.is(multiplicativeExpression, o2n(or(PLUS, MINUS), multiplicativeExpression)).skipIfOneChild();
	    
	    shiftExpression.is(additiveExpression, o2n(or(BSL, BSR), additiveExpression)).skipIfOneChild();

	    relationalExpression.is(shiftExpression, o2n(or(LT, GT, LE, GE), shiftExpression)).skipIfOneChild();

	    equalityExpression.is(relationalExpression, o2n(or(EQUAL, NOTEQUAL, EQUAL2, NOTEQUAL2), relationalExpression)).skipIfOneChild();

	    bitwiseAndExpression.is(equalityExpression, o2n(BAND, equalityExpression)).skipIfOneChild();

	    bitwiseXorExpression.is(bitwiseAndExpression, o2n(BXOR, bitwiseAndExpression)).skipIfOneChild();

	    bitwiseOrExpression.is(bitwiseXorExpression, o2n(BOR, bitwiseXorExpression)).skipIfOneChild();

	    logicalAndExpression.is(bitwiseOrExpression, o2n(AND, bitwiseOrExpression)).skipIfOneChild();

	    logicalOrExpression.is(logicalAndExpression, o2n(OR, logicalAndExpression)).skipIfOneChild();
	    
	    logicalXorExpression.is(logicalOrExpression, o2n(XOR, logicalOrExpression)).skipIfOneChild();
	    
	    shortCircuitOrElseExpression.is(logicalXorExpression, o2n(ORELSE, logicalXorExpression)).skipIfOneChild();
	    
	    shortCircuitAndAlsoExpression.is(shortCircuitOrElseExpression, o2n(ANDALSO, shortCircuitOrElseExpression)).skipIfOneChild();
	    
	    listOperationExpression.is(shortCircuitAndAlsoExpression, o2n(or(PLUSPLUS, MINUSMINUS), shortCircuitAndAlsoExpression)).skipIfOneChild();

	    assignmentExpression.is(
	    	or(
	    		and(listOperationExpression, assignmentOperator, assignmentExpression),
	    		listOperationExpression
	    	)
	    ).skipIfOneChild();

	    assignmentOperator.is(
	        MATCHOP);

	    expression.is(opt(CATCH), assignmentExpression);
	    
	    funExpression.is(
			ErlangKeyword.FUN,
			or(
				funcArity,
				and(functionDeclarationsNoName, END)
			)
		);
		functionDeclarationsNoName.is(
			functionDeclarationNoName,
			o2n(SEMI, functionDeclarationNoName)
		);
		functionDeclarationNoName.is(
			arguments, opt(guardSequenceStart), ARROW, statements
		);
		
		caseExpression.is(
				CASE, expression, OF, patternStatements, END
			);
		
		ifExpression.is(IF, branchExps, END);
	}

		/**
		 * A.4 Statement
		 **/
	private void statements() {
		eos.is(
			or(
				COMMA,
				next(EOF),
				next(DOT)
			)
		);
		statement.is(
			or(
				sendStatement,
				expressionStatement,
				//ifStatement,
				//caseStatement,
				receiveStatement,
				funExpression,
				tryStatement,
				blockStatement
			)
		);
		statements.is(
			statement,
			o2n(COMMA, statement)
		);
		expressionStatement.is(expression);
		
		patternStatements.is(
			patternStatement,
			o2n(SEMI, patternStatement)
		);
		
		catchPatternStatements.is(
			catchPatternStatement,
			o2n(SEMI, catchPatternStatement)
		);
		
		patternStatement.is(
			pattern,
			opt(guardSequenceStart),
			ARROW,
			statements
		);
		
		catchPatternStatement.is(
			catchPattern,
			opt(guardSequenceStart),
			ARROW,
			statements
		);
		
		pattern.is(
			assignmentExpression
		);
		
		catchPattern.is(
			opt(IDENTIFIER, COLON),
			assignmentExpression
		);
		
		branchExps.is(
			branchExp,
			o2n(
				ErlangPunctator.SEMI,
				branchExp
			)
		);
		
		branchExp.is(
			guardSequence,
			ErlangPunctator.ARROW,
			statements
		);
		
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
			expression
		);
		
		sendStatement.is(
			expression, EXCLAMATION, expression
		);
		
		receiveStatement.is(
			RECEIVE, patternStatements, opt(AFTER, expression, ARROW, statements), END
		);
		
		tryStatement.is(
			TRY, 
			statements, 
			opt(OF, patternStatements), 
			or(
				and(catchExpression, afterExpression),
				catchExpression,
				afterExpression
			), 
			END
		);
		
		afterExpression.is(
			AFTER, statements
		);
		
		catchExpression.is(
			CATCH,
			catchPatternStatements
		);
		
		blockStatement.is(
			BEGIN, statements, END
		);
	  }
}
