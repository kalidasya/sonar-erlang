package org.kalidasya.sonar.erlang.parser;

import org.kalidasya.sonar.erlang.api.ErlangGrammar;
import org.kalidasya.sonar.erlang.api.ErlangGrammar2;
import org.kalidasya.sonar.erlang.api.ErlangKeyword;
import org.kalidasya.sonar.erlang.api.ErlangPunctator;
import org.kalidasya.sonar.erlang.api.ErlangTokenType;

import com.sonar.sslr.api.GenericTokenType;
import com.sonar.sslr.api.Rule;

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
import static org.kalidasya.sonar.erlang.api.ErlangPunctator.*;
import static org.kalidasya.sonar.erlang.api.ErlangKeyword.*;
import static org.kalidasya.sonar.erlang.api.ErlangTokenType.*;

public class ErlangGrammarImpl2 extends ErlangGrammar2 {


	public ErlangGrammarImpl2() {
		expressions();
		statements();
		module();
		functions();
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
					typeOrFunctionSpec,
					genericAttr
				)
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
				and(funcDecl, ErlangPunctator.COMMA, expression)
			),
			ErlangPunctator.RPARENTHESIS, 
			ErlangPunctator.DOT
		);
		
		genericAttr.is(
				ErlangPunctator.MINUS, 
				IDENTIFIER, 
				ErlangPunctator.LPARENTHESIS, 
				or(
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
					funcDecl,
					o2n(
						ErlangPunctator.PIPE,
						funcDecl
					)
				),
				and(
					ErlangPunctator.ARROW,
					funcDecl
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
	    	or(
	    		and(
	    			assignmentExpression,
	    			LISTCOMP,
	    			one2n(qualifier)
	    		),
	    		o2n(or(COMMA, assignmentExpression))
	    	),
	    	RBRACKET
	    );
	    qualifier.is(
	    	or(
	    		and(
   					IDENTIFIER,
   					ErlangPunctator.ARROWBACK,
   					expression
   				),
   				expression
   			)
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
	   			expression
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
						or(NUMERIC_LITERAL, IDENTIFIER)
					)
				)
		   	)
		);
	    memberExpression.is(
	        or(
	        	funExpression,
	            primaryExpression
	        ));
	    /**
	     * It can be a record ref (originaly a.b['a']) as well
	     */
	    callExpression.is(
	    	or(
	    		and(opt(IDENTIFIER, COLON), memberExpression, arguments),
	    		memberExpression
	    	)
	     );
	    arguments.is(LPARENTHESIS, opt(assignmentExpression, o2n(COMMA, assignmentExpression)), RPARENTHESIS);
	    unaryExpression.is(or(
	        callExpression,
	        and(NOT, unaryExpression)
	        ));
	    multiplicativeExpression.is(unaryExpression, o2n(or(STAR, ErlangPunctator.DIV), unaryExpression)).skipIfOneChild();
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
	    		and(callExpression, assignmentOperator, assignmentExpression),
	    		listOperationExpression
	    	)
	    ).skipIfOneChild();

	    assignmentOperator.is(
	        MATCHOP);

	    expression.is(opt(CATCH), assignmentExpression, o2n(COMMA, assignmentExpression));
	    
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
				ifStatement,
				caseStatement,
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
		
		caseStatement.is(
			CASE, expression, OF, patternStatements, END
		);		
		
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
		
		ifStatement.is(IF, branchExps, END);
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
			expression, 
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
