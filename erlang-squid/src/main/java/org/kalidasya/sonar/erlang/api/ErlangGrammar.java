package org.kalidasya.sonar.erlang.api;

import static com.sonar.sslr.impl.matcher.GrammarFunctions.Standard.o2n;
import static com.sonar.sslr.impl.matcher.GrammarFunctions.Standard.or;

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
	public Rule guardSequenceStart;
	public Rule guardSequence;
	public Rule guard;
	public Rule guardExpression;
	public Rule term;
	public Rule pattern;
	public Rule list;
	public Rule tuple;
	public Rule funcArity;
	public Rule funcCall;
	public Rule funcExport;

	public Rule termCompareExp;
	public Rule arithmeticExp;
	public Rule booleanExp;
	public Rule shortcircuitExp;
	public Rule termCompOp;
	public Rule arithmeticOp;
	public Rule booleanOp;
	public Rule shortcircuitOp;
	public Rule listOp;
	public Rule listExp;

	public Rule flowExp;
	public Rule ifExp;
	public Rule caseExp;
	public Rule receiveExp;

	public Rule branchExpression;
	public Rule branchPatternExp;
	public Rule listedTermsOrFunCalls;
	public Rule termsOrFunCalls;
	public Rule typeOrFunctionSpec;
	public Rule recordRef;
	public Rule recordSet;
	public Rule recordAcc;
	public Rule matchExp;
	public Rule funcArgs;
	public Rule funExpr;
	public Rule qualifier;
	public Rule listComprehensionExp;
	public Rule casePattern;
	public Rule possibleExpressions;
	public Rule expressionSub;
	
	public Rule aExpression;
	public Rule additiveExp;
	public Rule multiplicativeExp;
	public Rule unaryExp;
	public Rule primaryExp;
	public Rule arithmeticPExp;
	public Rule macroExp;
	public Rule binary;
	public Rule bitValue;
	public Rule bitSyntaxExpression;
	
	public Rule funcDecl;
	public Rule expression;

	public Rule moduleAttributes;
	public Rule moduleAttr;
	public Rule exportAttr;
	public Rule compileAttr;
	public Rule genericAttr;
	public Rule defineAttr;
	public Rule branchPatternExps;
	public Rule binaryComprehensionExp;
	public Rule comprehensionExps;
	public Rule listedExpressions;
	
	
	@Override
	public Rule getRootRule() {
		return module;
	}

}
