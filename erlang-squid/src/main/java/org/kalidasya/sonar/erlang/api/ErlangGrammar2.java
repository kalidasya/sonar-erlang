package org.kalidasya.sonar.erlang.api;

import static com.sonar.sslr.impl.matcher.GrammarFunctions.Standard.o2n;
import static com.sonar.sslr.impl.matcher.GrammarFunctions.Standard.or;

import com.sonar.sslr.api.Grammar;
import com.sonar.sslr.api.Rule;

public class ErlangGrammar2 extends Grammar {

	public Rule module;
	public Rule moduleAttributes;
	public Rule functionDeclaration;
	public Rule moduleAttr;
	public Rule exportAttr;
	public Rule compileAttr;
	public Rule defineAttr;
	public Rule typeOrFunctionSpec;
	public Rule genericAttr;
	public Rule funcExport;
	public Rule expression;
	public Rule funcCall;
	public Rule funcArity;
	public Rule termCompOp;
	public Rule booleanOp;
	public Rule arithmeticOp;
	public Rule listOp;
	public Rule shortcircuitOp;
	public Rule functionClause;
	public Rule clauseHead;
	public Rule guardSequenceStart;
	public Rule funcDecl;
	public Rule clauseBody;
	public Rule pattern;
	public Rule funcArgs;
	public Rule funExpr;
	public Rule literal;
	public Rule primaryExpression;
	public Rule listLiteral;
	public Rule tupleLiteral;
	public Rule binaryLiteral;
	public Rule assignmentExpression;
	public Rule memberExpression;
	public Rule funExpression;
	public Rule arguments;
	public Rule unaryExpression;
	public Rule multiplicativeExpression;
	public Rule additiveExpression;
	public Rule shiftExpression;
	public Rule relationalExpression;
	public Rule equalityExpression;
	public Rule bitwiseAndExpression;
	public Rule bitwiseXorExpression;
	public Rule bitwiseOrExpression;
	public Rule logicalAndExpression;
	public Rule logicalOrExpression;
	public Rule leftHandSideExpression;
	public Rule assignmentOperator;
	public Rule callExpression;
	public Rule qualifier;
	public Rule listOperationExpression;
	public Rule logicalXorExpression;
	public Rule shortCircuitOrElseExpression;
	public Rule shortCircuitAndAlsoExpression;
	public Rule binaryElement;
	public Rule binaryQualifier;
	public Rule expressionStatement;
	public Rule statement;
	public Rule ifExpression;
	public Rule caseExpression;
	public Rule receiveStatement;
	public Rule tryStatement;
	public Rule branchPatternExps;
	public Rule branchExps;
	public Rule branchExp;
	public Rule guardSequence;
	public Rule eos;
	public Rule guard;
	public Rule guardExpression;
	public Rule functionDeclarationsNoName;
	public Rule functionDeclarationNoName;
	public Rule patternStatements;
	public Rule patternStatement;
	public Rule statements;
	public Rule sendStatement;
	public Rule catchExpression;
	public Rule afterExpression;
	public Rule catchPattern;
	public Rule catchPatternStatement;
	public Rule catchPatternStatements;
	public Rule blockStatement;
	public Rule recordCreateLiteral;
	public Rule recordAccLiteral;
	public Rule recordLiteral;
	public Rule recordLiteralHead;
	public Rule macroLiteral;
	public Rule otherArithmeticExpression;
	public Rule ifdefAttr;
	public Rule ifndefAttr;
	public Rule elseAttr;
	public Rule endifAttr;
	public Rule flowControlAttr;
	
	@Override
	public Rule getRootRule() {
		return module;
	}

}
