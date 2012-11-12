package org.kalidasya.sonar.erlang.metrics;

import java.util.List;

import org.kalidasya.sonar.erlang.api.ErlangGrammar;
import org.kalidasya.sonar.erlang.api.ErlangMetric;
import org.kalidasya.sonar.erlang.api.ErlangPunctuator;
import org.sonar.check.Cardinality;
import org.sonar.check.Priority;
import org.sonar.check.Rule;

import com.google.common.collect.ImmutableList;
import com.sonar.sslr.api.AstAndTokenVisitor;
import com.sonar.sslr.api.AstNode;
import com.sonar.sslr.api.Token;
import com.sonar.sslr.squid.checks.SquidCheck;

@Rule(key = "NumberOfFunctionArgument", priority = Priority.MAJOR, cardinality = Cardinality.SINGLE)
public class NumberOfFunctionArgument extends SquidCheck<ErlangGrammar> implements
		AstAndTokenVisitor {

	List<ErlangPunctuator> nonArg = ImmutableList.of(ErlangPunctuator.LPARENTHESIS, ErlangPunctuator.RPARENTHESIS, ErlangPunctuator.COMMA); 
	private ErlangGrammar grammar;

	@Override
	public void init() {
		grammar = getContext().getGrammar();
		subscribeTo(grammar.clauseHead);

	}

	@Override
	public void visitFile(AstNode astNode) {
	}

	@Override
	public void leaveFile(AstNode astNode) {
	}

	@Override
	public void visitNode(AstNode ast) {
		AstNode args = ast.findFirstDirectChild(grammar.funcDecl).findFirstDirectChild(
				grammar.arguments);
		int numOfArgs = 0;
		for (AstNode arg : args.getChildren()) {
			if(!nonArg.contains(arg.getType())){
				numOfArgs++;
			}
		}
		getContext().peekSourceCode().add(ErlangMetric.NUM_OF_FUNC_ARGS, numOfArgs);

	}

	@Override
	public void visitToken(Token token) {
	}

}
