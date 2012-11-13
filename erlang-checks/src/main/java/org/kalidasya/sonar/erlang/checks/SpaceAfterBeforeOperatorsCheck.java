package org.kalidasya.sonar.erlang.checks;

import java.util.ArrayList;
import java.util.List;

import org.kalidasya.sonar.erlang.api.ErlangGrammar;
import org.kalidasya.sonar.erlang.api.ErlangPunctuator;
import org.sonar.check.Cardinality;
import org.sonar.check.Priority;
import org.sonar.check.Rule;

import com.google.common.collect.ImmutableList;
import com.sonar.sslr.api.AstNode;
import com.sonar.sslr.squid.checks.SquidCheck;

@Rule(key = "SpaceAfterBeforeOperators", priority = Priority.MAJOR, cardinality = Cardinality.SINGLE)
public class SpaceAfterBeforeOperatorsCheck extends SquidCheck<ErlangGrammar> {

	List<ErlangPunctuator> operators = ImmutableList.of(ErlangPunctuator.MATCHOP,
			ErlangPunctuator.STAR, ErlangPunctuator.DIV, ErlangPunctuator.PLUS,
			ErlangPunctuator.MINUS);
	List<Integer> failedLines = new ArrayList<Integer>();

	@Override
	public void init() {
		subscribeTo(getContext().getGrammar().primaryExpression);
	}

	@Override
	public void visitFile(AstNode astNode) {
	}

	@Override
	public void leaveFile(AstNode astNode) {
	}

	@Override
	public void visitNode(AstNode ast) {
		AstNode compTo;
		if (!failedLines.contains(ast.getTokenLine())) {
			if (ast.nextSibling() != null && operators.contains(ast.nextSibling().getType())) {
				compTo = ast.nextSibling();
				failedLines.add(check(ast, compTo, false));
			} else if (ast.previousSibling() != null
					&& operators.contains(ast.previousSibling().getType())) {
				compTo = ast.previousSibling();
				failedLines.add(check(ast, compTo, true));
			}
		}
	}

	private int check(AstNode ast, AstNode compTo, boolean previous) {
		int actCol = ast.getToken().getColumn();
		int actLength = ast.getTokenOriginalValue().length();
		int compCol = compTo.getToken().getColumn();
		int compLength = compTo.getTokenOriginalValue().length();
		int actCheckPoint = (previous) ? actCol - 1 : actCol + actLength + 1;
		int compCheckPoint = (previous) ? compCol + compLength : compCol;
		if (actCheckPoint != compCheckPoint) {
			getContext().createLineViolation(this, "No space after operator in column: {0}.",
					ast.getToken().getLine(), actCol + 1);
			return ast.getToken().getLine();
		}
		return -1;
	}
}
