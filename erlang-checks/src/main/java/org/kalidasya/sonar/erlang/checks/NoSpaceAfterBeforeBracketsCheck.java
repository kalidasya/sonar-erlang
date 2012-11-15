/*
 * Sonar Erlang Plugin
 * Copyright (C) 2012 Tamas Kende
 * kende.tamas@gmail.com
 * 
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3 of the License, or (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02
 */
package org.kalidasya.sonar.erlang.checks;

import java.util.ArrayList;
import java.util.List;

import org.kalidasya.sonar.erlang.api.ErlangGrammar;
import org.kalidasya.sonar.erlang.api.ErlangPunctuator;
import org.sonar.check.BelongsToProfile;
import org.sonar.check.Cardinality;
import org.sonar.check.Priority;
import org.sonar.check.Rule;

import com.google.common.collect.ImmutableList;
import com.sonar.sslr.api.AstNode;
import com.sonar.sslr.api.Token;
import com.sonar.sslr.squid.checks.SquidCheck;

@Rule(key = "NoSpaceAfterBeforeBrackets", priority = Priority.MAJOR,
		cardinality = Cardinality.SINGLE)
@BelongsToProfile(title = CheckList.SONAR_WAY_PROFILE, priority = Priority.MAJOR)
public class NoSpaceAfterBeforeBracketsCheck extends SquidCheck<ErlangGrammar> {

	List<ErlangPunctuator> noSpaceBefore = ImmutableList.of(ErlangPunctuator.RBRACKET,
			ErlangPunctuator.RCURLYBRACE, ErlangPunctuator.RPARENTHESIS);
	List<ErlangPunctuator> noSpaceAfter = ImmutableList.of(ErlangPunctuator.LBRACKET,
			ErlangPunctuator.LCURLYBRACE, ErlangPunctuator.LPARENTHESIS);
	List<Integer> failedLines = new ArrayList<Integer>();

	@Override
	public void init() {
		subscribeTo(ErlangPunctuator.RBRACKET, ErlangPunctuator.RCURLYBRACE,
				ErlangPunctuator.RPARENTHESIS, ErlangPunctuator.LBRACKET,
				ErlangPunctuator.LCURLYBRACE, ErlangPunctuator.LPARENTHESIS);
	}

	@Override
	public void visitFile(AstNode astNode) {
	}

	@Override
	public void leaveFile(AstNode astNode) {
	}

	@Override
	public void visitNode(AstNode ast) {
		Token compTo;
		if (!failedLines.contains(ast.getTokenLine())) {
			if (ast.hasParents(getContext().getGrammar().clauseBody)) {
				if (noSpaceAfter.contains(ast.getType())) {
					compTo = ast.nextSibling().getToken();
					failedLines.add(check(ast, compTo, false));
				} else if (noSpaceBefore.contains(ast.getType())) {
					compTo = ast.previousSibling().getLastToken();
					failedLines.add(check(ast, compTo, true));
				}
			}
		}
	}

	private int check(AstNode ast, Token compTo, boolean previous) {
		int actCol = ast.getToken().getColumn();
		int actLength = ast.getTokenOriginalValue().length();
		int compCol = compTo.getColumn();
		int compLength = compTo.getOriginalValue().length();
		int actCheckPoint = (previous) ? actCol : actCol + actLength;
		int compCheckPoint = (previous) ? compCol + compLength : compCol;
		if (actCheckPoint != compCheckPoint) {
			getContext().createLineViolation(this, "Space after bracket in column: {0}.",
					ast.getToken().getLine(), actCol + 1);
			return ast.getToken().getLine();
		}
		return -1;
	}

}
