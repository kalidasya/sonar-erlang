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

import org.kalidasya.sonar.erlang.api.ErlangGrammar;
import org.sonar.check.BelongsToProfile;
import org.sonar.check.Cardinality;
import org.sonar.check.Priority;
import org.sonar.check.Rule;
import org.sonar.check.RuleProperty;

import com.sonar.sslr.api.AstAndTokenVisitor;
import com.sonar.sslr.api.AstNode;
import com.sonar.sslr.api.Token;
import com.sonar.sslr.squid.checks.SquidCheck;

@Rule(key = "IndentionSize", priority = Priority.MAJOR, cardinality = Cardinality.SINGLE,
		name = "IndentionSize", description="Allowed indention size")
@BelongsToProfile(title = CheckList.SONAR_WAY_PROFILE, priority = Priority.MAJOR)
public class IndentionSizeCheck extends SquidCheck<ErlangGrammar> implements AstAndTokenVisitor {

	private Token previousToken;
	private int previousCol;

	@RuleProperty(key = "regularExpression", defaultValue = "4")
	public int indentionSize = 4;

	@Override
	public void visitFile(AstNode astNode) {
		previousToken = null;
		previousCol = 0;
	}

	@Override
	public void leaveFile(AstNode astNode) {
		previousToken = null;
		previousCol = 0;
	}

	public void visitToken(Token token) {
		if (!token.isGeneratedCode()) {
			if (previousToken == null
					|| (previousToken != null && previousToken.getLine() != token.getLine())) {
				if (token.getColumn() % 4 != 0) {
					getContext()
							.createLineViolation(
									this,
									"The line starts with {0, number, integer} characters which is cannot be divided by {1, number, integer}.",
									token.getLine(), token.getColumn(), indentionSize);
				} else {
					int colDifference = (previousCol - token.getColumn()) * -1;
					if (colDifference > 0 && colDifference != indentionSize && colDifference != 0) {
						getContext()
								.createLineViolation(
										this,
										"The line starts with {0, number, integer} characters which is not {1, number, integer} more than the previous line, which started at {2, number, integer}.",
										token.getLine(), token.getColumn(), indentionSize,
										previousCol);
					} else {
						previousCol = token.getColumn();
					}
				}
				previousToken = token;
			}
		}
	}

}
