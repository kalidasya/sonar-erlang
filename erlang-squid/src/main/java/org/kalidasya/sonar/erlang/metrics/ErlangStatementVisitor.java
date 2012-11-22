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
package org.kalidasya.sonar.erlang.metrics;

import org.kalidasya.sonar.erlang.api.ErlangGrammar;
import org.kalidasya.sonar.erlang.api.ErlangMetric;

import com.sonar.sslr.api.AstNode;
import com.sonar.sslr.api.AstNodeType;
import com.sonar.sslr.squid.SquidAstVisitor;

/**
 * Erlang complexity calculation
 * +1 for every function clause over 1
 * +1 for every fun expression
 * +1 for every branch expression (if)
 * +1 for every pattern statement (case, try)
 * +1 for every catch pattern statement (catch)
 * @author Tamas Kende
 *
 */
public class ErlangStatementVisitor extends SquidAstVisitor<ErlangGrammar> {

	ErlangGrammar grammar;

	@Override
	public void init() {
		grammar = getContext().getGrammar();
		subscribeTo(grammar.statement);
	}

	@Override
	public void visitNode(AstNode astNode) {
		if (astNode.findFirstParent(grammar.functionDeclaration)!=null) {
			getContext().peekSourceCode().add(ErlangMetric.STATEMENTS, 1);
		}

	}
}