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
import org.sonar.squid.measures.Metric;

import com.sonar.sslr.api.AstNode;
import com.sonar.sslr.squid.SquidAstVisitor;

public class PublicDocumentedApiCounter extends SquidAstVisitor<ErlangGrammar> {

	private int numOfPublicAPIs;
	private ErlangGrammar g;

	public PublicDocumentedApiCounter() {
		this.numOfPublicAPIs = 0;
	}

	@Override
	public void init() {
		this.g = getContext().getGrammar();
		subscribeTo(g.exportAttr);
	}

	@Override
	public void visitNode(AstNode astNode) {
		/*
		 * Ignore test exports
		 */
		if (!(astNode.findFirstParent(g.ifdefAttr) != null && "TEST".equalsIgnoreCase(astNode
				.findFirstParent(g.ifdefAttr).getChild(3).getTokenOriginalValue()))) {
			numOfPublicAPIs += astNode.findChildren(getContext().getGrammar().funcArity).size();
		}
	}

	@Override
	public void leaveFile(AstNode astNode) {
		getContext().peekSourceCode().add(ErlangMetric.PUBLIC_API, numOfPublicAPIs);
	}

}
