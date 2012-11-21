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
package org.kalidasya.sonar.erlang.parser;

import static com.sonar.sslr.test.parser.ParserMatchers.parse;
import static org.junit.Assert.assertThat;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.kalidasya.sonar.erlang.ErlangConfiguration;
import org.kalidasya.sonar.erlang.api.ErlangGrammar;

import com.google.common.base.Charsets;
import com.google.common.base.Joiner;
import com.sonar.sslr.impl.Parser;
import com.sonar.sslr.impl.events.ExtendedStackTrace;
import com.sonar.sslr.impl.events.ExtendedStackTraceStream;

public class ErlangParserExpressionTest {
	ExtendedStackTrace listener = new ExtendedStackTrace();
	Parser<ErlangGrammar> p = ErlangParser.create(new ErlangConfiguration(Charsets.UTF_8),
			listener);

	ErlangGrammar g = p.getGrammar();

	@Before
	public void init() {
		p.setRootRule(g.expression);
	}

	@Test
	public void simpleExpression() {
		assertThat(p, parse(code("1+3")));
		assertThat(p, parse(code("true")));
		assertThat(p, parse(code("6 + 5 * 4 - 3 / 2")));
		assertThat(p, parse(code("ok")));
	}

	@Test
	public void simpleExpressionWithP() {
		assertThat(p, parse(code("(1+3)")));
		assertThat(p, parse(code("(6 + 5) * ((4 - 3) / 2)")));

	}

	@Test
	public void varMatch() {
		assertThat(p, parse(code("A=2")));
		assertThat(p, parse(code("A=-2")));
		assertThat(p, parse(code("A=N-2")));
		assertThat(p, parse(code("A=N--2")));
		assertThat(p, parse(code("B=[2,3]")));
		assertThat(p, parse(code("B={2,3}")));

	}
	
	@Test
	public void relationExpression() {
		assertThat(p, parse(code("A+5>=12")));

	}

	@Test
	public void listExpression() {
		assertThat(p, parse(code("[asd,ore,[ow,2,3],[hello,23]]")));
		assertThat(p, parse(code("[]")));
		assertThat(p, parse(code("[d|T]")));
		assertThat(p, parse(code("[c|[]]")));
		assertThat(p, parse(code("[a|[b|[c|[]]]]")));
		assertThat(p, parse(code("[a,2,{c,4}]")));
		assertThat(
				p,
				parse(code("[Name,proplists:get_value(description,Spec,[])|proplists:get_value(keywords,Spec,[])]")));

	}

	@Test
	public void tupleExpression() {
		assertThat(p, parse(code("{asd,ore,{ow,[2,23,as],3},[hello,{23,as}]}")));
		assertThat(p, parse(code("{float(), float()}")));
	}

	@Test
	public void listComprehensionExpressin() {
		assertThat(p, parse(code("[X*2 || X <- [1,2,3]]")));
		assertThat(p, parse(code("[X*2 || X <- method()]")));
		assertThat(p, parse(code("[X*2 || X <- method(), method2()]")));
		assertThat(p, parse(code("[X*2 || X <- [1,2,3]] ++ [7,8,9]")));
		assertThat(p, parse(code("[X*2 || X <- [1,2,3]] -- [7,8,9]")));
		assertThat(p, parse(code("[10, 23] -- [X*2 || X <- [1,2,3]] ++ [7,8,9]")));
		assertThat(p, parse(code("[756, 877] ++ [X*2 || X <- [1,2,3]] -- [7,8,9]")));
		assertThat(p, parse(code("[{A,B} || {A, B} <- method(), method2(File)]")));
		assertThat(p, parse(code("[Call || {_From, To} = Call <- ExtCalls, lists:member(To, RelevantAPICalls)]")));
		assertThat(p, parse(code("[{M, F, A} || {nowarn_unused_function, FAs} <- Opts,  {F, A} <- lists:flatten([FAs])]")));
		assertThat(p, parse(code("[Call || Call = {_From, To} <- ExtCalls, not dialyzer_plt:contains_mfa(InitPlt, To)]")));
	}

	@Test
	public void logicalExpressions() {
		assertThat(p, parse(code("not true")));
		assertThat(p, parse(code("true and false")));
		assertThat(p, parse(code("true xor false")));
		assertThat(p, parse(code("true or A")));
		assertThat(p, parse(code("A orelse B")));
		assertThat(p, parse(code("A andalso B")));
		assertThat(p, parse(code("not A andalso B or false")));
		assertThat(p, parse(code("(not (A andalso B)) or false")));
	}

	@Test
	public void binaryExpressions() {
		assertThat(p, parse(code("<<1,17,42>>")));
		assertThat(p, parse(code("<<1,17,42:16>>")));
		assertThat(p, parse(code("<<1024/utf8>>")));
		assertThat(p, parse(code("<<1024:16/utf8>>")));
		assertThat(p, parse(code("<<$a,$b,$c>>")));
		assertThat(p, parse(code("<<\"hello\">>")));
		assertThat(p, parse(code("<<A,B,C:16>> = <<1,17,42:16>>")));
		assertThat(p, parse(code("<<D:16,E,F>> = <<1,17,42:16>>")));
		assertThat(p, parse(code("<<G,H/binary>> = <<1,17,42:16>>")));
		assertThat(p, parse(code("<<G,H/bitstring>> = <<1,17,42:12>>")));
		assertThat(p, parse(code("<< << (X*2) >> || <<X>> <= << 1,2,3 >> >>")));
		assertThat(p, parse(code("<< << (X*2) >> || <<X>> <= method1() >>")));
		assertThat(p, parse(code("<< << (X*2) >> || <<X>> <= method1(), method2() >>")));
	}

	@Test
	public void functionCall() {
		assertThat(p, parse(code("method(\"hello\")")));
		assertThat(p, parse(code("method(12)")));
		assertThat(p, parse(code("method(\"hello\",234234)")));
		assertThat(p, parse(code("haho:method(\"hello\")")));
		assertThat(p, parse(code("io:format(\"assert error in module ~p on line ~p~n\")")));
		assertThat(p, parse(code("string:strip(erlang:system_info(system_architecture),right,$\n)")));
	}

	@Test
	public void catchExpressions() {
		assertThat(p, parse(code("catch 1+2")));
		assertThat(p, parse(code("catch 1+a")));
		assertThat(p, parse(code("A = (catch 1+2)")));
		assertThat(p, parse(code("catch throw(hello)")));
	}

	@Test
	public void recordCreate() {p.setRootRule(g.expression);
		assertThat(p, parse(code("#Name{Field1=Expr1,Field2=Expr2,FieldK=ExprK}")));
		assertThat(p, parse(code("#person{name=Name, _='_'}")));
		assertThat(p, parse(code("A = #Name{Field1=Expr1,Field2=Expr2,FieldK=ExprK}")));
		assertThat(p, parse(code("S = #person{name=Name, _='_'}")));
		assertThat(p, parse(code("User#user{ibuttons = User#user.ibuttons ++ [IButton]}")));
	}

	@Test
	public void recordAccess() {
		assertThat(p, parse(code("#person.name")));
		assertThat(p, parse(code("Expr#Name.Field")));
		assertThat(p, parse(code("N2#nrec2.nrec1#nrec1.nrec0.nrec00#nrec0.name.first")));
		assertThat(p, parse(code("N2#nrec2.nrec1#nrec1.nrec0#nrec0{name = \"nested0a\"}")));
		assertThat(p, parse(code("(PartialMsg#'MegacoMessage'.mess)#'Message'.version")));

	}

	@Test
	public void macroUse() {
		assertThat(p, parse(code("?TIMEOUT")));
		assertThat(p, parse(code("?MACRO1(a, b)")));
		assertThat(p, parse(code("?MACRO1(X, 123)")));
		assertThat(p, parse(code("server:call(refserver, Request, ?TIMEOUT)")));
		assertThat(p, parse(code("server:call(refserver, Request, ?MACRO1(a, b))")));
		assertThat(p, parse(code("?MODULE:report_event(DetailLevel, FromTo, FromTo, Label, Contents)")));
	}

	@Test
	public void ifSimple() {
		g.branchExps.mock();
		assertThat(p, parse(code("if branchExps end")));
	}

	@Test
	public void ifSimple2() {
		g.branchExp.mock();
		assertThat(p, parse(code("if branchExp; branchExp end")));
	}

	@Test
	public void ifSimple3() {
		g.guardSequence.mock();
		g.assignmentExpression.mock();
		p.setRootRule(g.ifExpression);
		assertThat(p,
				parse(code("if guardSequence -> assignmentExpression, assignmentExpression end")));
		assertThat(
				p,
				parse(code("if guardSequence -> assignmentExpression, assignmentExpression; guardSequence -> assignmentExpression end")));
	}

	@Test
	public void ifSimple4() {
		g.guard.mock();
		g.assignmentExpression.mock();
		p.setRootRule(g.ifExpression);
		assertThat(
				p,
				parse(code("if guard; guard; guard -> assignmentExpression, assignmentExpression end")));
		assertThat(
				p,
				parse(code("if guard; guard -> assignmentExpression, assignmentExpression; guard; guard -> assignmentExpression end")));
	}

	@Test
	public void ifSimple5() {
		g.guardExpression.mock();
		g.assignmentExpression.mock();
		p.setRootRule(g.ifExpression);
		assertThat(
				p,
				parse(code("if guardExpression, guardExpression; guardExpression; guardExpression, guardExpression ,guardExpression -> assignmentExpression, assignmentExpression end")));
		assertThat(
				p,
				parse(code("if guardExpression; guardExpression, guardExpression -> assignmentExpression, assignmentExpression; guardExpression, guardExpression; guardExpression -> assignmentExpression end")));
	}
	
	@Test
	public void minus(){
		assertThat(
				p,
				parse(code("t_from_range(-(1 bsl (N - 1)), 1 bsl (N - 1) - 1)")));
	}
	
	private static String code(String... lines) {
		return Joiner.on("\n").join(lines);
	}

	@After
	public void log() {
		try {
			ExtendedStackTraceStream.print(listener, System.out);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
}
