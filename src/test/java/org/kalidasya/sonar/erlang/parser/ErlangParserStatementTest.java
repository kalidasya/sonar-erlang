package org.kalidasya.sonar.erlang.parser;

import static com.sonar.sslr.test.parser.ParserMatchers.parse;
import static org.junit.Assert.assertThat;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.kalidasya.sonar.erlang.ErlangConfiguration;
import org.kalidasya.sonar.erlang.api.ErlangGrammar2;

import com.google.common.base.Charsets;
import com.google.common.base.Joiner;
import com.sonar.sslr.impl.Parser;
import com.sonar.sslr.impl.events.ExtendedStackTrace;
import com.sonar.sslr.impl.events.ExtendedStackTraceStream;

public class ErlangParserStatementTest {
	ExtendedStackTrace listener = new ExtendedStackTrace();
	Parser<ErlangGrammar2> p = ErlangParser2.create(new ErlangConfiguration(
			Charsets.UTF_8), listener);

	ErlangGrammar2 g = p.getGrammar();

	@Before
	public void init() {
		p.setRootRule(g.statements);
	}

	@Test
	public void statements() {
		assertThat(p, parse(code("1,", "A")));
		assertThat(p, parse(code("1+3,", "<<A>>")));
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
		g.expression.mock();
		assertThat(p,
				parse(code("if guardSequence -> expression, expression end")));
		assertThat(
				p,
				parse(code("if guardSequence -> expression, expression; guardSequence -> expression end")));
	}

	@Test
	public void ifSimple4() {
		g.guard.mock();
		g.expression.mock();
		assertThat(
				p,
				parse(code("if guard; guard; guard -> expression, expression end")));
		assertThat(
				p,
				parse(code("if guard; guard -> expression, expression; guard; guard -> expression end")));
	}

	@Test
	public void ifSimple5() {
		g.guardExpression.mock();
		g.expression.mock();
		assertThat(
				p,
				parse(code("if guardExpression, guardExpression; guardExpression; guardExpression, guardExpression ,guardExpression -> expression, expression end")));
		assertThat(
				p,
				parse(code("if guardExpression; guardExpression, guardExpression -> expression, expression; guardExpression, guardExpression; guardExpression -> expression end")));
	}

	@Test
	public void ifStatements() {
		assertThat(p, parse(code("if A =:= B -> ok end")));
		assertThat(
				p,
				parse(code("if A =:= B -> ok; true -> io:format(\"assert error in module ~p on line ~p~n\") end")));
	}

	@Test
	public void funStatements() {

		assertThat(p, parse(code("fun (Name) ->" + "Spec = agner:spec(Name),"
				+ "Searchable = string:to_lower(\"hElO\")" + "end")));

		assertThat(
				p,
				parse(code("fun	(Name) ->", "Spec = agner:spec(Name),",
						"Searchable = string:to_lower(\"hElO\");",
						"(Name, 23) when Name>=2 ->",
						"Spec = agner:spec(Name),",
						"Searchable = string:to_lower(\"hElO\")", "end")));
	}

	@Test
	public void caseStatements() {
		assertThat(
				p,
				parse(code("case Signal of", "{signal, _What, _From, _To} ->",
						"true;", "{signal, _What, _To} ->", "true;",
						"_Else -> false", "end")));
	}

	@Test
	public void sendStatements() {
		assertThat(p, parse(code("Client ! {self(), data_sent}")));
		assertThat(p, parse(code("Client ! {self(), data_sent}, A")));
		assertThat(p, parse(code("B, Client ! {self(), data_sent}, A")));
	}

	@Test
	public void receiveStatements() {
		assertThat(
				p,
				parse(code("receive", "onhook ->", "disconnect(),", "idle();",
						"{connect, B} ->", "B ! {busy, self()},",
						"wait_for_onhook()", "after", "60000 ->",
						"disconnect(),", "error()", "end")));
	}

	@Test
	public void tryStatements() {
		assertThat(
				p,
				parse(code("try Exprs of Pattern when GuardSeq -> Body after AfterBody end")));

		assertThat(
				p,
				parse(code("try Exprs catch ExpressionPattern -> ExpressionBody after AfterBody end")));

		assertThat(p, parse(code("try Exprs after AfterBody end")));

		assertThat(
				p,
				parse(code("try", "{ok,Bin} = file:read(F, 1024*1024),",
						"binary_to_term(Bin)", "after", "file:close(F)", "end")));

		assertThat(
				p,
				parse(code(
						"try Expr",
						"catch",
						"throw:Term -> Term;",
						"exit:Reason -> {'EXIT',Reason};",
						"error:Reason -> {'EXIT',{Reason,erlang:get_stacktrace()}}",
						"end")));
	}
	
	@Test
	public void blockStatements() {
		assertThat(p, parse(code("begin a, S=2 end")));
	}

	private static String code(String... lines) {
		return Joiner.on("\n").join(lines);
	}

	@After
	public void log() {
		ExtendedStackTraceStream.print(listener, System.out);
	}
}
