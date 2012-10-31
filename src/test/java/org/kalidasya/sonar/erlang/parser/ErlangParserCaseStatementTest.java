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

public class ErlangParserCaseStatementTest {
	ExtendedStackTrace listener = new ExtendedStackTrace();
	Parser<ErlangGrammar2> p = ErlangParser2.create(new ErlangConfiguration(
			Charsets.UTF_8), listener);

	ErlangGrammar2 g = p.getGrammar();

	@Before
	public void init() {
		p.setRootRule(g.statement);
	}


	@Test
	public void caseSimple1() {
		g.expression.mock();
		g.patternStatements.mock();
		assertThat(p, parse(code("case expression of patternStatements end")));
	}

	@Test
	public void caseSimple2() {
		g.expression.mock();
		g.patternStatement.mock();
		assertThat(p, parse(code("case expression of patternStatement end")));
		assertThat(p, parse(code("case expression of patternStatement; patternStatement end")));
	}
	
	@Test
	public void caseSimple3() {
		g.expression.mock();
		g.patternStatement.mock();
		assertThat(p, parse(code("case expression of patternStatement end")));
		assertThat(p, parse(code("case expression of patternStatement; patternStatement end")));
	}
	
	private static String code(String... lines) {
		return Joiner.on("\n").join(lines);
	}

	@After
	public void log() {
		ExtendedStackTraceStream.print(listener, System.out);
	}
	
}
