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

public class ErlangParserBinaryExpressionTest {
	ExtendedStackTrace listener = new ExtendedStackTrace();
	Parser<ErlangGrammar2> p = ErlangParser2.create(new ErlangConfiguration(
			Charsets.UTF_8), listener);

	ErlangGrammar2 g = p.getGrammar();

	@Before
	public void init() {
		p.setRootRule(g.binaryLiteral);
	}


	@Test
	public void binaryExpressions() {
		assertThat(p, parse(code("<<1,17,42>>")));
		assertThat(p, parse(code("<<1,17,42:16>>")));
		assertThat(p, parse(code("<<1024/utf8>>")));
		assertThat(p, parse(code("<<1024:16/utf8>>")));
		assertThat(p, parse(code("<<$a,$b,$c>>")));
		assertThat(p, parse(code("<<\"hello\">>")));
		assertThat(p, parse(code("<<A,B,C:16>>")));
		assertThat(p, parse(code("<<G,H/binary>>")));
		assertThat(p, parse(code("<<G,H:16/bitstring>>")));
		assertThat(p, parse(code("<< << X:8, 0:8/utf8 >> || << X >> <= << 1, A, 3 >> >>")));
	}
	
	private static String code(String... lines) {
		return Joiner.on("\n").join(lines);
	}

	@After
	public void log() {
		ExtendedStackTraceStream.print(listener, System.out);
	}
	
}
