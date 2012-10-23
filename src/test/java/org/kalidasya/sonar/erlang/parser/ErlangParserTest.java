package org.kalidasya.sonar.erlang.parser;

import java.nio.charset.Charset;

import org.junit.Before;
import org.junit.Test;
import org.kalidasya.sonar.erlang.ErlangConfiguration;
import org.kalidasya.sonar.erlang.api.ErlangGrammar;

import com.google.common.base.Charsets;
import com.google.common.base.Joiner;
import com.sonar.sslr.impl.Parser;
import static com.sonar.sslr.test.parser.ParserMatchers.parse;
import static org.junit.Assert.assertThat;

public class ErlangParserTest {

	 Parser<ErlangGrammar> p = ErlangParser.create(new ErlangConfiguration(Charsets.UTF_8));
	  ErlangGrammar g = p.getGrammar();

	  @Before
	  public void init() {
	    p.setRootRule(g.module);
	  }

	  @Test
	  public void realLife() {
	    assertThat(p, parse(code(
	        "-module(m).",
	        "-export([fact/1]).",
	        "",
	        "fact(N) when N>0 ->",
	        "N * fact(N-1);",
	        "fact(0) ->",
	    	"1.")));
	  }

	  private static String code(String... lines) {
	    return Joiner.on("\n").join(lines);
	  }
	
}
