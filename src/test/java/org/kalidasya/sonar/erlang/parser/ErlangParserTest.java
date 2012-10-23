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
	    assertThat(p, parse("{}"));
	    assertThat(p, parse("var a;"));
	    assertThat(p, parse("if (true) {}"));
	    assertThat(p, parse("document.write(\"Hello world\");"));
	    assertThat(p, parse("var r = /^\\s+/;"));
	    assertThat(p, parse("function func() { doSomething() }"));

	    // http://www.w3schools.com/js/tryit.asp?filename=tryjs_ifthenelse
	    assertThat(p, parse(code(
	        "var d = new Date();",
	        "var time = d.getHours();",
	        "if (time < 10) {",
	        " document.write(\"Good morning\");",
	        "} else {",
	        " document.write(\"Good day\");",
	        "}")));
	  }

	  private static String code(String... lines) {
	    return Joiner.on("\n").join(lines);
	  }
	
}
