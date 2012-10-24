package org.kalidasya.sonar.erlang.parser;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.net.URISyntaxException;
import java.nio.charset.Charset;

import org.junit.Before;
import org.junit.Test;
import org.kalidasya.sonar.erlang.ErlangConfiguration;
import org.kalidasya.sonar.erlang.api.ErlangGrammar;

import com.google.common.base.Charsets;
import com.google.common.base.Joiner;
import com.google.common.io.Files;
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
		assertThat(p, parse(code("-module(m).", "-export([fact/1]).", "", "fact(N) when N>0 ->",
				"N * fact(N-1);", "fact(0) ->", "1.")));
	}

	@Test
	public void realLife2() throws IOException, URISyntaxException {
		
		assertThat(p, parse(readFromFile("user_auth_mnesia.erl")));
	}
	
	@Test
	public void realLife3() throws IOException, URISyntaxException {
		
		assertThat(p, parse(code("-module(m).","dodo(A) ->","{a, node()}.")));
	}


	private static String code(String... lines) {
		return Joiner.on("\n").join(lines);
	}

	private String readFromFile(String fileName) throws IOException, URISyntaxException {
		StringBuilder text = new StringBuilder();
		File file = new File(ErlangParserTest.class.getClassLoader().getResource(fileName).toURI());
		BufferedReader reader = Files.newReader(file, Charsets.UTF_8);
		String line = null;
		while ((line = reader.readLine()) != null) {
			text.append(line).append("\n");
		}
		return text.toString();
	}

}
