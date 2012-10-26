package org.kalidasya.sonar.erlang.parser;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.net.URISyntaxException;
import java.nio.charset.Charset;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.kalidasya.sonar.erlang.ErlangConfiguration;
import org.kalidasya.sonar.erlang.api.ErlangGrammar;

import com.google.common.base.Charsets;
import com.google.common.base.Joiner;
import com.google.common.io.Files;
import com.sonar.sslr.impl.Parser;
import com.sonar.sslr.impl.events.ExtendedStackTrace;
import com.sonar.sslr.impl.events.ExtendedStackTraceStream;

import static com.sonar.sslr.test.parser.ParserMatchers.parse;
import static org.junit.Assert.assertThat;

public class ErlangParserTest {
	ExtendedStackTrace listener = new ExtendedStackTrace();
	Parser<ErlangGrammar> p = ErlangParser.create(new ErlangConfiguration(Charsets.UTF_8), listener);


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
	public void tuple() throws IOException, URISyntaxException {
		assertThat(p, parse(code("-module(m).","dodo(A) ->","{a, node()}.")));
	}
	
	@Test
	public void returnWithNumber() throws IOException, URISyntaxException {
		assertThat(p, parse(code("-module(m).","dodo(A) ->","1.")));
	}

	@Test
	public void caseTuple() throws IOException, URISyntaxException {
		assertThat(p, parse(code("-module(m).","dodo(A) ->","case A of", "{aborted, {already_exists, user}} -> ok end.")));
		assertThat(p, parse(code("-module(m).","dodo(A) ->","case A of","{atomic, ok} -> init_user_data();", " {aborted, {already_exists, user}} -> ok end.")));
	}
	
	
	
	@Test
	public void emptyArgFuncCall() throws IOException, URISyntaxException {
		
		assertThat(p, parse(code("-module(m).","dodo(A) ->","integer().")));
	}
	
	@Test
	public void recordSet() throws IOException, URISyntaxException {
		
		assertThat(p, parse(code("-module(m).","dodo(A) ->","#msg{to=void, no=3}.")));
	}
	
	@Test
	public void exports() throws IOException, URISyntaxException {
		
		assertThat(p, parse(code("-module(m).", "-export([dodo/1]).","-export(dodo/2).","-export([]).", "dodo(A) ->","{a, node()}.")));
	}

	@Test
	public void specs() throws IOException, URISyntaxException {
		
		assertThat(p, parse(code("-module(m).", "-type my_type() :: atom() | integer().", "-spec my_function(integer()) -> integer().", "-export(dodo/1).", "dodo(A) ->","{a, node()}.")));
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
	
	@After
	public void log(){
		ExtendedStackTraceStream.print(listener, System.out);
	}

}
