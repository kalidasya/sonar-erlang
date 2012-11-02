package org.kalidasya.sonar.erlang.parser;

import static com.sonar.sslr.test.parser.ParserMatchers.parse;
import static org.junit.Assert.assertThat;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.net.URISyntaxException;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.kalidasya.sonar.erlang.ErlangConfiguration;
import org.kalidasya.sonar.erlang.api.ErlangGrammar;

import com.google.common.base.Charsets;
import com.google.common.io.Files;
import com.sonar.sslr.impl.Parser;
import com.sonar.sslr.impl.events.ExtendedStackTrace;
import com.sonar.sslr.impl.events.ExtendedStackTraceStream;

public class ErlangRealCodeTest {
	ExtendedStackTrace listener = new ExtendedStackTrace();
	Parser<ErlangGrammar> p = ErlangParser.create(new ErlangConfiguration(
			Charsets.UTF_8), listener);

	ErlangGrammar g = p.getGrammar();

	@Before
	public void init() {
		p.setRootRule(g.module);
	}

	@Test
	public void realLife2() throws IOException, URISyntaxException {
		assertThat(p, parse(readFromFile("user_auth_mnesia.erl")));
	}

	@Test
	public void realLife3() throws IOException, URISyntaxException {
		assertThat(p, parse(readFromFile("agner_main_sub.erl")));
	}

	@Test
	public void realLife4() throws IOException, URISyntaxException {
		assertThat(p, parse(readFromFile("erl_img.erl")));
	}
	
	@Test
	public void realLife5() throws IOException, URISyntaxException {
		assertThat(p, parse(readFromFile("egs_proto.erl")));
	}
	

	private String readFromFile(String fileName) throws IOException,
			URISyntaxException {
		StringBuilder text = new StringBuilder();
		File file = new File(ErlangRealCodeTest.class.getClassLoader()
				.getResource(fileName).toURI());
		BufferedReader reader = Files.newReader(file, Charsets.UTF_8);
		String line = null;
		while ((line = reader.readLine()) != null) {
			text.append(line).append("\n");
		}
		return text.toString();
	}

	@After
	public void log() {
		ExtendedStackTraceStream.print(listener, System.out);
	}

}
