package org.kalidasya.sonar.erlang.parser;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.net.URISyntaxException;
import java.nio.charset.Charset;

import org.junit.After;
import org.junit.Before;
import org.junit.Ignore;
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

@Ignore
public class ErlangParserTest {
	ExtendedStackTrace listener = new ExtendedStackTrace();
	Parser<ErlangGrammar> p = ErlangParser.create(new ErlangConfiguration(
			Charsets.UTF_8), listener);

	ErlangGrammar g = p.getGrammar();

	@Before
	public void init() {
		p.setRootRule(g.module);
	}

	@Test
	public void realLife() {
		assertThat(
				p,
				parse(code("-module(m).", "-export([fact/1]).", "",
						"fact(N) when N>0 ->", "N * fact(N-1);", "fact(0) ->",
						"1.")));
	}

	@Test
	public void realLife2() throws IOException, URISyntaxException {
		assertThat(p, parse(readFromFile("user_auth_mnesia.erl")));
	}

	@Test
	public void realLife3() throws IOException, URISyntaxException {
		assertThat(p, parse(readFromFile("agner_main.erl")));
	}

	@Test
	public void realLife4() throws IOException, URISyntaxException {
		assertThat(p, parse(readFromFile("erl_img.erl")));
	}
	
	@Test
	public void realLife5() throws IOException, URISyntaxException {
		assertThat(p, parse(readFromFile("egs_proto.erl")));
	}
	
	@Test
	public void tuple() throws IOException, URISyntaxException {
		assertThat(p, parse(code("-module(m).", "dodo(A) ->", "{a, node()}.")));
	}

	@Test
	public void returnWithNumber() throws IOException, URISyntaxException {
		assertThat(p, parse(code("-module(m).", "dodo(A) ->", "1.")));
	}

	@Test
	public void returnWithCalc() throws IOException, URISyntaxException {
		assertThat(p, parse(code("-module(m).", "dodo(A) ->", "{a, A + 2}.")));
	}
	
	@Test
	public void returnWithCalcCase() throws IOException, URISyntaxException {
		assertThat(p, parse(code("-module(m).", "dodo(A) ->","case A of",
				"0->", "{a, (A + 2), <<0>>} end.")));
	}
	
	@Test
	public void caseTuple() throws IOException, URISyntaxException {
		assertThat(
				p,
				parse(code("-module(m).", "dodo(A) ->", "case A of",
						"{aborted, {already_exists, user}} -> ok end.")));
		assertThat(
				p,
				parse(code("-module(m).", "dodo(A) ->", "case A of",
						"{atomic, ok} -> init_user_data();",
						" {aborted, {already_exists, user}} -> ok end.")));
	}

	@Test
	public void deepFuncArg() {
		assertThat(
				p,
				parse(code(
						"-module(m).",
						"dodo(A) ->",
						"io:format(\"~s~n\",[agner_spec:property_to_list(lists:keyfind(list_to_atom(Property), 1, Spec))]).")));
	}

	@Test
	public void booleanReturn() {
		assertThat(
				p,
				parse(code(
						"-module(m).",
						"dodo(A) ->",
						"string:rstr(Searchable, string:to_lower(Search)) > 0.")));
	}
	
	
	
	@Test
	public void deepFuncArg2() {
		assertThat(
				p,
				parse(code(
						"-module(m).",
						"dodo(A) ->",
						"io:format(\"~s~n\",fun (Name) ->"
								+ "Spec = agner:spec(Name),"
								+ "Searchable = string:to_lower(lists:flatten([Name,proplists:get_value(description,Spec,[])|proplists:get_value(keywords,Spec,[])]))"
								+ "end).")));
	}

	@Test
	public void deepArithmetic(){
		assertThat(
				p,
				parse(code(
						"-module(m).",
						"dodo(A) ->",
						"2*4.")));
		
		assertThat(
				p,
				parse(code(
						"-module(m).",
						"dodo(A) ->",
						"((2)+3)+4*((3+2)*4).")));
		assertThat(
				p,
				parse(code(
						"-module(m).",
						"dodo(A) ->",
						"((A bsr 4) band 16#f).")));
		
	}
	
	@Test
	public void custom() {
		assertThat(
				p,
				parse(code("-module(m).", "dodo(A) ->",
						"Spec = agner:spec(Name).")));
	}
	
	@Test
	public void noArgFunction() {
		assertThat(
				p,
				parse(code("-module(m).", "dodo() ->",
						"{maci}.")));
	}
	
	@Test
	public void funWithArity() {
		assertThat(
				p,
				parse(code(
						"-module(m).",
						"dodo(A) ->",
						"Properties = lists:map(fun list_to_atom/1, string:tokens(proplists:get_value(properties, Opts,\"\"),\",\")).")));
	}

	@Test
	public void emptyArgFuncCall() throws IOException, URISyntaxException {

		assertThat(p, parse(code("-module(m).", "dodo(A) ->", "integer().")));
	}

	@Test
	public void recordSet() throws IOException, URISyntaxException {
		assertThat(
				p,
				parse(code("-module(m).", "dodo(A) ->", "#msg{to=void, no=3}.")));
	}

	@Test
	public void ifTest(){
		assertThat(
				p,
				parse(code("-module(m).", "dodo(A) ->", "if A =:= B -> ok; true -> io:format(\"assert error in module ~p on line ~p~n\", [?MODULE, ?LINE]) end.")));
	}
	
	@Test
	public void recordInFuncMatch(){
		assertThat(
				p,
				parse(code("-module(m).", "send_010a(ItemsList, Client=#client{gid=DestGID}) ->", "true.")));
		
	}
	

	@Test
	public void recordInFuncCall(){
		assertThat(
				p,
				parse(code(	"-module(m).", 
							"dodo(A) ->",
							"case mnesia:read(user, Username) of",
							"[User] -> mnesia:write(User#user{ibuttons = User#user.ibuttons ++ [IButton]});",
							"E -> E",
							"end.")));
	}
	
	@Test
	public void recordSetWithListExp(){
		assertThat(
				p,
				parse(code(	"-module(m).", 
							"dodo(A) ->",
							"User#user{ibuttons = User#user.ibuttons ++ [IButton]}",
							".")));
	}
	
	@Test
	public void nestedBinaryMatch(){
		assertThat(
				p,
				parse(code(	"-module(m).", 
							"dodo(A) ->",
							"UCS2Name = << << X:8, 0:8 >> || X <- Name >>.")));
	}
	
	@Test
	public void exports() throws IOException, URISyntaxException {

		assertThat(
				p,
				parse(code("-module(m).", "-export([dodo/1]).",
						"-export(dodo/2).", "-export([]).", "dodo(A) ->",
						"{a, node()}.")));
	}

	@Test
	public void specs() throws IOException, URISyntaxException {

		assertThat(
				p,
				parse(code("-module(m).",
						"-type my_type() :: atom() | integer().",
						"-spec my_function(integer()) -> integer().",
						"-export(dodo/1).", "dodo(A) ->", "{a, node()}.")));
	}

	private static String code(String... lines) {
		return Joiner.on("\n").join(lines);
	}

	private String readFromFile(String fileName) throws IOException,
			URISyntaxException {
		StringBuilder text = new StringBuilder();
		File file = new File(ErlangParserTest.class.getClassLoader()
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
