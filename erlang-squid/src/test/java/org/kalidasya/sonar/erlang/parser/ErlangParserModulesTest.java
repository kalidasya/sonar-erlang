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

import java.io.IOException;
import java.net.URISyntaxException;

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

public class ErlangParserModulesTest {
	ExtendedStackTrace listener = new ExtendedStackTrace();
	Parser<ErlangGrammar> p = ErlangParser.create(new ErlangConfiguration(Charsets.UTF_8),
			listener);

	ErlangGrammar g = p.getGrammar();

	@Before
	public void init() {
		p.setRootRule(g.getRootRule());
	}

	@Test
	public void realLife() {
		assertThat(p, parse(code("-module(m).", "-export([fact/1]).", "", "fact(N) when N>0 ->",
				"N * fact(N-1);", "fact(0) ->", "1.")));
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
		assertThat(p, parse(code("-module(m).", "dodo(A) ->", "case A of", "0->",
				"{a, (A + 2), <<0>>} end.")));
	}

	@Test
	public void caseTuple() throws IOException, URISyntaxException {
		assertThat(p, parse(code("-module(m).", "dodo(A) ->", "case A of",
				"{aborted, {already_exists, user}} -> ok end.")));
		assertThat(p, parse(code("-module(m).", "dodo(A) ->", "case A of",
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
		assertThat(p, parse(code("-module(m).", "dodo(A) ->",
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
	public void deepArithmetic() {
		assertThat(p, parse(code("-module(m).", "dodo(A) ->", "2*4.")));

		assertThat(p, parse(code("-module(m).", "dodo(A) ->", "((2)+3)+4*((3+2)*4).")));
		assertThat(p, parse(code("-module(m).", "dodo(A) ->", "((A bsr 4) band 16#f).")));

	}

	@Test
	public void custom() {
		assertThat(p, parse(code("-module(m).", "dodo(A) ->", "Spec = agner:spec(Name).")));
	}

	@Test
	public void noArgFunction() {
		assertThat(p, parse(code("-module(m).", "dodo() ->", "{maci}.")));
	}

	@Test
	public void functionDeclaration() {
		assertThat(p, parse(code("-module(m).", "method(A) -> a.")));
		assertThat(p, parse(code("-module(m).", "method(A)->b+2; method(_,V) -> true.")));
		assertThat(p, parse(code("-module(m).", "method(A) when A+5 >=12 -> a.")));
		assertThat(
				p,
				parse(code(
						"-module(m).",
						"%% Send the last chunk of a fragmented command.",
						"packet_fragment_send(#client{socket=Socket, transport=Transport}, Packet,",
						"Size, Current) when Size - Current =< 16#4000 ->",
						"FragmentSize = 16#10 + byte_size(Packet),",
						"Fragment = << FragmentSize:32/little, 16#0b030000:32, Size:32/little, Current:32/little, Packet/binary >>,",
						"Transport:send(Socket, Fragment);",
						"%% Send another chunk of a fragmented command.",
						"packet_fragment_send(Client=#client{socket=Socket, transport=Transport}, Packet,",
						"Size, Current) ->",
						"<< Chunk:131072/bits, Rest/bits >> = Packet,",
						"Fragment = << 16#10400000:32, 16#0b030000:32, Size:32/little, Current:32/little, Chunk/binary >>,",
						"Transport:send(Socket, Fragment),",
						"packet_fragment_send(Client, Rest, Size, Current + 16#4000).")));
		
		assertThat(p, parse(code("-module(m).","packet_prepare(Packet) ->", "Size = 4 + byte_size(Packet),",
				"case Size rem 4 of", "0 -> {ok, Size, <<>>};", "2 -> {ok, Size + 2, << 0:16 >>};",
				"_ -> {error, badarg}", "end.")));
		
		assertThat(p, parse(code("-module(m).",
				"hexstring(<< X:128/big-unsigned-integer >>) -> ",
				"lists:flatten(io_lib:format(\"~32.16.0b\", [X])).")));
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
		assertThat(p, parse(code("-module(m).", "dodo(A) ->", "#msg{to=void, no=3}.")));
	}

	@Test
	public void ifTest() {
		assertThat(
				p,
				parse(code(
						"-module(m).",
						"dodo(A) ->",
						"if A =:= B -> ok; true -> io:format(\"assert error in module ~p on line ~p~n\", [?MODULE, ?LINE]) end.")));
	}

	@Test
	public void recordInFuncMatch() {
		assertThat(p, parse(code("-module(m).",
				"send_010a(ItemsList, Client=#client{gid=DestGID}) ->", "true.")));

	}

	@Test
	public void recordInFuncCall() {
		assertThat(p, parse(code("-module(m).", "dodo(A) ->",
				"case mnesia:read(user, Username) of",
				"[User] -> mnesia:write(User#user{ibuttons = User#user.ibuttons ++ [IButton]});",
				"E -> E", "end.")));
	}

	@Test
	public void recordSetWithListExp() {
		assertThat(p, parse(code("-module(m).", "dodo(A) ->",
				"User#user{ibuttons = User#user.ibuttons ++ [IButton]}", ".")));
	}

	@Test
	public void nestedBinaryMatch() {
		assertThat(p, parse(code("-module(m).", "dodo(A) ->",
				"UCS2Name = << << X:8, 0:8 >> || << X >> <= <<Name>> >>.")));
	}

	@Test
	public void exports() throws IOException, URISyntaxException {

		assertThat(p, parse(code("-module(m).", "-export([dodo/1]).", "-export(dodo/2).",
				"-export([]).", "dodo(A) ->", "{a, node()}.")));
	}

	@Test
	public void typeTest() throws IOException, URISyntaxException {

		assertThat(p, parse(code("-module(m).", "-export(dodo/1).",
				"-type my_type() :: atom() | integer().",
				"dodo(A) ->",
				"{a, node()}.")));
		
		assertThat(p, parse(code("-module(m).", "-export(dodo/1).",
				"-type my_type() :: {non_reg_integer(), non_reg_integer(), non_reg_integer()}.",
				"dodo(A) ->",
				"{a, node()}.")));

	}
	
	
	@Test
	public void macroDefine(){
		assertThat(p, parse(code("-module(m).", "-define(ASSERT_EQ(A, B), if A =:= B -> ok; true -> io:format(\"assert error in module ~p on line ~p~n\", [?MODULE, ?LINE]) end).",
				"dodo(A) ->","{a, node()}.")));
	}
	
	@Test
	public void flowControlMacros(){
		assertThat(p, parse(code(
			"-module(m).", 
			"-ifdef(debug).",
			"-define(LOG(X), io:format(\"{~p,~p}: ~p~n\", [?MODULE,?LINE,X])).",
			"-else.",
			"-define(LOG(X), true).",
			"-endif.",
			"dodo(A) ->","{a, node()}.")));
	}

	@Test
	public void specTest(){
		assertThat(p, parse(code(
			"-module(m).", 
			"-spec split_nodename(atom() | string()) -> {atom(), nonempty_string()}.",
			"dodo(A) ->","{a, node()}.")));
		
		assertThat(p, parse(code(
				"-module(m).", 
				"-spec test_fun(any(), fun(() -> ok), pos_integer(), pos_integer()) -> {float()}.",
				"dodo(A) ->","{a, node()}.")));
		
		assertThat(p, parse(code(
				"-module(m).", 
				"-spec doit(calendar:datetime(), calendar:datetime()) -> [reload | error | unmodified | gone].",
				"dodo(A) ->","{a, node()}.")));

		assertThat(p, parse(code(
				"-module(m).", 
				"-spec init([]) -> {ok, record(state)}.",
				"dodo(A) ->","{a, node()}.")));
		
		assertThat(p, parse(code(
				"-module(m).", 
				"-spec hexstring(binary()) -> string().",
				"dodo(A) ->","{a, node()}.")));
		
		assertThat(p, parse(code(
				"-module(m).", 
				"-spec join(list(I), Sep) -> list(I | Sep) when Sep :: term(), I :: term().",
				"dodo(A) ->","{a, node()}.")));
		
		assertThat(p, parse(code(
				"-module(m).", 
				"-spec now_ms({MegaSecs::pos_integer(),Secs::pos_integer(),MicroSecs::pos_integer()}) -> pos_integer().",
				"dodo(A) ->","{a, node()}.")));
		
		assertThat(p, parse(code(
				"-module(m).", 
				"-spec trace_named([atom()], pos_integer()) -> {ok, timer:tref()}.",
				"dodo(A) ->","{a, node()}.")));

	}
	
	@Test
	public void moduleAttrTest(){
		assertThat(p, parse(code(
			"-module(m).", 
			"-ignore_xref([{json, decode, 1}]).",
			"dodo(A) ->","{a, node()}.")));
	}
	
	private static String code(String... lines) {
		return Joiner.on("\n").join(lines);
	}

	@After
	public void log() {
		ExtendedStackTraceStream.print(listener, System.out);
	}
}
