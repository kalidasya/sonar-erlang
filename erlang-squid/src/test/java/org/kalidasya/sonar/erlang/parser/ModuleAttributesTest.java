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

public class ModuleAttributesTest {
	ExtendedStackTrace listener = new ExtendedStackTrace();
	Parser<ErlangGrammar> p = ErlangParser
			.create(new ErlangConfiguration(Charsets.UTF_8), listener);

	ErlangGrammar g = p.getGrammar();

	@Before
	public void init() {
		p.setRootRule(g.moduleHeadAttr);
	}

	@Test
	public void moduleTest() {
		assertThat(p, parse(code("-module(m).")));
	}

	@Test
	public void flowControlMacros() {
		assertThat(p, parse(code("-ifdef(debug).",
				"-define(LOG(X), io:format(\"{~p,~p}: ~p~n\", [?MODULE,?LINE,X])).", 
				"-else.",
				"-define(LOG(X), true).", 
				"-endif.")));
	}

	@Test
	public void moduleAttrTest() {
		assertThat(p, parse(code("-ignore_xref([{json, decode, 1}]).")));
	}
	
	@Test
	public void recordDefTest() {
		assertThat(p, parse(code("-record(state, {last::calendar:datetime(), tref::timer:tref()}).")));
		assertThat(p, parse(code("-record(auth, {","token :: string() | binary()","}).")));
		
	}
	
	@Test
	public void defineTest() {
		assertThat(p, parse(code("-define(PARAM_TOKEN_TIMEOUT,                    60*15).")));
	}
	
	@Test
	public void onLoadTest() {
		assertThat(p, parse(code("-on_load(init/0).")));
	}
	
	@Test
	public void typeTest() {
		assertThat(p, parse(code("-type ascii_string() :: [1..255].")));
		
		assertThat(p, parse(code("-type timestamp() :: {MegaSecs::non_neg_integer(), Secs::non_neg_integer(), MicroSecs::non_neg_integer()}.")));
	}

	@Test
	public void specTest(){
		assertThat(p, parse(code("-spec nif_now/0 :: ( ) -> timestamp().")));
		assertThat(p, parse(code("-spec nif_rot13/1 :: ( ascii_string() ) -> ascii_string().")));
		
		assertThat(p, parse(code("-spec init([",
		          					"non_neg_integer() | callback_module()]) ->",
		          						"{'ok', #state{	nodes::[],",
		          									   "table::atom() | ets:tid(),",
		          									   "host_names::maybe_improper_list()",
		          									 "}",
										"}.")));
	}
	
	@Test
	public void exportTypeTest(){
		assertThat(p, parse(code("-export_type([compile_init_data/0,",
	              "one_file_result/0,",
	              "compile_result/0]).")));
	}
	
	private static String code(String... lines) {
		return Joiner.on("\n").join(lines);
	}

	@After
	public void log() {
		try {
			ExtendedStackTraceStream.print(listener, System.out);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
}
