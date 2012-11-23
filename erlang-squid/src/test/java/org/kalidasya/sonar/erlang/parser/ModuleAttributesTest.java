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
				"-define(LOG(X), io:format(\"{~p,~p}: ~p~n\", [?MODULE,?LINE,X])).", "-else.",
				"-define(LOG(X), true).", "-endif.")));

	}

	@Test
	public void moduleAttrTest() {
		assertThat(p, parse(code("-ignore_xref([{json, decode, 1}]).")));
	}

	@Test
	public void recordDefTest() {
		assertThat(p,
				parse(code("-record(state, {last::calendar:datetime(), tref::timer:tref()}).")));
		assertThat(p, parse(code("-record(auth, {", "token :: string() | binary()", "}).")));
		assertThat(p, parse(code("-record(map, {dict = dict:new()   :: dict(),",
				"subst = dict:new()  :: dict(),", "modified = []       :: [Key :: term()],",
				"modified_stack = [] :: [{[Key :: term()],reference()}],",
				"ref = undefined     :: reference() | undefined}).")));

		assertThat(
				p,
				parse(code("-record(fun_var, {'fun' :: fun((_) -> erl_types:erl_type()), deps :: [dep()], origin :: integer()}).")));
		
		assertThat(p, parse(code("-record(cat, {}).")));
	}

	@Test
	public void defineTest() {
		assertThat(p, parse(code("-define(PARAM_TOKEN_TIMEOUT,                    60*15).")));

		assertThat(p, parse(code("-define(TC_AWAIT_CANCEL_EVENT(),",
				"case megaco_tc_controller:lookup(block_on_cancel) of",
				"{value, {Tag, Pid}} when is_pid(Pid) ->", "Pid ! {Tag, self()},", "receive",
				"{Tag, Pid} ->", "ok", "end;",
				"{value, {sleep, To}} when is_integer(To) andalso (To > 0) ->",
				"receive after To -> ok end;", "_ ->", "ok", "end).")));
	}

	@Test
	public void onLoadTest() {
		assertThat(p, parse(code("-on_load(init/0).")));
	}

	@Test
	public void typeTest() {
		assertThat(p, parse(code("-type ascii_string() :: [1..255].")));

		assertThat(
				p,
				parse(code("-type timestamp() :: {MegaSecs::non_neg_integer(), Secs::non_neg_integer(), MicroSecs::non_neg_integer()}.")));

		assertThat(p, parse(code("-opaque my_opaq_type() :: Type.")));

		assertThat(p, parse(code("-opaque codeserver() :: #codeserver{}.")));
	}

	@Test
	public void specTest() {
		assertThat(p, parse(code("-spec nif_now/0 :: ( ) -> timestamp().")));
		assertThat(p, parse(code("-spec nif_rot13/1 :: ( ascii_string() ) -> ascii_string().")));

		assertThat(p, parse(code("-spec init([", "non_neg_integer() | callback_module()]) ->",
				"{'ok', #state{	nodes::[],", "table::atom() | ets:tid(),",
				"host_names::maybe_improper_list()", "}", "}.")));
		assertThat(
				p,
				parse(code("-spec in_neighbours(mfa_or_funlbl(), callgraph()) -> 'none' | [mfa_or_funlbl(),...].")));

		assertThat(
				p,
				parse(code("-spec analyze(cerl:c_module()) -> {dict(), ordset('external' | label()), dict()}.")));

		assertThat(
				p,
				parse(code("-spec method(c_module(), Param :: module:flyable(), Param2 :: module:stringiflyable()) -> module:ok_mokes(Id :: integer()).")));

		assertThat(p, parse(code("-spec method(#b{}, {error, {db, any()}},",
				"(fun((id()) -> ok | {error, term()})))",
				"-> {{error, term()} | {ok, id()}, #b{}}.")));
		
		
		assertThat(p, parse(code("-spec test_fun(any(), fun(() -> ok), pos_integer(), pos_integer()) -> {float()}.")));
	}

	@Test
	public void exportTypeTest() {
		assertThat(p, parse(code("-export_type([compile_init_data/0,", "one_file_result/0,",
				"compile_result/0]).")));
	}

	@Test
	public void importTest() {
		assertThat(p, parse(code("-import(erl_types,",
				"[any_none/1, t_any/0, t_atom/0, t_atom/1, t_atom_vals/1,",
				"t_binary/0, t_boolean/0,",
				"t_bitstr/0, t_bitstr/2, t_bitstr_concat/1, t_bitstr_match/2,",
				"t_cons/0, t_cons/2, t_cons_hd/1, t_cons_tl/1, t_contains_opaque/1,",
				"t_find_opaque_mismatch/2, t_float/0, t_from_range/2, t_from_term/1,",
				"t_fun/0, t_fun/2, t_fun_args/1, t_fun_range/1,",
				"t_inf/2, t_inf/3, t_inf_lists/2, t_inf_lists/3, t_inf_lists_masked/3,",
				"t_integer/0, t_integers/1,",
				"t_is_any/1, t_is_atom/1, t_is_atom/2, t_is_boolean/1, t_is_equal/2,",
				"t_is_integer/1, t_is_nil/1, t_is_none/1, t_is_none_or_unit/1,",
				"t_is_number/1, t_is_reference/1, t_is_pid/1, t_is_port/1,",
				"t_is_subtype/2, t_is_unit/1,",
				"t_limit/2, t_list/0, t_maybe_improper_list/0, t_module/0,",
				"t_none/0, t_non_neg_integer/0, t_number/0, t_number_vals/1,",
				"t_opaque_match_atom/2, t_opaque_match_record/2,",
				"t_opaque_matching_structure/2,", "t_pid/0, t_port/0, t_product/1, t_reference/0,",
				"t_sup/1, t_sup/2, t_subtract/2, t_to_string/2, t_to_tlist/1,",
				"t_tuple/0, t_tuple/1, t_tuple_args/1, t_tuple_subtypes/1,",
				"t_unit/0, t_unopaque/1]).")));
	}

	@Test
	public void compileTest() {
		assertThat(p, parse(code("-compile([{nowarn_deprecated_function,{gs,button,2}},",
				"{nowarn_deprecated_function,{gs,config,2}},",
				"{nowarn_deprecated_function,{gs,destroy,1}},",
				"{nowarn_deprecated_function,{gs,editor,2}},",
				"{nowarn_deprecated_function,{gs,entry,2}},",
				"{nowarn_deprecated_function,{gs,frame,2}},",
				"{nowarn_deprecated_function,{gs,label,2}},",
				"{nowarn_deprecated_function,{gs,listbox,2}},",
				"{nowarn_deprecated_function,{gs,menu,2}},",
				"{nowarn_deprecated_function,{gs,menubar,2}},",
				"{nowarn_deprecated_function,{gs,menubutton,2}},",
				"{nowarn_deprecated_function,{gs,menuitem,2}},",
				"{nowarn_deprecated_function,{gs,radiobutton,2}},",
				"{nowarn_deprecated_function,{gs,read,2}},",
				"{nowarn_deprecated_function,{gs,start,0}},",
				"{nowarn_deprecated_function,{gs,stop,0}},",
				"{nowarn_deprecated_function,{gs,window,2}}]).")));
	}

	@Test
	public void fileTest() {
		assertThat(p, parse(code("-file(\"megaco_text_parser_prev3b.yrl\", 1593).")));
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
