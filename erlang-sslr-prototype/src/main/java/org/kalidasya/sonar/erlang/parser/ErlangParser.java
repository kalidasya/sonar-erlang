package org.kalidasya.sonar.erlang.parser;

import org.kalidasya.sonar.erlang.ErlangConfiguration;
import org.kalidasya.sonar.erlang.api.ErlangGrammar;
import org.kalidasya.sonar.erlang.lexer.ErlangLexer;

import com.sonar.sslr.impl.Parser;
import com.sonar.sslr.impl.events.ParsingEventListener;

public final class ErlangParser {
	private ErlangParser(){
		
	}

	public static Parser<ErlangGrammar> create(ErlangConfiguration conf, ParsingEventListener... parsingEventListeners) {
	    return Parser.builder((ErlangGrammar) new ErlangGrammarImpl())
	        .withLexer(ErlangLexer.create(conf))
	        .setParsingEventListeners(parsingEventListeners).build();
	  }
	
}
