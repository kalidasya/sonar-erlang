package org.kalidasya.sonar.erlang.parser;

import org.kalidasya.sonar.erlang.ErlangConfiguration;
import org.kalidasya.sonar.erlang.api.ErlangGrammar2;
import org.kalidasya.sonar.erlang.lexer.ErlangLexer;

import com.sonar.sslr.impl.Parser;
import com.sonar.sslr.impl.events.ParsingEventListener;

public final class ErlangParser2 {
	private ErlangParser2(){
		
	}

	public static Parser<ErlangGrammar2> create(ErlangConfiguration conf, ParsingEventListener... parsingEventListeners) {
	    return Parser.builder((ErlangGrammar2) new ErlangGrammarImpl2())
	        .withLexer(ErlangLexer.create(conf))
	        .setParsingEventListeners(parsingEventListeners).build();
	  }
	
}
