package org.kalidasya.sonar.erlang.lexer;

import static com.sonar.sslr.api.GenericTokenType.LITERAL;
import static com.sonar.sslr.impl.channel.RegexpChannelBuilder.commentRegexp;
import static com.sonar.sslr.impl.channel.RegexpChannelBuilder.regexp;
import static org.kalidasya.sonar.erlang.api.ErlangTokenType.NUMERIC_LITERAL;

import org.kalidasya.sonar.erlang.ErlangConfiguration;

import com.sonar.sslr.impl.Lexer;
import com.sonar.sslr.impl.channel.BlackHoleChannel;

public final class ErlangLexer {

	private ErlangLexer(){
	}
	
	public static Lexer create(ErlangConfiguration conf){
		return Lexer.builder()
				.withChannel(new BlackHoleChannel("\\s++"))
				.withChannel(commentRegexp("%[^\\n\\r]*+"))
				.withChannel(regexp(NUMERIC_LITERAL, ""))
				.build();
				
	}
	
}
