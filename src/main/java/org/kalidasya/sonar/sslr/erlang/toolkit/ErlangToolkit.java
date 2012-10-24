package org.kalidasya.sonar.sslr.erlang.toolkit;

import java.nio.charset.Charset;
import java.util.List;

import org.kalidasya.sonar.erlang.ErlangConfiguration;
import org.kalidasya.sonar.erlang.api.ErlangKeyword;
import org.kalidasya.sonar.erlang.parser.ErlangParser;
import org.sonar.colorizer.CDocTokenizer;
import org.sonar.colorizer.CppDocTokenizer;
import org.sonar.colorizer.JavadocTokenizer;
import org.sonar.colorizer.KeywordsTokenizer;
import org.sonar.colorizer.StringTokenizer;
import org.sonar.colorizer.Tokenizer;
import org.sonar.sslr.toolkit.Toolkit;

import com.google.common.collect.ImmutableList;

public final class ErlangToolkit {
	private ErlangToolkit(){
	}
	
	public static void main(String[] args) {
	    System.setProperty("com.apple.mrj.application.apple.menu.about.name", "SSDK");
	    new Toolkit(ErlangParser.create(new ErlangConfiguration(Charset.defaultCharset())), getTokenizers(), "SSLR JavaScript Toolkit").run();
	 }
	
	public static List<Tokenizer> getTokenizers() {
	    return ImmutableList.of(
	        new StringTokenizer("<span class=\"s\">", "</span>"),
	        new CDocTokenizer("<span class=\"cd\">", "</span>"),
	        new JavadocTokenizer("<span class=\"cppd\">", "</span>"),
	        new CppDocTokenizer("<span class=\"cppd\">", "</span>"),
	        new KeywordsTokenizer("<span class=\"k\">", "</span>", ErlangKeyword.keywordValues()));
	  }
}
