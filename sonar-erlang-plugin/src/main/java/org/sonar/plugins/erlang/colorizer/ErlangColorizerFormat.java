package org.sonar.plugins.erlang.colorizer;

import java.util.List;

import org.kalidasya.sonar.erlang.api.ErlangKeyword;
import org.sonar.api.web.CodeColorizerFormat;
import org.sonar.colorizer.CDocTokenizer;
import org.sonar.colorizer.CppDocTokenizer;
import org.sonar.colorizer.JavadocTokenizer;
import org.sonar.colorizer.KeywordsTokenizer;
import org.sonar.colorizer.StringTokenizer;
import org.sonar.colorizer.Tokenizer;
import org.sonar.plugins.erlang.core.Erlang;

import com.google.common.collect.ImmutableList;

public class ErlangColorizerFormat extends CodeColorizerFormat {

  public ErlangColorizerFormat() {
    super(Erlang.KEY);
  }

  @Override
  public List<Tokenizer> getTokenizers() {
    return ImmutableList.of(
        new StringTokenizer("<span class=\"s\">", "</span>"),
        new CDocTokenizer("<span class=\"cd\">", "</span>"),
        new JavadocTokenizer("<span class=\"cppd\">", "</span>"),
        new CppDocTokenizer("<span class=\"cppd\">", "</span>"),
        new KeywordsTokenizer("<span class=\"k\">", "</span>", ErlangKeyword.keywordValues()));
  }

}
