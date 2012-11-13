package org.sonar.plugins.erlang.cpd;

import java.io.File;
import java.nio.charset.Charset;
import java.util.List;

import net.sourceforge.pmd.cpd.SourceCode;
import net.sourceforge.pmd.cpd.TokenEntry;
import net.sourceforge.pmd.cpd.Tokenizer;
import net.sourceforge.pmd.cpd.Tokens;

import org.kalidasya.sonar.erlang.ErlangConfiguration;
import org.kalidasya.sonar.erlang.lexer.ErlangLexer;

import com.sonar.sslr.api.GenericTokenType;
import com.sonar.sslr.api.Token;
import com.sonar.sslr.impl.Lexer;

public class ErlangTokenizer implements Tokenizer {

  private final Charset charset;

  public ErlangTokenizer(Charset charset) {
    this.charset = charset;
  }

  public final void tokenize(SourceCode source, Tokens cpdTokens) {
    Lexer lexer = ErlangLexer.create(new ErlangConfiguration(charset));
    String fileName = source.getFileName();
    List<Token> tokens = lexer.lex(new File(fileName));
    for (Token token : tokens) {
      TokenEntry cpdToken = new TokenEntry(getTokenImage(token), fileName, token.getLine());
      cpdTokens.add(cpdToken);
    }
    cpdTokens.add(TokenEntry.getEOF());
  }

  private String getTokenImage(Token token) {
    if (token.getType() == GenericTokenType.LITERAL) {
      return GenericTokenType.LITERAL.getValue();
    }
    return token.getValue();
  }

}
