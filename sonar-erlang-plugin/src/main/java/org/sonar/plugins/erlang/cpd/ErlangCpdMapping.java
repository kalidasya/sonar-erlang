package org.sonar.plugins.erlang.cpd;

import java.nio.charset.Charset;

import net.sourceforge.pmd.cpd.Tokenizer;

import org.sonar.api.batch.AbstractCpdMapping;
import org.sonar.api.resources.Language;
import org.sonar.api.resources.ProjectFileSystem;
import org.sonar.plugins.erlang.core.Erlang;

public class ErlangCpdMapping extends AbstractCpdMapping {

  private final Erlang language;
  private final Charset charset;

  public ErlangCpdMapping(Erlang language, ProjectFileSystem fs) {
    this.language = language;
    this.charset = fs.getSourceCharset();
  }

  public Tokenizer getTokenizer() {
    return new ErlangTokenizer(charset);
  }

  public Language getLanguage() {
    return language;
  }

}
