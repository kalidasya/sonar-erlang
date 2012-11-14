package org.sonar.plugins.erlang.core.cpd;

import static org.fest.assertions.Assertions.assertThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.io.File;
import java.nio.charset.Charset;

import net.sourceforge.pmd.cpd.SourceCode;
import net.sourceforge.pmd.cpd.TokenEntry;
import net.sourceforge.pmd.cpd.Tokens;

import org.junit.Test;
import org.sonar.plugins.erlang.cpd.ErlangTokenizer;

public class ErlangTokenizerTest {

	@Test
	public void test() {
		ErlangTokenizer tokenizer = new ErlangTokenizer(Charset.forName("UTF-8"));
		SourceCode source = mock(SourceCode.class);
		when(source.getFileName()).thenReturn(
				new File("src/test/resources/cpd/person.erl").getAbsolutePath());
		Tokens tokens = new Tokens();
		tokenizer.tokenize(source, tokens);
		assertThat(tokens.getTokens().size()).isGreaterThan(1);
		assertThat(tokens.getTokens().get(tokens.size() - 1)).isEqualTo(TokenEntry.getEOF());
	}

}
