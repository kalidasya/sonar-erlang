package org.sonar.plugins.erlang.colorizer;

import static junit.framework.Assert.fail;
import static org.fest.assertions.Assertions.assertThat;

import java.util.List;

import org.junit.Test;
import org.sonar.colorizer.CppDocTokenizer;
import org.sonar.colorizer.JavadocTokenizer;
import org.sonar.colorizer.Tokenizer;

public class ErlangColorizerFormatTest {

	@Test
	public void testGetTokenizers() {
		List<Tokenizer> list = (new ErlangColorizerFormat()).getTokenizers();
		assertThat(indexOf(list, JavadocTokenizer.class)).isLessThan(
				indexOf(list, CppDocTokenizer.class));
	}

	private Integer indexOf(List<Tokenizer> tokenizers, Class tokenizerClass) {
		for (int i = 0; i < tokenizers.size(); i++) {
			if (tokenizers.get(i).getClass().equals(tokenizerClass)) {
				return i;
			}
		}

		fail("Tokenizer not found: " + tokenizerClass);
		return null;
	}
}
