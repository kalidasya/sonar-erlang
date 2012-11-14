package org.sonar.plugins.erlang.core.cpd;

import static org.fest.assertions.Assertions.assertThat;
import static org.mockito.Mockito.mock;

import org.junit.Test;
import org.sonar.api.resources.ProjectFileSystem;
import org.sonar.plugins.erlang.core.Erlang;
import org.sonar.plugins.erlang.cpd.ErlangCpdMapping;
import org.sonar.plugins.erlang.cpd.ErlangTokenizer;

public class ErlangCpdMappingTest {

	@Test
	public void test() {
		Erlang language = mock(Erlang.class);
		ProjectFileSystem fs = mock(ProjectFileSystem.class);
		ErlangCpdMapping mapping = new ErlangCpdMapping(language, fs);
		assertThat(mapping.getLanguage()).isSameAs(language);
		assertThat(mapping.getTokenizer()).isInstanceOf(ErlangTokenizer.class);
	}

}
