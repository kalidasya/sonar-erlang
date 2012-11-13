package org.sonar.plugins.erlang;

import static org.fest.assertions.Assertions.assertThat;

import org.junit.Test;

public class ErlangCommonRulesEngineProviderTest {

  @Test
  public void shouldProvideExpectedExtensions() {
    ErlangCommonRulesEngineProvider provider = new ErlangCommonRulesEngineProvider();
    assertThat(provider.provide().size()).isGreaterThan(1);

    provider = new ErlangCommonRulesEngineProvider(null);
    assertThat(provider.provide().size()).isGreaterThan(1);
  }

}
