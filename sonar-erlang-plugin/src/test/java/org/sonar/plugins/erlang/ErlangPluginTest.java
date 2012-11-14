package org.sonar.plugins.erlang;

import static org.fest.assertions.Assertions.assertThat;

import org.junit.Before;
import org.junit.Test;

public class ErlangPluginTest {

  private ErlangPlugin plugin;

  @Before
  public void setUp() throws Exception {
    plugin = new ErlangPlugin();
  }

  @Test
  public void testGetExtensions() throws Exception {
    assertThat(plugin.getExtensions().size()).isEqualTo(8);
  }

}
