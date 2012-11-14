package org.sonar.plugins.erlang.core;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertSame;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import org.apache.commons.configuration.Configuration;
import org.junit.Before;
import org.junit.Test;
import org.sonar.plugins.erlang.ErlangPlugin;

public class ErlangTest {

  private Configuration configuration;
  private Erlang erlang;

  @Before
  public void setUp() {
    configuration = mock(Configuration.class);
    erlang = new Erlang(configuration);
  }

  @Test
  public void defaultSuffixes() {
    when(configuration.getStringArray(ErlangPlugin.FILE_SUFFIXES_KEY))
        .thenReturn(null)
        .thenReturn(new String[] {});
    assertArrayEquals(erlang.getFileSuffixes(), new String[] {"erl"});
    assertArrayEquals(erlang.getFileSuffixes(), new String[] {"erl"});
    assertSame(configuration, erlang.getConfiguration());
  }

  @Test
  public void customSuffixes() {
    when(configuration.getStringArray(ErlangPlugin.FILE_SUFFIXES_KEY)).thenReturn(new String[] {"erlang"});
    assertArrayEquals(erlang.getFileSuffixes(), new String[] {"erlang"});
  }

}
