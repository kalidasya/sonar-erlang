/*
 * Sonar Erlang Plugin
 * Copyright (C) 2011 Eriks Nukis and SonarSource
 * dev@sonar.codehaus.org
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02
 */
package org.kalidasya.sonar.erlang;

import static org.fest.assertions.Assertions.assertThat;

import java.io.File;

import org.junit.Test;
import org.kalidasya.sonar.erlang.api.ErlangGrammar;
import org.kalidasya.sonar.erlang.api.ErlangMetric;
import org.sonar.squid.api.SourceFile;
import org.sonar.squid.api.SourceProject;
import org.sonar.squid.indexer.QueryByType;

import com.google.common.base.Charsets;
import com.google.common.collect.ImmutableList;
import com.sonar.sslr.squid.AstScanner;

public class ErlangAstScannerTest {

  @Test
  public void files() {
    AstScanner<ErlangGrammar> scanner = ErlangAstScanner.create(new ErlangConfiguration(Charsets.UTF_8));
    scanner.scanFiles(ImmutableList.of(new File("src/test/resources/metrics/lines.erl"), new File("src/test/resources/metrics/lines_of_code.erl")));
    SourceProject project = (SourceProject) scanner.getIndex().search(new QueryByType(SourceProject.class)).iterator().next();
    assertThat(project.getInt(ErlangMetric.FILES)).isEqualTo(2);
  }

  @Test
  public void comments() {
    SourceFile file = ErlangAstScanner.scanSingleFile(new File("src/test/resources/metrics/functions.erl"));
    assertThat(file.getInt(ErlangMetric.COMMENT_BLANK_LINES)).isEqualTo(6);
    assertThat(file.getInt(ErlangMetric.COMMENT_LINES)).isEqualTo(5);
    assertThat(file.getNoSonarTagLines()).contains(35);
    assertThat(file.getNoSonarTagLines().size()).isEqualTo(1);
  }

  @Test
  public void lines() {
    SourceFile file = ErlangAstScanner.scanSingleFile(new File("src/test/resources/metrics/lines.erl"));
    assertThat(file.getInt(ErlangMetric.LINES)).isEqualTo(5);
  }

  @Test
  public void lines_of_code() {
    SourceFile file = ErlangAstScanner.scanSingleFile(new File("src/test/resources/metrics/lines_of_code.erl"));
    assertThat(file.getInt(ErlangMetric.LINES_OF_CODE)).isEqualTo(3);
  }

  @Test
  public void statements() {
    SourceFile file = ErlangAstScanner.scanSingleFile(new File("src/test/resources/metrics/statements.erl"));
    assertThat(file.getInt(ErlangMetric.STATEMENTS)).isEqualTo(11);
  }

  @Test
  public void functions() {
    SourceFile file = ErlangAstScanner.scanSingleFile(new File("src/test/resources/metrics/functions.erl"));
    assertThat(file.getInt(ErlangMetric.FUNCTIONS)).isEqualTo(7);
  }

  @Test
  public void complexity() {
    SourceFile file = ErlangAstScanner.scanSingleFile(new File("src/test/resources/metrics/statements.erl"));
    assertThat(file.getInt(ErlangMetric.COMPLEXITY)).isEqualTo(16);
  }

}
