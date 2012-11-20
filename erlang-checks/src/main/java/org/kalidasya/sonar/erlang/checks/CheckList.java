/*
 * Sonar Erlang Plugin
 * Copyright (C) 2012 Tamas Kende
 * kende.tamas@gmail.com
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
package org.kalidasya.sonar.erlang.checks;

import java.util.List;

import com.google.common.collect.ImmutableList;

public final class CheckList {

  public static final String REPOSITORY_KEY = "erlang-checks";

  public static final String SONAR_WAY_PROFILE = "Erlang checks";

  private CheckList() {
  }

  public static List<Class> getChecks() {
    return ImmutableList.<Class> of(
        BlockSeparatorCharacterCheck.class,
        CommentRegularExpressionCheck.class,
        DepthOfCasesCheck.class,
        DoNotUseExportAllCheck.class,
        ExportOneFunctionPerLineCheck.class,
        FunctionComplexityCheck.class,
        FunctionDefAndClausesSeparationCheck.class,
        IndentionSizeCheck.class,
        LineLengthCheck.class,
        MultipleBlankLinesCheck.class,
        NoEmacsStyleLeadingCommasCheck.class,
        NoSpaceAfterBeforeBracketsCheck.class,
        NoTabsForIndentionCheck.class,
        NoTrailingWhitespaceCheck.class,
        NumberOfFunctionArgsCheck.class,
        SpaceAfterBeforeOperatorsCheck.class,
        XPathCheck.class);
  }

}
