package org.kalidasya.sonar.erlang.checks;

import com.google.common.collect.ImmutableList;

import java.util.List;

public final class CheckList {

  public static final String REPOSITORY_KEY = "erlang";

  public static final String SONAR_WAY_PROFILE = "Sonar way";

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
        NumberFormatException.class,
        SpaceAfterBeforeOperatorsCheck.class,
        XPathCheck.class);
  }

}
