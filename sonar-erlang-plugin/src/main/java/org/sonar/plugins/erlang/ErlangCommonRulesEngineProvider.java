package org.sonar.plugins.erlang;

import org.sonar.api.resources.Project;
import org.sonar.commonrules.api.CommonRulesEngine;
import org.sonar.commonrules.api.CommonRulesEngineProvider;
import org.sonar.plugins.erlang.core.Erlang;

public class ErlangCommonRulesEngineProvider extends CommonRulesEngineProvider {

  public ErlangCommonRulesEngineProvider() {
    super();
  }

  public ErlangCommonRulesEngineProvider(Project project) {
    super(project);
  }

  @Override
  protected void doActivation(CommonRulesEngine engine) {
    engine.activateRule("DuplicatedBlocks");
    engine.activateRule("InsufficientCommentDensity");
    engine.activateRule("InsufficientLineCoverage");
    engine.activateRule("InsufficientBranchCoverage");
  }

  @Override
  protected String getLanguageKey() {
    return Erlang.KEY;
  }

}
