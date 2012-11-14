package org.kalidasya.sonar.erlang.checks;

import org.kalidasya.sonar.erlang.api.ErlangGrammar;
import org.kalidasya.sonar.erlang.api.ErlangMetric;
import org.sonar.check.BelongsToProfile;
import org.sonar.check.Priority;
import org.sonar.check.Rule;
import org.sonar.check.RuleProperty;
import org.sonar.squid.api.SourceFunction;

import com.sonar.sslr.api.AstNode;
import com.sonar.sslr.squid.checks.SquidCheck;

@Rule(
  key = "NumberOfFunctionArgs",
  priority = Priority.MAJOR)
@BelongsToProfile(title = CheckList.SONAR_WAY_PROFILE, priority = Priority.MAJOR)
public class NumberOfFunctionArgsCheck extends SquidCheck<ErlangGrammar> {

  private static final int DEFAULT_MAXIMUM_FUNCTION_ARGUMENT_THRESHOLD = 6;

  @RuleProperty(
    key = "maximumFunctionArgumentThreshold",
    defaultValue = "" + DEFAULT_MAXIMUM_FUNCTION_ARGUMENT_THRESHOLD)
  private int maximumFunctionArgumentThreshold = DEFAULT_MAXIMUM_FUNCTION_ARGUMENT_THRESHOLD;

  @Override
  public void init() {
    subscribeTo(getContext().getGrammar().functionClause);
  }

  @Override
  public void leaveNode(AstNode node) {
    SourceFunction function = (SourceFunction) getContext().peekSourceCode();
    if (function.getInt(ErlangMetric.NUM_OF_FUNC_ARGS) > maximumFunctionArgumentThreshold) {
      getContext().createLineViolation(this,
          "Function has {0,number,integer} arguments which is greater than {1,number,integer} authorized.", node,
          function.getInt(ErlangMetric.NUM_OF_FUNC_ARGS), maximumFunctionArgumentThreshold);
    }
  }

  public void setMaximumFunctionComplexityThreshold(int threshold) {
    this.maximumFunctionArgumentThreshold = threshold;
  }

}
