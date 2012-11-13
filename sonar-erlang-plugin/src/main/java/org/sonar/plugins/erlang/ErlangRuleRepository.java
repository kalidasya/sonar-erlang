package org.sonar.plugins.erlang;

import java.util.List;

import org.kalidasya.sonar.erlang.checks.CheckList;
import org.sonar.api.rules.AnnotationRuleParser;
import org.sonar.api.rules.Rule;
import org.sonar.api.rules.RuleRepository;
import org.sonar.plugins.erlang.core.Erlang;

public class ErlangRuleRepository extends RuleRepository {

  private static final String REPOSITORY_NAME = "Sonar";

  private final AnnotationRuleParser annotationRuleParser;

  public ErlangRuleRepository(AnnotationRuleParser annotationRuleParser) {
    super(CheckList.REPOSITORY_KEY, Erlang.KEY);
    setName(REPOSITORY_NAME);
    this.annotationRuleParser = annotationRuleParser;
  }

  @Override
  public List<Rule> createRules() {
    return annotationRuleParser.parse(CheckList.REPOSITORY_KEY, CheckList.getChecks());
  }

}
