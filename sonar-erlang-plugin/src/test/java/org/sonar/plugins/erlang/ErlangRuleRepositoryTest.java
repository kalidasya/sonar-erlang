package org.sonar.plugins.erlang;

import static org.fest.assertions.Assertions.assertThat;

import java.util.List;

import org.junit.Test;
import org.kalidasya.sonar.erlang.checks.CheckList;
import org.sonar.api.rules.AnnotationRuleParser;
import org.sonar.api.rules.Rule;

public class ErlangRuleRepositoryTest {

	@Test
	public void test() {
		ErlangRuleRepository ruleRepository = new ErlangRuleRepository(new AnnotationRuleParser());
		assertThat(ruleRepository.getKey()).isEqualTo("erlang");
		assertThat(ruleRepository.getName()).isEqualTo("Sonar");
		List<Rule> rules = ruleRepository.createRules();
		assertThat(rules.size()).isEqualTo(CheckList.getChecks().size());
	}

}
