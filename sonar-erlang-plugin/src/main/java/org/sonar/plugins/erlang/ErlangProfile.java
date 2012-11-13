package org.sonar.plugins.erlang;

import org.kalidasya.sonar.erlang.checks.CheckList;
import org.sonar.api.profiles.AnnotationProfileParser;
import org.sonar.api.profiles.ProfileDefinition;
import org.sonar.api.profiles.RulesProfile;
import org.sonar.api.utils.ValidationMessages;
import org.sonar.plugins.erlang.core.Erlang;

public class ErlangProfile extends ProfileDefinition {

	private final AnnotationProfileParser annotationProfileParser;

	public ErlangProfile(AnnotationProfileParser annotationProfileParser) {
		this.annotationProfileParser = annotationProfileParser;
	}

	@Override
	public RulesProfile createProfile(ValidationMessages validation) {
		return annotationProfileParser.parse(CheckList.REPOSITORY_KEY, CheckList.SONAR_WAY_PROFILE,
				Erlang.KEY, CheckList.getChecks(), validation);
	}

}
