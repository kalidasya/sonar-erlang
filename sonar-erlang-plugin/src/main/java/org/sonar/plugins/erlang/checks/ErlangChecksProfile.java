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
package org.sonar.plugins.erlang.checks;

import org.sonar.erlang.checks.CheckList;

import org.sonar.api.profiles.AnnotationProfileParser;
import org.sonar.api.profiles.ProfileDefinition;
import org.sonar.api.profiles.RulesProfile;
import org.sonar.api.utils.ValidationMessages;
import org.sonar.plugins.erlang.core.Erlang;

public class ErlangChecksProfile extends ProfileDefinition {

  private final AnnotationProfileParser annotationProfileParser;

  public ErlangChecksProfile(AnnotationProfileParser annotationProfileParser) {
    this.annotationProfileParser = annotationProfileParser;
  }

  @Override
  public RulesProfile createProfile(ValidationMessages validation) {
    return annotationProfileParser.parse(CheckList.REPOSITORY_KEY, CheckList.SONAR_WAY_PROFILE,
        Erlang.KEY, CheckList.getChecks(), validation);
  }

}
