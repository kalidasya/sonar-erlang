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
package org.sonar.plugins.erlang;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.fest.assertions.Assertions.assertThat;

import java.io.File;
import java.nio.charset.Charset;

import org.junit.Before;
import org.junit.Test;
import org.mockito.Mockito;
import org.sonar.api.batch.SensorContext;
import org.sonar.api.measures.CoreMetrics;
import org.sonar.api.measures.FileLinesContext;
import org.sonar.api.measures.FileLinesContextFactory;
import org.sonar.api.profiles.RulesProfile;
import org.sonar.api.resources.InputFile;
import org.sonar.api.resources.InputFileUtils;
import org.sonar.api.resources.Language;
import org.sonar.api.resources.Project;
import org.sonar.api.resources.ProjectFileSystem;
import org.sonar.api.resources.Resource;
import org.sonar.plugins.erlang.core.Erlang;

import com.google.common.collect.ImmutableList;

public class ErlangSquidSensorTest {

	private ErlangSquidSensor sensor;

	@Before
	public void setUp() {
		FileLinesContextFactory fileLinesContextFactory = mock(FileLinesContextFactory.class);
		FileLinesContext fileLinesContext = mock(FileLinesContext.class);
		when(fileLinesContextFactory.createFor(Mockito.any(Resource.class))).thenReturn(
				fileLinesContext);
		sensor = new ErlangSquidSensor(mock(RulesProfile.class), fileLinesContextFactory);
	}

	@Test
	public void should_execute_on_erlang_project() {
		Project project = new Project("key");
		
		Language java = mock(Language.class);
		when(java.getKey()).thenReturn("java");
		project.setLanguage(java);
		assertThat(sensor.shouldExecuteOnProject(project)).isFalse();
		
		Language erl = mock(Language.class);
		when(erl.getKey()).thenReturn("erl");
		project.setLanguage(erl);
		assertThat(sensor.shouldExecuteOnProject(project)).isTrue();
	}

	@Test
	public void should_analyse() {
		ProjectFileSystem fs = mock(ProjectFileSystem.class);
		when(fs.getSourceCharset()).thenReturn(Charset.forName("UTF-8"));
		InputFile inputFile = InputFileUtils.create(new File("src/test/resources/cpd"), new File(
				"src/test/resources/cpd/person.erl"));
		when(fs.mainFiles(Erlang.KEY)).thenReturn(ImmutableList.of(inputFile));
		Project project = new Project("key");
		project.setFileSystem(fs);
		SensorContext context = mock(SensorContext.class);

		sensor.analyse(project, context);

		verify(context).saveMeasure(Mockito.any(Resource.class), Mockito.eq(CoreMetrics.FILES),
				Mockito.eq(1.0));
		verify(context).saveMeasure(Mockito.any(Resource.class), Mockito.eq(CoreMetrics.LINES),
				Mockito.eq(18.0));
		//TODO it should be 14 not 13
		verify(context).saveMeasure(Mockito.any(Resource.class), Mockito.eq(CoreMetrics.NCLOC),
				Mockito.eq(14.0));
		verify(context).saveMeasure(Mockito.any(Resource.class), Mockito.eq(CoreMetrics.FUNCTIONS),
				Mockito.eq(4.0));
		verify(context).saveMeasure(Mockito.any(Resource.class),
				Mockito.eq(CoreMetrics.STATEMENTS), Mockito.eq(8.0));
		verify(context).saveMeasure(Mockito.any(Resource.class),
				Mockito.eq(CoreMetrics.COMPLEXITY), Mockito.eq(4.0));
		verify(context).saveMeasure(Mockito.any(Resource.class),
				Mockito.eq(CoreMetrics.COMMENT_LINES), Mockito.eq(1.0));
		
	}

}
