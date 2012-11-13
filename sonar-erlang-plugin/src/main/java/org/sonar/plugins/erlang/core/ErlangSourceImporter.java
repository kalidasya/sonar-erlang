package org.sonar.plugins.erlang.core;

import org.sonar.api.batch.AbstractSourceImporter;
import org.sonar.api.batch.Phase;
import org.sonar.api.batch.SensorContext;
import org.sonar.api.resources.InputFileUtils;
import org.sonar.api.resources.ProjectFileSystem;

@Phase(name = Phase.Name.PRE)
public class ErlangSourceImporter extends AbstractSourceImporter {

  public ErlangSourceImporter(Erlang erlang) {
    super(erlang);
  }

  protected void analyse(ProjectFileSystem fileSystem, SensorContext context) {
    parseDirs(context, InputFileUtils.toFiles(fileSystem.mainFiles(Erlang.KEY)), fileSystem.getSourceDirs(), false, fileSystem.getSourceCharset());
  }

  @Override
  public String toString() {
    return getClass().getSimpleName();
  }

}
