package org.sonar.plugins.erlang.core;

import org.apache.commons.configuration.Configuration;
import org.apache.commons.lang.StringUtils;
import org.sonar.api.resources.AbstractLanguage;
import org.sonar.plugins.erlang.ErlangPlugin;

public class Erlang extends AbstractLanguage{

	public static final String KEY = "erl";

	  private Configuration configuration;

	  public Erlang(Configuration configuration) {
	    super(KEY, "Erlang");
	    this.configuration = configuration;
	  }

	  public Configuration getConfiguration() {
	    return this.configuration;
	  }

	  public String[] getFileSuffixes() {
	    String[] suffixes = configuration.getStringArray(ErlangPlugin.FILE_SUFFIXES_KEY);
	    if (suffixes == null || suffixes.length == 0) {
	      suffixes = StringUtils.split(ErlangPlugin.FILE_SUFFIXES_DEFVALUE, ",");
	    }
	    return suffixes;
	  }

}
