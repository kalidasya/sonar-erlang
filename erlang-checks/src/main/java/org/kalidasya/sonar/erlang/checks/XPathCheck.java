package org.kalidasya.sonar.erlang.checks;

import org.kalidasya.sonar.erlang.api.ErlangGrammar;
import org.sonar.check.Cardinality;
import org.sonar.check.Priority;
import org.sonar.check.Rule;
import org.sonar.check.RuleProperty;

import com.sonar.sslr.squid.checks.AbstractXPathCheck;

@Rule(
  key = "XPath",
  priority = Priority.MAJOR,
  cardinality = Cardinality.MULTIPLE)
public class XPathCheck extends AbstractXPathCheck<ErlangGrammar> {

  private static final String DEFAULT_XPATH_QUERY = "";
  private static final String DEFAULT_MESSAGE = "The XPath expression matches this piece of code";

  @RuleProperty(
    key = "xpathQuery",
    defaultValue = "" + DEFAULT_XPATH_QUERY)
  public String xpathQuery = DEFAULT_XPATH_QUERY;

  @RuleProperty(
    key = "message",
    defaultValue = "" + DEFAULT_MESSAGE)
  public String message = DEFAULT_MESSAGE;

  @Override
  public String getXPathQuery() {
    return xpathQuery;
  }

  @Override
  public String getMessage() {
    return message;
  }

}
