package org.kalidasya.sonar.erlang.api;

import org.sonar.squid.measures.CalculatedMetricFormula;
import org.sonar.squid.measures.MetricDef;

public enum ErlangMetric implements MetricDef{
	FILES,
	LINES,
	LINES_OF_CODE,
	COMMENT_LINES,
	COMMENT_BLANK_LINES,
	STATEMENTS,
	COMPLEXITY,
	FUNCTIONS;

	public boolean aggregateIfThereIsAlreadyAValue() {
		return true;
	}

	public CalculatedMetricFormula getCalculatedMetricFormula() {
		return null;
	}

	public String getName() {
		return name();
	}

	public boolean isCalculatedMetric() {
		return false;
	}

	public boolean isThereAggregationFormula() {
		return true;
	}

}
