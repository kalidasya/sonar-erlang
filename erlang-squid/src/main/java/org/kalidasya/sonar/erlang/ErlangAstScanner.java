package org.kalidasya.sonar.erlang;

import java.io.File;
import java.util.Collection;

import org.kalidasya.sonar.erlang.api.ErlangGrammar;
import org.kalidasya.sonar.erlang.api.ErlangMetric;
import org.kalidasya.sonar.erlang.parser.ErlangParser;
import org.sonar.squid.api.SourceCode;
import org.sonar.squid.api.SourceFile;
import org.sonar.squid.api.SourceFunction;
import org.sonar.squid.api.SourceProject;
import org.sonar.squid.indexer.QueryByType;

import com.google.common.base.Charsets;
import com.sonar.sslr.api.AstNode;
import com.sonar.sslr.impl.Parser;
import com.sonar.sslr.squid.AstScanner;
import com.sonar.sslr.squid.SourceCodeBuilderCallback;
import com.sonar.sslr.squid.SourceCodeBuilderVisitor;
import com.sonar.sslr.squid.SquidAstVisitor;
import com.sonar.sslr.squid.SquidAstVisitorContextImpl;
import com.sonar.sslr.squid.metrics.CounterVisitor;

public final class ErlangAstScanner {

	private ErlangAstScanner() {
	}

	public static SourceFile scanSingleFile(File file,
			SquidAstVisitor<ErlangGrammar>... visitors) {
		if (!file.isFile()) {
			throw new IllegalArgumentException("File '" + file + "' not found.");
		}
		AstScanner<ErlangGrammar> scanner = create(new ErlangConfiguration(
				Charsets.UTF_8), visitors);
		scanner.scanFile(file);
		Collection<SourceCode> sources = scanner.getIndex().search(
				new QueryByType(SourceFile.class));
		if (sources.size() != 1) {
			throw new IllegalStateException(
					"Only one SourceFile was expected whereas "
							+ sources.size() + " has been returned.");
		}
		return (SourceFile) sources.iterator().next();
	}

	public static AstScanner<ErlangGrammar> create(ErlangConfiguration conf, SquidAstVisitor<ErlangGrammar>... visitors) {
		final SquidAstVisitorContextImpl<ErlangGrammar> context = new SquidAstVisitorContextImpl<ErlangGrammar>(
				new SourceProject("JavaScript Project"));
		final Parser<ErlangGrammar> parser = ErlangParser.create(conf);

		AstScanner.Builder<ErlangGrammar> builder = AstScanner
				.<ErlangGrammar> builder(context).setBaseParser(parser);

		/* Metrics */
		builder.withMetrics(ErlangMetric.values());

		/* Comments */
		builder.setCommentAnalyser(new ErlangCommentAnalyser());

		/* Files */
		builder.setFilesMetric(ErlangMetric.FILES);

		/* Functions */
		builder.withSquidAstVisitor(new SourceCodeBuilderVisitor<ErlangGrammar>(
				new SourceCodeBuilderCallback() {
					public SourceCode createSourceCode(
							SourceCode parentSourceCode, AstNode astNode) {
						String functionName = astNode.getChild(1)
								.getTokenValue();
						SourceFunction function = new SourceFunction(
								functionName + ":"
										+ astNode.getToken().getLine());
						function.setStartAtLine(astNode.getTokenLine());
						return function;
					}
				}, parser.getGrammar().functionDeclaration));

		builder.withSquidAstVisitor(CounterVisitor.<ErlangGrammar> builder()
				.setMetricDef(ErlangMetric.FUNCTIONS)
				.subscribeTo(parser.getGrammar().functionDeclaration).build());

		/* Metrics */
		/*
		 * builder.withSquidAstVisitor(new
		 * LinesVisitor<ErlangGrammar>(ErlangMetric.LINES));
		 * builder.withSquidAstVisitor(new
		 * LinesOfCodeVisitor<ErlangGrammar>(ErlangMetric.LINES_OF_CODE));
		 * builder.withSquidAstVisitor(CommentsVisitor.<ErlangGrammar>
		 * builder().withCommentMetric(ErlangMetric.COMMENT_LINES)
		 * .withBlankCommentMetric(ErlangMetric.COMMENT_BLANK_LINES)
		 * .withNoSonar(true)
		 * .withIgnoreHeaderComment(conf.getIgnoreHeaderComments()) .build());
		 * builder.withSquidAstVisitor(CounterVisitor.<ErlangGrammar> builder()
		 * .setMetricDef(ErlangMetric.STATEMENTS) .subscribeTo(
		 * parser.getGrammar().variableStatement,
		 * parser.getGrammar().emptyStatement,
		 * parser.getGrammar().expressionStatement,
		 * parser.getGrammar().ifStatement,
		 * parser.getGrammar().iterationStatement,
		 * parser.getGrammar().continueStatement,
		 * parser.getGrammar().breakStatement,
		 * parser.getGrammar().returnStatement,
		 * parser.getGrammar().withStatement,
		 * parser.getGrammar().switchStatement,
		 * parser.getGrammar().throwStatement, parser.getGrammar().tryStatement,
		 * parser.getGrammar().debuggerStatement) .build());
		 * 
		 * AstNodeType[] complexityAstNodeType = new AstNodeType[] { // Entry
		 * points parser.getGrammar().functionDeclaration,
		 * parser.getGrammar().functionExpression,
		 * 
		 * // Branching nodes parser.getGrammar().ifStatement,
		 * parser.getGrammar().iterationStatement,
		 * parser.getGrammar().switchStatement, parser.getGrammar().caseClause,
		 * parser.getGrammar().defaultClause, parser.getGrammar().catch_,
		 * parser.getGrammar().returnStatement,
		 * parser.getGrammar().throwStatement,
		 * 
		 * // Expressions ErlangPunctuator.QUERY, ErlangPunctuator.ANDAND,
		 * ErlangPunctuator.OROR };
		 * builder.withSquidAstVisitor(ComplexityVisitor.<ErlangGrammar>
		 * builder() .setMetricDef(ErlangMetric.COMPLEXITY)
		 * .subscribeTo(complexityAstNodeType) .build());
		 */

		/* External visitors (typically Check ones) */
		for (SquidAstVisitor<ErlangGrammar> visitor : visitors) {
			builder.withSquidAstVisitor(visitor);
		}

		return builder.build();
	}
}
