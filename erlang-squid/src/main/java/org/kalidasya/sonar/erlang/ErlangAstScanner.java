package org.kalidasya.sonar.erlang;

import java.io.File;
import java.util.Collection;

import org.kalidasya.sonar.erlang.api.ErlangGrammar;
import org.kalidasya.sonar.erlang.api.ErlangMetric;
import org.kalidasya.sonar.erlang.metrics.DepthOfCases;
import org.kalidasya.sonar.erlang.metrics.NumberOfFunctionArgument;
import org.kalidasya.sonar.erlang.metrics.PublicDocumentedApiCounter;
import org.kalidasya.sonar.erlang.parser.ErlangParser;
import org.sonar.squid.api.SourceClass;
import org.sonar.squid.api.SourceCode;
import org.sonar.squid.api.SourceFile;
import org.sonar.squid.api.SourceFunction;
import org.sonar.squid.api.SourceProject;
import org.sonar.squid.indexer.QueryByType;

import com.google.common.base.Charsets;
import com.sonar.sslr.api.AstNode;
import com.sonar.sslr.api.AstNodeType;
import com.sonar.sslr.impl.Parser;
import com.sonar.sslr.squid.AstScanner;
import com.sonar.sslr.squid.SourceCodeBuilderCallback;
import com.sonar.sslr.squid.SourceCodeBuilderVisitor;
import com.sonar.sslr.squid.SquidAstVisitor;
import com.sonar.sslr.squid.SquidAstVisitorContextImpl;
import com.sonar.sslr.squid.metrics.CommentsVisitor;
import com.sonar.sslr.squid.metrics.ComplexityVisitor;
import com.sonar.sslr.squid.metrics.CounterVisitor;
import com.sonar.sslr.squid.metrics.LinesOfCodeVisitor;
import com.sonar.sslr.squid.metrics.LinesVisitor;

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

	public static AstScanner<ErlangGrammar> create(ErlangConfiguration conf,
			SquidAstVisitor<ErlangGrammar>... visitors) {
		final SquidAstVisitorContextImpl<ErlangGrammar> context = new SquidAstVisitorContextImpl<ErlangGrammar>(
				new SourceProject("Erlang Project"));
		final Parser<ErlangGrammar> parser = ErlangParser.create(conf);

		AstScanner.Builder<ErlangGrammar> builder = AstScanner
				.<ErlangGrammar> builder(context).setBaseParser(parser);

		/* Metrics */
		builder.withMetrics(ErlangMetric.values());

		/* Comments */
		builder.setCommentAnalyser(new ErlangCommentAnalyser());

		/* Files */
		builder.setFilesMetric(ErlangMetric.FILES);

		/* Classes = modules */
		builder.withSquidAstVisitor(new SourceCodeBuilderVisitor<ErlangGrammar>(new SourceCodeBuilderCallback() {
		      public SourceCode createSourceCode(SourceCode parentSourceCode, AstNode astNode) {
		        String className = astNode.getChild(3).getTokenValue();
		        SourceClass cls = new SourceClass(className + ":" + astNode.getToken().getLine());
		        cls.setStartAtLine(astNode.getTokenLine());
		        return cls;
		      }
		    }, parser.getGrammar().moduleAttr));
		
		builder.withSquidAstVisitor(CounterVisitor.<ErlangGrammar> builder()
				.setMetricDef(ErlangMetric.MODULES)
				.subscribeTo(parser.getGrammar().moduleAttr).build());
		
		/* Functions */
		builder.withSquidAstVisitor(new SourceCodeBuilderVisitor<ErlangGrammar>(
				new SourceCodeBuilderCallback() {
					public SourceCode createSourceCode(
							SourceCode parentSourceCode, AstNode astNode) {
						String functionName = astNode.getChild(0)
								.getTokenValue();
						/**
						 * TODO: use function signature instead? (other constructor. what is the difference?)
						 */
						SourceFunction function = new SourceFunction(
								functionName + ":"
										+ astNode.getToken().getLine());
						function.setStartAtLine(astNode.getTokenLine());
						return function;
					}
				}, parser.getGrammar().functionClause));

		builder.withSquidAstVisitor(CounterVisitor.<ErlangGrammar> builder()
				.setMetricDef(ErlangMetric.FUNCTIONS)
				.subscribeTo(parser.getGrammar().functionDeclaration).build());

		/* Metrics */

		builder.withSquidAstVisitor(new LinesVisitor<ErlangGrammar>(
				ErlangMetric.LINES));
		builder.withSquidAstVisitor(new LinesOfCodeVisitor<ErlangGrammar>(
				ErlangMetric.LINES_OF_CODE));

		builder.withSquidAstVisitor(CommentsVisitor.<ErlangGrammar> builder()
				.withCommentMetric(ErlangMetric.COMMENT_LINES)
				.withBlankCommentMetric(ErlangMetric.COMMENT_BLANK_LINES)
				.withNoSonar(true).withIgnoreHeaderComment(false).build());
		builder.withSquidAstVisitor(CounterVisitor
				.<ErlangGrammar> builder()
				.setMetricDef(ErlangMetric.STATEMENTS)
				.subscribeTo(
						parser.getGrammar().statement
						/*,
						parser.getGrammar().receiveStatement,
						parser.getGrammar().expressionStatement,
						parser.getGrammar().tryStatement*/).build());

		
		  AstNodeType[] complexityAstNodeType = new AstNodeType[] { 
		 // Entry points 
				  parser.getGrammar().functionClause,
				  parser.getGrammar().funExpression,
		  
		  // Branching nodes 
				  parser.getGrammar().branchExp,
				  parser.getGrammar().patternStatement,
				  parser.getGrammar().catchPatternStatement,
		  
		  // Expressions
		  //cannot add guardExpression, it only counts if it is over 1
				  //parser.getGrammar().guardExpression
		  /**
		   * TODO: boolean expression complexity?
		   */
		  };

		  builder
		  .withSquidAstVisitor(ComplexityVisitor.<ErlangGrammar > builder()
		  .setMetricDef(ErlangMetric.COMPLEXITY)
		  .subscribeTo(complexityAstNodeType) .build());
		  
		  /* Public API counter */
		  builder.withSquidAstVisitor(new PublicDocumentedApiCounter());
		  
		  /* Number of fun expressions*/
		  builder
		  .withSquidAstVisitor(ComplexityVisitor.<ErlangGrammar > builder()
		  .setMetricDef(ErlangMetric.NUM_OF_FUN_EXRP)
		  .subscribeTo(parser.getGrammar().funExpression) .build());
		  
		  /* Number of function arguments*/
		  builder
		  .withSquidAstVisitor(new NumberOfFunctionArgument());
		  
		  /* Depth of cases*/
		  builder
		  .withSquidAstVisitor(new DepthOfCases());
		  
		  /* Number of function clauses*/
		  builder
		  .withSquidAstVisitor(ComplexityVisitor.<ErlangGrammar > builder()
		  .setMetricDef(ErlangMetric.NUM_OF_FUN_CLAUSES)
		  .subscribeTo(parser.getGrammar().functionClause) .build());
		 
		/* External visitors (typically Check ones) */
		for (SquidAstVisitor<ErlangGrammar> visitor : visitors) {
			builder.withSquidAstVisitor(visitor);
		}

		return builder.build();
	}
}
