package org.kalidasya.sonar.erlang;

import java.io.File;
import java.util.Collection;

import org.kalidasya.sonar.erlang.api.ErlangGrammar;
import org.sonar.squid.api.SourceCode;
import org.sonar.squid.api.SourceFile;
import org.sonar.squid.indexer.QueryByType;

import com.google.common.base.Charsets;
import com.sonar.sslr.squid.AstScanner;
import com.sonar.sslr.squid.SquidAstVisitor;

public final class ErlangAstScanner {

	private ErlangAstScanner() {
	}
	
	public static SourceFile scanSingleFile(File file, SquidAstVisitor<ErlangGrammar>... visitors) {
	    if (!file.isFile()) {
	      throw new IllegalArgumentException("File '" + file + "' not found.");
	    }
	    AstScanner<ErlangGrammar> scanner = create(new ErlangConfiguration(Charsets.UTF_8), visitors);
	    scanner.scanFile(file);
	    Collection<SourceCode> sources = scanner.getIndex().search(new QueryByType(SourceFile.class));
	    if (sources.size() != 1) {
	      throw new IllegalStateException("Only one SourceFile was expected whereas " + sources.size() + " has been returned.");
	    }
	    return (SourceFile) sources.iterator().next();
	  }

	private static AstScanner<ErlangGrammar> create(ErlangConfiguration erlangConfiguration,
			SquidAstVisitor<ErlangGrammar>[] visitors) {
		// TODO Auto-generated method stub
		return null;
	}
}
