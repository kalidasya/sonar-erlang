package org.kalidasya.sonar.erlang;

import com.sonar.sslr.api.CommentAnalyser;

public class ErlangCommentAnalyser extends CommentAnalyser {

	@Override
	public boolean isBlank(String line) {
		for (int i = 0; i < line.length(); i++) {
			if (Character.isLetterOrDigit(line.charAt(i))) {
				return false;
			}
		}
		return true;
	}

	@Override
	public String getContents(String comment) {
		if (comment.startsWith("%")) {
			return comment.substring(1);
		} else {
			throw new IllegalArgumentException();
		}
	}

}
