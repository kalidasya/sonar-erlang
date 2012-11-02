package org.kalidasya.sonar.erlang;

import static org.fest.assertions.Assertions.assertThat;

import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

public class ErlangCommentAnalyserTest {


	  @Rule
	  public ExpectedException thrown = ExpectedException.none();

	  private ErlangCommentAnalyser analyser = new ErlangCommentAnalyser();

	  @Test
	  public void content() {
	    assertThat(analyser.getContents("% comment")).isEqualTo(" comment");
	    assertThat(analyser.getContents("%% comment")).isEqualTo("% comment");
	    assertThat(analyser.getContents("%%== comment")).isEqualTo("%== comment");
	  }

	  @Test
	  public void blank() {
	    assertThat(analyser.isBlank(" ")).isTrue();
	    assertThat(analyser.isBlank("comment")).isFalse();
	  }

	  @Test
	  public void unknown_type_of_comment() {
	    thrown.expect(IllegalArgumentException.class);
	    analyser.getContents("");
	  }
	
}
