package org.kalidasya.sonar.erlang.beamparser;

public class BeamHeader {

	String numberIndicator;
	Integer formLength;
	String formType;
	String chunks;
	public String getNumberIndicator() {
		return numberIndicator;
	}
	public void setNumberIndicator(String numberIndicator) {
		this.numberIndicator = numberIndicator;
	}
	public Integer getFormLength() {
		return formLength;
	}
	public void setFormLength(Integer formLength) {
		this.formLength = formLength;
	}
	public String getFormType() {
		return formType;
	}
	public void setFormType(String formType) {
		this.formType = formType;
	}
	public String getChunks() {
		return chunks;
	}
	public void setChunks(String chunks) {
		this.chunks = chunks;
	}
	
	
	
}
