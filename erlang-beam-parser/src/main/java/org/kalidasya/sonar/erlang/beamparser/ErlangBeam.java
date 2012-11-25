package org.kalidasya.sonar.erlang.beamparser;

public class ErlangBeam {

	BeamHeader header;
	AtomChunk atomChunk;
	
	

	public AtomChunk getAtomChunk() {
		return atomChunk;
	}

	public void setAtomChunk(AtomChunk atomChunk) {
		this.atomChunk = atomChunk;
	}

	public BeamHeader getHeader() {
		return header;
	}

	public void setHeader(BeamHeader header) {
		this.header = header;
	}
	
	
}
