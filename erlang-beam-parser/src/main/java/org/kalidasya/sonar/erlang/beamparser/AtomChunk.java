package org.kalidasya.sonar.erlang.beamparser;

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.lang.ArrayUtils;



public class AtomChunk extends BeamChunk {

	public AtomChunk(byte[] fragment) {
		super(fragment);
		populateStrings();
	}

	private void populateStrings() {
		int i = 0;
		while (i < data.length && atoms.size()<getNumOfEntries()) {
			atoms.add(new String(ArrayUtils.subarray(data, i+1, i+1+Integer.valueOf(data[i]))));
			i = Integer.valueOf(data[i])+i+1;
		}
	}

	List<String> atoms = new ArrayList<String>();
	
	public List<String> getAtoms() {

		return atoms;
	}
	
}
