package org.kalidasya.sonar.erlang.beamparser;

import java.nio.ByteBuffer;
import java.util.ArrayList;
import java.util.List;

public class AtomChunk extends BeamChunk {

	public AtomChunk(byte[] fragment) {
		super(fragment);
		populateStrings();
	}

	private void populateStrings() {
		ByteBuffer wrap = ByteBuffer.wrap(data);
		while (atoms.size()<getNumOfEntries()) {
			int l = wrap.get();
			byte[] atom = new byte[l];
			wrap.get(atom, 0, l);
			atoms.add(new String(atom));
		}
	}

	List<String> atoms = new ArrayList<String>();
	
	public List<String> getAtoms() {

		return atoms;
	}
	
}
