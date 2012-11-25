package org.kalidasya.sonar.erlang.beamparser;

import java.nio.ByteBuffer;
import java.util.ArrayList;
import java.util.List;

import org.apache.commons.lang.ArrayUtils;




public class ExportTableChunk extends BeamChunk {

	public ExportTableChunk(byte[] fragment) {
		super(fragment);
		populateFunctions();
	}

	private void populateFunctions() {
		int i = 0;
		ByteBuffer wrap = ByteBuffer.wrap(data);
		while (functions.size()<getNumOfEntries()) {
			String func = "";
			func+=wrap.getInt()+"/";
			func+=wrap.getInt()+":";
			func+=wrap.getInt();
			functions.add(func);
			i = i+12;
		}
	}

	List<String> functions = new ArrayList<String>();
	
	public List<String> getFunctions() {

		return functions;
	}
}
