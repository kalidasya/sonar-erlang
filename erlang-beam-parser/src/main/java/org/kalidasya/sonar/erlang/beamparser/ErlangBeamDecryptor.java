package org.kalidasya.sonar.erlang.beamparser;

import java.nio.ByteBuffer;

import org.apache.commons.lang.ArrayUtils;

public class ErlangBeamDecryptor {

	public static ErlangBeam decrypt(byte[] beamContent) {
		ErlangBeam ret = new ErlangBeam();
		BeamHeader header = new BeamHeader();
		byte[] beam = ArrayUtils.subarray(beamContent, 1, beamContent.length);
		header.setNumberIndicator(new String(ArrayUtils.subarray(beam, 0, 4)));
		header.setFormLength(ByteBuffer.wrap(ArrayUtils.subarray(beam, 4, 8)).getInt());
		header.setFormType(new String(ArrayUtils.subarray(beam, 8, 12)));
		ret.setHeader(header);

		byte[] chunks = ArrayUtils.subarray(beam, 12, header.getFormLength() - 8);

		findAtom(chunks);
		findExportTable(chunks);
		return ret;
	}

	private static void findExportTable(byte[] chunks) {
		int atomStart = ByteFinder.indexOf(chunks, "ExpT".getBytes());
		
		ExportTableChunk atom = new ExportTableChunk(ArrayUtils.subarray(chunks, atomStart, chunks.length));

		System.out.println(atomStart);
		System.out.println(atom.getLength());
		
	}

	private static void findAtom(byte[] chunks) {
		int atomStart = ByteFinder.indexOf(chunks, "Atom".getBytes());
		AtomChunk atom = new AtomChunk(ArrayUtils.subarray(chunks, atomStart, chunks.length));

		System.out.println(atomStart);
		System.out.println(atom.getLength());
	}

}
