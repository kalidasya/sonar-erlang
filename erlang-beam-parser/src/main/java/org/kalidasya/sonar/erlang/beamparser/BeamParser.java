package org.kalidasya.sonar.erlang.beamparser;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.nio.charset.Charset;

import javax.naming.BinaryRefAddr;

import org.apache.commons.lang.ArrayUtils;

public class BeamParser {

	/**
	 * @param args
	 * @throws IOException 
	 */
	public static void main(String[] args) throws IOException {
		File test = new File("src/test/resources/erlcount.beam");
		FileReader reader = new FileReader(test);
		BufferedReader bReader = new BufferedReader(reader);
		Integer s;
		byte[] bytes = new byte[1];
		while ((s = bReader.read()) != -1) {
			bytes = ArrayUtils.add(bytes, s.byteValue());
		}
		System.out.println(new String(bytes));
		ErlangBeam a = ErlangBeamDecryptor.decrypt(bytes);
		System.out.println(a);
		bReader.close();
	}

}
