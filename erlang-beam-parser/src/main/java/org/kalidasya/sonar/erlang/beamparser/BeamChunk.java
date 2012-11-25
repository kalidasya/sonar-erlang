package org.kalidasya.sonar.erlang.beamparser;

import java.nio.ByteBuffer;

import org.apache.commons.lang.ArrayUtils;

public class BeamChunk {

	protected byte[] id = new byte[4];
	protected byte[] length = new byte[4];
	protected byte[] numOfEntries = new byte[4];
	protected byte[] data;

	public BeamChunk(byte[] fragment){
		this.id = ArrayUtils.subarray(fragment, 0, 4);
		this.length= ArrayUtils.subarray(fragment, 4, 8);
		this.numOfEntries = ArrayUtils.subarray(fragment, 8, 12);
		this.data = ArrayUtils.subarray(fragment, 12, getLength()+11);
	}
	
	public Integer getLength() {
		return ByteBuffer.wrap(length).getInt();
	}
	
	public Integer getNumOfEntries() {
		return ByteBuffer.wrap(numOfEntries).getInt();
	}
	

}
