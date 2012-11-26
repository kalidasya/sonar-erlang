/*
 * Sonar Erlang Plugin
 * Copyright (C) 2012 Tamas Kende
 * kende.tamas@gmail.com
 * 
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3 of the License, or (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02
 */
package org.kalidasya.sonar.erlang.checks;

import java.io.File;

import org.junit.Test;
import org.kalidasya.sonar.erlang.ErlangAstScanner;
import org.sonar.squid.api.SourceFile;

import com.sonar.sslr.squid.checks.CheckMessagesVerifier;

public class NumberOfFunctionClausesCheckTest {

	@Test
	public void test() {
		NoMacrosCheck check = new NoMacrosCheck();

		SourceFile file = ErlangAstScanner.scanSingleFile(new File(
				"src/test/resources/checks/nomacros.erl"), check);
		CheckMessagesVerifier.verify(file.getCheckMessages())
		.next().atLine(2).withMessage("Do not use macros.")
		.next().atLine(10)
		.noMore();
	}
	
	@Test
	public void test2() {
		NoMacrosCheck check = new NoMacrosCheck();
		check.setSkipDefineInFlowControl(false);
		SourceFile file = ErlangAstScanner.scanSingleFile(new File(
				"src/test/resources/checks/nomacros.erl"), check);
		CheckMessagesVerifier.verify(file.getCheckMessages())
		.next().atLine(2).withMessage("Do not use macros.")
		.next().atLine(4).withMessage("Do not use macros.")
		.next().atLine(6).withMessage("Do not use macros.")
		.next().atLine(10)
		.noMore();
	}
}
