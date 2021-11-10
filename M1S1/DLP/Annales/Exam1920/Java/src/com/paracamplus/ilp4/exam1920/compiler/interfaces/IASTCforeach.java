package com.paracamplus.ilp4.exam1920.compiler.interfaces;

import com.paracamplus.ilp1.compiler.interfaces.IASTCvariable;
import com.paracamplus.ilp4.exam1920.interfaces.IASTforeach;

// not strictily necessary, but useful to get more precise type information
public interface IASTCforeach  extends IASTforeach {
	IASTCvariable getVariable();
}
