package com.paracamplus.ilp2.ilp2tme5.compiler;

import java.util.Set;

import com.paracamplus.ilp1.compiler.CompilationException;
import com.paracamplus.ilp1.compiler.interfaces.IASTCglobalVariable;
import com.paracamplus.ilp2.ilp2tme5.interfaces.IASTbreak;
import com.paracamplus.ilp2.ilp2tme5.interfaces.IASTcontinue;

public class GlobalVariableCollector extends com.paracamplus.ilp2.compiler.GlobalVariableCollector
implements com.paracamplus.ilp2.ilp2tme5.interfaces.IASTvisitor<Set<IASTCglobalVariable>, 
                        Set<IASTCglobalVariable>, 
                        CompilationException> {
	
	@Override
	public Set<IASTCglobalVariable> visit(IASTbreak iast,
			Set<IASTCglobalVariable> data) throws CompilationException {
		return data;
	}


	@Override
	public Set<IASTCglobalVariable> visit(IASTcontinue iast,
			Set<IASTCglobalVariable> data) throws CompilationException {
		return data;
	}
}