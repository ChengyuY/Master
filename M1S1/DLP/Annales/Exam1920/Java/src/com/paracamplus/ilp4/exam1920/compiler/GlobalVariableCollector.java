package com.paracamplus.ilp4.exam1920.compiler;

import java.util.Set;

import com.paracamplus.ilp1.compiler.interfaces.IASTCglobalVariable;
import com.paracamplus.ilp4.exam1920.compiler.interfaces.IASTCforeach;
import com.paracamplus.ilp4.exam1920.compiler.interfaces.IASTCvisitor;
import com.paracamplus.ilp1.compiler.CompilationException;

public class GlobalVariableCollector 
extends com.paracamplus.ilp4.compiler.GlobalVariableCollector
implements IASTCvisitor<Set<IASTCglobalVariable>, 
                        Set<IASTCglobalVariable>, 
                        CompilationException> {

	@Override
	public Set<IASTCglobalVariable> visit(IASTCforeach iast, Set<IASTCglobalVariable> env)
			throws CompilationException {
		iast.getIterator().accept(this,  env);
		iast.getBody().accept(this,  env);
		return env;
	}

 
}
