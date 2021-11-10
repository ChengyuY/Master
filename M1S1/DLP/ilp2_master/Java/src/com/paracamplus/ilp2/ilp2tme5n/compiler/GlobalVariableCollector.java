package com.paracamplus.ilp2.ilp2tme5n.compiler;

import java.util.Set;

import com.paracamplus.ilp1.compiler.CompilationException;
import com.paracamplus.ilp1.compiler.interfaces.IASTCglobalVariable;
import com.paracamplus.ilp2.ilp2tme5n.interfaces.IASTbreak;
import com.paracamplus.ilp2.ilp2tme5n.interfaces.IASTcontinue;
import com.paracamplus.ilp2.ilp2tme5n.interfaces.IASTloop;

public class GlobalVariableCollector extends com.paracamplus.ilp2.compiler.GlobalVariableCollector
implements com.paracamplus.ilp2.ilp2tme5n.interfaces.IASTvisitor<Set<IASTCglobalVariable>, 
                        Set<IASTCglobalVariable>, 
                        CompilationException> {

	@Override
	public Set<IASTCglobalVariable> visit(IASTbreak iast, Set<IASTCglobalVariable> data) throws CompilationException {
		// TODO Auto-generated method stub
		return data;
	}

	@Override
	public Set<IASTCglobalVariable> visit(IASTcontinue iast, Set<IASTCglobalVariable> data)
			throws CompilationException {
		// TODO Auto-generated method stub
		return data;
	}

	@Override
	public Set<IASTCglobalVariable> visit(IASTloop iast, Set<IASTCglobalVariable> data) throws CompilationException {
		// TODO Auto-generated method stub
		data = iast.getCondition().accept(this, data);
        data = iast.getBody().accept(this, data);
		return data;
	}
}
