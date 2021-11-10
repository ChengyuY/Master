package com.paracamplus.ilp2.ilp2tme5n.compiler;

import java.util.Set;

import com.paracamplus.ilp1.compiler.CompilationException;
import com.paracamplus.ilp1.compiler.interfaces.IASTClocalVariable;
import com.paracamplus.ilp2.compiler.interfaces.IASTCprogram;
import com.paracamplus.ilp2.ilp2tme5n.interfaces.IASTbreak;
import com.paracamplus.ilp2.ilp2tme5n.interfaces.IASTcontinue;
import com.paracamplus.ilp2.ilp2tme5n.interfaces.IASTloop;
import com.paracamplus.ilp2.ilp2tme5n.interfaces.IASTvisitor;

public class FreeVariableCollector extends com.paracamplus.ilp2.compiler.FreeVariableCollector
implements IASTvisitor<Void, Set<IASTClocalVariable>, CompilationException> {

    public FreeVariableCollector(IASTCprogram program) {
        super(program);
    }
    
	@Override
	public Void visit(IASTbreak iast, Set<IASTClocalVariable> data)
			throws CompilationException {
		return null;
	}

	@Override
	public Void visit(IASTcontinue iast, Set<IASTClocalVariable> data)
			throws CompilationException {
		return null;
	}

	@Override
	public Void visit(IASTloop iast, Set<IASTClocalVariable> data) throws CompilationException {
		// TODO Auto-generated method stub
		iast.getCondition().accept(this, data);
		iast.getBody().accept(this, data);
		return null;
	}
    
   
}