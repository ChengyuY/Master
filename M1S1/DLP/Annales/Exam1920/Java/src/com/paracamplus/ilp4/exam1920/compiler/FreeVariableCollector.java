package com.paracamplus.ilp4.exam1920.compiler;


import java.util.HashSet;
import java.util.Set;

import com.paracamplus.ilp1.compiler.CompilationException;
import com.paracamplus.ilp1.compiler.interfaces.IASTClocalVariable;
import com.paracamplus.ilp4.compiler.interfaces.IASTCprogram;
import com.paracamplus.ilp4.exam1920.compiler.interfaces.IASTCforeach;
import com.paracamplus.ilp4.exam1920.compiler.interfaces.IASTCvisitor;


public class FreeVariableCollector 
extends com.paracamplus.ilp4.compiler.FreeVariableCollector
implements IASTCvisitor<Void, Set<IASTClocalVariable>, CompilationException> {
    
    public FreeVariableCollector(IASTCprogram program) {
        super(program);
    }

	@Override
	public Void visit(IASTCforeach iast, Set<IASTClocalVariable> vars) throws CompilationException {
        Set<IASTClocalVariable> vars2 = new HashSet<>();
        iast.getBody().accept(this,vars2);
        vars2.remove(iast.getVariable());
        vars.addAll(vars2);
        iast.getIterator().accept(this,  vars);
        return null;
	}
    
}
