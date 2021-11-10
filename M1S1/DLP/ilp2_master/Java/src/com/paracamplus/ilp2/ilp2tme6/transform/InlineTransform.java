package com.paracamplus.ilp2.ilp2tme6.transform;

import com.paracamplus.ilp1.compiler.CompilationException;
import com.paracamplus.ilp1.interfaces.IASTblock;
import com.paracamplus.ilp1.interfaces.IASTexpression;
import com.paracamplus.ilp1.interfaces.IASTinvocation;
import com.paracamplus.ilp1.interfaces.IASTvariable;
import com.paracamplus.ilp1.interfaces.IASTblock.IASTbinding;
import com.paracamplus.ilp2.interfaces.IASTfactory;

public class InlineTransform<Data> extends CopyTransform<Data> {
	
	public InlineTransform(IASTfactory factory) {
		super(factory);
	}
	
	public IASTexpression visit(IASTblock iast, Data data)
			throws CompilationException {
		
		IASTbinding[] binding = iast.getBindings();
		IASTexpression body = iast.getBody().accept(this, data);
		return factory.newBlock(binding, body);
	}
	
	public IASTexpression visit(IASTinvocation iast, Data data)
			throws CompilationException {
		IASTexpression function = iast.getFunction().accept(this, data);   
        IASTbinding[] arguments = new IASTbinding[iast.getArguments().length];
        String str = "abc";
        int i = 0;
        IASTvariable var = factory.newVariable(str);
        for ( IASTexpression arg : iast.getArguments()) {
        	IASTvariable varr = factory.newVariable(var.getMangledName()+i);
        	arguments[i] = factory.newBinding(varr,arg.accept(this, data));
        	i++;
        }
        return factory.newBlock(arguments,function);
	}
	
	
	
	
	
}