package com.paracamplus.ilp2.ilp2tme6.transform;

import com.paracamplus.ilp1.compiler.CompilationException;
import com.paracamplus.ilp1.compiler.normalizer.INormalizationEnvironment;
import com.paracamplus.ilp1.compiler.normalizer.NoSuchLocalVariableException;
import com.paracamplus.ilp1.interfaces.IASTblock;
import com.paracamplus.ilp1.interfaces.IASTexpression;
import com.paracamplus.ilp1.interfaces.IASTvariable;
import com.paracamplus.ilp1.interfaces.IASTblock.IASTbinding;
import com.paracamplus.ilp2.interfaces.IASTfactory;
import com.paracamplus.ilp2.interfaces.IASTfunctionDefinition;

public class RenameTransform extends CopyTransform<INormalizationEnvironment> {

	protected int cpt;

	public RenameTransform(IASTfactory factory) {
		super(factory);
		cpt = 0;
	}
	
	@Override
	public IASTexpression visit(IASTblock iast, INormalizationEnvironment data)
			throws CompilationException {
		
		IASTbinding[] old = iast.getBindings();
		IASTbinding[] binding = new IASTbinding[old.length];
		INormalizationEnvironment data2 = data;
		
		for (int i = 0; i < old.length; i++) {
			IASTvariable oldv = old[i].getVariable();
			IASTvariable newv = factory.newVariable(oldv.getMangledName()+cpt);
			data2 = data2.extend(oldv,newv);		
			IASTexpression exp = old[i].getInitialisation().accept(this, data);
			binding[i] = factory.newBinding(newv, exp);	
			cpt++;
		}
		IASTexpression body = iast.getBody().accept(this,data2);
		return factory.newBlock(binding, body);
	}
	
	@Override
	public IASTfunctionDefinition visit(IASTfunctionDefinition iast, INormalizationEnvironment data) 
			throws CompilationException {
		
		IASTvariable functionVariable  = (IASTvariable) iast.getFunctionVariable().accept(this,data);
		IASTvariable[] old = iast.getVariables();
		IASTvariable[] variables = new IASTvariable[old.length];
		
		for (int i = 0; i < old.length; i++) {
			IASTvariable oldv = old[i];
			IASTvariable newv = factory.newVariable(oldv.getMangledName()+cpt);
			data = data.extend(oldv, newv);
			variables[i] = newv;
			cpt++;
		}
		IASTexpression body = iast.getBody().accept(this, data);
		return factory.newFunctionDefinition(functionVariable, variables, body);
	}

	@Override
	public IASTexpression visit(IASTvariable iast,
			INormalizationEnvironment data) throws CompilationException {
			try {
				return data.renaming(iast);
			}catch(NoSuchLocalVariableException e){
				return iast;
			}
					
	}
}