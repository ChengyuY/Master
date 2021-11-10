package com.paracamplus.ilp4.exam1920.compiler.normalizer;

import com.paracamplus.ilp1.compiler.CompilationException;
import com.paracamplus.ilp1.compiler.interfaces.IASTClocalVariable;
import com.paracamplus.ilp1.compiler.normalizer.INormalizationEnvironment;
import com.paracamplus.ilp1.interfaces.IASTexpression;
import com.paracamplus.ilp4.compiler.interfaces.IASTCclassDefinition;
import com.paracamplus.ilp4.exam1920.interfaces.IASTforeach;
import com.paracamplus.ilp4.exam1920.interfaces.IASTvisitor;

public class Normalizer 
extends com.paracamplus.ilp4.compiler.normalizer.Normalizer
implements IASTvisitor<IASTexpression, INormalizationEnvironment, CompilationException> {

	private INormalizationFactory factory;
	
	public Normalizer(INormalizationFactory factory, IASTCclassDefinition objectClass) {
		super(factory, objectClass);
		this.factory = factory;
	}
	
	@Override
	public IASTexpression visit(IASTforeach iast, INormalizationEnvironment env) throws CompilationException {
        IASTClocalVariable var = factory.newLocalVariable(iast.getVariable().getName());
        IASTexpression iterator = iast.getIterator().accept(this, env);
        INormalizationEnvironment env2 = env.extend(iast.getVariable(), var);
        IASTexpression body = iast.getBody().accept(this, env2);
        return factory.newForeach(var, iterator, body);
	}

}
