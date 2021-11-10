package com.paracamplus.ilp4.exam1920.interpreter;

import com.paracamplus.ilp4.exam1920.ast.ASTfactory;
import com.paracamplus.ilp4.exam1920.ast.ForeachTransform;
import com.paracamplus.ilp4.exam1920.interfaces.IASTforeach;
import com.paracamplus.ilp4.exam1920.interfaces.IASTvisitor;
import com.paracamplus.ilp4.interfaces.IASTprogram;
import com.paracamplus.ilp4.interpreter.interfaces.IClassEnvironment;
import com.paracamplus.ilp4.interpreter.interfaces.IInstance;
import com.paracamplus.ilp4.parser.ilpml.ILPMLProgramPrinter;
import com.paracamplus.ilp1.interpreter.interfaces.EvaluationException;
import com.paracamplus.ilp1.interpreter.interfaces.IGlobalVariableEnvironment;
import com.paracamplus.ilp1.interpreter.interfaces.ILexicalEnvironment;
import com.paracamplus.ilp1.interpreter.interfaces.IOperatorEnvironment;

public class Interpreter 
extends com.paracamplus.ilp4.interpreter.Interpreter
implements IASTvisitor<Object, ILexicalEnvironment, EvaluationException> {

	public Interpreter(IGlobalVariableEnvironment globalVariableEnvironment, IOperatorEnvironment operatorEnvironment,
			IClassEnvironment classEnvironment) {
		super(globalVariableEnvironment, operatorEnvironment, classEnvironment);
	}
	
	
	@Override
	public Object visit(IASTprogram prog, ILexicalEnvironment data) throws EvaluationException {

		// transformation
		if (false) {
			ForeachTransform<Void> v = new ForeachTransform<>(new ASTfactory());
			prog = (IASTprogram) v.visit(prog, null);
			ILPMLProgramPrinter printer = new ILPMLProgramPrinter();
			printer.setInput(prog);
			try { System.out.println(printer.getString()); } catch (Exception e) {	e.printStackTrace(); 	}
		}
		
		return super.visit(prog,data);
	}

	@Override
	public Object visit(IASTforeach iast, ILexicalEnvironment data) throws EvaluationException {
		// get iterable
		Object iterableVal = iast.getIterator().accept(this, data);
		if (!(iterableVal instanceof IInstance))
			throw new EvaluationException("iterable should be an object");
		IInstance iterableObj = (IInstance) iterableVal; 
		// get iterator
		Object iteratorVal = iterableObj.send(this, "iterator", new Object[0]);
		if (!(iteratorVal instanceof IInstance))
			throw new EvaluationException("iterator() should return an object");
		IInstance iteratorObj = (IInstance)iteratorVal;
		// loop
		while (true) {
			Object val = iteratorObj.send(this, "next", new Object[0]);
			if (val.equals(Boolean.FALSE)) break;
			ILexicalEnvironment data2 = data.extend(iast.getVariable(), val);
			iast.getBody().accept(this, data2);
		}
		// loop end
		return Boolean.FALSE;
	}

}
