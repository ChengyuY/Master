package com.paracamplus.ilp4.exam1920.compiler;

import java.io.BufferedWriter;
import java.io.IOException;
import java.io.StringWriter;
import java.util.Set;

import com.paracamplus.ilp1.compiler.AssignDestination;
import com.paracamplus.ilp1.compiler.CompilationException;
import com.paracamplus.ilp1.compiler.NoDestination;
import com.paracamplus.ilp1.compiler.VoidDestination;
import com.paracamplus.ilp1.compiler.interfaces.IASTCglobalVariable;
import com.paracamplus.ilp1.compiler.interfaces.IGlobalVariableEnvironment;
import com.paracamplus.ilp1.compiler.interfaces.IOperatorEnvironment;
import com.paracamplus.ilp1.interfaces.IASTvariable;
import com.paracamplus.ilp1.interfaces.Inamed;
import com.paracamplus.ilp4.compiler.interfaces.IASTCclassDefinition;
import com.paracamplus.ilp4.compiler.interfaces.IASTCprogram;
import com.paracamplus.ilp4.exam1920.compiler.normalizer.INormalizationFactory;
import com.paracamplus.ilp4.exam1920.compiler.normalizer.NormalizationFactory;
import com.paracamplus.ilp4.exam1920.compiler.normalizer.Normalizer;
import com.paracamplus.ilp4.exam1920.compiler.interfaces.IASTCforeach;
import com.paracamplus.ilp4.exam1920.compiler.interfaces.IASTCvisitor;
import com.paracamplus.ilp4.interfaces.IASTprogram;


public class Compiler extends com.paracamplus.ilp4.compiler.Compiler
implements IASTCvisitor<Void, Compiler.Context, CompilationException> {
  
	public Compiler(IOperatorEnvironment ioe, IGlobalVariableEnvironment igve) {
			super(ioe, igve);
	}

	@Override
	   public IASTCprogram normalize(IASTprogram program, 
			   IASTCclassDefinition objectClass) 
					   throws CompilationException {
		INormalizationFactory nf = new NormalizationFactory();
		Normalizer normalizer = new Normalizer(nf, objectClass);
		IASTCprogram newprogram = normalizer.transform(program);
		return newprogram;
	}
	
    public String compile(IASTprogram program, 
            IASTCclassDefinition objectClass) 
            		throws CompilationException {

    	IASTCprogram newprogram = normalize(program, objectClass);
    	newprogram = (IASTCprogram) optimizer.transform(newprogram);

    	GlobalVariableCollector gvc = new GlobalVariableCollector();
    	Set<IASTCglobalVariable> gvs = gvc.analyze(newprogram);
    	newprogram.setGlobalVariables(gvs);

    	FreeVariableCollector fvc = new FreeVariableCollector(newprogram);
    	newprogram = fvc.analyze();

    	Context context = new Context(NoDestination.NO_DESTINATION);
    	StringWriter sw = new StringWriter();
    	try {
    		out = new BufferedWriter(sw);
    		visit(newprogram, context);
    		out.flush();
    	} catch (IOException exc) {
    		throw new CompilationException(exc);
    	}
    	return sw.toString();
    }

   
    // utility to simplify method calls
    private void emitMethodCall(IASTvariable dst, IASTvariable obj, String method, Context context) throws CompilationException {
        emit("{ \n");
        IASTvariable tmpMethod = context.newTemporaryVariable();
        emit("  ILP_general_function " + tmpMethod.getMangledName() + "; \n");
        emit(tmpMethod.getMangledName());
        emit(" = ILP_find_method(");
        emit(obj.getMangledName());
        emit(", &ILP_object_");
        emit(Inamed.computeMangledName(method));
        emit("_method, ");
        emit(1);
        emit(");\n");
        emit(dst.getMangledName() + " = " + tmpMethod.getMangledName() + "(NULL, " + obj.getMangledName() + ");");
        emit("}\n");
    }

    
    @Override
	public Void visit(IASTCforeach iast, Context context) throws CompilationException {
        emit("{ \n");
        // get iterable
        IASTvariable iterable = context.newTemporaryVariable();
        emit("  ILP_Object " + iterable.getMangledName() + "; \n");
        Context c1 = context.redirect(new AssignDestination(iterable));
        iast.getIterator().accept(this, c1);
        // get iterator
        IASTvariable iterator = context.newTemporaryVariable();
        emit("  ILP_Object " + iterator.getMangledName() + "; \n");
        emitMethodCall(iterator, iterable, "iterator", context);
        // loop
        emit("while (1) {\n");
       // next
        IASTvariable var = iast.getVariable();
        emit("  ILP_Object " + var.getMangledName() + "; \n");
        emitMethodCall(var, iterator, "next", context);
        // exit condition
        emit("  if (");
        emit(var.getMangledName());
        emit("== ILP_FALSE) break;\n");
        // body
        Context cb = context.redirect(VoidDestination.VOID_DESTINATION);
        iast.getBody().accept(this, cb);
        // end loop
        emit("}\n");
        emit("}\n");
        whatever.accept(this, context);
		return null;
	}

}
