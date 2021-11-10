package com.paracamplus.ilp4.exam1920.compiler.interfaces;

public interface IASTCvisitor<Result, Data, Anomaly extends Throwable>
extends com.paracamplus.ilp4.compiler.interfaces.IASTCvisitor<Result, Data, Anomaly> {
    Result visit(IASTCforeach iast, Data data) throws Anomaly;
}
