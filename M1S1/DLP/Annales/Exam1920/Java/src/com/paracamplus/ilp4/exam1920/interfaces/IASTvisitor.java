package com.paracamplus.ilp4.exam1920.interfaces;

public interface IASTvisitor<Result,Data,Anomaly extends Throwable> 
extends com.paracamplus.ilp4.interfaces.IASTvisitor<Result, Data, Anomaly> {
    Result visit(IASTforeach iast, Data data) throws Anomaly;
}
