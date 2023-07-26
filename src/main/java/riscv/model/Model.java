package riscv.model;

//    val verilatorNativeImplCode =
//            s"""package wrapper_${workspaceName};
//         |import spinal.sim.IVerilatorNative;
//         |
//         |public class VerilatorNative implements IVerilatorNative {
//         |    public long newHandle(String name, int seed) { return newHandle_${uniqueId}(name, seed);}
//         |    public boolean eval(long handle) { return eval_${uniqueId}(handle);}
//         |    public int get_time_precision(long handle) { return getTimePrecision_${uniqueId}(handle);}
//public native long newHandle_${uniqueId}(String name, int seed);
//public native boolean eval_${uniqueId}(long handle);
//public native int getTimePrecision_${uniqueId}(long handle);
//public native void sleep_${uniqueId}(long handle, long cycles);
//public native long getU64_${uniqueId}(long handle, int id);
//public native long getU64mem_${uniqueId}(long handle, int id, long index);
//public native void setU64_${uniqueId}(long handle, int id, long value);
//public native void setU64mem_${uniqueId}(long handle, int id, long value, long index);
//public native void getAU8_${uniqueId}(long handle, int id, byte[] value);
//public native void getAU8mem_${uniqueId}(long handle, int id, byte[] value, long index);
//public native void setAU8_${uniqueId}(long handle, int id, byte[] value, int length);
//public native void setAU8mem_${uniqueId}(long handle, int id, byte[] value, int length, long index);
//public native void deleteHandle_${uniqueId}(long handle);

import java.io.File;

public class Model {
    public native long newModel();
    public native void deleteModel(long model);

    static {
        System.load(new File("src/main/cpp/riscv/model/model.so").getAbsolutePath());
    }
}


/*
    public boolean eval(long handle);
    public int get_time_precision(long handle);
    public void sleep(long handle, long cycles);
    public long getU64(long handle, int id);
    public void setU64(long handle, int id, long value);
    public void getAU8(long handle, int id, byte[] value);
    public void setAU8(long handle, int id, byte[] value, int length);
    public long getU64_mem(long handle, int id, long index);
    public void setU64_mem(long handle, int id, long value, long index);
    public void getAU8_mem(long handle, int id, byte[] value, long index);
    public void setAU8_mem(long handle, int id, byte[] value, int length, long index);
 */