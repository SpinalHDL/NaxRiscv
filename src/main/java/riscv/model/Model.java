package riscv.model;



import java.io.File;

public class Model {
    public native long newModel();
    public native void deleteModel(long model);

    static {
        System.load(new File("ext/rvls/build/apps/rvls").getAbsolutePath());
    }
}


