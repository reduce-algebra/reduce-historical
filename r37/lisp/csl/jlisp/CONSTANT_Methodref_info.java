//created 02/02/02

import java.io.*;

public class CONSTANT_Methodref_info extends Cp_info
{
    public static void main(String[] args) throws IOException
    {
        short cidx = (short)0x4;
        short ntidx = (short)0xf;
        CONSTANT_Methodref_info cm = new CONSTANT_Methodref_info(cidx, ntidx);
        cm.printBytes(cm.dumpBytes());
        Jlisp.println("\n");
                
        short cidx2 = (short)0x3;
        short ntidx2 = (short)0x10;
        CONSTANT_Methodref_info cm2 = new CONSTANT_Methodref_info(cidx2, ntidx2);
        cm2.printBytes(cm2.dumpBytes());
        Jlisp.println("\n");
    }
        
    short class_index;
    short name_and_type_index;
                

    //constructor
    CONSTANT_Methodref_info(short classIndex, short ntIndex)
        throws IOException
    {   tag = CONSTANT_Methodref;        
        class_index = classIndex;
        name_and_type_index = ntIndex;
        //below is the toInfo() method of Code_Attribute.java
        byte[][] infoTemp = new byte[2][0];
        infoTemp[0] = shortToByteArray(class_index);
        infoTemp[1] = shortToByteArray(name_and_type_index);
                                
        info = new byte[4];
        info = flatBytes(infoTemp);
    }
}

// end of CONSTANT_Methodref_info.java
