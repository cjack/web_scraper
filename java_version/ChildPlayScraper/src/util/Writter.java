package util;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileOutputStream;
import java.io.OutputStreamWriter;

public class Writter {
	
	private BufferedWriter out;

	public Writter (String filename) {
		try {			
			out = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(filename), "UTF-8"));			
		}catch(Exception e) {
			e.printStackTrace();
		}
	}
	
	public Writter(String filename, boolean append) {
        if(append) {
            try {
                out = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(filename, true), "utf-8"));            
            }catch(Exception e) {
                e.printStackTrace();
            }
        } else {
            try {
                out = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(filename), "utf-8"));            
            }catch(Exception e) {
                e.printStackTrace();
            }
        }
    }
	
	public void write(String text) {
		
		try {
			out.write(text);			
			out.flush();
		}catch(Exception e) {
			e.printStackTrace();
		}	
	}
	
	public void close() {
		try {
			out.close();
		}catch(Exception e) {
			e.printStackTrace();
		}
	}
	
	public void writeLine(String text) {
		
		try {
			out.write(text);
			out.newLine();
			out.flush();
		}catch(Exception e) {
			e.printStackTrace();
		}	
	}
}
