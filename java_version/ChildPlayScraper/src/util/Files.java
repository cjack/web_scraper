package util;

import java.io.BufferedReader;
import java.io.FileReader;
import java.util.ArrayList;

public class Files {
	
	public static String loadFileData(String filename) {    	
    	try {			
    		StringBuilder sb = new StringBuilder();
			BufferedReader in = new BufferedReader(new FileReader(filename));
			String line = null;
			
			while ((line = in.readLine()) != null) {								
				sb.append(line).append("\n");
			}
			
			in.close();
			return sb.toString();
		} catch (Exception e) {
			System.out.println("");
		}
    	
    	return null;
    }

    public static ArrayList<String> loadListFromFile(String filename) {
    	ArrayList<String> list = new ArrayList<String>();
    	try {			
			BufferedReader in = new BufferedReader(new FileReader(filename));
			String line = null;
			
			while ((line = in.readLine()) != null) {								
				list.add(line.trim());
			}
			
			in.close();
			return list;
		} catch (Exception e) {
			e.printStackTrace();
		}
    	
    	return null;
    }
    
    public static ArrayList<String> loadListFromFile(String filename, int column) {
    	return loadListFromFile(filename, column, ",");
    }
    
    public static ArrayList<String> loadListFromFile(String filename, int column, String separator) {
    	ArrayList<String> list = new ArrayList<String>();
    	try {			
			BufferedReader in = new BufferedReader(new FileReader(filename));
			String line = null;
			
			while ((line = in.readLine()) != null) {								
				String parts[] = line.split(separator);
				list.add(parts[column-1].trim().replaceAll("\"", ""));
			}
			
			in.close();
			return list;
		} catch (Exception e) {
			e.printStackTrace();
		}
    	
    	return null;
    }
}
