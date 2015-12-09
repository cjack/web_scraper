package util;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.zip.GZIPInputStream;

import org.apache.commons.httpclient.Header;
import org.apache.commons.httpclient.HttpMethod;
import org.apache.commons.httpclient.methods.GetMethod;

public class Utils {

    public static String joinStringFromArray(ArrayList<String> array, String append) {
    	StringBuilder retString = new StringBuilder();
		for (String one : array) {
			retString.append(one).append(append);
		}
		
		if (retString.length() > 2) {
			retString.delete(retString.length()-append.length(), retString.length());
		}
		
		return retString.toString();
    }
    
    public static String getResponseString(InputStream input) {
    	try {			
			BufferedReader in = new BufferedReader(new InputStreamReader(input));
			String line = null;
			StringBuilder sb = new StringBuilder();
			
			while ((line = in.readLine()) != null) {								
				sb.append(line).append("\n");
			}
			
			in.close();
			return sb.toString();
		} catch (Exception e) {
			e.printStackTrace();
		}
    	
    	return null;
    }
    
	public static String getHtml(HttpMethod get) {
		BufferedReader in = null;
		try {
			StringBuilder sb = new StringBuilder();
			String line;
			
			
			if (get.getResponseHeader("Content-Encoding") != null && get.getResponseHeader("Content-Encoding").getValue().equals("gzip")) {
				in = new BufferedReader(new InputStreamReader(new GZIPInputStream(get.getResponseBodyAsStream())));
			} else {
				in = new BufferedReader(new InputStreamReader(get.getResponseBodyAsStream()));
			}				
			 
			while ((line = in.readLine()) != null) {
				sb.append(line).append("\n");
			}
			
			in.close();
			get.releaseConnection();
			
			return sb.toString();
		} catch (Exception e) {
			e.printStackTrace();
		}
		return null;
	}
    
	public static void printHeaders(Header[] headers) {
		for (Header h : headers) {
			System.out.println(h.getName() + "=" + h.getValue());
		}
		
	}

	public static HashMap<String, String> loadParamsFromFile(String fileString) {
		
		ArrayList<String> lines = Files.loadListFromFile(fileString);
		HashMap<String, String> paramMap = new HashMap<String, String>();
		
		for (String line : lines) {
			String parts [] = line.split("=");
			paramMap.put(parts[0], parts.length>1?parts[1]:"");
		}
		
		return paramMap;
	}
	
	public static HashMap<String, String> loadRequestHeadersFromFile(String fileString) {
		
		ArrayList<String> lines = Files.loadListFromFile(fileString);
		HashMap<String, String> paramMap = new HashMap<String, String>();
		
		for (String line : lines) {
			String parts [] = line.split(":");
			paramMap.put(parts[0], parts.length>1?parts[1].trim():"");
		}
		
		return paramMap;
	}
}
