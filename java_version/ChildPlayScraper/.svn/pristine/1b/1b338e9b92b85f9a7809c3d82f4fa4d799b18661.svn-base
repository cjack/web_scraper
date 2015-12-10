package util;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Patterns {

	public static ArrayList<String> findAllInString(String string, Pattern pattern) {
		ArrayList<String> list = new ArrayList<String>();
		
		Matcher m = pattern.matcher(string);
		while (m.find()) {
			String value = m.group(1).trim();
			list.add(value);
		}
		
		return list;		
	}

	public static String [] findGroupsInString(String string, Pattern pattern, int groups) {
		String [] parts = new String[groups];
		
		Matcher m = pattern.matcher(string);
		while (m.find()) {
			for (int curGroup = 0; curGroup < groups; curGroup++) {
				parts[curGroup] = m.group(curGroup+1).trim();
			}
			
			return parts;
		}
		return null;
	}
	
	public static ArrayList<String []> findAllGroupsInString(String string, Pattern pattern) {
		ArrayList<String []> parts = new ArrayList<String[]>();
		
		Matcher m = pattern.matcher(string);
		while (m.find()) {
			String groups[] = new String[m.groupCount()]; 
			for (int curGroup = 0; curGroup < m.groupCount(); curGroup++) {
				groups[curGroup] = m.group(curGroup+1).trim();
			}
			
			parts.add(groups);
		}
		return parts;
	}
	
	public static String findInString(String string, Pattern pattern) {
		Matcher m = pattern.matcher(string);
		while (m.find()) {
			String value = m.group(1).trim();
			return value;
		}
		return "";
	}
	
	public static String formatValue(String value) {
		value = value.replaceAll("\"", "'");
		return "\"" + value + "\",";
	}
	
	public static String formatValueWithoutAppend(String value) {
		value = value.replaceAll("\"", "'");
		return "\"" + value + "\"";
	}
	
	public static String formatValue(String value, String separator) {
		value = value.replaceAll("\"", "'");
		return "\"" + value + "\"" + separator;
	}
}
