package util;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.regex.Pattern;

public class PatternProcessor {

	protected HashMap<Integer, ArrayList<Pattern>> patternMap;
	protected HashMap<Integer, HashMap<String, String>> replacementsMap;
	protected HashMap<Integer, String> dataMap;
	
	protected int lastPosition = 0;
	
	public PatternProcessor () {		
		patternMap = new HashMap<Integer, ArrayList<Pattern>>();
		replacementsMap = new HashMap<Integer, HashMap<String,String>>();
		dataMap = new HashMap<Integer, String>();
	}
	
	
	public void addPattern(int position, Pattern pattern) {
		if (!patternMap.containsKey(position)) {
			patternMap.put(position, new ArrayList<Pattern>());
		}
		
		patternMap.get(position).add(pattern);
		if (position > lastPosition) lastPosition = position;
	}
	
	public void addReplacement(int position, String what, String with) {
		if (!replacementsMap.containsKey(position)) {
			replacementsMap.put(position, new HashMap<String, String>());
		}
		
		replacementsMap.get(position).put(what, with);
		
	}


	public String processPatterns(String data, Writter writter) {
		StringBuilder sb = new StringBuilder();
		
		for (int pos = 1; pos <= lastPosition; pos++) {
			
			ArrayList<Pattern> patternList = patternMap.get(pos);
			
			String value = "";
			
			if (dataMap.containsKey(pos)) {
				value = dataMap.get(pos);
			} else {
				value = getDataFromPatterns(data, patternList);
			}			
			
			if (replacementsMap.containsKey(pos)) {
				HashMap<String, String> replacements = replacementsMap.get(pos);
				for (String key : replacements.keySet()) {
					value = value.replaceAll(key, replacements.get(key));
					value = value.trim();
				}
			}
			
			sb.append(Patterns.formatValue(value.trim()));
		}		
		
		sb.deleteCharAt(sb.length()-1);
		writter.writeLine(sb.toString());
		return sb.toString();
	}
	
	private String getDataFromPatterns(String data, ArrayList<Pattern> patternList) {
		String value = "";
		
		for (Pattern pattern : patternList) {
			value = Patterns.findInString(data, pattern);
			if (value.equals("")) {
				continue;
			} else {
				break;
			}
		}
		
		return value.trim();
	}


	public void addData(int position, String data) {
		dataMap.put(position, data);
		if (position > lastPosition) lastPosition = position;
	}
	
	public String getData(int position) {
		return dataMap.get(position);
	}
	
}
