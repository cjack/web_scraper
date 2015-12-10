package bots;

import java.io.*;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.lang.*;

import master.Master;
import util.PatternProcessor;
import util.Sha1Hash;
import util.Writter;

public class ChildPlay extends Master{

	private Pattern breadcrumbsWrapperPattern;
	private Pattern breadcrumbPattern;
	private String breadcrumbsWrapper;
	private ArrayList<String> breadcrumbs;
	private String breadcrumbText;
	private Pattern imagePattern;
	private ArrayList<String> images;
	private Pattern idPattern;
	private String id;
	private int i;
	private String imagesText;
	private Pattern descriptionPattern;
	private String desc;
	private ArrayList<String> idList;
	private int counter;


	public ChildPlay() {
		
		idList = new ArrayList<String>();
		
		writter = new Writter("childplay.csv");
		writter.writeLine("Detail Page URL,Name,Price,Item condition,Availability,Brand,Category,Age Group,ID,Description,The navigation link,Images,Images URL");
		
		pProc = new PatternProcessor();
		
		paginationPattern = Pattern.compile("<a class=\"next_page disabled");
		detailsPagePattern = Pattern.compile("<div class='TidalWave ShopDiscover-item'>.+?<a.+?href='(/a/.+?)'", Pattern.DOTALL);
		imagePattern = Pattern.compile("<img alt=\".+?\" class=\"RatioBox-child\\s*\" src=\"(.+?)\"");

		pProc.addPattern(2, Pattern.compile("itemprop='name'>(.+?)<"));
		pProc.addPattern(3, Pattern.compile("itemprop='price'>(.+?)<"));
		pProc.addPattern(4, Pattern.compile("<b>Item condition:</b>\\s*(.+?)\\s*</li>"));
		pProc.addPattern(5, Pattern.compile("<li><b>Availability:</b>(.+?)</li>"));
		pProc.addPattern(6, Pattern.compile("<li><b>Brand:</b>(.+?)</li>"));
		pProc.addPattern(7, Pattern.compile("<li>\\s*<b>Category:</b>\\s*(.+?)\\s*</li>"));
		pProc.addPattern(8, Pattern.compile("<li>\\s*<b>Age Group:</b>\\s*(.+?)\\s*</li>"));
		pProc.addPattern(9, Pattern.compile("<li><b>ID:</b>(.+?)</li>"));
		pProc.addPattern(10, imagePattern);

		breadcrumbsWrapperPattern = Pattern.compile("<div class='Breadcrumbs'>(.+?)<div class='layout-withRightSidebar-content'>", Pattern.DOTALL);
		breadcrumbPattern = Pattern.compile("<span itemprop='title'>(.+?)<");
		idPattern = Pattern.compile("<li><b>ID:</b>(.+?)</li>");
		descriptionPattern = Pattern.compile(">Description</h2>(.+?)<div class=\" Collapsible", Pattern.DOTALL);
		
		pProc.addReplacement(6, "<.+?>", "");
		pProc.addReplacement(7, "<.+?>", "");

		for(int i = 1; i <= 13; i++) {
			pProc.addReplacement(i, "<.+?>", "");
			pProc.addReplacement(i, "&nbsp;", "");
			pProc.addReplacement(i, "\\s+", " ");
			pProc.addReplacement(i, "&amp;", "&");
			pProc.addReplacement(i, "&#39;", "'");
		}

		
	}


	public int getCounter(){
		return this.counter;
	}



	public void run() {
		
		baseURL = "http://www.childplay.com.au";
		
		firstPageNumber = 1;
		pageIncrement = 1;
		
		incrementPartOneURL = "http://www.childplay.com.au/s?page=";
		
		typeOneProcessing();

		
	}
	
	public void write(String data) {
		
		desc = findInString(data, descriptionPattern);
		
		if(!desc.equals("")) {
			desc = desc.replaceAll("<br>", "\\\\n");
			desc = desc.replaceAll("<.+?>", "");
			
			pProc.addData(10, desc);
		}
		else {
			pProc.addData(10, "");
		}
		
		id = findInString(data, idPattern);
		id = id.replaceAll("\\s+", "");
		
		if(idList.contains(id)) {
			return;
		}
		
		idList.add(id);
		
		breadcrumbsWrapper = findInString(data, breadcrumbsWrapperPattern);
		
		breadcrumbs = findAllInString(breadcrumbsWrapper, breadcrumbPattern);
		
		breadcrumbText = "Home,";
		
		for(String brcr : breadcrumbs) {
			breadcrumbText += brcr + ",";
		}
		
		breadcrumbText = breadcrumbText.substring(0, breadcrumbText.lastIndexOf(","));
		
		pProc.addData(11, breadcrumbText);
		
		images = findAllInString(data, imagePattern);
		String imagesUrl = "";
		if(!images.isEmpty()) {
			
			i = 1;
			imagesText = "";
			
			for(String img : images) {

				//File f = new File ("imagesChildPlay/");
//				if (!f.exists()) {
//                    f.mkdirs();
//                }
				//saveImage("http://www.childplay.com.au" + img , "imagesChildPlay/" + id + "_" + i + ".jpg");
				imagesUrl = imagesUrl + "http://www.childplay.com.au" + img + "; ";
				//imagesUrl.append(baseURL + img + ',');
				imagesText += id + "_" + i + ".jpg" + ",";

				i++;
			}

			pProc.addData(13, imagesUrl);
			if(!imagesText.equals("")) {
				imagesText = imagesText.substring(0, imagesText.lastIndexOf(","));
			}

			pProc.addData(12, imagesText);
		}
		else {
			pProc.addData(12, "");
		}
		pProc.processPatterns(data, writter);
		
	}
	
	public static void saveImage(String imageUrl, String destinationFile) throws IOException {
		URL url = new URL(imageUrl);
		InputStream is = url.openStream();		
		OutputStream os = new FileOutputStream(destinationFile);

		byte[] b = new byte[2048];
		int length;

		while ((length = is.read(b)) != -1) {
			os.write(b, 0, length);
		}

		is.close();
		os.close();
	}
	
	protected void typeOneProcessing() {
		paginationURL = null;
		detailsPageURL = null;
		String pageHtml = null;
		String detailsPageHtml = null;
		String carURLHash = null;
		long carID;
		int errorDelay = initialErrorDelay;
		int counter = 0;

		Matcher paginationMatcher;
		Matcher urlMatcher;
		HashSet<String> carURLs;


		lastPageNumber = 2;
		mainLoop: for (int i = firstPageNumber; i <= lastPageNumber; i += pageIncrement) {
			paginationURL = incrementPartOneURL + formParam + formValue + pageNumberParam + i + incrementPartTwoURL;
			pageHtml = getHtml(paginationURL);
			
		//	System.out.println(pageHtml);

			if (pageHtml != null) {

				System.out.println("-Pagination page: " + paginationURL);
//				counter++;
				errorCount = 0;
				errorDelay = initialErrorDelay;
				urlMatcher = detailsPagePattern.matcher(pageHtml);
				carURLs = new HashSet<String>();
				while (urlMatcher.find()) {
					counter++;
					detailsPageURL = urlMatcher.group(1);
					if (!detailsPageURL.startsWith("http:")) {
						detailsPageURL = baseURL + detailsPageURL;
					}

					detailsPageURL = fixURL(detailsPageURL);
					System.out.println("\t"+ detailsPageURL);
					carURLHash = Sha1Hash.SHA1(detailsPageURL);

					if (carURLs.contains(carURLHash)) {
						continue;
					} else {
						carURLs.add(carURLHash);
					}
		
					detailsPageHtml = getHtml(detailsPageURL);
					//detailsPageHtml = getHtml("http://www.childplay.com.au/a/marble-run/hape/vic/clifton-hill/quadrilla-music-motion/100040663");

					if (detailsPageHtml != null) {
						pProc.addData(1, detailsPageURL);
						write(detailsPageHtml);
						recordCount++;
						newRecordsCount++;
					}
					// Just for the test
//					sleep(5000);
				}
				paginationMatcher = paginationPattern.matcher(pageHtml);
				if (paginationMatcher.find()) {
					break;
				}
			} else {
				errorCount++;
				if (errorCount == 5) {
					break;
				}
				i -= pageIncrement;
				
				sleep(errorDelay);
				errorDelay *= 5;				
			}
			if (stopped) {
				break;
			}
		}
		System.out.println("The total number of the items is: " + counter);
	}


	public static void main(String[] args) throws FileNotFoundException {


		long start = System.nanoTime();
		new ChildPlay().run();
		long elapsedTime = System.nanoTime() - start;
		elapsedTime = elapsedTime / 1000000000;
		System.out.println("The entire time used: " + elapsedTime + "s");
		if(elapsedTime > 600){
			System.out.println("Namely, " + elapsedTime / 60 + "mins");
		}
	}

}
