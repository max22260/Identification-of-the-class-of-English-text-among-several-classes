
import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Scanner;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.tartarus.snowball.SnowballStemmer;

/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
/**
 *
 * @author fake
 */
public class lib {

    public static String Remove_punctuations(String Document) {
        String[] Splitted_Words = Document.split(" ");
        ArrayList<String> Clean_SplittedWords = new ArrayList<String>();
        for (String splitted_word : Splitted_Words) {
            String[] Words_temp = splitted_word.replaceAll("[^a-zA-Z ]", "").toLowerCase().split("\\s+");
            String CleanWord_temp = "";
            for (String CW : Words_temp) {
                if (CW.trim().length() == 0) {

                    continue;
                }
                CleanWord_temp += CW.trim();
            }
            Clean_SplittedWords.add(CleanWord_temp.trim());
        }
        String Clean_Document = "";
        for (String cleanword : Clean_SplittedWords) {
            Clean_Document = Clean_Document.trim() + " " + cleanword;
        }
        return (Clean_Document);
    }

    public static String RemoveStopWords(String Document, ArrayList<String> StopWords) {
        String[] Words_temp = Document.split(" ");
        ArrayList<String> Words = new ArrayList<>(Arrays.asList(Words_temp));
        int Size = Words.size();
        for (int i = 0; i < Size; i++) {
            for (String StopWord : StopWords) {
                if (Words.get(i).trim().toLowerCase().equals(StopWord)) {
                    Words.remove(i);
                    i--;
                    Size = Words.size();
                    break;
                }
            }
        }

        String Clean_Document = "";
        for (String word : Words) {
            Clean_Document = Clean_Document.trim() + " " + word;
        }
        return (Clean_Document);
    }

    public static ArrayList<String> loadFile(String Dir) {

        ArrayList<String> lines = new ArrayList<String>();
        try {
            Scanner Reading = new Scanner(new File(Dir));
            while (Reading.hasNext()) {
                lines.add(Reading.nextLine());
            }
        } catch (FileNotFoundException e) {
            System.out.println("Error , Because of loadFile" + e.getMessage());
            System.exit(0);
        }
        return (lines);

    }

    public static String Run_Command(String command) {
        ArrayList<String> all = new ArrayList<String>();
        try {
            Process process = Runtime.getRuntime().exec(command);
            BufferedReader reader
                    = new BufferedReader(new InputStreamReader(process.getInputStream()));
            String line = "";
            while ((line = reader.readLine()) != null) {
                all.add(line);
            }
        } catch (Exception e) {
        }
        String result = "";
        for (String a : all) {
            result = result + a;
        }
        String[] final_result = result.split("\"");
        return (final_result[1]);
    }
    
       public static String Stemm(String content){
        String all = "";
        try {
            Class stemClass = Class.forName("org.tartarus.snowball.ext."
                    + "porter" + "Stemmer");
            SnowballStemmer stemmer = (SnowballStemmer) stemClass.newInstance();
            String[] content_words = content.split(" ");
            ArrayList<String> words = new ArrayList<>();
            for(String c:content_words){
                stemmer.setCurrent(c);
                stemmer.stem();
                words.add(stemmer.getCurrent());
            }
            
            for(String s:words){
                all = all.trim() + " " +s;
            }
            return(all);
            
        } catch (ClassNotFoundException ex) {
            Logger.getLogger(Model.class.getName()).log(Level.SEVERE, null, ex);
        } catch (InstantiationException ex) {
            Logger.getLogger(Model.class.getName()).log(Level.SEVERE, null, ex);
        } catch (IllegalAccessException ex) {
            Logger.getLogger(Model.class.getName()).log(Level.SEVERE, null, ex);
        }
        return(all);
    }


}
