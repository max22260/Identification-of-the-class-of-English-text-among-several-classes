
import com.sun.xml.internal.bind.v2.runtime.unmarshaller.Loader;
import java.io.File;
import java.io.FileWriter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.tartarus.snowball.*;

/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
/**
 *
 * @author fake
 */
public class Model {
 
    
    public static void SaveContent(String content) {
        content = content.toLowerCase();
        ArrayList<String> StopWords = lib.loadFile("./R_Workspace/stopwords.txt");
         String cleaned_content = lib.RemoveStopWords(content, StopWords);
        cleaned_content= lib.Remove_punctuations(cleaned_content);
        String stemmed_content = lib.Stemm(cleaned_content);
        saveFile(stemmed_content, "./R_Workspace/content.txt");
    }
    
    public static String getResult(){
        String result = lib.Run_Command("Rscript Script.R");
        return(result);
    }
    
    

    private static void saveFile(String content, String dir) {
        try {
            FileWriter file = new FileWriter(new File(dir));
            file.write(content);
            file.flush();
            file.close();
        } catch (Exception e) {
            System.out.println("Error in saveFile" + e.getMessage());
        }
    }
}
