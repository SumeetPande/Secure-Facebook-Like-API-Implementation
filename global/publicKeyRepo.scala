package global

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.security.KeyPair;
import java.security.KeyPairGenerator;
import java.security.NoSuchAlgorithmException;
import java.security.PrivateKey;
import java.security.PublicKey;
import javax.crypto.Cipher;
import main.client

/**
 * @author dd
 */
object publicKeyRepo extends App {
  
  def addPublicKey(user_id:Int,pubKey:PublicKey) = {
    println("Public key and userid  in global : "+pubKey.toString+" , "+user_id)
    //publicKeyMap.put(user_id, pubKey)
    client.publicKeyTable += (user_id -> pubKey)
    println("current size of public key table: "+client.publicKeyTable.size)
  }
  
  def getPublicKey(user_id:Int):PublicKey = {
    //println("returning: "+client.publicKeyTable.getOrElse(user_id,null))
    return client.publicKeyTable.getOrElse(user_id,null)
  }  
}