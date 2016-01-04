package main

import akka.actor._
import spray.http._
import spray.client.pipelining._
import globalData.dataPool
import scala.util.Random
import javax.security._
import java.security.AlgorithmParameters._
import java.security._
import javax.crypto._
import javax.crypto.spec._
import java.util.UUID
import java.lang.Object
import java.security.MessageDigest._
import java.net.URLEncoder._
import java.net.URLDecoder._
import org.apache.commons.codec.binary.Base64
import scala.collection.mutable.ArrayBuffer
import java.security.KeyPair
import java.security.KeyPairGenerator
import java.security.NoSuchAlgorithmException
import java.security.PrivateKey
import java.security.PublicKey
//import javax.crypto.Cipher
import global.publicKeyRepo;
import java.util.Arrays
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import org.apache.commons.codec.binary.Hex

/**
 * @author Drumil Deshpande
   @author Sumeet Pande
 */

class user(num_users:Int,userId : Int, actSys:ActorSystem) extends Actor{
  
  import actSys.dispatcher
  val pipeline2 = sendReceive
  val pipeline3 = sendReceive
  var password:String = UUID.randomUUID.toString.replaceAll("-", "")
  var salt:String = UUID.randomUUID.toString.replaceAll("-", "")
  var passHash:String = ""
  val client = context.actorSelection("akka://FacebookSimulator/user/clientSystem")
  var loginStatus:Boolean = false;
  var iv1:IvParameterSpec = null
  var iv = ArrayBuffer[IvParameterSpec] ()
  var msg = Array[Byte]()
  var cipher:Cipher = Cipher.getInstance("AES/CBC/PKCS5Padding")
  var secret = ArrayBuffer[SecretKey] ()
  var secretK:SecretKey = null
  var pubKey:PublicKey = null
  var priKey:PrivateKey = null
  var key:KeyPair = null
  
  def stringToBytes(msg : String) : Array[Byte] = {
    Hex.decodeHex(msg.toCharArray)
  }

  def byteToString(bytes : Array[Byte]): String = {
    Hex.encodeHexString(bytes)
  }
  
  def receive = {
    
    /* This method was used in first part of the proj */
    case "generateInitialPost" =>{
        //println("User : " + userId +" posting new message.")
        var origPost:String = dataPool.posts(Random.nextInt(dataPool.posts.length))
        //println("The new Post is : " + origPost)
        var modPost:String= origPost.replaceAll(" ", "%20")
        pipeline2(Post("http://localhost:8080/facebook/initPost?userid="+userId+"&initPost="+modPost))
    }
    
    /* AES key for the user is created
     * */
    case "createKey" => {
      //println("In user for create key: "+userId)
      var factory:SecretKeyFactory = SecretKeyFactory.getInstance("PBKDF2WithHmacSHA256")
      var spec = new PBEKeySpec(password.toCharArray, salt.getBytes , 65536, 256);
      var tmp:SecretKey = factory.generateSecret(spec)
      secretK = new SecretKeySpec(tmp.getEncoded,"AES")
      //secret += secretK
      cipher.init(Cipher.ENCRYPT_MODE, secretK)
      iv1 = new IvParameterSpec(cipher.getIV)
      self ! "createRSAKeys"
    }
    
    /* RSA keys for the user are created 
     * Public RSA key is stored in a data Pool which contains public keys of all users
     * */
    case "createRSAKeys" => {
      var keyGen:KeyPairGenerator = KeyPairGenerator.getInstance("RSA")
      keyGen.initialize(2048)
      key = keyGen.generateKeyPair
      pubKey = key.getPublic
      priKey = key.getPrivate
      //println("Public and Private key is: "+pubKey.toString+" , "+priKey.toString+" for user: "+userId)
      publicKeyRepo.addPublicKey(userId, pubKey)
      //pipeline2(Post("http://localhost:8080/facebook/initPubKeyRepo?userid="+userId+"&pubKey="+pubKey))
      //self ! "login"
      var fileName = "key_"+userId.toString+".key"
      fileName = "C:/Users/dd/workspace/Project-4a/keys/"+fileName 
      var PublicKeyOS:ObjectOutputStream  = new ObjectOutputStream(new FileOutputStream(fileName));
      PublicKeyOS.writeObject(pubKey);
      //scala.tools.nsc.io.File("C:/Users/dd/workspace/Project-4a/keys/"+fileName).writeAll(pubKey.toString)
      client ! doneKey(userId)
    }
    
    /* test method for testing purposes */
    case "test" => {
      val publicKey = publicKeyRepo.getPublicKey(userId)
      println("got public key as: "+publicKey)
      var bytes = Array[Byte] ()
      println("Aes original key is: "+secretK)
      var cipherRsa:Cipher = Cipher.getInstance("RSA")
      cipherRsa.init(Cipher.ENCRYPT_MODE, publicKey)
      var Ekey = cipherRsa.doFinal(secretK.getEncoded)
      println("encrypted aes key is: "+Ekey)
      
      cipherRsa.init(Cipher.DECRYPT_MODE, priKey)
      var Dkey = new SecretKeySpec(cipherRsa.doFinal(Ekey),"AES")//cipherRsa.doFinal(Ekey)
      println("Decrypted aes key is: "+Dkey)
      
    }
    
    /* User is registered at server */
    case "register" => {      
      var filename = "key_"+userId.toString+".key"
      filename = "C:/Users/dd/workspace/Project-4a/keys/"+filename
      var inputStream:ObjectInputStream = null;
      inputStream = new ObjectInputStream(new FileInputStream(filename));
      var key:PublicKey = inputStream.readObject().asInstanceOf[PublicKey]
      
      /* testing*/
      var temp = "mynameisdrumil"
      var cipherRsa:Cipher = Cipher.getInstance("RSA")
      cipherRsa.init(Cipher.ENCRYPT_MODE,key)
      var temp1 = cipherRsa.doFinal(temp.getBytes())
      var encrText=new String (Base64.encodeBase64(temp1))
      println("encrypted temp is: "+encrText)
      
      cipherRsa.init(Cipher.DECRYPT_MODE,priKey)
      var temp2 = Base64.decodeBase64(encrText.getBytes())
      var temp3=cipherRsa.doFinal(temp2)
      var temp4 = new String(temp3)
      println("Decrypted temp is: "+temp4)
      /* -- testing --*/
      
      val result = pipeline2(Get("http://localhost:8080/facebook/register?userid="+userId))
      result.foreach
      {
        response => println(s"facebook register request completed ${response.status} and content:\n${response.entity.asString}")
        var Etoken = response.entity.asString.replaceAll("\\[", "").replaceAll("\\]","").replaceAll("(\r\n|\n)","").replaceAll("\"", "")
        println("Mazi String is " + Etoken )
        var token1 = java.net.URLDecoder.decode(Etoken).replaceAll(" ","+")
        println("url decoded token is: "+token1)
        var token2 = Base64.decodeBase64(token1.getBytes())
        println("url decoded base64 decoded token is: "+token2)
        
        var cipherRsaw:Cipher = Cipher.getInstance("RSA")
        cipherRsaw.init(Cipher.DECRYPT_MODE, priKey)
        //var Dtoken1 = cipherRsaw.doFinal(token2)
        //var Dtoken = new String(Dtoken1)
        var Dtoken = new SecretKeySpec(cipherRsa.doFinal(token2),"AES")
        println("Decrypted token is: "+Dtoken)
        val result1 = pipeline2(Get("http://localhost:8080/facebook/login?userid="+userId+"&token="+Dtoken))
        result1.foreach
        {
          response => println(s"facebook login req ${response.status} and content:\n${response.entity.asString}")
          println("Login status: "+response.entity.asString)
          if(response.entity.asString.contains("success"))
            self ! "loggedin"
        }
      }
      //self ! "createKey"
      client ! doneReg(userId)
    }
    
    case "loggedin" => 
    {
      loginStatus = true;    
    }
    
    /* User is logged in 
     * Password and salt for the user are used to create initial passHash
     * this is then used to compute hash in loop, to prevent Rainbow table attacks 
     * this computed hash is then verified with the one stored on server for authentication
     * */
    case "login"=>{
      println("In user Login ...")
      var passHash1:String = ""
      var md1:MessageDigest = MessageDigest.getInstance("SHA-256")
      var input1:String = password+salt
      passHash1 = md1.digest(input1.getBytes).foldLeft("")((s:String, b: Byte) => s + Character.forDigit((b & 0xf0) >> 4, 16) +Character.forDigit(b & 0x0f, 16))
      for(i<-0 to 9)
      {
        passHash1 = md1.digest(passHash1.getBytes).foldLeft("")((s:String, b: Byte) => s + Character.forDigit((b & 0xf0) >> 4, 16) +Character.forDigit(b & 0x0f, 16))  
      }
      println("passHash for user in login: "+userId+" is: "+passHash1)
      val result = pipeline3(Get("http://localhost:8080/facebook/login?userid="+userId+"&passHash="+passHash1+"&salt="+salt))
      result.foreach
      {
        response => println(s"facebook login request completed ${response.status} and content:\n${response.entity.asString}")
      }
      client ! doneReg(userId)
    }
    
    /* New post is posted 
     * This data in only available to the user himself.
     * Hence it is encrypted using user's AES key befor sending the post to server
     * */
    case "postNewMsg" => {
        println("In user post new msg : "+userId)
        var origStr:String = dataPool.posts(Random.nextInt(dataPool.posts.length))
        println("Orignal Post is : " +origStr)
        var temp = cipher.doFinal(origStr.getBytes("UTF-8"))
        println("Encrypted Post is: "+temp)
        var EPost = Base64.encodeBase64String(temp)
        println("base 64 encoded encrypted post is : "+EPost)
        EPost = java.net.URLEncoder.encode(EPost,"UTF-8")
        println("Url encoded and base 64 encoded encrypted Image is : "+EPost)
        pipeline2(Post("http://localhost:8080/facebook/facebookPost?userid="+userId+"&fbPost="+ EPost))
    }
    
    /* Returns friend list of user */
    case "returnFriends" =>{
      val result = pipeline3(Get("http://localhost:8080/facebook/returnFriends?userid="+userId))
      //println(result.toString)
      println("Getting Friend List for user: " +userId )
      result.foreach 
      { 
        response => println(s"facebook request completed ${response.status} and content:\n${response.entity.asString}")
      }
      //client ! "postImage"
    }
    
    /* An image is posted by user
     * This data is only for the user himself
     * Hence it encrypted using user's AES key and then sent to the server
     * */
    case "postImage" => {
      println("In user for posting image: "+userId)
      var origImg:String =dataPool.imageBase64(Random.nextInt(dataPool.imageBase64.length))
      var temp = cipher.doFinal(origImg.getBytes("UTF-8"))
      println("encrypted image is: "+temp)
      var Eimg = Base64.encodeBase64String(temp)
      println("base 64 encoded encrypted Image is : "+Eimg)
      Eimg = java.net.URLEncoder.encode(Eimg,"UTF-8")
      println("Url encoded and base 64 encoded encrypted Image is : "+Eimg)
      //println("The sent image length is " + origImg.length())
      //var postImg:String= origImg.replaceAll(" ", "%20")
      //var albID=Random.nextInt(4)
      var albID = 0;
      pipeline2(Post("http://localhost:8080/facebook/imagePost?userid="+userId+"&albumID="+albID+"&imagePost="+Eimg))
    }
    
    /* Get user his/her profile
     * Encrypted information such as posts,photos are brought from the server
     * They are then decrypted using user's AES key 
     * */
    case "getProfile" => {
      val result = pipeline2(Get("http://localhost:8080/facebook/getProfile?userid="+userId))
      println("Getting Profile for user: " +userId )
      result.foreach 
      {
        response => println(s"facebook request completed ${response.status} and content:\n${response.entity.asString}")
      }
      var albID = 0
      val result1 = pipeline2(Get("http://localhost:8080/facebook/getAlbum?userid="+userId+"&albumID="+albID))
      result1.foreach 
      { 
        response => println(s"facebook request completed ${response.status} and content:${response.entity.asString}")
        val finString=response.entity.asString.replaceAll("\\[", "").replaceAll("\\]","").replaceAll("(\r\n|\n)","")
        val Images=finString.split(",")
        for(i<-0 to Images.length-1)
        {
          Images(i)=Images(i).replaceAll("\"", "").replaceAll(" ", "")
          Images(i)=java.net.URLDecoder.decode(Images(i)).replaceAll(" ","+")
        }
        println("Images for user: ")
        var size = Images.length-1
        for(i<-0 to size)
        {
          var decrypt = Base64.decodeBase64(Images(i))
          cipher.init(Cipher.DECRYPT_MODE, secretK, iv1)
          var img = new String(cipher.doFinal(decrypt), "UTF-8")
          println(img+" , ")
        }
      }  
      val result2 = pipeline2(Get("http://localhost:8080/facebook/getSelfPost?userid="+userId))
      result2.foreach 
      { 
        response => println(s"Self Post Retrieved completed ${response.status} and content:${response.entity.asString}")
        val finString=response.entity.asString.replaceAll("\\[", "").replaceAll("\\]","").replaceAll("(\r\n|\n)","")
        val Posts=finString.split(",")
        for(i<-0 to Posts.length-1)
        {
          Posts(i)=Posts(i).replaceAll("\"", "").replaceAll(" ", "")
          Posts(i)=java.net.URLDecoder.decode(Posts(i)).replaceAll(" ","+")
        }
        println("Posts for user: ")
        var size = Posts.length-1
        for(i<-0 to size)
        {
          var decrypt = Base64.decodeBase64(Posts(i))
          cipher.init(Cipher.DECRYPT_MODE, secretK, iv1)
          var userPost = new String(cipher.doFinal(decrypt), "UTF-8")
          println(userPost+" , ")
        }
      }
    }
    
    /* Returns a page */
    case "viewPage" => {
      var page_title = dataPool.page_titles(Random.nextInt(5))
      var page_title_op = page_title.replaceAll(" ", "%20")
      val result = pipeline2(Get("http://localhost:8080/facebook/page?userid="+userId+"&pageTitle="+page_title_op))
      result.foreach 
      { 
        response => println(s"facebook request completed ${response.status} and content:\n${response.entity.asString}")
      }  
    }
    
    /* Returns photos for user
     * The photos are decrypted using user's AES key 
     * */
    case "viewAlbum" => {
      //var albID=Random.nextInt(4) 
      var albID = 0
      val result = pipeline2(Get("http://localhost:8080/facebook/getAlbum?userid="+userId+"&albumID="+albID))
      result.foreach 
      { 
        response => println(s"facebook request completed ${response.status} and content:${response.entity.asString}")
        val finString=response.entity.asString.replaceAll("\\[", "").replaceAll("\\]","").replaceAll("(\r\n|\n)","")
        val Images=finString.split(",")
        println("Lenght of album array is : " +Images.length )
        for(i<-0 to Images.length-1)
        {
          //val temp = java.net.URLDecoder.decode(Images(i))
          //Images(i) = temp
          Images(i)=Images(i).replaceAll("\"", "").replaceAll(" ", "")
          Images(i)=java.net.URLDecoder.decode(Images(i)).replaceAll(" ","+")
        }
        println("First Element of album array now is : " +Images(0))
        var size = Images.length-1
        for(i<-0 to size)
        {
          var decrypt = Base64.decodeBase64(Images(i))
          println("decoded img is: "+decrypt)
          cipher.init(Cipher.DECRYPT_MODE, secretK, iv1)
          var img = new String(cipher.doFinal(decrypt), "UTF-8")
          println("decrypted images: "+img)
        }
      }  
    }
    
    /* A friend is added using this method */
    case "addFriend" => {
      var newFriend:Int = userId
      while (newFriend == userId)
        newFriend = Random.nextInt(num_users)
      val result = pipeline2(Get("http://localhost:8080/facebook/addNewFriend?userid="+userId+"&friendID="+newFriend))
      result.foreach 
      { 
        response => println(s"facebook request completed ${response.status} and content:\n${response.entity.asString}")
      }
    }
    
    /* This method was used in first part of the project */
     case "getFriendPost" => {
      //val friendsPost = new ArrayBuffer[String]() 
      var newFriend:Int = userId
      while (newFriend == userId)
        newFriend = Random.nextInt(num_users)
      val result = pipeline2(Get("http://localhost:8080/facebook/getRandomFriendPost?userid="+userId+"&friendID="+newFriend))
      result.foreach 
      { 
         response => println(s"facebook request completed ${response.status} and content:\n${response.entity.asString}")
         val finString=response.entity.asString.replaceAll("\\[", "").replaceAll("\\]","");
         val friendsPost=finString.split(",")
         println("Lenght of shembda array is : " +friendsPost.length )
         println("First Element of shembda array is : " +friendsPost(0) )
      }
    }
    
     /* User sends a facebook msg to another user1
      * This msg is encrypted using the users AES key
      * then user's AES key is encrypted using user1's public (RSA) key
      * then then encrypted message and key are sent to the server 
      * */
     case "sendFbMsg" => {
       var fbUser:Int = 7
       /*while (fbUser == userId)
          fbUser = Random.nextInt(num_users)*/
       println("sending msg to rand user: "+fbUser)
       var fbMsg = dataPool.posts(Random.nextInt(dataPool.posts.length))
       println("Msg chosen: "+fbMsg)
       var temp = cipher.doFinal(fbMsg.getBytes("UTF-8"))
       println("encrypted Fb msg is: "+temp)
       var EMsg = Base64.encodeBase64String(temp)
       println("base 64 encoded encrypted Image is : "+EMsg)
       EMsg = java.net.URLEncoder.encode(EMsg,"UTF-8")
       println("Url encoded and base 64 encoded encrypted Image is : "+EMsg)
   
       var userPubKey:PublicKey= publicKeyRepo.getPublicKey(fbUser)
       println("got public key as: "+userPubKey)
       println("Aes original key is: "+secretK)
       var cipherRsa:Cipher = Cipher.getInstance("RSA")
       cipherRsa.init(Cipher.ENCRYPT_MODE, userPubKey)
       var Ekey = cipherRsa.doFinal(secretK.getEncoded)
       println("encrypted aes key is: "+Ekey)
       var temp1 = Base64.encodeBase64String(Ekey)
       println("base 64 encoded encrypted Key is : "+temp1)
       var EKeyAES = java.net.URLEncoder.encode(temp1,"UTF-8")
       println("Url encoded and base 64 encoded encrypted Key is : "+EKeyAES)
       
       pipeline2(Post("http://localhost:8080/facebook/fbMsg?userid="+userId+"&fbUser="+fbUser+"&msg="+EMsg+"&key="+EKeyAES))
       //client ! startFacebookActivity(fbUser)
     }
     
     /* The user receives a message from other user1
      * also a encrypted AES key of user1 is received
      * the AES key is decrypted using user's Private key (RSA)
      * the message is then decrypted using this decrypted AES key 
      * */     
     case "receiveMsg" => {
       if(loginStatus == true)
       {
         val result = pipeline2(Get("http://localhost:8080/facebook/getFbMsg?userid="+userId)) 
         result.foreach 
         { 
           //val message
           response => println(s"facebook msg received ${response.status} and content:\n${response.entity.asString}")
           val finMsgString=response.entity.asString.replaceAll("\"", "")
           val Messages=finMsgString.split(",")
           for (j<-0 to Messages.length-1)
           {
             println("msg array: "+Messages(j))
           }
           println("msg array len: "+Messages.length)
           if(!Messages(0).contains("Problem"))
           {
             var Dmsg1 = java.net.URLDecoder.decode(Messages(1)).replaceAll(" ","+")
             println("url decoded msg is: "+Dmsg1)
             var Dmsg2 = Base64.decodeBase64(Dmsg1)
             println("url decoded base64 decoded msg is: "+Dmsg2)
             
             var Dkey1 = java.net.URLDecoder.decode(Messages(2)).replaceAll(" ","+")
             println("url decoded key is: "+Dkey1)
             var Dkey2 = Base64.decodeBase64(Dkey1.getBytes("UTF-8"))
             println("url decoded base64 decoded key is: "+Dkey2)
             
             var cipherRsa:Cipher = Cipher.getInstance("RSA")
             cipherRsa.init(Cipher.DECRYPT_MODE, priKey)
             var Dkey = new SecretKeySpec(cipherRsa.doFinal(Dkey2),"AES")//cipherRsa.doFinal(Ekey)
             println("Decrypted aes key is: "+Dkey)
             
             //var iv:IvParameterSpec = new IvParameterSpec(Dkey.getEncoded)
             var cipher1:Cipher = Cipher.getInstance("AES/CBC/PKCS5Padding")
             //var iv2 = new IvParameterSpec(cipher1.getIV) 
             var ivSpec:Array[Byte]= new Array[Byte] (16) //0x00
             for(i<-0 to 15)
             {
               ivSpec(i) = (0x00).toByte
             }
             var iv:IvParameterSpec  = new IvParameterSpec(ivSpec);
             //var iv:IvParameterSpec  = new IvParameterSpec(Arrays.copyOf(Dkey2, 16))
             cipher1.init(Cipher.DECRYPT_MODE, Dkey, iv)
             var OrigMsg = new String(cipher.doFinal(Dmsg2), "UTF-8")
             println("decrypted msg: "+OrigMsg)
           }
           else
           {
             println("No new post")
           }
        }
      }
    }     
  }
}