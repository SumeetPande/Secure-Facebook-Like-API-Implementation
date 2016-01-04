package main

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import akka.actor._
import java.security.KeyPair;
import java.security.KeyPairGenerator;
import java.security.NoSuchAlgorithmException;
import java.security.PrivateKey;
import java.security.PublicKey;
import javax.crypto._
//import javax.crypto.Cipher;
import global.publicKeyRepo;
import org.apache.commons.codec.binary.Hex
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer
import scala.concurrent.ExecutionContext.Implicits.global
import util.Random
import scala.util.Random
import scala.concurrent.duration._
import globalData.dataPool
import akka.routing.RoundRobinRouter
import spray.routing.SimpleRoutingApp
import spray.json.AdditionalFormats
import spray.routing._
import spray.routing.Directive._
import spray.http._
import java.io
import shapeless.get
import akka.pattern.AskTimeoutException
import akka.pattern.ask
import akka.util.Timeout
import scala.concurrent.Future._
import spray.httpx._
import scala.concurrent.Future
import scala.concurrent.Await
import akka.util.Timeout
import org.json4s.ShortTypeHints
import org.json4s.native.Serialization
import org.json4s.native.Serialization._
import scala.collection.mutable.SynchronizedBuffer
import java.net.URLEncoder._
import java.net.URLDecoder._
import java.util.UUID
import org.apache.commons.codec.binary.Base64
import scala.collection.mutable.HashMap

case class buildInitialData(noUsers:Int)
case class postMsg(user_id:Int, post:String)
case class returnFriends(userId:Int)
case class postImg(user_id:Int, albumid:Int , post:String)
case class fetchProfile(user_id:Int)
case class fetchPage(user_id:Int, page_title:String)
case class fetchAlbum (user_id:Int, album_id:Int)
case class addFriend(user_Id:Int,friend_ID :Int)
case class getRandomFriendPost(user_id:Int, friend_ID:Int)
case class initPost(user_id:Int, post:String)
case class register(user_id:Int)
case class login(user_id:Int, token:String)
case class fbMsg(user_Id:Int,fb_user:Int,msg:String,key:String)
case class getFbMsg(user_id:Int)
case class fetchPost(user_id:Int)

/**
 * @author Drumil Deshpande
   @author Sumeet Pande
 */

object server extends App with SimpleRoutingApp{
  
  override def main(args:Array[String])
  {
    var num_users:Int = 0
    implicit val timeout = Timeout(2.seconds)
    
    implicit val system = ActorSystem("FacebookSimulator")
    println("At Facebook WebServer ...")
    val server = system.actorOf(Props(new server(num_users)),"apiServer")
    
    startServer(interface="localhost", port=8080){
     get {
      path("facebook"/"returnFriends") {
        parameter("userid") { (userId) =>                      
            complete { 
              (server ? returnFriends(userId.toInt)).recover{
                 case ex: AskTimeoutException => {
                "Problem in Retrieving Post"
                }
                 
             }
             .mapTo[String]
             .map(s => s"Your FriendList is for user $s")
            }              
          }
        }
      }~
      get {
      path("facebook"/"getProfile") {
        parameter("userid") { (userId) =>                      
            complete { 
              (server ? fetchProfile(userId.toInt)).recover{
                 case ex: AskTimeoutException => {
                "Problem in fetching Pofile"
                }
                 
             }
             .mapTo[String]
             .map(s => s"Profile is : $s")
            }              
          }
        }
      }~
      get {
      path("facebook"/"page") {
        parameter("userid" , "pageTitle") { (userId,page_title) =>                      
            complete { 
              (server ? fetchPage(userId.toInt,page_title.toString)).recover{
                 case ex: AskTimeoutException => {
                "Problem in fetching Page"
                }
                 
             }
             .mapTo[String]
             .map(s => s"Page is : $s")
            }              
          }
        }
      }~
      get {
      path("facebook"/"getAlbum") {
        parameter("userid" , "albumID") { (userId,albumId) =>                      
            complete { 
              (server ? fetchAlbum(userId.toInt,albumId.toInt)).recover{
                 case ex: AskTimeoutException => {
                "Problem in fetching Album"
                }
                 
             }
             .mapTo[String]
             .map(s => s"$s")
            }              
          }
        }
      }~
      get {
      path("facebook"/"addNewFriend") {
        parameter("userid" , "friendID") { (userId,friendID) =>                      
            complete { 
              (server ? addFriend(userId.toInt,friendID.toInt)).recover{
                 case ex: AskTimeoutException => {
                "Problem in adding new friend"
                }
                 
             }
             .mapTo[String]
             .map(s => s"Updated Friend list is : $s")
            }              
          }
        }
      }~
      get {
      path("facebook"/"getRandomFriendPost") {
        parameter("userid" , "friendID") { (userId,friendID) =>                      
            complete { 
              (server ? getRandomFriendPost(userId.toInt,friendID.toInt)).recover{
                 case ex: AskTimeoutException => {
                "Problem in adding new friend"
                }
                 
             }
             .mapTo[String]
             .map(s => s"$s")
            }              
          }
        }
      }~
      get {
      path("facebook"/"login") {
        parameter("userid".as[Int],"token".as[String]) { (user_id,token) =>                      
            complete { 
              (server ? login(user_id,token)).recover{
                 case ex: AskTimeoutException => {
                "Problem in logging user"
                }
             }
             .mapTo[String]
             .map(s => s"$s")
            }              
          }
        }
      }~
      get {
      path("facebook"/"getFbMsg") {
        parameter("userid".as[Int]) { (user_id) =>                      
            complete { 
              (server ? getFbMsg(user_id)).recover{
                 case ex: AskTimeoutException => {
                "Problem in get fb msg"
                }
             }
             .mapTo[String]
             .map(s => s"$s")
            }              
          }
        }
      }~
      get {
      path("facebook"/"getSelfPost") {
        parameter("userid") { (userId) =>                      
            complete { 
              (server ? fetchPost(userId.toInt)).recover{
                 case ex: AskTimeoutException => {
                "Problem in fetching Album"
                }
                 
             }
             .mapTo[String]
             .map(s => s"$s")
            }              
          }
        }
      }~
      get {
      path("facebook"/"register") {
        parameter("userid") { (userId) =>                      
            complete { 
              (server ? register(userId.toInt)).recover{
                 case ex: AskTimeoutException => {
                "Problem in registering"
                }
                 
             }
             .mapTo[String]
             .map(s => s"$s")
            }              
          }
        }
      }~
      post{
       path("facebook"/"initiate"){
         parameters("num_users") { (num_users) =>
           complete {
             (server ? buildInitialData(num_users.toInt)).recover
             {
               case ex: AskTimeoutException => 
                 {
                  "Initiation failed"
                }  
             }
             .mapTo[String]
           }
         }
       }
      }~ 
      post {
       path("facebook"/"initPost"){
         parameters("userid".as[Int], "initPost".as[String]) { (user_id,post) =>
          complete {
            (server?initPost(user_id,post)).recover{
              case ex: AskTimeoutException => {
                  "Posting Failed"
                }
            }
           .mapTo[String]
           }
         }
       }
     }~
      post {
       path("facebook"/"facebookPost"){
         parameters("userid".as[Int], "fbPost".as[String]) { (user_id,post) =>
          complete {
            (server?postMsg(user_id,post)).recover{
              case ex: AskTimeoutException => {
                  "Posting Failed"
                }
            }
           .mapTo[String]
           }
         }
       }
     }~
       post {
       path("facebook"/"imagePost"){
         parameters("userid".as[Int],"albumID".as[Int],"imagePost".as[String]) { (user_id,album_id,imgPost) =>
          complete {
            (server?postImg(user_id,album_id,imgPost)).recover{
              case ex: AskTimeoutException => {
                  "Image Posting Failed"
                }
            }
           .mapTo[String]
           }
         }
       }
     }~
     post {
       path("facebook"/"fbMsg"){
         parameters("userid".as[Int],"fbUser".as[Int],"msg".as[String],"key".as[String]) { (user_id,fb_user,msg,key) =>
          complete {
            (server?fbMsg(user_id,fb_user,msg,key)).recover{
              case ex: AskTimeoutException => {
                  "fb msg Posting Failed"
                }
            }
           .mapTo[String]
           }
         }
       }
     }
    }
  }
}

class server(no_users:Int/*, actSys:ActorSystem*/) extends Actor{
  
  var num_friends = 0
  var userDataArray = new ArrayBuffer[userData] ()
  val client = context.actorSelection("akka://FacebookSimulator/user/clientSystem")
  val postTable = new ArrayBuffer[ArrayBuffer[String]] ()
  val imageTable=new ArrayBuffer[ArrayBuffer[ArrayBuffer[String]]]()
  val pageLiked = new ArrayBuffer[ArrayBuffer[Int]] ()
  val pages = new ArrayBuffer[pageData]()
  val num_page_likes =  new ArrayBuffer[ArrayBuffer[Int]] ()
  val userid:Int=0
  var num_users:Int = 0
  val tokenDb = HashMap.empty[Int,String]
  val msgQTable = new ArrayBuffer[msgQ] ()
  
  private implicit val formats = Serialization.formats(ShortTypeHints(List(classOf[String])))
  
  def receive = {
    
    case buildInitialData(noUsers:Int) => {
      
      println("In Build Data func")
      num_users = noUsers
      
      var high_users = (num_users * 0.7).toInt
      var mid_users  = (num_users * 0.2).toInt
      var low_users  = (num_users * 0.1).toInt
      
      for(i<-0 to 4)
      {
        var likes = ArrayBuffer[Int] ()
        var page = new pageData(dataPool.page_titles(i),dataPool.page_info(i))
        pages += page
        num_page_likes += likes
      }
      
      //println("num_users: "+num_users+", "+noUsers)
      for(i<-0 to num_users-1)
      {
         var friendList = new ArrayBuffer[Int] ()
         //println(dataPool.name.length+", "+dataPool.dob.length+", "+dataPool.placeOfBirth.length)
         var randName = dataPool.name(Random.nextInt(dataPool.name.length))
         var randDob = dataPool.dob(Random.nextInt(dataPool.dob.length))
         var randPlace = dataPool.placeOfBirth(Random.nextInt(dataPool.placeOfBirth.length))
         if(i<high_users)
         {
           num_friends = (num_users * 0.1).toInt
           createFriends(i,friendList)
           val friendObj = new userData(i,3,friendList, randName , randDob , randPlace)
           userDataArray += friendObj
           //generatePost(i)
           val tempAlbum= new ArrayBuffer[ArrayBuffer[String]]()
            val tempString=new ArrayBuffer[String]()
            tempAlbum.+=(tempString)
            imageTable.+=(tempAlbum)
           val tempBuffer=new ArrayBuffer[String]()
           postTable.+=(tempBuffer)
           val tempPWBuffer=new ArrayBuffer[String]()
           //passDb.+=(tempPWBuffer)
           //generateAlbum(i)
           getLikedPages(i)
         }
         else if(i>=high_users && i<(high_users+mid_users))
         {
           num_friends = (num_users * 0.05).toInt
           createFriends(i,friendList)
           val friendObj = new userData(i,2,friendList, randName, randDob, randPlace) 
           userDataArray += friendObj
           //generatePost(i)
           val tempAlbum= new ArrayBuffer[ArrayBuffer[String]]()
            val tempString=new ArrayBuffer[String]()
            tempAlbum.+=(tempString)
            imageTable.+=(tempAlbum)
           val tempBuffer=new ArrayBuffer[String]()
           postTable.+=(tempBuffer)
           val tempPWBuffer=new ArrayBuffer[String]()
           //passDb.+=(tempPWBuffer)
           //generateAlbum(i)
           getLikedPages(i)
         }
         else
         {
           num_friends = (num_users * 0.02).toInt
           createFriends(i,friendList)
           val friendObj = new userData(i,1,friendList, randName, randDob, randPlace)
           userDataArray += friendObj
           //generatePost(i)
           val tempAlbum= new ArrayBuffer[ArrayBuffer[String]]()
            val tempString=new ArrayBuffer[String]()
            tempAlbum.+=(tempString)
            imageTable.+=(tempAlbum)
           val tempBuffer=new ArrayBuffer[String]()
           postTable.+=(tempBuffer)
           val tempPWBuffer=new ArrayBuffer[String]()
           //passDb.+=(tempPWBuffer)
           //generateAlbum(i)
           getLikedPages(i)
         }
      }
      modifyFriendList
      
      for(i<-0 to num_users-1)
      {
        println("userid: "+userDataArray(i).uid+", level: "+userDataArray(i).level+", name: "+userDataArray(i).name+", Dob:"+userDataArray(i).dob+", place: "+userDataArray(i).place)
        print("Friends :")
        for(j<-0 to userDataArray(i).friends.length-1)
        {
          print(userDataArray(i).friends(j)+", ")
        }
        println(" ")
      }      
    }
    
    /* This method was used in first part of the project */
    case initPost(user_id:Int, post:String)=>{
      var origPost=post.replaceAll("%20", " ")
      generatePost(user_id,origPost)
    }
    
    case register(user_id:Int) => {
      //self ! "hi"
      println("In register at server for: "+user_id)
      //var rand:String = UUID.randomUUID.toString.replaceAll("-", "")
      var keyGen:KeyGenerator  = KeyGenerator.getInstance("AES");
      keyGen.init(256); // for example
      var  rand:SecretKey = keyGen.generateKey();      
      println("rand String at server: "+rand)
      var cipherRsa:Cipher = Cipher.getInstance("RSA")
      var filename = "key_"+user_id.toString+".key"
      filename = "C:/Users/dd/workspace/Project-4a/keys/"+filename   
      tokenDb += (user_id -> rand.toString)
      
      var inputStream:ObjectInputStream = null;
      inputStream = new ObjectInputStream(new FileInputStream(filename));
      var key:PublicKey = inputStream.readObject().asInstanceOf[PublicKey]
      println("public key at server is: "+key)
      cipherRsa.init(Cipher.ENCRYPT_MODE, key)
      var token = cipherRsa.doFinal(rand.getEncoded)
      println("encrypted token is: "+token)
      //var temp1 = Base64.encodeBase64String(token)
      //var temp1 = byteToString(token)
      var temp1=new String (Base64.encodeBase64(token))
      println("base 64 encoded encrypted token is : "+temp1)
      var Etoken = java.net.URLEncoder.encode(temp1)
      //var Etoken = byteToString(token)
      println("Url encoded and base 64 encoded encrypted token is : "+Etoken)
      sender ! writePretty(Etoken)
    }
    
    case login(user_id:Int, token:String) => {
      var loginStatus:String = ""
      var OrigToken = tokenDb.getOrElse(user_id,"error")
      println("db entry: "+OrigToken+" and token is: "+token)
      if(OrigToken==token)
      {
        loginStatus = "success"
      }
      else
      {
        loginStatus = "failed"
      }
      println(loginStatus)
      sender ! loginStatus
    }
    
    case postMsg(user_id:Int, post:String) => {
      var origPost=java.net.URLDecoder.decode(post,"UTF-8")
      postTable(user_id) += origPost
      println("The  news no of post after addition for userid "+user_id+" is " + postTable(user_id).length)
      for(j<-0 to postTable(user_id).length-1)
      { 
        print(" @@post: "+postTable(user_id)(j)+", ")
      }
      println(" ")
    }
    
    case returnFriends(userId:Int) => {
      println("In the case return friends")
      sender ! returnFriends(userId)      
    }
    
    case postImg(user_id:Int, albumid:Int, post:String) => {
      //var origImg=post.replaceAll("%20", " ")
      var origImg = java.net.URLDecoder.decode(post,"UTF-8")
      println("The received image is " + origImg)
      imageTable(user_id)(albumid) += origImg
      for(j<-0 to imageTable(user_id)(albumid).length-1)
      { 
        print(" @@images: " + imageTable(user_id)(albumid)(j)+", ")
      }
      println(" ")
    }
    
    case fetchProfile(user_id:Int) => {
      println("In case fetch profile")
      val myPosts =new ArrayBuffer[String]() with SynchronizedBuffer[String]
      val myFriends = new ArrayBuffer[String]() with SynchronizedBuffer[String]
      val myPictures = new ArrayBuffer[String]() with SynchronizedBuffer[String]
      val myPages = new ArrayBuffer[String]() with SynchronizedBuffer[String]
      for(i<-0 to userDataArray(user_id).friends.length-1)
      {
        myFriends += userDataArray(userDataArray(user_id).friends(i)).name
      }
      for(i<-0 to pageLiked(user_id).length-1)
      {
        myPages += pages(pageLiked(user_id)(i)).title
      }

      sender ! writePretty("Name: "+userDataArray(user_id).name+", Date of Birth: "+userDataArray(user_id).dob+", Place of Birth: "+userDataArray(user_id).place+", Friends: "+myFriends+", Pages Liked: "+myPages/*+" , Picture : " +myPictures+", Wall: "+myPosts*/)
    }
    
    case fetchPage(user_id:Int, page_title:String) => {
      println("In case fetch Page")
      var page_name = page_title.replaceAll("%20", " ")
      val likedBy = new ArrayBuffer[String]() with SynchronizedBuffer[String]
      var page_id = -1
      var ii = 0
      while(ii<5 && page_id == -1)
      {
        if(dataPool.page_titles(ii) == page_name)
          page_id = ii
        ii += 1
      }
      for(i<-0 to num_page_likes(page_id).length-1)
      {
        likedBy += userDataArray(num_page_likes(page_id)(i)).name
      }
      sender ! writePretty("Page Title: "+page_name+", Page Information: "+dataPool.page_info(page_id)+", Page is Liked By: "+likedBy)
    }
    
    case fetchAlbum (user_id:Int, album_id:Int) => {
      val usrname:String=userDataArray(user_id).name
      val tempBuffer =new ArrayBuffer[String]()
      for (x<-0 to imageTable(user_id)(album_id).length-1)
      {
        tempBuffer += java.net.URLEncoder.encode(imageTable(user_id)(album_id)(x),"UTF-8")
      }
      sender ! writePretty(tempBuffer)
    }
    
    case addFriend(user_Id:Int,friend_ID :Int)=> {
      if(!userDataArray(user_Id).friends.contains(friend_ID))
      {
        userDataArray(user_Id).friends +=friend_ID
        
      }
      val myFriends =new ArrayBuffer[String]() with SynchronizedBuffer[String]
      for(i<-0 to userDataArray(user_Id).friends.length-1)
      {
        myFriends += userDataArray(userDataArray(user_Id).friends(i)).name
      }
      sender ! writePretty(myFriends)
    }
    
    /* This method was used in first part of the project */
    case getRandomFriendPost(user_Id:Int,friend_ID :Int)=> {
      val friendsPost =new ArrayBuffer[String]() with SynchronizedBuffer[String]
      if(!userDataArray(user_Id).friends.contains(friend_ID))
      {
        val errorMsg:String="Sorry "+userDataArray(friend_ID).name +" is not your friend."
        friendsPost+=errorMsg
      }
      else
      {
         for(i<-0 to postTable(friend_ID).length-1)
            { 
              friendsPost += postTable(friend_ID)(i)
            }
      }
      sender ! writePretty(friendsPost)
      println("No of friends post returned are: "+friendsPost.length)
    }
    
    case fetchPost(user_id:Int)=>{
      val tempBuffer=new ArrayBuffer[String]()
      for(x<-0 to postTable(user_id).length-1)
      {
        tempBuffer += java.net.URLEncoder.encode(postTable(user_id)(x),"UTF-8")
      }
      sender ! writePretty(tempBuffer)
    }
    
    case fbMsg(user_Id:Int,fb_user:Int,msg:String,key:String)=> {
      val tempObj = new msgQ(user_Id,fb_user,msg,key)
      msgQTable.+=(tempObj)
    }
    
    case getFbMsg(user_id:Int) => {
      var temp = new msgQ(0,0,"","")
      for(i<-0 to msgQTable.length-1)
      {
        if(msgQTable(i).to_user==user_id)
        {
          temp = msgQTable(i)  
        }
      }
      if(temp.message!="")
      {
        println("in get fb msg at server: to->user: "+temp.to_user+", msg: "+temp.message)
        sender ! writePretty(temp.from_user+","+temp.message+","+temp.Ekey)
      }
    }
    
    case "hi" => {
      println("Hiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii")
      var client = context.actorSelection("akka://FacebookSimulator/user/clientSystem")
      client ! "hi"
    }
    
  }
  
  def createFriends(uid:Int, friendList:ArrayBuffer[Int]) = {
    var randFriend:Int = 0
    var i:Int = 0
    while(i<num_friends)
    {      
      randFriend = Random.nextInt(num_users-1)
      if(randFriend != uid && !friendList.contains(randFriend))
      {
        friendList += randFriend
        i += 1
      }
    }
  }
  
  def modifyFriendList = {
    for(i<-0 to num_users-1)
    {
      for(j<-0 to userDataArray(i).friends.length-1)
      {
        if(!userDataArray(userDataArray(i).friends(j)).friends.contains(i))
        {
          userDataArray(userDataArray(i).friends(j)).friends += i
        }
      }
    }
  }
  
  def returnFriends(userID:Int):String={
    println("In the method return friends")
    val usrname:String=userDataArray(userID).name
    val myFriends =new ArrayBuffer[String]() with SynchronizedBuffer[String]
    //myArray+=userDataArray(userID).friends
    for(i<-0 to userDataArray(userID).friends.length-1)
    {
      myFriends += userDataArray(userDataArray(userID).friends(i)).name
    }
    //return writePretty(userDataArray(userID).friends) 
    return writePretty(usrname+" : "+myFriends)
  }
  
  /* This method was used in first part of the project */
  def generatePost(userid:Int,post:String) = {
    /*val tempBuffer=new ArrayBuffer[String]()
    tempBuffer.+=(dataPool.posts(Random.nextInt(dataPool.posts.length)))
    tempBuffer.+=(dataPool.posts(Random.nextInt(dataPool.posts.length)))
    postTable.+=(tempBuffer)*/
    postTable(userid).+=(post)
    //println("The first message posted is : "+postTable(userid))
  }
  
  /* This method was used in first part of the project */
  def generateAlbum(userid:Int) ={
    val tempAlbum= new ArrayBuffer[ArrayBuffer[String]]()
    for(z<-0 to 4)
    {
    val tempString=new ArrayBuffer[String]()
    tempString.+=(dataPool.imageBase64(Random.nextInt(dataPool.imageBase64.length)))
    tempString.+=(dataPool.imageBase64(Random.nextInt(dataPool.imageBase64.length)))
    tempString.+=(dataPool.imageBase64(Random.nextInt(dataPool.imageBase64.length)))
    tempAlbum.+=(tempString)
    }
    imageTable.+=(tempAlbum)
  }
  
  def getLikedPages(user_id:Int) = {
    var randLikes = 0
    while(randLikes == 0)
     randLikes = Random.nextInt(5)
    //println("raand likes: "+randLikes+" for user "+user_id)
    var mypages = new ArrayBuffer[Int]()
    for(i<-0 to randLikes-1)
    {
       var temp = Random.nextInt(5)
       if(!mypages.contains(temp))
       {
         num_page_likes(temp) += user_id
         mypages += temp
         //println("liked: "+temp+" for user "+user_id)
       }
    }
    pageLiked.+=(mypages)
  }
  
  def stringToBytes(msg : String) : Array[Byte] = {
    Hex.decodeHex(msg.toCharArray)
  }

  def byteToString(bytes : Array[Byte]): String = {
    Hex.encodeHexString(bytes)
  }
}