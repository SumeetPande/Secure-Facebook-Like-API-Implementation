package main
import akka.actor._
import scala.collection.mutable._
import spray.http._
import spray.client.pipelining._
import scala.util.Random
import scala.concurrent.duration._
import global.publicKeyRepo
import scala.collection.mutable.HashMap
import java.security.KeyPair;
import java.security.KeyPairGenerator;
import java.security.NoSuchAlgorithmException;
import java.security.PrivateKey;
import java.security.PublicKey;
import javax.crypto.Cipher;

case class registerUsers(i:Int)
case class doneReg(i:Int)
case class startFacebookActivity(user_id:Int)
case class doneKey(i:Int)
case class genKeys(i:Int)

/**
 * @author Drumil Deshpande
   @author Sumeet Pande
 */
object client 
{
  val publicKeyTable = HashMap.empty[Int,PublicKey]
  def main(args: Array[String])
  {
    //Accept input arguements
    if(args(0).toLowerCase == "client" && args.length==2) 
    { 
      println("Client server initiated")
      var num_users = args(1).toInt
      val system = ActorSystem("FacebookSimulator")
      val clientSystem = system.actorOf(Props(new client(num_users,system)),"clientSystem")
      clientSystem ! "Initiate"
    }
  }


class client(num_users:Int, actSys:ActorSystem) extends Actor{
  
  import actSys.dispatcher
  val pipeline = sendReceive
  val pipeline2 = sendReceive
  val start_time:Long=System.currentTimeMillis
  val flag:Boolean = false
  
  def receive = {
    
    case "Initiate" => {      
      
      //Initiating facebook users.
      for (i<-0 to num_users-1)
      {
          val users=actSys.actorOf(Props(new user(num_users,i,actSys)),name="user_"+i.toString())
      }
      //Sending the "POST" request to webserver to initiate users 
      println("Initialized " + num_users + " facebook users")      
      val result = pipeline(Post("http://localhost:8080/facebook/initiate?num_users="+num_users))
      println(num_users + " Facebook Users Created")
      self ! "generateKeys"
    }
    
    case "generateKeys" => {
      var i:Int = 0
      self ! genKeys(i)
    }
    
    case genKeys(i:Int) => {
      
      println("creating keys for user ...")
      var user = context.actorSelection("akka://FacebookSimulator/user/user_"+i.toString)
      user ! "createKey"
    }
    
    case doneKey(i:Int) => {
      
      if(i<num_users-1)
      {
        var uid = i+1
        self ! genKeys(uid)
      }
      else
        self ! "generateInitialData"
    }
    
    case "generateInitialData"=>{
      var i:Int = 0
      self ! registerUsers(i)
    }
    
    case registerUsers(i:Int) => {
      
      println("Registering all users ...")
      var user = context.actorSelection("akka://FacebookSimulator/user/user_"+i.toString)
      user ! "register"
    }
    
    case doneReg(i:Int) => {
      
      if(i<num_users-1)
      {
        var uid = i+1
        self ! registerUsers(uid)
      }
      else
        self ! "startFacebookScheduling"
    }
    
    case "startFacebookScheduling"=> {
      //val sch=context.system.scheduler.schedule(0 seconds, 2 seconds,self,"startFacebookActivity")
      var user_id:Int = Random.nextInt(num_users)
      while(user_id==7)
        user_id = Random.nextInt(num_users)
      self ! startFacebookActivity(user_id)
    }
    
    case startFacebookActivity(user_id:Int) => {
      var flag:Boolean = true
      if(publicKeyTable.size == num_users)
      {
        println("Start facebook api activity")
        var randUser1 = context.actorSelection("akka://FacebookSimulator/user/user_"+user_id.toString)
        //randUser1 ! "register"
        randUser1 ! "returnFriends"
        randUser1 ! "viewPage"
        randUser1 ! "addFriend"
        randUser1 ! "postImage"
        randUser1 ! "postImage"
        randUser1 ! "postNewMsg"
        randUser1 ! "sendFbMsg"
        randUser1 !"getProfile"
        randUser1 ! "viewAlbum"
        //randUser1 ! "getFriendPost"
        randUser1 ! "receiveMsg"
       
        Thread.sleep(2000)
        
        var randUser2 = context.actorSelection("akka://FacebookSimulator/user/user_7")
        //randUser1 ! "createKey"
        randUser2 ! "returnFriends"
        randUser2 ! "viewPage"
        randUser2 ! "addFriend"
        randUser2 ! "postImage"
        randUser2 ! "postImage"
        randUser2 ! "postNewMsg"
        randUser2 ! "sendFbMsg"
        randUser2 !"getProfile"
        randUser2 ! "viewAlbum"
        //randUser2 ! "getFriendPost"
        randUser2 ! "receiveMsg"
        
      }
      
      /*var randUser2 = context.actorSelection("akka://FacebookSimulator/user/user_"+Random.nextInt(num_users).toString)
      randUser2 ! "postNewMsg" 
      randUser2! "returnFriends"
      randUser2! "postImage"
      randUser2 ! "getProfile"
      randUser2 ! "viewPage"
      randUser2 ! "viewAlbum"
      randUser2 ! "addFriend"
      randUser2 !"getFriendPost"
      
      var randUser3 = context.actorSelection("akka://FacebookSimulator/user/user_"+Random.nextInt(num_users).toString)
      randUser3 ! "postNewMsg" 
      randUser3! "returnFriends"
      randUser3! "postImage"
      randUser3 ! "getProfile"
      randUser3 ! "viewPage"
      randUser3 ! "viewAlbum"
      randUser3 ! "addFriend"
      randUser3 ! "getFriendPost"
      
      if(System.currentTimeMillis - start_time > 60000)
      {
        println("Shutting down .....")
        //context.system.shutdown()
        actSys.shutdown
      }
      */
    }    
  }
 }
}