package main

import scala.collection.mutable.ArrayBuffer

/**
 * @author Drumil Deshpande
   @author Sumeet Pande
 */

// This class is used to create an object for user Infromation

class userData(userId:Int, userLevel:Int, friendList:ArrayBuffer[Int], uName:String, uDob:String, uPlaceOfBirth:String)
{
  var friends = new ArrayBuffer[Int] ()
  var uid:Int = userId
  var level:Int = userLevel
  var name:String = uName
  var dob:String = uDob
  var place:String = uPlaceOfBirth
  
  friends = friendList
}