package com.lookout.borderpatrol.sessions

import scala.collection.mutable.Map

class SessionStore {
  /*
  {
    <session_id>: {
      <master_token>: String,
      <destination>: String,
      <service1>: String, ...
    }
  }
   */

  private var store = Map[String, Map[String, String]]()

  def session(id: String) = new Session(id)

  def addSession(id: String) = store += (id -> Map[String,String]())

  class Session(id: String) {

    var sessionStore = store get id

    def masterToken_= (token: String): Unit =  sessionStore.get += ("master_token" -> token)

    def masterToken = store get id flatMap { _ get "master_token" }

    def serviceToken_= (t: (String, String)) =  sessionStore.get += (t._1 -> t._2)

    def serviceToken = store get id

    def destination_= (destination: String) =  sessionStore.get += ("destination" -> destination)

    def destination = store get id flatMap { _ get "destination" }

  }
}