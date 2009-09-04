/**
Copyright (c) 2009 ymnk, JCraft,Inc. All rights reserved.
    
Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

   1. Redistributions of source code must retain the above copyright notice,
      this list of conditions and the following disclaimer.

   2. Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in
      the documentation and/or other materials provided with the distribution.

   3. The names of the authors may not be used to endorse or promote products
      derived from this software without specific prior written permission.

 THIS SOFTWARE IS PROVIDED ``AS IS'' AND ANY EXPRESSED OR IMPLIED WARRANTIES,
 INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
 FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL JCRAFT,
 INC. OR ANY CONTRIBUTORS TO THIS SOFTWARE BE LIABLE FOR ANY DIRECT, INDIRECT,
 INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA,
 OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
 EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/
package com.jcraft.dstream_client

import java.io.{ByteArrayOutputStream, IOException, OutputStream, File}
import java.io.{File, FileInputStream, DataInputStream}
import java.net.{URL, URLConnection, HttpURLConnection}
import java.util.Random

abstract sealed class Param
case class FieldParam(key:String, value:String) extends Param
case class DataParam(key:String, filename:String,
                     data:Array[Byte], typ:Option[String]) extends Param
case class FileParam(key:String, file:File, typ:Option[String]) extends Param

class PostMultiPartFormData {

  var cookie:Option[String] = None

  private lazy val boundary:Array[Byte] = {
    val chars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
    val rand = new Random()
    (0 until 10).foldLeft(new StringBuilder){
      case (b, _) => b.append(chars(rand.nextInt(chars.length)))
    }.toString.getBytes
  }

  def apply(url:String, param:Param*):Option[String] = {
    try{

      val con = new URL(url).openConnection match{
        case con:HttpURLConnection => con
        case _ => throw new Exception
      }

      con.setUseCaches(false)
      con.setDoOutput(true)
      con.setRequestProperty("Content-Type",
                             "multipart/form-data; boundary=%s".format(new String(boundary)))

      cookie.map{c =>
        con.setRequestProperty("Cookie", c)
      }

      implicit val baos = new ByteArrayOutputStream

      param foreach {
        case FieldParam(k, v) => writeField(k, v)
        case DataParam(k, f, d, t) => writeData(k, f, d, t)
        case FileParam(k, f, t) =>
          val data = new Array[Byte](f.length.asInstanceOf[Int])
          new DataInputStream(new FileInputStream(f)) match{ 
            case i => i.readFully(data); i.close 
          }
          writeData(k, f.getName, data, t)
      }

      writeEnd

      baos.flush
      baos.close

      con.setRequestProperty("Content-Length", baos.size.toString)

      con.getOutputStream match{
        case cout =>
          baos.writeTo(cout)
          cout.close
      }

      con.getHeaderField("Set-Cookie") match{
        case null =>
        case c => 
          cookie = Some(c.split(";").filter(!_.startsWith("Path=")).mkString("; "))
      }

      con.getResponseCode match{
        case code if code!=200 => None
        case _ =>
          val result = new StringBuilder

          con.getInputStream match{
            case in =>
            for(c <- Stream.const(in.read _).map(_()).takeWhile(_ != -1))
              result.append(c.toChar)
            in.close
          }

          Some(result.toString)
      }
    }
    catch{
      case ioe => return None
    }
  }

  private val CRLF = "\r\n".getBytes
  private val -- = "--".getBytes

  private val cd = "Content-Disposition: form-data; name=\"%s\""
  private def writeField(k:String, v:String)(implicit b:OutputStream){
    write(--, boundary, CRLF)
    write(cd.format(k).getBytes, CRLF)
    write(CRLF)
    write(v.getBytes, CRLF)
  }

  private val cdf = "Content-Disposition: form-data; name=\"%s\"; filename=\"%s\""
  private val ct = "Content-Type: %s"
  private val os = "application/octet-stream"
  private def writeData(k:String, f:String, d:Array[Byte], t:Option[String])
                       (implicit b:OutputStream){
    val typ = (t getOrElse os)
    write(--, boundary, CRLF)
    write(cdf.format(k, f).getBytes, CRLF)
    write(ct.format(typ).getBytes, CRLF)
    write(CRLF)
    write(d, CRLF)
  }

  private def writeEnd(implicit b:OutputStream){
    write(--, boundary, --, CRLF)
  }

  private def write(data:Array[Byte]*)(implicit b:OutputStream){
    data.foreach(b.write(_))
  }
}
