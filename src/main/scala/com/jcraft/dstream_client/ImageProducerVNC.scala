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

import _root_.scala.concurrent.ops.spawn
import _root_.scala.collection.mutable.{Map,Set}
import _root_.java.awt.{Graphics, Rectangle, Image}
import _root_.java.awt.image.BufferedImage

import _root_.com.jcraft.rfb._

class ImageProducerVNC(override val uri:String, w:Int, h:Int, 
                       host:String, port:Int, password:Option[String]) 
  extends ImageProducer with Uploader { self =>

  var running:Boolean = true

  private val blockWidth=256
  private val blockHeight=256

  val imgType = "jpg" 

  private var image:BufferedImage = _
  private var imageWidth:Int = _
  private var imageHeight:Int = _

  private var cursorX = -1
  private var cursorY = -1
  private var cursorMoved = false 
  private var offAir = false 

  private val dirty=new Dirty

  lazy val grid = {
    for{(y,j)<-(0 until imageHeight by blockHeight).toList.zipWithIndex
        (x,i) <- (0 until imageWidth by blockWidth).toList.zipWithIndex}
      yield ((i -> j) -> new Rectangle(x, y, blockWidth, blockHeight))
  }

  val iUpdater = new ImageUpdater{
    def setImage(image:java.awt.Image){
      //ImageProducerVNC.this.setImage(image)
      self.setImage(image)
    }
    def update(x:Int, y:Int, width:Int, height:Int){
      dirty.add(x, y, width, height)
    }
    def moveCursor(x:Int, y:Int){
      if(cursorX != x || cursorY != y){
        cursorX = x
        cursorY = y
        cursorMoved = true
      }
    }
    def removeImage(){
      self.removeImage()
    }
  }

  var vnc:RFBProtocolMainImage = _
  spawn{
    while(running){
      vnc=new RFBProtocolMainImage(host, port, (password getOrElse ""),
                                   iUpdater) 
      try{vnc.connect }catch{ case e=> println(e) }
      try{Thread.sleep(1000) }catch{ case e=> }
    }
  }

  import java.io._
  def update[A](imgh: Image => A):Seq[Param]={ 
    try { 
      var params:List[Param] = Nil

      if(image==null) return params

      imgh(image)

      if(cursorMoved){
        params ::= FieldParam("move_cursor", cursorX+","+cursorY)
        cursorMoved = false
      }

      if(offAir){
        params ::= FieldParam("offair", "offair")
        offAir = true
      } 

      dirty.find(grid) match{
        case area if area.size > 0 =>
          if(area.size == 12){
            params ::= FieldParam("update", 
                                  "0,0,%d,%d".format(imageWidth, imageHeight))
            val data = toByteArray(image)
            params ::= DataParam("data", "data",  data, None)
          }
          else{
            val _image = new BufferedImage(area.size*blockWidth, 
                                           blockHeight, 
                                           BufferedImage.TYPE_3BYTE_BGR)
            val _graphics = _image.getGraphics

            import java.awt.Color
            _graphics.setColor(Color.white)
            _graphics.fillRect(0, 0, imageWidth, imageHeight)
            val updates = {
              for(((_x, _y), index) <- area.toList.zipWithIndex)
                yield {
                  val x = -(_x*blockWidth) + index*blockWidth
                  val y = -(_y*blockHeight)
                  _graphics.setClip(index*blockWidth, 0, 
                                    blockWidth, blockHeight)
                  _graphics.drawImage(image, x, y, null)
                  "%d,%d,%d,%d".format(_x*blockWidth, _y*blockHeight,
                                       blockWidth, blockHeight)
              }
            }
            params ::= FieldParam("update", updates.mkString("&"))
            val data = toByteArray(_image)
            params ::= DataParam("data", "data",  data, None)
            _graphics.dispose
            _image.flush
          }              
        case _ =>
      }
      params
    }
    catch{ case e => Nil} 
  }

  def dispose{
    if(image!=null) image.flush
    image=null
  }

  def setImage(_image:java.awt.Image){
    image=_image.asInstanceOf[java.awt.image.BufferedImage]
    imageWidth=image.getWidth
    imageHeight=image.getHeight

      // TODO
    imageWidth = w
    imageHeight = h
  }

  def removeImage()=this.synchronized{
    if(image!=null){
      dirty.add(0, 0, imageWidth, imageHeight)
      image = null
      offAir = true
    }
  }

  def stop(){
    running = false
    try{vnc.close }catch{ case e=> }
  }

  class Dirty{
    var pool=Set.empty[Rectangle]
    def add(x:Int, y:Int, w:Int, h:Int):Unit=synchronized{
      var r=new Rectangle(x, y, w, h)
      val rr=for(p<-pool if p.intersects(r)) yield p
      if(rr.isEmpty){
        pool += r
      }
      else{
        pool --= rr
        pool += rr.foldLeft(r){(b, r)=>b.union(r)} 
      }
    }

    def find[T](arr:Seq[(T, Rectangle)]):Set[T]=synchronized{
      try{
        arr.foldLeft(Set.empty[T]){(s, tr) => tr match{
          case (t, r) if(!pool.filter((rr)=>r.intersects(rr)).isEmpty) => {
            pool -= r
            s + t
          }
          case _ => s
        }
        }
      }
      finally{
        pool.clear
      }
    }
  }
}
