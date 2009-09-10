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
import _root_.java.awt.Image
import _root_.java.awt.image.BufferedImage

import _root_.com.jcraft.rfb._

class ImageProducerVNC(override val uri:String, w:Int, h:Int, 
                       host:String, port:Int, password:Option[String]) 
  extends ImageProducer with Uploader { self =>

  private var image:BufferedImage = _

  var lastUpdate:Long = _

  var running:Boolean = true

  private var cursorX = -1
  private var cursorY = -1
  private var cursorMoved = false 
  private var offAir = false 

  val iUpdater = new ImageUpdater{
    def setImage(image:Image){
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

  lastUpdate = System.currentTimeMillis
  var vnc:RFBProtocolMainImage = _
  spawn{
    while(running){
      vnc=new RFBProtocolMainImage(host, port, (password getOrElse ""),
                                   iUpdater) 
      try{vnc.connect }catch{ case e => println(e) }
      try{Thread.sleep(1000) }catch{ case e => }
    }
  }

  def update[A](imgh: Image => A):Seq[Param]={ 
    try { 
      var params:List[Param] = Nil

      if(image==null) return params

      imgh(image)

      if(cursorMoved || 
         (System.currentTimeMillis - lastUpdate > 30*1000)){ // heart beat
        params ::= FieldParam("move-cursor", cursorX+","+cursorY)
        cursorMoved = false
      }

      if(offAir){
        params ::= FieldParam("off-air", "off-air")
        offAir = false
      } 

      params :::= dataParam(image) 

      if(params.size>0){
        lastUpdate = System.currentTimeMillis
      }

      params
    }
    catch{ case e => Nil} 
  }

  def dispose{
    if(image!=null) image.flush
    image=null
  }

  def setImage(_image:Image){
    image=_image.asInstanceOf[java.awt.image.BufferedImage]
    // TODO
    //setSize(image.getWidth, image.getHeight)
    setSize(w, h)
  }

  def removeImage()=this.synchronized{
    if(image!=null){
      dirty.add(0, 0, imageWidth, imageHeight)
      image = null
      offAir = true
    }
  }

  override def stop(){
    super.stop()
    running = false
    try{vnc.close }catch{ case e=> }
  }
}
