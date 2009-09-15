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

import _root_.java.io.ByteArrayOutputStream
import _root_.java.awt.{Image, Rectangle}
import _root_.java.awt.image.BufferedImage
import _root_.javax.imageio._
import _root_.scala.collection.mutable.{Map,Set}

trait ImageProducer{ self:Uploader =>

  protected var imageWidth:Int = _
  protected var imageHeight:Int = _
  protected var blockWidth = 256
  protected var blockHeight = 256

  protected val damaged = new DamagedArea

  protected var thumbnail = false
  protected var last_thumbnail = 0L

  protected var last_fullupdate = 0L

  protected var grid: Seq[((Int,Int), Rectangle)] = Nil

  def setSize(w:Int, h:Int){
    imageWidth = w
    imageHeight = h
    grid = 
      for{(x,i)<-(0 until imageWidth by blockWidth).toList.zipWithIndex
          (y,j)<-(0 until imageHeight by blockHeight).toList.zipWithIndex}
        yield ((i -> j) -> new Rectangle(x, y, blockWidth, blockHeight))
    damaged.clear()
    damaged.add(0, 0, w, h)
  }

  var imageFormat = ImageFormat.default

  def update[A](imgh: Image => A):Seq[Param]

  def upload[A](imgh: Image => A){
    self.post(update(imgh))
  }

  def stop(){
    self.post(List(FieldParam("off-air", "off-air")))
  }

  protected def dataParam(image:Image):List[Param] = {
    var params:List[Param] = Nil
    damaged.find(grid) match{
      case area if area.size > 0 =>

        val _image = new BufferedImage(area.size*blockWidth, blockHeight, 
                                       BufferedImage.TYPE_3BYTE_BGR)
        val _graphics = _image.getGraphics
        try{
          val updates = image.synchronized{
            for(((_x, _y), index) <- area.zipWithIndex)
              yield {
                val x = -(_x*blockWidth) + index*blockWidth
                val y = -(_y*blockHeight)
                _graphics.setClip(index*blockWidth, 0, blockWidth, blockHeight)
                _graphics.drawImage(image, x, y, null)
                "%d,%d,%d,%d".format(_x*blockWidth, _y*blockHeight,
                                     blockWidth, blockHeight)
              }
          }
          params ::= FieldParam("image-format", imageFormat.toString)
          params ::= FieldParam("update", updates.mkString("&"))
          val data = imageFormat.toByteArray(_image)
          params ::= DataParam("data", "data",  data, None)

          if(area.size == grid.size){
            last_fullupdate = System.currentTimeMillis
            params ::=  FieldParam("full-update", "full-update")
          }
        }
        finally{
          _graphics.dispose
          _image.flush
	}
        
        val currentTime = System.currentTimeMillis
        if(thumbnail || ( currentTime - last_thumbnail) > 60*1000){
          last_thumbnail = currentTime
          val _image = new BufferedImage(64, 48, BufferedImage.TYPE_3BYTE_BGR);
          val _graphics = _image.getGraphics
          try{ 
            image.synchronized{
              _graphics.drawImage(image, 0, 0, null)
            }
            val data = imageFormat.toByteArray(_image)
            params ::= DataParam("thumbnail", "thumbnail", data, None)
          }
          finally{
            _graphics.dispose
            _image.flush
            thumbnail = false
          }
        }

        if(!params.isEmpty)
          params ::= FieldParam("desktop-size", imageWidth+"x"+imageHeight)

      case _ =>
    }

    params
  }
} 

trait Uploader{
  val uri:String
  val pmpfd = new PostMultiPartFormData
  def post(param:Seq[Param]){
    if(!param.isEmpty)
      pmpfd(uri, param:_*)
  }
}
