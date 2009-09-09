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

object ImageProducer{
  val imageFormats = Array("jpg", "png")

  val iWriter = imageFormats.foldLeft(Map.empty[String, ImageWriter]){
     case (m, fmt) => 
       val writer = ImageIO.getImageWritersByFormatName(fmt).next
       m + (fmt -> writer.asInstanceOf[ImageWriter])
  }
}

trait ImageProducer{ self:Uploader =>
  import ImageProducer._

  protected var imageWidth:Int = _
  protected var imageHeight:Int = _
  protected var blockWidth = 256
  protected var blockHeight = 256

  protected val dirty = new Dirty

  protected var grid: Seq[((Int,Int), Rectangle)] = Nil

  protected var resized =  false
  def setSize(w:Int, h:Int){
    imageWidth = w
    imageHeight = h
    grid = 
      for{(y,j)<-(0 until imageHeight by blockHeight).toList.zipWithIndex
          (x,i) <- (0 until imageWidth by blockWidth).toList.zipWithIndex}
        yield ((i -> j) -> new Rectangle(x, y, blockWidth, blockHeight))
    dirty.add(0, 0, w, h)
    resized = true
  }

  private var _imageFormat = imageFormats.first

  def imageFormat = _imageFormat

  def imageFormat_=(format:String){
    if(imageFormats.exists(_==format)){
      _imageFormat = format
    }
  }

  def upload[A](imgh: Image => A){
    self.post(update(imgh))
  }

  def update[A](imgh: Image => A):Seq[Param]

  def stop(){
    self.post(List(FieldParam("off-air", "off-air")))
  }

  protected def toByteArray(image:BufferedImage) = {
    new ByteArrayOutputStream match { case b =>
      val writer = iWriter(imageFormat)
      writer.synchronized{
        writer.setOutput(ImageIO.createImageOutputStream(b))
        writer.write(image)
        writer.reset
      }
      b.close
      b.toByteArray
    }
  }

  class Dirty{
    private var pool = Set.empty[Rectangle]
    def add(x:Int, y:Int, w:Int, h:Int):Unit = synchronized{
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

    def find[T](arr:Seq[(T, Rectangle)]):List[T] = synchronized{
      try{
        arr.foldLeft(Set.empty[T]){
          case (s, (t, r)) if(pool.exists(r.intersects(_))) => s + t
          case (s, _) => s
        }.toList
      }
      finally{
        pool.clear
      }
    }
  }

  private val comparePair:((Int,Int),(Int,Int))=>Boolean =
    (i,j) => (i._1<j._1)||((i._1==j._1)&&(i._2<=j._2))

  protected def dataParam(image:Image):List[Param] = {
    var params:List[Param] = Nil
    dirty.find(grid).sort(comparePair) match{
      case area if area.size > 0 =>
        val _image = new BufferedImage(area.size*blockWidth, blockHeight, 
                                       BufferedImage.TYPE_3BYTE_BGR)
        val _graphics = _image.getGraphics
        try{
          val updates = {
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
          params ::= FieldParam("image-format", imageFormat)
          params ::= FieldParam("update", updates.mkString("&"))
          val data = toByteArray(_image)
          params ::= DataParam("data", "data",  data, None)
        }
        finally{
          _graphics.dispose
          _image.flush
	}
      case _ =>
    }

    if(resized){
      params ::= FieldParam("desktop-size", imageWidth+"x"+imageHeight)
      resized = false
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
