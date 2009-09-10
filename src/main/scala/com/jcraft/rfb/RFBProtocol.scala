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
package com.jcraft.rfb

import java.net.{InetAddress, Socket, InetSocketAddress}
import java.io.{DataInputStream, DataOutputStream}

class RFBProtocol{
  import RFBProtocol._

  val TIMEOUT = 3*1000

  var updateInterval =  0 // 1000

  var remoteVersion:Int = _
  var password = ""

  var shareConnection = true

  var frameWidth:Int = _
  var frameHeight:Int = _

  val pixelFormat = new PixelFormat(32, 24, 
                                    0, 1,
                                    0xff, 0xff, 0xff,
                                    24, 16, 8)

  var favoriteAuth:List[Authentication] = List(noneAuthentication)

  import EncodeType._
  var favoriteEncodings:List[Int] =
    List(/*Raw,*/ Hextile, /*CopyRect, */ Cursor, PointerPos)

  var s:Socket = _
  var out:DOS = _
  var in:DIS = _

  def readChar = in.readChar&0xffff
  def readByte = in.readByte&0xff
  def readBytes(n:Int) = in.readBytes(n)
  def writeByte(arg:Int*) = arg.foreach(out.writeByte(_))
  def writeChar(arg:Int*) = arg.foreach(out.writeChar(_))

  var updateAdapter:UpdateAdapter = new DumbUpdateAdapter

  def close{
    in.close
    out.close
    s.close
  }

  def connect(host:String, port:Int) = try{

    openSocket(InetAddress.getByName(host), port) match{
      case(i, o) => in=i; out=o
    }

    remoteVersion = exchangeProtocolVersion

    securityCheck

    clientInit
    val serverPixelFormat = serverInit

    setPixelFormat(pixelFormat)

    updateAdapter.init(frameWidth, frameHeight, pixelFormat, in)

    setEncodings

    frameBufferUpdateRequest(false,
                             0, 0,
                             frameWidth, frameHeight)
    while(true){
      import MessageTypeS2C._
      var command = readByte
//println(".")
      command match{
        case FramebufferUpdate =>{
          in.skipBytes(1)  // padding
          var numberOfRectangles = readChar
          while(numberOfRectangles > 0){
            val x = readChar
            val y = readChar
            val width =  readChar
            val height = readChar
            val encodingType = in.readInt

            import EncodeType._

            encodingType match{
              case Raw => 
                updateAdapter.updateRaw(x, y, width, height)
              case CopyRect =>
                val srcX = readChar
                val srcY = readChar
                updateAdapter.updateCopyRect(srcX, srcY, x, y, width, height)
              case Hextile =>
                updateAdapter.updateHextile(x, y, width, height)
              case Cursor => 
                updateAdapter.updateCursor(x, y, width, height)
              case PointerPos => 
                updateAdapter.updatePointerPos(x, y)
              case  _ =>
                updateAdapter.updateUnknown(encodingType)
            }
            numberOfRectangles -= 1
          }

          if(in.available <= 0 &&
             updateInterval>0){
            try{ Thread.sleep(updateInterval)}catch{case e=> }
          }

          if(in.available > 0){
          }
          else{
          frameBufferUpdateRequest(true,
                                   0, 0, frameWidth, frameHeight)
//println("--")
          }
        }

        case SetColorMapEntries =>{
          in.skipBytes(1) // padding
          val firstColor = readChar
          var numberOfColors = readChar
          while(numberOfColors > 0){
            val red = readChar
            val green = readChar
            val blue = readChar
            numberOfColors -= 1
          }
        }

        case Bell => updateAdapter.bell()

        case ServerCutText =>{
          in.skipBytes(3) // padding
          val length = in.readInt
          val text = readBytes(length)
          updateAdapter.serverCutText(text)
        }

        case _ =>
      }
    }
  }
  catch{
    case e => println(e)
  }

  def exchangeProtocolVersion = {
    val versionString = RFBProtocol.versionString
    val protocolVersion = readBytes(12)
    val remoteVersion = new String(protocolVersion) match{
      case versionString(n) => 30+n.toInt
      case _ => 0
    }
    out.write(protocolVersion); out.flush
    remoteVersion
  }

  def securityCheck(){
    val number_of_security_types = readByte
    val security_types=new Array[Byte](number_of_security_types)
    in.readFully(security_types)

    val ERROR = -1
    val request_security_type =
      (ERROR /: favoriteAuth.map(_.typ)){
        case(b@ERROR, n) => if(security_types.exists(_==n)) n else b
        case (b, _) => b
      }

    if(request_security_type != ERROR){
      out.writeByte(request_security_type); out.flush
      val List(auth) = favoriteAuth.filter(_.typ==request_security_type)
      auth(this)
    }
    else{
      println("failed to negociate about auth type.")
    }
  }

  def clientInit{
    out.writeByte(if(shareConnection) 1 else 0 )
    out.flush
  }

  def serverInit = {
    frameWidth = readChar
    frameHeight = readChar

    val bitsPerPixel = readByte
    val depth = readByte
    val bigEndianFlag = readByte
    val trueColorFlag = readByte

    val (redMask, greenMask, blueMask) = (readChar, readChar, readChar)
    val (redShift, greenShift, blueShift) = (readByte, readByte, readByte)

    in.skipBytes(3)

    val nameLength = in.readInt
    val name = readBytes(nameLength)

    new PixelFormat(bitsPerPixel, depth, 
                    bigEndianFlag, trueColorFlag,
                    redMask, greenMask, blueMask,
                    redShift, greenShift, blueShift)
  }

  def setPixelFormat(pf:PixelFormat){
    import pf._
    writeByte(MessageTypeC2S.SetPixelFormat, 0, 0, 0)
    writeByte(bitsPerPixel)
    writeByte(depth)
    writeByte(bigEndianFlag)
    writeByte(trueColorFlag)
    writeChar(redMask, greenMask, blueMask)
    writeByte(redShift, greenShift, blueShift)
    writeByte(0, 0, 0)
    out.flush
  }

  def setEncodings{
    writeByte(MessageTypeC2S.SetEncodings, 0)
    out.writeChar(favoriteEncodings.size)
    favoriteEncodings.foreach(out.writeInt(_))
    out.flush
  }

  def frameBufferUpdateRequest(incremental:Boolean,
                               x:Int, y:Int,
                               width:Int, height:Int){
    writeByte(MessageTypeC2S.FramebufferUpdateRequest,
              if(incremental) 1 else 0)
    writeChar(x, y, width, height)
    out.flush
  }


  private def openSocket(ip:InetAddress, port:Int) = {
    s = new Socket
    s.connect(new InetSocketAddress(ip, port), TIMEOUT)
    (new DataInputStream(s.getInputStream) with ReadBytes,
     new DataOutputStream(s.getOutputStream))
  }
}

object RFBProtocol{
  val versionString="RFB 003.00([1-9])\\n".r

  trait ReadBytes{ self:DataInputStream  =>
    def readBytes(n:Int) = {
      val buf=new Array[Byte](n)
      self.readFully(buf)
      buf
    }
  }

  type DOS = DataOutputStream
  type DIS = DataInputStream with ReadBytes

  object MessageTypeC2S{
    val SetPixelFormat = 0
    val SetEncodings = 2
    val FramebufferUpdateRequest = 3
    val KeyEvent = 4
    val PointerEvent = 5
    val ClientCutText = 6
  }

  object MessageTypeS2C{
    val FramebufferUpdate = 0
    val SetColorMapEntries = 1
    val Bell = 2
    val ServerCutText = 3
  }

  object SecurityType{
    val Invalid = 0
    val None = 1
    val VNCAuthentication = 2
  }

  object EncodeType{
    val Raw = 0
    val CopyRect = 1
    val RRE = 2
    val Hextile = 5
    val ZRLE = 16
    val Cursor = -239
    val PointerPos = -232
  }

  object HextileType{
    val Raw = 1
    val BackgroundSpecified = 2
    val ForegroundSpecified = 4
    val AnySubrects = 8
    val SubrectsColored = 16
  }

  trait Authentication{
    def typ:Int
    def apply(rfb:RFBProtocol):Int
    def securityResult(rfb:RFBProtocol):Int = rfb.in.readInt
  }

  object noneAuthentication extends Authentication{
    def typ = SecurityType.None
    def apply(rfb:RFBProtocol):Int =
      if(rfb.remoteVersion>=38){ securityResult(rfb) }else{ 0 }
  }

  object vncAuthentication extends Authentication{
    import javax.crypto._
    import javax.crypto.spec._
    import java.security._

    // generate an array: 0x01 0x02 0x04 0x08 0x10 0x20 0x40 0x80
    private lazy val bits = for(i<-(0 until 8)) yield 1<<i

    def typ = SecurityType.VNCAuthentication

    def apply(rfb:RFBProtocol):Int = {
      import rfb.{in, out, password}

      val challenge = in.readBytes(16)
      val key = new Array[Byte](8) match{
        case key =>
           val p = password.getBytes
           val plen = if(p.length<key.length) p.length else key.length
           System.arraycopy(p, 0, key, 0, plen); key
      }


      // Invert every byte.  For example,
      //   0x20 (0010 0000) should be converted to 0x04 (0000 0100) .
      val _key=for(x<-key)
                 yield
                 (0 /: bits){(b, i) => b<<1|(if((x&i)==i) 1 else 0)}
                 .asInstanceOf[Byte]

      val keyFac = SecretKeyFactory.getInstance("DES")
      val secKey = keyFac.generateSecret(new DESKeySpec(_key))
      val c = Cipher.getInstance("DES/ECB/NoPadding")
      c.init(Cipher.ENCRYPT_MODE, secKey);
      out.write(c.doFinal(challenge)); out.flush
      securityResult(rfb)
    }
  }

  class PixelFormat(val bitsPerPixel:Int, val depth:Int,
                    val bigEndianFlag:Int, val trueColorFlag:Int,
                    val redMask:Int, val greenMask:Int, val blueMask:Int, 
                    val redShift:Int, val greenShift:Int, val blueShift:Int) 

  trait UpdateAdapter{
    var frameWidth:Int = _
    var frameHeight:Int = _
    var pixelFormat:PixelFormat = _
    var in:DIS = _
    def init(w:Int, h:Int, pixelFormat:PixelFormat, in:DIS){ 
      this.frameWidth=w; this.frameHeight=h; 
      this.pixelFormat=pixelFormat; this.in=in 
    }
    def updateRaw(x:Int, y:Int, w:Int, h:Int):Unit
    def updateCopyRect(sx:Int, sy:Int, dx:Int, dy:Int, w:Int, h:Int):Unit
    def updateHextile(x:Int, y:Int, w:Int, h:Int):Unit
    def updateCursor(x:Int, y:Int, w:Int, h:Int):Unit
    def updatePointerPos(x:Int, y:Int):Unit
    def updateUnknown(encodingType:Int):Unit
    def bell():Unit
    def serverCutText(text:Array[Byte]):Unit
  }

  class DumbUpdateAdapter extends UpdateAdapter{
    def updateRaw(x:Int, y:Int, w:Int, h:Int){
      in.skipBytes(w*h*pixelFormat.bitsPerPixel/8)
      println("update: Raw %d %d %d %d".format(x, y, w, h))
    }
    def updateCopyRect(sx:Int, sy:Int, dx:Int, dy:Int, w:Int, h:Int){
      println("update: CopyRect %d %d %d %d %d %d".format(sx, sy, dx, dy, w, h))
    }
    def updateHextile(x:Int, y:Int, w:Int, h:Int){
      import HextileType._

      def subrect(tx:Int, ty:Int, tw:Int, th:Int){
        val subencoding = in.readByte&0xff
        if((subencoding & Raw) != 0){
          in.skipBytes(tw*th*pixelFormat.bitsPerPixel/8)
          return
        } 
        if((subencoding & BackgroundSpecified) != 0){
          in.skipBytes(pixelFormat.bitsPerPixel/8)
        }
        if((subencoding & ForegroundSpecified) != 0){
          in.skipBytes(pixelFormat.bitsPerPixel/8)
        } 
        if((subencoding & AnySubrects) == 0){
          return
        }
        val nSubrects = in.readByte&0xff
        in.skipBytes(nSubrects*2)
        if((subencoding & SubrectsColored) != 0){
          in.skipBytes(nSubrects*pixelFormat.bitsPerPixel/8)
        }
      }

      for(ty <- (y until y+h by 16)){
        val th = if(y+h-ty<16) y+h-ty else 16
        for(tx <- (x until x+w by 16)){
          val tw = if(x+w-tx<16) x+w-tx else 16
          subrect(tx, ty, tw, th)
        }
      }

      println("update: Hextile %d %d %d %d".format(x, y, w, h))
    }
    def updateCursor(x:Int, y:Int, w:Int, h:Int){
      in.skipBytes(w*h*pixelFormat.bitsPerPixel/8)
      in.skipBytes(h*Math.floor((w+7)/8).asInstanceOf[Int])
      println("update: Cursor")
    }
    def updatePointerPos(x:Int, y:Int){
      println("update: PointerPos %d %d".format(x, y))
    }
    def updateUnknown(encodingType:Int){
      println("unknown encodingType %d".format(encodingType))
    }
    def bell(){
      System.out.println("bell")
    }
    def serverCutText(text:Array[Byte]){
      System.out.println("ServerCutText: "+new String(text))
    }
  }
}

object RFBProtocolMain{
  def main(arg:Array[String]){
    val Array(host, port, password@_*) = arg
    val rfbp=new RFBProtocol
    if(password.length>0){
      rfbp.favoriteAuth = 
        rfbp.favoriteAuth ::: List(RFBProtocol.vncAuthentication)
      rfbp.password = password.first
    }
    rfbp.connect(host, port.toInt)
  }
}

class ImageGenerator(iu: {def setImage(i:java.awt.Image):Unit
                          def update(x:Int, y:Int, w:Int, h:Int):Unit
                          def moveCursor(x:Int, y:Int):Unit
                        }) extends AnyRef with RFBProtocol.UpdateAdapter{

  import RFBProtocol.{PixelFormat, DIS, HextileType}
  import java.awt.Graphics
  import java.awt.Color
  import java.awt.image._
  import java.awt.image.renderable._

  val alpha = 255.asInstanceOf[Byte]
  var bytesPerPixel:Int = _
  var frameWidthb:Int = _

  var image:BufferedImage = _
  var graphics:Graphics = _
  var imageData:Array[Byte] = _
  var cBuffer:Array[Byte] = _

  override def init(w:Int, h:Int, pixelFormat:PixelFormat, in:DIS){ 
    super.init(w:Int, h:Int, pixelFormat:PixelFormat, in:DIS)

    bytesPerPixel = 3  //for imageData; BufferedImage.TYPE_3BYTE_BGR

    cBuffer=new Array[Byte](pixelFormat.bitsPerPixel/8)

    frameWidthb = frameWidth * bytesPerPixel

    image = new BufferedImage(w, h, BufferedImage.TYPE_3BYTE_BGR) 
    graphics = image.getGraphics
    val raster = image.getRaster
    val db = (raster.getDataBuffer).asInstanceOf[DataBufferByte]
    imageData = db.getBankData()(0)

    iu.setImage(image)
  }

  def updateRaw(x:Int, y:Int, w:Int, h:Int):Unit  = image.synchronized{
    updateRaw(x, y, w, h, true)
  }

  private def updateRaw(x:Int, y:Int, w:Int, h:Int, update:Boolean):Unit = {
    val wb = w * bytesPerPixel
    var index = (y * frameWidth + x)*bytesPerPixel
    val last = index + frameWidthb * h

    while(index < last){
      val _last = index + wb
      while(index<_last){
        in.skip(1)
        in.readFully(imageData, index, 3)
        index += 3
      }
      index = (index - wb) + frameWidthb
/*
      // code for BufferedImage.TYPE_4BYTE_ABGR
      in.readFully(imageData, index, wb)
      var i = 0
      while(i<wb){
        imageData(index+i) = alpha
        i += 4
      }
      index += frameWidthb
*/
    }

    if(update) {
image.flush
      iu.update(x, y, w, h)
    }
  }

  private def readColor()={
    in.readFully(cBuffer)
    if(bytesPerPixel==1){
      Color.black // it shoud be from color map
    }
    else{
      new Color(cBuffer(3)&0xff, cBuffer(2)&0xff, cBuffer(1)&0xff)
    }
  }

  def updateCopyRect(sx:Int, sy:Int, x:Int, y:Int, w:Int, h:Int):Unit = image.synchronized{
    val dx = x - sx
    val dy = y - sy
    graphics.copyArea(sx, sy, w, h, dx ,dy)

image.flush
    iu.update(x, y, w, h)
  }

  def updateHextile(x:Int, y:Int, w:Int, h:Int):Unit = image.synchronized{
      import HextileType._

      var bg = java.awt.Color.black
      var fg = java.awt.Color.black

      def subrect(tx:Int, ty:Int, tw:Int, th:Int){
        val subencoding = in.readByte&0xff

        if((subencoding & Raw) != 0){
          updateRaw(tx, ty, tw, th, false)
          return
        } 

        if((subencoding & BackgroundSpecified) != 0){
          bg = readColor
        }

	graphics.setColor(bg)
	graphics.fillRect(tx, ty, tw, th)

        if((subencoding & ForegroundSpecified) != 0){
          fg = readColor
        } 

        if((subencoding & AnySubrects) == 0){
          return
        }

        val rects = in.readByte&0xff
        val colored = ((subencoding & SubrectsColored) != 0)
 
        if(!colored)
          graphics.setColor(fg)

        var b = 0
        var i = 0
        while(i < rects){ 
          if(colored){
	    fg = readColor
            graphics.setColor(fg)
	  }
          b = in.readByte&0xff
          val sx = tx + ((b>>4)&0x0f)
          val sy = ty + (b&0x0f)
          b = in.readByte&0xff
          val sw = ((b>>4)&0x0f) + 1       
          val sh = (b&0x0f) + 1
          graphics.fillRect(sx, sy, sw, sh)

          i += 1 
	}
      }

      val yh = y+h
      val xw = x+w
      var ty = y
      while(ty < yh){
        val th = if(yh-ty<16) yh-ty else 16
        var tx = x
        while(tx < xw){
          val tw = if(xw-tx<16) xw-tx else 16
          subrect(tx, ty, tw, th)
          tx += 16
        }
        ty += 16
      }

image.flush
      iu.update(x, y, w, h)
  } 

  def updateCursor(x:Int, y:Int, w:Int, h:Int){
    in.skipBytes(w*h*pixelFormat.bitsPerPixel/8)
    in.skipBytes(h*Math.floor((w+7)/8).asInstanceOf[Int])
  }
  def updatePointerPos(x:Int, y:Int){
    iu.moveCursor(x, y)
  }
  def updateUnknown(encodingType:Int){
  }
  def bell(){
  }
  def serverCutText(text:Array[Byte]){
  }
} 

object RFBProtocolMainImage{
  def main(arg:Array[String]){
    val Array(host, port, password@_*) = arg
    val rfbp=new RFBProtocol
    if(password.length>0){
      rfbp.favoriteAuth = 
        rfbp.favoriteAuth ::: List(RFBProtocol.vncAuthentication)
      rfbp.password = password.first
    }
    rfbp.updateInterval = 0
//rfbp.updateAdapter = new ImageGenerator
    rfbp.connect(host, port.toInt)
  }
}

class RFBProtocolMainImage(val host:String,
                           val port:Int,
                           val password:String,
                           val ia:{
                             def setImage(i:java.awt.Image):Unit
                             def update(x:Int, y:Int, w:Int, h:Int):Unit
                             def moveCursor(x:Int, y:Int):Unit
                           }){
  private val rfbp=new RFBProtocol
  rfbp.favoriteAuth = 
    rfbp.favoriteAuth ::: List(RFBProtocol.vncAuthentication)
  rfbp.password = password
  rfbp.updateAdapter = new ImageGenerator(ia)

  def connect{
    rfbp.connect(host, port)
  }

  def close{
    try{rfbp.close}catch{case e=> }
  }
}
