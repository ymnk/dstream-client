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

import _root_.java.net.{Authenticator, PasswordAuthentication}
import _root_.java.awt.{Image, Color}
import _root_.javax.swing.ImageIcon
import swing._
import swing.event._
import Swing._

object DStreamClient extends SimpleSwingApplication {

  val url = "http://lift.jcraft.com/dstream/update/"

  var imagePoster:Option[ImagePoster] = None

  val label = new Label
  val iconWidth = 256
  val iconHeight = 192

  var imageFormat = ImageFormat.default

  var desktop = Desktop.default

  var interval = Interval.default

  var userpass:Option[(String, String)] = None 

  def top = new MainFrame() {

    menuBar = new MenuBar {
      contents += new Menu("File") {
        contents += new MenuItem(Action("Exit") {
          System.exit(0)
        })
      }
      contents += new Menu("Tool") {

        contents += new Menu("Image Format") {
          val items:Seq[CheckMenuItem] = ImageFormat.list.map{_imageFormat =>
                        new CheckMenuItem(_imageFormat.toString){
                          peer.setState(_imageFormat == imageFormat)
                          action = Action(_imageFormat.toString) { 
                            imageFormat = _imageFormat
                            imagePoster.map(_.ip.imageFormat = imageFormat)
                            items.foreach{m =>
                               m.peer.setState(m.peer.getText == imageFormat.toString)
                            }
                            update
                          }
                        }
                      }
          items.foreach{contents += _}
        }

        contents += new Menu("Descktop Size") {
          val items:Seq[CheckMenuItem] = Desktop.list.map{ _desktop =>
                        new CheckMenuItem(_desktop.toString){
                          peer.setState(_desktop == desktop)
                          action = Action(_desktop.toString) { 
                            val (w, h) = (_desktop.width, _desktop.height)
                            imagePoster.map(_.ip.setSize(w, h))
                            desktop = _desktop
                            items.foreach{m =>
                               m.peer.setState(m.peer.getText == desktop.toString)
                            }
                            update
                          }
                        }
                      }
          items.foreach{ contents += _ }
        }

        contents += new Menu("Interval") {
          val items:Seq[CheckMenuItem] = Interval.list.map{ _interval =>
                        new CheckMenuItem(_interval.toString){
                          peer.setState(_interval == interval)
                          action = Action(_interval.toString) { 
                            imagePoster.map(_.interval = _interval)
                            interval = _interval
                            items.foreach{m =>
                               m.peer.setState(m.peer.getText == interval.toString)
                            }
                            update
                          }
                        }
                      }
          items.foreach{ contents += _ }
        }

      }
    }

    val channel = new TextField("channel name")

    val imageProducer = new ComboBox(List("Robot", "VNC"))

    val btnConnect = new Button("Connect"){
      reactions += {
        case ButtonClicked(_) =>
          imagePoster = imagePoster match{
            case Some(ip) =>
              ip ! ImagePoster.Stop
              this.text = "Connect"
              clearImage
              None
            case _ =>
              import Util._
              (userpass orElse promptUserPasswd).map{ case up@(user, passwd) =>
                auth(user, passwd)
                userpass = Some(up)
                getImageProducer(imageProducer.selection.item,
                                 url+channel.text.trim).map{ iproducer => 
                  iproducer.imageFormat = imageFormat
                  val iposter = new ImagePoster(iproducer)(
                    new Updater{ def update(img:Image){ drawImage(img) } }
                  )
                  iposter.interval = interval 
                  iposter.start
                  this.text = "Disconnect"
                  iposter
  	        }
              }.getOrElse(None)
          }
          update
      }
    }

    contents = new BorderPanel{
      import BorderPanel.Position._
      import _root_.java.awt.image.BufferedImage
      import BufferedImage.{TYPE_INT_RGB => RGB}
      label.icon = new ImageIcon(new BufferedImage(iconWidth, iconHeight, RGB))
      this.add(new ScrollPane(label), Center)
      val p = new FlowPanel{
        this.contents.append(channel, imageProducer, btnConnect)
      }
      this.add(p, South)
    }

    def setTitle{
      title = "DStream Client: "+
               imagePoster.map(_ => channel.text.trim).getOrElse("")+" "+
               desktop.toString+" "+
               imageFormat
    } 

    def update{
      setTitle
    }

    update

    size = (350, 300)
  }

  private def clearImage() = label.icon match{ case icon:ImageIcon =>
    val g = icon.getImage.getGraphics
    g.setColor(Color.black)
    g.fillRect(0, 0, icon.getIconWidth, icon.getIconHeight)
    g.dispose
    label.repaint
  }

  private def drawImage(img:Image) = label.icon match{ case icon:ImageIcon =>
    val g = icon.getImage.getGraphics
    g.drawImage(img, 0, 0, iconWidth, iconHeight, null)
    g.dispose
    label.repaint
  }

  private def getImageProducer(typ:String, url:String) =  typ match {
    case "VNC" => 
      Util.promptVNC.map{ case (host, _port, passwd) =>
        val port = if(_port.toInt < 5900) _port.toInt+5900 else _port.toInt
        new ImageProducerVNC(url, desktop.width, desktop.height,
                             host, port, Some(passwd))
      }
    case _ => 
      Some(new ImageProducerRobot(url, desktop.width, desktop.height))
  }
}
