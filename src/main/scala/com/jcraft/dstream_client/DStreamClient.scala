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
import swing.{Label, SimpleGUIApplication, MainFrame, Button}
import swing.{BorderPanel, ScrollPane, ComboBox, FlowPanel, TextField}
import swing.event.ButtonClicked

object DStreamClient extends SimpleGUIApplication {

  val url = "http://lift.jcraft.com/dstream/update/"
  
  import java.awt.{GraphicsEnvironment => GE}
  val (dWidth, dHeight) = GE.getLocalGraphicsEnvironment match{
    case env =>
      val displayMode = env.getDefaultScreenDevice.getDisplayMode
//      (displayMode.getWidth, displayMode.getHeight)
      (1024, 768)
  }

  var imagePoster:Option[ImagePoster] = None

  val label = new Label
  val iconWidth = 256
  val iconHeight = 192

  lazy val Array(username, passwd):Array[String] = {
    prompt("DStream Login", Array("E-mail address", "Password"), Array(true, false)). 
      getOrElse{System.exit(0); Array[String]() }
  }

  def top = new MainFrame() {
    title = "DStream Client"

    val channel = new TextField("channel name")

    val imageProducer = new ComboBox(List("Robot", "VNC"))
    val imageFormat = new ComboBox(ImageProducer.imageFormats){ self =>
      import javax.swing.JComboBox
      import java.awt.event.{ItemListener, ItemEvent}
      peer.addItemListener(new ItemListener(){
        def itemStateChanged(e:ItemEvent){
          if(e.getStateChange == ItemEvent.SELECTED){
            imagePoster.map(_.ip.imageFormat = self.selection.item)
          }
        }
      })
    }

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
              val _passwd = passwd.toCharArray
              Authenticator.setDefault(
                new Authenticator {
                  override def getPasswordAuthentication = {
                    new PasswordAuthentication(username, _passwd)
                  }
                }
              )

              val iproducer = getImageProducer(imageProducer.selection.item,
                                               url+channel.text.trim)
              iproducer.imageFormat = imageFormat.selection.item
              val iposter = new ImagePoster(iproducer)(
                new { def update(img:Image){ drawImage(img) } }
              )
              iposter.start
              this.text = "Disconnect"
              Some(iposter)
          }
      }
    }

    contents = new BorderPanel{
      import BorderPanel.Position._
      import _root_.java.awt.image.BufferedImage
      import BufferedImage.{TYPE_INT_RGB => RGB}
      label.icon = new ImageIcon(new BufferedImage(iconWidth, iconHeight, RGB))
      this.add(new ScrollPane(label), Center)
      val p = new FlowPanel{
        this.contents.append(channel, imageProducer, imageFormat, btnConnect)
      }
      this.add(p, South)
    }

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
      val Array(host, _port, passwd):Array[String] = {
        prompt("VNC Connection", 
               Array("Host", "TCP Port", "Password"), 
               Array(true, true, false)).
          getOrElse{System.exit(0); Array[String]() }
      }
      val port = if(_port.toInt < 5900) _port.toInt+5900 else 5900
      new ImageProducerVNC(url, dWidth, dHeight, host, port, Some(passwd))
    case _ => 
      new ImageProducerRobot(url, dWidth, dHeight)
  }

  def prompt(instruction:String,
             prompt:Array[String],
             echo:Array[Boolean]):Option[Array[String]]={

    import java.awt._
    import javax.swing._

    val gbc = new GridBagConstraints(0,0,1,1,1,1,
                                     GridBagConstraints.NORTHWEST,
                                     GridBagConstraints.NONE,
                                     new Insets(0,0,0,0),0,0)

    val panel = new JPanel()
    panel.setLayout(new GridBagLayout())

    gbc.weightx = 1.0
    gbc.gridwidth = GridBagConstraints.REMAINDER
    gbc.gridx = 0
    panel.add(new JLabel(instruction), gbc)
    gbc.gridy += 1

    gbc.gridwidth = GridBagConstraints.RELATIVE

    val texts = new Array[JTextField](prompt.length)
    for(i <- 0 until prompt.length){
      gbc.fill = GridBagConstraints.NONE
      gbc.gridx = 0
      gbc.weightx = 1
      panel.add(new JLabel(prompt(i)), gbc)

      gbc.gridx = 1
      gbc.fill = GridBagConstraints.HORIZONTAL
      gbc.weighty = 1
      if(echo(i)){
        texts(i) = new JTextField(20)
      }
      else{
        texts(i) = new JPasswordField(20)
      }
      panel.add(texts(i), gbc)
      gbc.gridy += 1
    }

    import JOptionPane._
    if(showConfirmDialog(null, panel, "", 
                         OK_CANCEL_OPTION, QUESTION_MESSAGE) == OK_OPTION){
      Some(texts.map(_.getText))
    }
    else{
      None
    }
  }
}
