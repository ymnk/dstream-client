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

object Util{
  def prompt(instruction:String,
             prompt:Array[String],
             echo:Array[Boolean],
             default:Array[String]):Option[Array[String]]={

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
      texts(i).setText(default(i))
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
