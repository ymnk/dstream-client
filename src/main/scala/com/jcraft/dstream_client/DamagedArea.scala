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

import _root_.java.awt.Rectangle
import _root_.scala.collection.mutable

class DamagedArea{
  private val pool = mutable.Set.empty[Rectangle]
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
      arr.foldRight(List[T]()){
        case ((t, r), l) if(pool.exists(r.intersects(_))) => t :: l
        case (_, l) => l
      }
    }
    finally{
      clear()
    }
  }

  def clear():Unit = synchronized{
    pool.clear
  }
}
