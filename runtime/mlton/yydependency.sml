(* utf8-sig.sml
 *
 * COPYRIGHT (c) 2020 John Reppy (http://www.cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Routines for working with UTF8 encoded strings.
 *)

signature UTF8 =
  sig

    type wchar = word

    val maxCodePoint : wchar	(* = 0wx0010FFFF *)

    exception Incomplete
	(* raised by some operations when applied to incomplete strings. *)

  (** Character operations **)

    val getu : (char, 'strm) StringCvt.reader -> (wchar, 'strm) StringCvt.reader
	(* convert a character reader to a wide-character reader *)

    val encode : wchar -> string
	(* return the UTF8 encoding of a wide character *)

    val isAscii : wchar -> bool
    val toAscii : wchar -> char		(* truncates to 7-bits *)
    val fromAscii : char -> wchar	(* truncates to 7-bits *)

    val toString : wchar -> string
	(* return a printable string representation of a wide character *)

  (** String operations **)

    val size : string -> int
	(* return the number of Unicode characters *)

    val explode : string -> wchar list
	(* return the list of wide characters that are encoded by a string *)
    val implode : wchar list -> string
	(* return the UTF-8 encoded string that represents the list of
	 * Unicode code points.
	 *)

    val map : (wchar -> wchar) -> string -> string
	(* map a function over the Unicode characters in the string *)
    val app : (wchar -> unit) -> string -> unit
	(* apply a function to the Unicode characters in the string *)
    val fold : ((wchar * 'a) -> 'a) -> 'a -> string -> 'a
	(* fold a function over the Unicode characters in the string *)
    val all : (wchar -> bool) -> string -> bool
    val exists : (wchar -> bool) -> string -> bool

  end


(* utf8.sml
 *
 * COPYRIGHT (c) 2020 John Reppy (http://www.cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Routines for working with UTF8 encoded strings.
 *
 *	Unicode value		        1st byte    2nd byte    3rd byte    4th byte
 *	-----------------------	        --------    --------    --------    --------
 *	00000 00000000 0xxxxxxx	        0xxxxxxx
 *	00000 00000yyy yyxxxxxx	        110yyyyy    10xxxxxx
 *	00000 zzzzyyyy yyxxxxxx	        1110zzzz    10yyyyyy	10xxxxxx
 *      wwwzz zzzzyyyy yyxxxxxx         11110www    10zzzzzz    10yyyyyy    10xxxxxx
 *
 *)

structure UTF8 :> UTF8 =
  struct

    structure W = Word
    structure SS = Substring

    type wchar = W.word

    fun w2c w = Char.chr(W.toInt w)

    val maxCodePoint : wchar = 0wx0010FFFF

  (* maximum values for the first byte for each encoding length *)
    val max1Byte : W.word = 0wx7f (* 0xxx xxxx *)
    val max2Byte : W.word = 0wxdf (* 110x xxxx *)
    val max3Byte : W.word = 0wxef (* 1110 xxxx *)
    val max4Byte : W.word = 0wxf7 (* 1111 0xxx *)

  (* bit masks for the first byte for each encoding length *)
    val mask2Byte : W.word = 0wx1f
    val mask3Byte : W.word = 0wx0f
    val mask4Byte : W.word = 0wx07

    exception Incomplete
	(* raised by some operations when applied to incomplete strings. *)

  (* add a continuation byte to the end of wc.  Continuation bytes have
   * the form 0b10xxxxxx.
   *)
    fun getContByte getc (wc, ss) = (case (getc ss)
	   of NONE => raise Incomplete
	    | SOME(c, ss') => let
		val b = W.fromInt(Char.ord c)
		in
		  if (W.andb(0wxc0, b) = 0wx80)
		    then (W.orb(W.<<(wc, 0w6), W.andb(0wx3f, b)), ss')
		    else raise Incomplete
		end
	  (* end case *))

  (* convert a character reader to a wide-character reader *)
    fun getu getc = let
	  val getContByte = getContByte getc
	  fun get strm = (case getc strm
		 of NONE => NONE
		  | SOME(c, ss) => let
		      val w = W.fromInt(Char.ord c)
		      val (wc, ss) = if (w <= max1Byte)
			  then (w, ss)
			else if (w <= max2Byte)
			  then getContByte (W.andb(mask2Byte, w), ss)
			else if (w <= max3Byte)
			  then getContByte(getContByte(W.andb(mask3Byte, w), ss))
			else if (w <= max4Byte)
			  then getContByte(getContByte(getContByte(W.andb(mask4Byte, w), ss)))
			  else raise Incomplete
		      in
			SOME(wc, ss)
		      end
		(* end case *))
	  in
	    get
	  end

    fun isAscii (wc : wchar) = (wc <= max1Byte)
    fun toAscii (wc : wchar) = w2c(W.andb(0wx7f, wc))
    fun fromAscii c = W.andb(0wx7f, W.fromInt(Char.ord c))

  (* return a printable string representation of a wide character *)
    fun toString wc =
	  if isAscii wc
	    then Char.toCString(toAscii wc)
	  else if (wc <= max2Byte)
	    then "\\u" ^ (StringCvt.padLeft #"0" 4 (W.toString wc))
	  (* NOTE: the following is not really SML syntax *)
	    else "\\u" ^ (StringCvt.padLeft #"0" 8 (W.toString wc))

  (* return a list of characters that is the UTF8 encoding of a wide character *)
    fun encode' (wc, chrs) = if (wc <= 0wx7f)
	    then w2c wc :: chrs
	  else if (wc <= 0wx7ff)
	    then w2c(W.orb(0wxc0, W.>>(wc, 0w6))) ::
	      w2c(W.orb(0wx80, W.andb(wc, 0wx3f))) :: chrs
	  else if (wc <= 0wxffff)
	    then w2c(W.orb(0wxe0, W.>>(wc, 0w12))) ::
	      w2c(W.orb(0wx80, W.andb(W.>>(wc, 0w6), 0wx3f))) ::
	      w2c(W.orb(0wx80, W.andb(wc, 0wx3f))) :: chrs
	  else if (wc <= maxCodePoint)
	    then w2c(W.orb(0wxf0, W.>>(wc, 0w18))) ::
	      w2c(W.orb(0wx80, W.andb(W.>>(wc, 0w12), 0wx3f))) ::
	      w2c(W.orb(0wx80, W.andb(W.>>(wc, 0w6), 0wx3f))) ::
	      w2c(W.orb(0wx80, W.andb(wc, 0wx3f))) :: chrs
	    else raise Domain

    fun encode wc = String.implode(encode'(wc, []))

    val getContByte = getContByte SS.getc

    fun getWC (c1, ss) = let
	  val w = W.fromInt(Char.ord c1)
	  val (wc, ss) = if (w <= max1Byte)
	      then (w, ss)
	    else if (w <= max2Byte)
	      then getContByte (W.andb(mask2Byte, w), ss)
	    else if (w <= max3Byte)
	      then getContByte(getContByte(W.andb(mask3Byte, w), ss))
	    else if (w <= max4Byte)
	      then getContByte(getContByte(getContByte(W.andb(mask4Byte, w), ss)))
	      else raise Incomplete
	  in
	    (wc, ss)
	  end

  (* return the number of Unicode characters *)
    fun size s = let
	  fun len (ss, n) = (case SS.getc ss
		 of NONE => n
		  | SOME arg => let
		      val (_, ss) = getWC arg
		      in
			len (ss, n+1)
		      end
		(* end case *))
	  in
	    len (SS.full s, 0)
	  end

    fun map f s = let
	  fun mapf (ss, chrs) = (case SS.getc ss
		 of NONE => String.implode (List.rev chrs)
		  | SOME arg => let
		      val (wc, ss) = getWC arg
		      in
			mapf (ss, List.revAppend(encode'(wc, []), chrs))
		      end
		(* end case *))
	  in
	    mapf (SS.full s, [])
	  end

    fun app f s = let
	  fun appf ss = (case SS.getc ss
		 of NONE => ()
		  | SOME arg => let
		      val (wc, ss) = getWC arg
		      in
			f wc; appf ss
		      end
		(* end case *))
	  in
	    appf (SS.full s)
	  end

  (* fold a function over the Unicode characters in the string *)
    fun fold f = let
	  fun foldf (ss, acc) = (case SS.getc ss
		 of NONE => acc
		  | SOME arg => let
		      val (wc, ss) = getWC arg
		      in
			foldf (ss, f (wc, acc))
		      end
		(* end case *))
	  in
	    fn init => fn s => foldf (SS.full s, init)
	  end

    fun all pred s = let
	  fun allf ss = (case SS.getc ss
		 of NONE => true
		  | SOME arg => let
		      val (wc, ss) = getWC arg
		      in
			pred wc andalso allf ss
		      end
		(* end case *))
	  in
	    allf (SS.full s)
	  end

    fun exists pred s = let
	  fun existsf ss = (case SS.getc ss
		 of NONE => true
		  | SOME arg => let
		      val (wc, ss) = getWC arg
		      in
			pred wc orelse existsf ss
		      end
		(* end case *))
	  in
	    existsf (SS.full s)
	  end

  (* return the list of wide characters that are encoded by a string *)
    fun explode s = List.rev(fold (op ::) [] s)

    fun implode wcs = String.implode(List.foldr encode' [] wcs)

  end


  (* random-sig.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature RANDOM =
  sig

    type rand
	(* the internal state of a random number generator *)

    val rand : (int * int) -> rand
	(* create rand from initial seed *)

    val toString : rand -> string
    val fromString : string -> rand
        (* convert state to and from string
         * fromString raises Fail if its argument
         * does not have the proper form.
         *)

    val randInt : rand -> int
	(* generate ints uniformly in [minInt,maxInt] *)

    val randNat : rand -> int
	(* generate ints uniformly in [0,maxInt] *)

    val randReal : rand -> real
	(* generate reals uniformly in [0.0,1.0) *)

    val randRange : (int * int) -> rand -> int
	(* randRange (lo,hi) generates integers uniformly [lo,hi].
	 * Raises Fail if hi < lo.
	 *)

  end; (* RANDOM *)




(* random.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * This package implements a random number generator using a subtract-with-borrow
 * (SWB) generator as described in Marsaglia and Zaman, "A New Class of Random Number
 * Generators," Ann. Applied Prob. 1(3), 1991, pp. 462-480.
 *
 * The SWB generator is a 31-bit generator with lags 48 and 8. It has period
 * (2^1487 - 2^247)/105 or about 10^445. In general, these generators are
 * excellent. However, they act locally like a lagged Fibonacci generator
 * and thus have troubles with the birthday test. Thus, we combine this SWB
 * generator with the linear congruential generator (48271*a)mod(2^31-1).
 *
 * Although the interface is fairly abstract, the implementation uses
 * 31-bit ML words. At some point, it might be good to use 32-bit words.
 *)

structure Random : RANDOM =
  struct
    structure A   = Array
    structure LW  = LargeWord
    structure W8A = Word8Array
    structure W8V = Word8Vector
    structure P   = PackWord32Big

    val << = Word.<<
    val >> = Word.>>
    val & = Word.andb
    val ++ = Word.orb
    val xorb = Word.xorb
    infix << >> & ++

    val nbits = 31                                      (* bits per word *)
    val maxWord : Word.word = 0wx7FFFFFFF               (* largest word *)
    val bit30 : Word.word   = 0wx40000000
    val lo30 : Word.word    = 0wx3FFFFFFF

    val N = 48
    val lag = 8
    val offset = N-lag

    fun error (f,msg) = raise Fail ("Random" ^ f ^ msg)

    val two2neg30 = 1.0/((real 0x8000)*(real 0x8000))   (* 2^~30 *)

    fun minus(x,y,false) = (x - y, y > x)
      | minus(x,y,true) = (x - y - 0w1, y >= x)

    datatype rand = RND of {
        vals   : Word.word A.array,(* seed array *)
        borrow : bool ref,           (* last borrow *)
        congx  : Word.word ref,    (* congruential seed *)
        index  : int ref             (* index of next available value in vals *)
      }

      (* We represent state as a string, starting with an initial
       * word acting as an magic cookie (with bit 0 determining the
       * value of borrow), followed by a word containing index and a word
       * containing congx, followed by the seed array.
       *)
    val numWords = 3 + N
    val magic : LW.word = 0wx72646e64
    fun toString (RND{vals, borrow, congx, index}) = let
          val arr = W8A.array (4*numWords, 0w0)
          val word0 = if !borrow then LW.orb (magic, 0w1) else magic
          fun fill (src,dst) =
                if src = N then ()
                else (
                  P.update (arr, dst, Word.toLargeWord (A.sub (vals, src)));
                  fill (src+1,dst+1)
                )
          in
            P.update (arr, 0, word0);
            P.update (arr, 1, LW.fromInt (!index));
            P.update (arr, 2, Word.toLargeWord (!congx));
            fill (0,3);
            Byte.bytesToString (W8A.vector arr)
          end

    fun fromString s = let
          val bytes = Byte.stringToBytes s
          val _ = if W8V.length bytes = 4 * numWords then ()
                  else error ("fromString","invalid state string")
          val word0 = P.subVec (bytes, 0)
          val _ = if LW.andb(word0, 0wxFFFFFFFE) = magic then ()
                  else error ("fromString","invalid state string")
          fun subVec i = P.subVec (bytes, i)
          val borrow = ref (LW.andb(word0,0w1) = 0w1)
          val index = ref (LW.toInt (subVec 1))
          val congx = ref (Word.fromLargeWord (subVec 2))
          val arr = A.array (N, 0w0 : Word.word)
          fun fill (src,dst) =
                if dst = N then ()
                else (
                  A.update (arr, dst, Word.fromLargeWord (subVec src));
                  fill (src+1,dst+1)
                )
          in
            fill (3, 0);
            RND{vals = arr,
                index = index,
                congx = congx,
                borrow = borrow}
          end

      (* linear congruential generator:
       * multiplication by 48271 mod (2^31 - 1)
       *)
    val a : Word.word = 0w48271
    val m : Word.word = 0w2147483647
    val q = m div a
    val r = m mod a
    fun lcg seed = let
          val left = a * (seed mod q)
          val right = r * (seed div q)
          in
            if left > right then left - right
            else (m - right) + left
          end

      (* Fill seed array using subtract-with-borrow generator:
       *  x[n] = x[n-lag] - x[n-N] - borrow
       * Sets index to 1 and returns 0th value.
       *)
    fun fill (RND{vals,index,congx,borrow}) = let
          fun update (ix,iy,b) = let
                val (z,b') = minus(A.sub(vals,ix), A.sub(vals,iy),b)
                in
                  A.update(vals,iy,z); b'
                end
          fun fillup (i,b) =
                if i = lag then b
                else fillup(i+1, update(i+offset,i,b))
          fun fillup' (i,b) =
                if i = N then b
                else fillup'(i+1, update(i-lag,i,b))
          in
            borrow := fillup' (lag, fillup (0,!borrow));
            index := 1;
            A.sub(vals,0)
          end

      (* Create initial seed array and state of generator.
       * Fills the seed array one bit at a time by taking the leading
       * bit of the xor of a shift register and a congruential sequence.
       * The congruential generator is (c*48271) mod (2^31 - 1).
       * The shift register generator is c(I + L18)(I + R13).
       * The same congruential generator continues to be used as a
       * mixing generator with the SWB generator.
       *)
    fun rand (congy, shrgx) = let
          fun mki (i,c,s) = let
                val c' = lcg c
                val s' = xorb(s, s << 0w18)
                val s'' = xorb(s', s' >> 0w13)
                val i' = (lo30 & (i >> 0w1)) ++ (bit30 & xorb(c',s''))
                in (i',c',s'') end
	  fun iterate (0, v) = v
	    | iterate (n, v) = iterate(n-1, mki v)
          fun mkseed (congx,shrgx) = iterate (nbits, (0w0,congx,shrgx))
          fun genseed (0,seeds,congx,_) = (seeds,congx)
            | genseed (n,seeds,congx,shrgx) = let
                val (seed,congx',shrgx') = mkseed (congx,shrgx)
                in genseed(n-1,seed::seeds,congx',shrgx') end
          val congx = ((Word.fromInt congy & maxWord) << 0w1)+0w1
          val (seeds,congx) = genseed(N,[],congx, Word.fromInt shrgx)
          in
            RND{vals = A.fromList seeds,
                index = ref 0,
                congx = ref congx,
                borrow = ref false}
          end

      (* Get next random number. The tweak function combines
       * the number from the SWB generator with a number from
       * the linear congruential generator.
       *)
    fun randWord (r as RND{vals, index,congx,...}) = let
         val idx = !index
         fun tweak i = let
               val c = lcg (!congx)
               in
                 congx := c;
                 xorb(i, c)
               end
         in
           if idx = N then tweak(fill r)
           else tweak(A.sub(vals,idx)) before index := idx+1
         end

    fun randInt state = Word.toIntX(randWord state)
    fun randNat state = Word.toIntX(randWord state & lo30)
    fun randReal state =
      (real(randNat state) + real(randNat state) * two2neg30) * two2neg30

    fun randRange (i,j) =
          if j < i
            then error ("randRange", "hi < lo")
            else let
              val R = two2neg30*real(j - i + 1)
              in
                fn s => i + trunc(R*real(randNat s))
              end handle _ => let
                val ri = real i
                val R = (real j)-ri+1.0
                in
                  fn s => trunc(ri + R*(randReal s))
                end

  end; (* Random *)

