structure RichTextDocument =
struct
    datatype textColor = Black | Blue | Green | Red
    datatype textFormatting = Regular | Header
    datatype richTextSegment = RichTextSegment of textColor * textFormatting * UTF8String.t
                             | Indent
                             | Newline
                             | HyperLink of UTF8String.t (* content of the link *)  * string (* target of the link *)

    type richTextDocument = richTextSegment list

    type t = richTextDocument


    fun indentN(n : int) = List.tabulate(n, (fn _ => Indent))

    fun htmlHeader () : string = 
    "<html><head><meta charset=\"utf8\"></head><body>"

    fun htmlFooter () : string = 
    "</body></html>"

    fun getColorHTML (c : textColor) = case c of 
        Black => "Black"
        | Red => "FireBrick"
        | Blue => "DarkBlue"
        | Green => "DarkGreen"

    fun getHTMLElement (f : textFormatting) = case f of
        Regular => "span"
        | Header => "h1"

    fun outputToHTML (doc : richTextDocument) : string = 
        let fun outputSegmentToHTML (x : richTextSegment) = 
            case x of RichTextSegment (c, f, s)  => "<" ^ getHTMLElement f^" style=\"color: " ^ 
            getColorHTML c ^"\">" ^ UTF8String.toString s ^ "</" ^ getHTMLElement f ^">"
                | Indent => "<span style=\"display: inline-block; width: 2ch;\"></span>"
                | Newline => "<br>"
                | HyperLink(content, target) => "<a href=\""^ target ^"\">" ^ UTF8String.toString content ^ "</a>"
            val htmlBody = String.concatWith "" (map outputSegmentToHTML doc)
        in htmlHeader() ^ htmlBody ^ htmlFooter()
        end

end