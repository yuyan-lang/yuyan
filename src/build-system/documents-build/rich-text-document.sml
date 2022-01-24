structure RichTextDocument =
struct
    datatype textColor = Black | Blue | Green | Red
    datatype textFormatting = Regular | Header
    datatype richTextSegment = RichTextSegment of textColor * textFormatting * UTF8String.t
                             | Indent
                             | Newline

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

    fun outputToHTML (doc : richTextDocument) : string = 
        let fun outputSegmentToHTML (x : richTextSegment) = 
            case x of RichTextSegment (c, f, s)  => "<span style=\"color: " ^ 
            getColorHTML c ^"\">" ^ UTF8String.toString s ^ "</span>"
                | Indent => "<span style=\"display: inline-block; width: 2ch;\"></span>"
                | Newline => "<br>"
            val htmlBody = String.concatWith "" (map outputSegmentToHTML doc)
        in htmlHeader() ^ htmlBody ^ htmlFooter()
        end

end