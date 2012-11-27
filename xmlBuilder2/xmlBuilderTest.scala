object xmlBuilderTest extends App {
    import xmlbuilder.XmlBuilder._
    
    val searchLinks = List(
        "Google" -> "http://www.google.com",
        "Bing" -> "http://www.bing.com",
        "Ask.com" -> "http://www.ask.com",
        "Yahoo!" -> "http://www.yahoo.com",
        "Alta Vista" -> "http://www.altavista.com",
        "Excite" -> "http://www.excite.com",
        "HotBot" -> "http://www.hotbot.com")

    val pageStyle = """
#searchList {
    padding: 15px 25px;
    border: solid 2px black;   
    width: 300px;
}

#searchList li {
    margin: 3px;
    list-style: none;
}

p.title {
    margin: 0px;
    padding: 30px;
    font-size: 3em;
}
"""

    println(xml {
        'koko {
            'okok
        }
    })
    
}