package toolkitUtils

import com.github.jknack.handlebars.{Handlebars, Template, Helper, Options}
import com.github.jknack.handlebars.helper.ConditionalHelpers
import scala.jdk.CollectionConverters._

object HandlebarsRenderer:

  private val handlebars = new Handlebars()

  // Recursively converts Scala Maps, Seqs, and Options into Java-compatible data structures for Handlebars.java
  private def convertToJava(obj: Any): Any = obj match
    case map: Map[_, _] => map.map { case (k, v) => k.toString -> convertToJava(v) }.asJava
    case list: List[_]   => list.map(convertToJava).asJava
    case seq: Seq[_]    => seq.map(convertToJava).asJava
    case set: Set[_]     => set.map(convertToJava).asJava
    case opt: Option[_] => opt.map(convertToJava).orNull
    case other          => other

  // Register conditional helpers (provides 'eq', 'ne', etc.)
  handlebars.registerHelpers(classOf[ConditionalHelpers])

  // Compiles a Handlebars template string and renders it with a Scala Map context 
  def render(templateContent: String, context: Map[String, Any]): String =
    // Compile the template
    val compiledTemplate: Template = handlebars.compileInline(templateContent)
    
    // Convert Scala Map -> Java Map
    val javaContext = convertToJava(context)
    
    // Render
    compiledTemplate.apply(javaContext)