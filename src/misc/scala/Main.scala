package misc.scala

import scala.io.Source
import scala.collection.immutable.Nil

object Main {
  
	def parse(s:String):Array[Float] = (s.split(",").map(x=>x.toFloat))
	
	def distance(f0:Array[Float], f1:Array[Float]):Float = {
		Math.sqrt((f0, f1).zipped.map((x,y) => (x - y) * (x - y)).foldLeft(0f)(_ + _).toDouble).toFloat
	}
	
	def find_class(item:Array[Float], data:List[Array[Float]]):Float = {
		var min = Double.MaxValue;
		var klass = 0f;
		for(comp <- data){
			if(item != comp){ // is not myself
				val d = distance(item.tail, comp.tail);
				if(d < min){
					min = d;
					klass = comp(0);
				}
			}
		}
		return klass;
	}
	
	def parseAll(lines:Iterator[String]):List[Array[Float]] = {
		var list = List.empty[Array[Float]];
		for(line <- lines){
			list = parse(line)::list
		}
		return list
	}
	
	def main(args:Array[String]) = {
		val source = Source.fromFile(args(0));
		val lines = source.getLines;
		val list = parseAll(lines)
		var cnt = 0
		for(item <- list){
			val result = find_class(item, list);
			if(item(0) == result){
			  cnt = cnt + 1;
			}
			printf("%b %s <-> %s\n", (item(0) == result), item(0), result);
		}
		printf("match rate: %f\n", cnt.toFloat / list.length.toFloat);
		source.close;
	}
	
}

