package systemDesign


class MonitoringSystem() {

  type AppName    = String
  type Api        = String
  type Latency    = Int
  type ErrorCode  = Int
  type Count      = Int

  import collection.mutable.Map
  import collection.mutable.PriorityQueue

  var latencies: Map[(AppName, Api), PriorityQueue[Latency]] = Map.empty.withDefaultValue(PriorityQueue.empty(Ordering.Int.reverse))

  var errors   : Map[(AppName, Api), Map[ErrorCode, Count]] = Map.empty

  def logLatency(applicationName: AppName, api: Api, latencyInMills: Latency) = {
    val pq = latencies(applicationName, api)
    pq += latencyInMills
    latencies((applicationName, api)) = pq
  }

  def logError(applicationName: AppName, api: Api, errorCode: ErrorCode): Unit = {
    val count: Map[ErrorCode, Count] = errors.getOrElse((applicationName, api), Map.empty)
    count(errorCode) = count.getOrElse(errorCode, 0) + 1
    errors((applicationName, api)) = count
  }

  def getPercentileLatency(percentile: Int, applicationName: AppName, api: Api): Int = {
    val sortedLatencies = latencies(applicationName, api).clone().dequeueAll
    val index = percentileIndex(percentile, sortedLatencies)
    if (index >= 0 && index < sortedLatencies.size)
      sortedLatencies(index)
    else {
      val s2 = latencies.filter(_._1._1 == applicationName).values.flatten.toList.sorted.reverse
      s2(percentileIndex(percentile, s2))
    }
  }

  private def percentileIndex(p: Int, sorted: Seq[Int]): Int =
    (sorted.size * p / 100) - 1

  def getTopErrors(applicationName: AppName, api: Api): Array[Int] = {
    val r = errors(applicationName, api).toArray.sortBy(-_._2).takeRight(3).map(_._1).sorted
    if (r.length < 3) 
      errors
      .filter { case ((app, api), _) => app == applicationName }
      .map { case (_, count) => count }
      .flatten .toArray .sortBy(-_._2) .take(3) .map(_._1).sorted
    else
      r
  }

}

@main def mainMS =
  import zio.json.*
  import zio.json.ast.Json

  val o = Ordering.Int.reverse

  enum Instructions:
    case MonitoringSystem, logLatency, logError, getPercentileLatency, getTopErrors

  object Instructions:
      given JsonDecoder[Instructions] = JsonDecoder[String].map(Instructions.valueOf)
    

  type T1 = (String, String, Int)
  type T2 = (Int, String, String)
  type T3 = (String, String)

  enum In:
    case In1(value: T1)
    case In2(value: T2)
    case In3(value: T3)

  import In.*

  given e1: JsonDecoder[In1] = JsonDecoder[Json].mapOrFail(_.as[T1].map(In1.apply))
  given e2: JsonDecoder[In2] = JsonDecoder[Json].mapOrFail(_.as[T2].map(In2.apply))
  given e3: JsonDecoder[In3] = JsonDecoder[Json].mapOrFail(_.as[T3].map(In3.apply))

  given JsonDecoder[In] = JsonDecoder[Json].mapOrFail { j => 
    j.as[T1].map(In1.apply)
      .orElse(j.as[T2].map(In2.apply))
      .orElse(j.as[T3].map(In3.apply))
  }
  
  given JsonEncoder[Unit] = JsonEncoder[Json].contramap(_ => Json.Null)

 


  val input = """["logLatency","logLatency","logLatency","logLatency","logLatency","logLatency","logLatency","logLatency","logLatency","logLatency","logError","logError","logError","logError","logError","logError","getPercentileLatency","getPercentileLatency","logError","logError","logError","getTopErrors","getTopErrors"]"""
  .fromJson[List[Instructions]].right.get

  val data = """[["service","getuser",100],["service","getuser",200],["service","getuser",300],["service","getuser",400],["service","getuser",500],["service","getuser",600],["service","getuser",700],["service","getuser",800],["service","getuser",900],["service","getuser",1000],["service","getuser",500],["service","getuser",500],["service","getuser",404],["service","getuser",500],["service","getuser",503],["service","getuser",404],[95,"service","getuser"],[95,"service","nouser"],["service","updateuser",401],["service","updateuser",401],["service","updateuser",401],["service","getuser"],["service","updateuser"]]"""
  .fromJson[List[In]].right.get
  val expected = """[null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,900,900,null,null,null,[500,404,503],[401,500,404]]"""
    .fromJson[List[Json]].right.get

  

  val monitoringSystem = MonitoringSystem()

  input.zip(data).zip(expected).foreach { case ((i, d), e) =>
    val out = i match
      case Instructions.MonitoringSystem     => ().toJson
      case Instructions.logLatency           => { val In1(a, b, c) = d.asInstanceOf[In1];  monitoringSystem.logLatency(a, b, c).toJson }
      case Instructions.logError             => { val In1(a, b, c) = d.asInstanceOf[In1];  monitoringSystem.logError(a, b, c).toJson }
      case Instructions.getPercentileLatency => { val In2(a, b, c) = d.asInstanceOf[In2];  monitoringSystem.getPercentileLatency(a, b, c).toJson }
      case Instructions.getTopErrors         => { val In3(a, b) = d.asInstanceOf[In3];     monitoringSystem.getTopErrors(a, b).toJson }
    
    
    println(s"---> input: $i, $d, output: $out, expected: $e")
    println((monitoringSystem.errors, monitoringSystem.latencies))

    assert(e.toString == out)
  }

