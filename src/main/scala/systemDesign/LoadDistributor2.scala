package systemDesign

import collection.mutable.SeqMap
import scala.language.unsafeNulls

class DCLoadBalancer() {

  type Capacity = Int
  type MachineId = Int
  type AppId = Int

  object MachineId {
    val missing = -1
  }

  case class RunningApp(ladUse: Capacity, machineId: MachineId)

  val machineCapacities = SeqMap[MachineId, Capacity]()
  val appLocation = SeqMap[AppId, RunningApp]()

  def addMachine(machineId: MachineId, capacity: Capacity): Unit =
    machineCapacities(machineId) = capacity


  def removeMachine(machineId: MachineId): Unit = {
    machineCapacities.get(machineId) match {
      case None => ()
      case Some(machineCapacity) =>
        val runnigApps = getRunningApps(machineId)
        machineCapacities -= machineId
        appLocation --= runnigApps.keys
        for { (appId, RunningApp(loadUse, _)) <- runnigApps } {
          addApplication(appId, loadUse)
        }
    }
  }.ensuring(machineCapacities.get(machineId).isEmpty)



  def addApplication(appId: AppId, loadUse: Capacity): MachineId = {
    findAllocatableMachine(loadUse) match {
      case None => MachineId.missing
      case Some((machineId, remaining)) =>
        machineCapacities(machineId) = remaining
        appLocation(appId) = RunningApp(loadUse, machineId)
        machineId
    }
  }

  def stopApplication(appId: AppId): Unit = {
    appLocation.get(appId) match {
      case None => ()
      case Some(RunningApp(loadUse, machineId)) =>
        appLocation -= appId
        machineCapacities(machineId) += loadUse
    }
  }

  def getApplications(machineId: MachineId): List[AppId] =
    getRunningApps(machineId).keys.toList


    /**
      * @return An allocatable machine and the remaining capacity
      */
  private def findAllocatableMachine(loadUse: Capacity): Option[(MachineId, Capacity)] = 
    machineCapacities.toList
    .filter { case (_, cap) => cap >= loadUse }
    .minByOption { case (id, cap) => (-cap, id) }
    .map { case (id, cap) => (id, cap - loadUse) }

  private def getRunningApps(machineId: MachineId): SeqMap[AppId, RunningApp] =
    appLocation.filter { case (_, app) => 
      app.machineId == machineId 
    }
}





@main def mainLoadDistributor =
  import math.BigDecimal.javaBigDecimal2bigDecimal
  import zio.json.*
  import zio.json.ast.Json
  import zio.json.ast.Json.Obj
  import zio.json.ast.Json.Arr
  import zio.json.ast.Json.Bool
  import zio.json.ast.Json.Str
  import zio.json.ast.Json.Num

  type Capacity = Int
  type MachineId = Int
  type AppId = Int


  enum Instructions:
    case DCLoadBalancer, addMachine, addApplication, getApplications, stopApplication, removeMachine

  object Instructions:
    given JsonDecoder[Instructions] = JsonDecoder[String].map(Instructions.valueOf)

  enum Output:
    case Null
    case Number(value: Int)
    case Array(values: List[Int])

  object Output:
    given JsonDecoder[Output] = JsonDecoder[Json].map { s => 
      s match
        case Arr(elements) => Output.Array(elements.map(e => e.asInstanceOf[Num].value.toInt).toList)
        case Num(value) => Output.Number(value.toInt)
        case Json.Null => Output.Null
        case _ => ???
    }

  val input = """["DCLoadBalancer", "addMachine", "addMachine", "addMachine", "addMachine", "addApplication", "addApplication", "addApplication", "addApplication", "getApplications", "addMachine", "addApplication", "stopApplication", "addApplication", "getApplications", "removeMachine", "getApplications"]"""
    .fromJson[List[Instructions]].right.get

  val data = """[[], [1, 1], [2, 10], [3, 10], [4, 15], [1, 3], [2, 11], [3, 6], [4, 5], [2], [5, 10], [5, 5], [3], [6, 5], [4], [4], [2]]"""
    .fromJson[List[List[Int]]].right.get

  val expected = """[null, null, null, null, null, 4, 4, 2, 3, [3], null, 5, null, 2, [1, 2], null, [6, 1]]"""
    .fromJson[List[Output]].right.get

  var loadBalancer: DCLoadBalancer = null

  def toOut (r : Unit | Int | List[AppId]): Output = r match {
    case () => Output.Null
    case machineId: Int => Output.Number(machineId)
    case list: List[AppId] => Output.Array(list)
  }

  input.zip(data).zip(expected).foreach { case ((i, d), e) =>
    val out = i match
      case Instructions.DCLoadBalancer  => toOut { loadBalancer = DCLoadBalancer() }
      case Instructions.addMachine      => toOut(loadBalancer.addMachine((d(0)), (d(1))))
      case Instructions.addApplication  => toOut(loadBalancer.addApplication((d(0)), (d(1))))
      case Instructions.getApplications => toOut(loadBalancer.getApplications((d(0))))
      case Instructions.stopApplication => toOut(loadBalancer.stopApplication((d(0))))
      case Instructions.removeMachine   => toOut(loadBalancer.removeMachine((d(0))))
    
    // println(s"---> input: $i, ${d.toJson}, output: $out, expected: $e")
    // if loadBalancer != null then
    //   println((loadBalancer.machineCapacities, loadBalancer.appLocation))

    assert(e == out)
  }
  println("OK")
  