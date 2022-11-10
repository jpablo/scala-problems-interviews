package systemDesign
import scala.collection.mutable

import scala.language.unsafeNulls

type LogId = Int
type ServiceId = Int
// type MachineId = Int

class LogAggregator(_machines: Int, _services: Int) {

  val logs: mutable.Map[LogId, String] = mutable.Map.empty
  val byMachine: mutable.Map[MachineId, List[LogId]] = mutable.Map.empty.withDefault(_ => List.empty)
  val byService: mutable.Map[ServiceId, List[LogId]] = mutable.Map.empty.withDefault(_ => List.empty)


  def pushLog(logId: LogId, machineId: MachineId, serviceId: ServiceId, message: String): Unit = {
    logs += (logId -> message)
    byMachine(machineId) = logId :: byMachine(machineId)
    byService(serviceId) = logId :: byService(serviceId)
      
  }

  def getLogsFromMachine(machineId: MachineId): List[Int] =
    byMachine(machineId).reverse

  def getLogsOfService(serviceId: ServiceId): List[Int] =
    byService(serviceId).reverse

  def search(serviceId: ServiceId, searchString: String): List[String] =
    byService(serviceId).map(logs).filter(_.contains(searchString)).reverse

}

/**
 * Your LogAggregator object will be instantiated and called as such:
 * var obj = new LogAggregator(machines, services)
 * obj.pushLog(logId,machineId,serviceId,message)
 * var param_2 = obj.getLogsFromMachine(machineId)
 * var param_3 = obj.getLogsOfService(serviceId)
 * var param_4 = obj.search(serviceId,searchString)
 */