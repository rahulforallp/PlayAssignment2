package controllers

import play.api.data.Form
import play.api.data.Forms._
import play.api.mvc._
import scala.collection.mutable.ListBuffer

case class Employee(empName:String,address:String,dateOfBirth:String,dateOfJoining:String,designation:String)
case class SearchEmployee(empName:String)

class Application extends Controller {

  var listOfEmployee= ListBuffer(Employee("Rahul Kumar","NSEZ","4/8/1991","21/1/2016","Trainee Software Consultant"),Employee("Prabhat Kashyap","NSEZ","4/8/1991","21/1/2016","Trainee Software Consultant"),Employee("Aakash","NSEZ","4/8/1991","21/1/2016","Trainee Software Consultant"),Employee("Sahil","NSEZ","4/8/1991","21/1/2016","Trainee Software Consultant"),Employee("Kunal","NSEZ","4/8/1991","21/1/2016","Trainee Software Consultant"),Employee("Sahil","NSEZ","4/8/1989","21/1/2016","Trainee Software Consultant"),Employee("Kunal","NSEZ","4/8/1990","21/1/2016","Trainee Software Consultant")).sortBy(_.empName)

  val search:Form[SearchEmployee]=Form(
    mapping(
      "empName"-> nonEmptyText
    )(SearchEmployee.apply)(SearchEmployee.unapply)
  )

  val add:Form[Employee]=Form(
    mapping(
      "empName"-> nonEmptyText,
      "address"->nonEmptyText,
      "dateOfBirth"-> nonEmptyText,
      "dateOfJoining"-> nonEmptyText,
      "designation"-> nonEmptyText
    )(Employee.apply)(Employee.unapply)
  )

  def indexController = Action {
    Ok(views.html.index("Your new application is ready."))
  }

  def listAllEmployeeController = Action {
    Ok(views.html.dashboard(listOfEmployee.toList))
  }

  def searchEmployee(empName:String):ListBuffer[Employee]={
    listOfEmployee.filter(_.empName==empName)
  }

  def searchEmployeesController=Action{ implicit request =>
    search.bindFromRequest().fold(
      formWithErrors => {
      Redirect("/")
      },
      success => {
        val searchedEmployee=searchEmployee(success.empName)
        Ok(views.html.dashboard(searchedEmployee.toList))
      })
  }

  def showAddEmployeeController=Action{
    Ok(views.html.addEmployee("Add Employee"))
  }

  def addEmployeeController=Action{implicit request =>
    add.bindFromRequest().fold(
      formWithErrors => {
        Redirect("/")
      },
      success => {
        val df = new java.text.SimpleDateFormat("dd/MM/yyyy")
        if(!success.dateOfBirth.equals(df.format(df.parse(success.dateOfBirth))))
          {
            Redirect("/addEmployee")
          }

        if(!success.dateOfJoining.equals(df.format(df.parse(success.dateOfJoining))))
        {
          Redirect("/addEmployee")
        }

        listOfEmployee.+=(Employee(success.empName,success.address,success.dateOfBirth,success.dateOfJoining,success.designation))
        Ok(views.html.dashboard(listOfEmployee.toList.sortBy(_.empName))).flashing("message"->"Employee added sucessfully")
      })
  }

 def showUpdateEmployeeController(empName:String) = Action{implicit request =>
       Ok(views.html.updateEmployee(searchEmployee(empName).toList.head))
  }

  def deleteEmployee(emp:Employee)=Action{
    listOfEmployee -= listOfEmployee.filter(_==emp).head
    Ok(views.html.dashboard(listOfEmployee.toList))
  }

  def updateEmployeeController=Action{implicit request =>
    search.bindFromRequest().fold(
      formWithErrors => {
        Redirect("/")
      },
      success => {
        Redirect(routes.Application.listAllEmployeeController)
      })
  }




}
