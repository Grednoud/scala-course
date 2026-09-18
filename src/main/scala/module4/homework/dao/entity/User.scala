package module4.homework.dao.entity

case class UserId(value: String) extends AnyVal

case class User(id: String, firstName: String, lastName: String, age: Int) {
  def typedId: UserId = UserId(id)
}

case class RoleCode(value: String) extends AnyVal

case class Role(code: String, name: String) {
  def typedCode: RoleCode = RoleCode(code)
}

case class UserToRole(userId: String, roleCode: String)
