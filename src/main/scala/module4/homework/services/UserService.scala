package module4.homework.services

import module4.homework.dao.entity.{Role, RoleCode, User}
import module4.homework.dao.repository.UserRepository
import module4.phoneBook.db
import zio.macros.accessible
import zio.{Has, RIO, ZIO, ZLayer}

@accessible
object UserService {
  type UserService = Has[Service]

  trait Service {
    def listUsers(): RIO[db.DataSource, List[User]]

    def listUsersDTO(): RIO[db.DataSource, List[UserDTO]]

    def addUserWithRole(user: User, roleCode: RoleCode): RIO[db.DataSource, UserDTO]

    def listUsersWithRole(roleCode: RoleCode): RIO[db.DataSource, List[UserDTO]]
  }

  private case class Impl(userRepo: UserRepository.Service) extends Service {
    private val dc = db.Ctx

    def listUsers(): RIO[db.DataSource, List[User]] = userRepo.list()


    def listUsersDTO(): RIO[db.DataSource, List[UserDTO]] = for {
      users <- listUsers()
      result <- ZIO.foreach(users) { user =>
        userRepo.userRoles(user.typedId).map(x => UserDTO(user, x.toSet))
      }
    } yield result

    def addUserWithRole(user: User, roleCode: RoleCode): RIO[db.DataSource, UserDTO] = dc.transaction(
      for {
        user <- userRepo.createUser(user)
        _ <- userRepo.insertRoleToUser(roleCode, user.typedId)
        roles <- userRepo.userRoles(user.typedId)
      } yield UserDTO(user, roles.toSet))

    def listUsersWithRole(roleCode: RoleCode): RIO[db.DataSource, List[UserDTO]] = for {
      users <- userRepo.listUsersWithRole(roleCode)
      result <- ZIO.foreach(users) { user =>
        userRepo.userRoles(user.typedId).map(x => UserDTO(user, x.toSet))
      }
    } yield result


  }

  val live: ZLayer[UserRepository.UserRepository, Nothing, UserService] = ZLayer.fromService(Impl.apply)
}

case class UserDTO(user: User, roles: Set[Role])