# Scala Developer Course / Курс разработчика Scala

[English](#english) | [Русский](#russian)

---

## English

An educational Scala course covering functional programming concepts, ZIO ecosystem, and modern Scala development practices.

### Course Structure

- **module1** - Scala basics: variables, functions, ADTs, domain modeling, subtyping
- **module2** - Higher-kinded types, implicits, type classes  
- **module3** - ZIO fundamentals: functional effects, error handling, concurrency, resources, dependency injection
- **module4** - Building real applications: HTTP servers (http4s), database access (Quill), configuration (zio-config)
- **monad** - Monad laws and for-comprehensions
- **collections** - Scala collections exercises
- **futures** - Scala Futures

### Technology Stack

- **Scala 2.13.18**
- **sbt 1.13.0**
- **ZIO 2.1.26** - Effect system
- **http4s 0.23.37** (Ember) - HTTP server/client
- **Quill 4.8.5** - Compile-time SQL queries
- **Circe 0.14.16** - JSON processing
- **zio-config 4.0.7** - Configuration
- **zio-interop-cats 23.1.0.13** - Cats Effect interop
- **testcontainers-scala 0.44.1** - Docker-based testing

### Prerequisites

- JDK 11 or higher
- sbt (will be auto-downloaded via wrapper)
- Docker (for integration tests with testcontainers)

### Getting Started

```bash
# Clone the repository
git clone https://github.com/Grednoud/scala-course.git
cd scala-course

# Compile the project
sbt compile

# Run tests
sbt test

# Run the phone book application (requires database configuration)
sbt run
```

### Running Tests

```bash
# Run all tests (unit tests will always run)
sbt test

# Integration tests require Docker
# Set DOCKER_AVAILABLE=true to enable testcontainers tests
DOCKER_AVAILABLE=true sbt test
```

### Project Configuration

For the phone book application, configure the database in `src/main/resources/application.conf`:

```hocon
api {
  host = "0.0.0.0"
  port = 8080
}

liquibase {
  changeLog = "src/main/resources/liquibase/main.xml"
}

db {
  dataSourceClassName = "org.postgresql.ds.PGSimpleDataSource"
  dataSource.url = "jdbc:postgresql://localhost:5432/phonebook"
  dataSource.user = "postgres"
  dataSource.password = "postgres"
}
```

---

## Russian

Образовательный курс по Scala, охватывающий концепции функционального программирования, экосистему ZIO и современные практики разработки на Scala.

### Структура курса

- **module1** - Основы Scala: переменные, функции, ADT, моделирование предметной области, подтипирование
- **module2** - Higher-kinded типы, implicits, type classes
- **module3** - Основы ZIO: функциональные эффекты, обработка ошибок, конкурентность, ресурсы, внедрение зависимостей
- **module4** - Создание реальных приложений: HTTP-серверы (http4s), работа с БД (Quill), конфигурация (zio-config)
- **monad** - Законы монад и for-comprehensions
- **collections** - Упражнения с коллекциями Scala
- **futures** - Scala Futures

### Технологический стек

- **Scala 2.13.18**
- **sbt 1.13.0**
- **ZIO 2.1.26** - Система эффектов
- **http4s 0.23.37** (Ember) - HTTP сервер/клиент
- **Quill 4.8.5** - Компилируемые SQL-запросы
- **Circe 0.14.16** - Работа с JSON
- **zio-config 4.0.7** - Конфигурация
- **zio-interop-cats 23.1.0.13** - Интероп с Cats Effect
- **testcontainers-scala 0.44.1** - Тестирование с Docker

### Требования

- JDK 11 или выше
- sbt (будет загружен автоматически)
- Docker (для интеграционных тестов с testcontainers)

### Начало работы

```bash
# Клонирование репозитория
git clone https://github.com/Grednoud/scala-course.git
cd scala-course

# Компиляция проекта
sbt compile

# Запуск тестов
sbt test

# Запуск приложения телефонной книги (требуется настройка БД)
sbt run
```

### Запуск тестов

```bash
# Запуск всех тестов (юнит-тесты запустятся всегда)
sbt test

# Интеграционные тесты требуют Docker
# Установите DOCKER_AVAILABLE=true для включения тестов testcontainers
DOCKER_AVAILABLE=true sbt test
```

### Конфигурация проекта

Для приложения телефонной книги настройте базу данных в `src/main/resources/application.conf`:

```hocon
api {
  host = "0.0.0.0"
  port = 8080
}

liquibase {
  changeLog = "src/main/resources/liquibase/main.xml"
}

db {
  dataSourceClassName = "org.postgresql.ds.PGSimpleDataSource"
  dataSource.url = "jdbc:postgresql://localhost:5432/phonebook"
  dataSource.user = "postgres"
  dataSource.password = "postgres"
}
```

### Ключевые изменения в ZIO 2

При обновлении курса с ZIO 1 на ZIO 2 были внесены следующие изменения:

1. **Has[A] удалён** - сервисы указываются напрямую в R-типе
2. **ZManaged заменён на Scope** - `ZIO.acquireRelease` + `ZIO.scoped`
3. **Стандартные сервисы** - `Console`, `Clock`, `Random` доступны через companion-объекты
4. **ZIO.effect → ZIO.attempt** - переименование конструкторов
5. **App → ZIOAppDefault** - новый базовый класс для приложений
6. **zio-magic удалён** - автоматическое разрешение слоёв встроено в ZIO 2

---

## License

This project is for educational purposes.
