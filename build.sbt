name := "BreathOfTheEmpire"

val scala3Version = "3.6.4"
ThisBuild / scalaVersion := scala3Version

// Used for scala fix
ThisBuild / semanticdbEnabled := true
ThisBuild / semanticdbVersion := scalafixSemanticdb.revision

ThisBuild / scalafixOnCompile := true
ThisBuild / scalafmtOnCompile := true

enablePlugins(JavaAppPackaging, DockerPlugin, AshScriptPlugin)

ThisBuild / publish / skip                      := true
ThisBuild / githubWorkflowJavaVersions          := Seq(JavaSpec.temurin("17"))
ThisBuild / crossScalaVersions                  := List(scala3Version)
ThisBuild / githubWorkflowIncludeClean          := false
ThisBuild / githubWorkflowTargetBranches        := Seq("master")
ThisBuild / githubWorkflowTargetPaths           := Paths.Include(List("**version.sbt"))
ThisBuild / githubWorkflowPublishTargetBranches := Seq(RefPredicate.Equals(Ref.Branch("master")))

ThisBuild / githubWorkflowPublishPreamble := Seq(
  WorkflowStep.Use(
    name = Some("Login to DockerHub"),
    ref = UseRef.Public("docker", "login-action", "v2"),
    params = Map(
      "username" -> "${{ secrets.DOCKERHUB_USERNAME }}",
      "password" -> "${{ secrets.DOCKERHUB_PASS }}",
    ),
  ),
)

ThisBuild / githubWorkflowPublish := Seq(
  WorkflowStep.Sbt(
    List("Docker / publish"),
    name = Some("Publish to docker hub"),
  ),
)

ThisBuild / githubWorkflowJobSetup ++= Seq(
  WorkflowStep.Sbt(
    List("+scalafmtCheckAll", "scalafmtSbtCheck"),
    name = Some("Scalafmt"),
  ),
)
Universal / javaOptions            ++= Seq(
  "-Dconfig.file=/opt/docker/conf/application.conf",
)

Docker / dockerRepository := Some("amuxix")
dockerUpdateLatest        := true
dockerBaseImage           := "openjdk:17-jdk"
publish / skip            := false
dockerBuildOptions        += "--platform=linux/amd64"

javacOptions  ++= Seq("-Xlint", "-encoding", "UTF-8")
scalacOptions ++= Seq(
  "-explain",              // Explain errors in more detail.
  "-explain-types",        // Explain type errors in more detail.
  "-indent",               // Allow significant indentation.
  "-new-syntax",           // Require `then` and `do` in control expressions.
  "-feature",              // Emit warning and location for usages of features that should be imported explicitly.
  "-source:future",        // better-monadic-for
  "-language:higherKinds", // Allow higher-kinded types
  "-deprecation",          // Emit warning and location for usages of deprecated APIs.
  "-Wunused:all",          // Emit warnings for unused imports, local definitions, explicit parameters implicit,
  // parameters method, parameters
  "-Xcheck-macros",
)

libraryDependencies ++= Seq(
  // Effect system
  catsEffect,
  catsRetry,
  fs2,
  fs2IO,
  // Discord
  jda,
  // Logging
  log4cats,
  logbackClassic,
  // Config
  pureconfig,
  pureconfigCE,
  // DB
  postgres,
  flyway,
  flywayPostgres,
  doobie,
  doobiePostgres,
  // HTTP
  http4s,
  http4sDSL,
  http4sCirce,
  http4sEmberClient,
  http4sXml,
  // Json
  circe,
  circeParser,
)

lazy val catsEffect        = "org.typelevel"         %% "cats-effect"                % "3.6.1"
lazy val circe             = "io.circe"              %% "circe-core"                 % "0.14.12"
lazy val circeParser       = circe.organization      %% "circe-parser"               % circe.revision
lazy val catsRetry         = "com.github.cb372"      %% "cats-retry"                 % "4.0.0"
lazy val fs2               = "co.fs2"                %% "fs2-core"                   % "3.12.0"
lazy val fs2IO             = fs2.organization        %% "fs2-io"                     % fs2.revision
lazy val jda               = "net.dv8tion"            % "JDA"                        % "5.3.2"
lazy val log4cats          = "org.typelevel"         %% "log4cats-slf4j"             % "2.7.0"
lazy val logbackClassic    = "ch.qos.logback"         % "logback-classic"            % "1.5.18"
lazy val pureconfig        = "com.github.pureconfig" %% "pureconfig-core"            % "0.17.8"
lazy val pureconfigCE      = pureconfig.organization %% "pureconfig-cats-effect"     % pureconfig.revision
lazy val postgres          = "org.postgresql"         % "postgresql"                 % "42.7.5"
lazy val flyway            = "org.flywaydb"           % "flyway-core"                % "11.7.0"
lazy val flywayPostgres    = flyway.organization      % "flyway-database-postgresql" % flyway.revision
lazy val doobie            = "org.tpolecat"          %% "doobie-core"                % "1.0.0-RC8"
lazy val doobiePostgres    = doobie.organization     %% "doobie-postgres"            % doobie.revision
lazy val http4s            = "org.http4s"            %% "http4s-core"                % "0.23.30"
lazy val http4sDSL         = http4s.organization     %% "http4s-dsl"                 % http4s.revision
lazy val http4sCirce       = http4s.organization     %% "http4s-circe"               % http4s.revision
lazy val http4sEmberClient = http4s.organization     %% "http4s-ember-client"        % http4s.revision
lazy val http4sXml         = http4s.organization     %% "http4s-scala-xml"           % "0.23.14"
