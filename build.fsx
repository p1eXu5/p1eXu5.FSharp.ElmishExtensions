#r "nuget: Fake.Api.GitHub, 6.1.4"
#r "nuget: Fake.BuildServer.GitHubActions, 6.1.4"
#r "nuget: Fake.Core.Target, 6.1.4"
#r "nuget: Fake.Core.Vault, 6.1.4"
#r "nuget: Fake.Core.ReleaseNotes, 6.1.4"
#r "nuget: Fake.DotNet.Cli, 6.1.4"
#r "nuget: Fake.IO.FileSystem, 6.1.4"
#r "nuget: Fake.DotNet.NuGet, 6.1.4"
#r "nuget: Fake.Tools.Git, 6.1.4"
#r "nuget: Fake.Core.Xml, 6.1.4"
#r "nuget: MSBuild.StructuredLogger, 2.2.386" // MSBuild log version fix
#r "nuget: System.Formats.Asn1, 9.0.0" // vulnerabilities

open Fake.BuildServer
open Fake.Core
open Fake.Core.TargetOperators
open Fake.DotNet
open Fake.IO
open Fake.IO.Globbing.Operators

// -------------------
//   Bootstrap Fake
// -------------------
#if !FAKE
// To run script without fake.exe (no need multiple .NET sdk versions) - https://fake.build/guide/fake-debugging.html#Run-script-without-fake-exe-via-fsi
Fake.Core.Context.setExecutionContextFromCommandLineArgs __SOURCE_FILE__
#endif

do Environment.setEnvironVar "COREHOST_TRACE" "0"
// CoreTracing.ensureConsoleListener () // duplicates logs on ci

// https://fake.build/guide/buildserver.html
BuildServer.install [
    GitHubActions.Installer
]

// ------------------
//    Properties
// ------------------
module Project =

    let [<Literal>] fsprojPath = "./src/p1eXu5.FSharp.ElmishExtensions/p1eXu5.FSharp.ElmishExtensions.fsproj"
    let [<Literal>] releaseNotesPath = "./RELEASE_NOTES.md"
    let [<Literal>] nugetsFolderPath = "./nugets/"
    let nugetPath = "p1eXu5.FSharp.ElmishExtensions.*.nupkg" 

    let releaseNotes =
        lazy (
            ReleaseNotes.load releaseNotesPath
        )

// ---------------------
// Environment & Secrets
// ---------------------
module Secrets =
    let mutable secrets = []

    let vault = Vault.fromFakeEnvironmentVariable()

    let getFromVaultOrEnvOrDefault name defaultValue =
        match vault.TryGet name with
        | Some v -> v
        | None -> Environment.environVarOrDefault name defaultValue

    let releaseSecret replacement name =
        let secret =
            lazy
                let env =
                    match getFromVaultOrEnvOrDefault name "default_unset" with
                    | "default_unset" -> failwithf "variable '%s' is not set" name
                    | s -> s
                TraceSecrets.register replacement env
                env
        secrets <- secret :: secrets
        secret

    let [<Literal>] GITHUB_NUGET_SOURCE = "github"

    let githubReleaseUser = getFromVaultOrEnvOrDefault "GITHUB_ACTOR" "p1eXu5"
    let gitName = getFromVaultOrEnvOrDefault "REPOSITORY_NAME_GITHUB" "p1eXu5.FSharp.ElmishExtensions"

    let githubToken = releaseSecret "<githubtoken>" "GITHUB_TOKEN"
    let nugetOrgToken = releaseSecret "<githubtoken>" "PUBLISH_TO_NUGET_ORG"

module FakeVars =
    let [<Literal>] versionString = "VersionString"

// ------------------
//    Targets
// ------------------

Target.create "CheckReleaseSecrets" (fun p ->
    // Environment.environVars ()
    // |> Seq.sortBy fst
    // |> Seq.iter (fun (k, v) -> Trace.log (sprintf "%s: %s" k v))
    // 
    // Trace.log (sprintf "TargetParamer.Context.Arguments: %A" (p.Context.Arguments))

    for secret in Secrets.secrets do
        secret.Force() |> ignore
)

Target.create "GetVersion" (fun _ ->
    let versionString =
        Xml.read true Project.fsprojPath "" "" "//Version"
        |> Seq.head

    let releaseNotes = Project.releaseNotes.Value

    if versionString <> releaseNotes.AssemblyVersion then
        failwith (sprintf "Release notes for version %s has not been found" versionString)

    Trace.log $"Version string: {versionString}"
    // Store the version string in the context for later use
    FakeVar.set FakeVars.versionString versionString
)

Target.create "Clean" (fun _ ->
    !! "src/**/bin/Release"
    ++ "src/**/obj/Release"
    ++ "test/**/bin/Release"
    ++ "test/**/obj/Release"
    |> Shell.cleanDirs
)

// Restore target
Target.create "Restore" (fun _ ->
    !!("./**/*.*sproj")
    |> Seq.iter (DotNet.restore id)
)

// Build target
Target.create "Build" (fun _ ->
    DotNet.build (fun opts ->
        { opts with
            Configuration = DotNet.BuildConfiguration.Release
            NoRestore = true
        }) Project.fsprojPath
)

// Build target
Target.create "Build_Debug" (fun _ ->
    DotNet.build (fun opts ->
        { opts with
            Configuration = DotNet.BuildConfiguration.Debug
            NoRestore = true
        }) Project.fsprojPath
)

// Test target
// See https://fake.build/guide/buildserver.html#General-API-usage
Target.create "Test" (fun _ ->
    DotNet.test (fun opts ->
        { opts with
            Configuration = DotNet.BuildConfiguration.Release
            NoRestore = true
            NoBuild = false
        }) "."
)

// Publish target
Target.create "Pack" (fun _ ->
    let versionString = FakeVar.getOrFail FakeVars.versionString
    Trace.log $"Publishing version: {versionString}"

    let releaseNotes = Project.releaseNotes.Value

    DotNet.pack (fun opts ->
        { opts with
            Configuration = DotNet.BuildConfiguration.Release
            OutputPath = Some Project.nugetsFolderPath
            VersionSuffix = Some versionString
            NoRestore = true
            NoBuild = true
            NoLogo = true
            MSBuildParams =
                { opts.MSBuildParams with
                    Properties =
                        [
                            // Join notes with newline or literal \n for MSBuild
                            "PackageReleaseNotes", releaseNotes.Notes |> String.concat "\n"
                        ]
                }
        }) Project.fsprojPath
)

// dotnet nuget add source --username p1eXu5 --password ${{ secrets.GITHUB_TOKEN }} --store-password-in-clear-text --name github "https://nuget.pkg.github.com/p1eXu5/index.json"
Target.create "AddGithubNugetSource" (fun _ ->
    let result =
        DotNet.exec id "nuget" (
            sprintf
                "add source --username %s --password %s --store-password-in-clear-text --name %s https://nuget.pkg.github.com/p1eXu5/index.json"
                Secrets.githubReleaseUser
                Secrets.githubToken.Value
                Secrets.GITHUB_NUGET_SOURCE
        )
    
    if not result.OK then
        failwithf "dotnet nuget failed with errors: %A" result.Errors
)

// Publish on nuget.org target
Target.create "PublishOnNugetOrg" (fun _ ->
    DotNet.nugetPush (fun opts ->
        { opts with
            PushParams =
                { opts.PushParams with
                    ApiKey = Secrets.nugetOrgToken.Value |> Some
                    Source = "https://api.nuget.org/v3/index.json" |> Some
                }
            Common =
                { opts.Common with
                    WorkingDirectory = Project.nugetsFolderPath
                }
            
        }) Project.nugetPath
)

// Publish target
Target.create "PublishOnGithub" (fun _ ->
    DotNet.nugetPush (fun opts ->
        { opts with
            PushParams =
                { opts.PushParams with
                    ApiKey = Secrets.githubToken.Value |> Some
                    Source = Secrets.GITHUB_NUGET_SOURCE |> Some
                }
            Common =
                { opts.Common with
                    WorkingDirectory = Project.nugetsFolderPath
                }
            
        }) Project.nugetPath
)


Target.create "All" ignore

let isGitHubActions = Environment.hasEnvironVar "GITHUB_ACTIONS"

"CheckReleaseSecrets"
    ==> "GetVersion"
    ==> "Clean"
    ==> "Restore"
    ==> "Build"
    ==> "Test"
    ==> "Pack"
    ==> "PublishOnNugetOrg"
    ==> "All"

//"Pack"
//    =?> ("PublishOnGithub", isGitHubActions)

//"AddGithubNugetSource"
//    ==> "PublishOnGithub"

//"PublishOnGithub"
//    ?=> "All"

let ctx = Target.WithContext.runOrDefaultWithArguments "All"
Target.updateBuildStatus ctx
Target.raiseIfError ctx // important to have proper exit code on build failures.
