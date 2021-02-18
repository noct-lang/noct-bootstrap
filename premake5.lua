-- premake5.lua

-- Helpers

function os.winSdkVersion()
    local reg_arch = iif( os.is64bit(), "\\Wow6432Node\\", "\\" )
    local sdk_version = os.getWindowsRegistry( "HKLM:SOFTWARE" .. reg_arch .."Microsoft\\Microsoft SDKs\\Windows\\v10.0\\ProductVersion" )
    if sdk_version ~= nil then return sdk_version end
end

-- Premake

workspace "Noctis"
    configurations { "Debug", "OptDebug", "Release" }
    location "build" -- Generate files in build folder
    cppdialect "C++17" -- enable C++17 features
    architecture "x86_64"
    kind "ConsoleApp"
    language "C++"
    targetdir "build/bin/"
    includedirs { "src" }
    flags { "FatalWarnings", "MultiProcessorCompile" }

filter "Configurations:Debug"
    defines { "_DEBUG" }
    symbols "On"
    targetdir "build/bin/debug"

filter "Configurations:OptDebug"
    defines { "NDEBUG" }
    symbols "On"
    targetdir "build/bin/optdebug"
    optimize "Debug"

filter "Configurations:Release"
    defines { "NDEBUG" }
    symbols "On"
    targetdir "build/bin/release"
    optimize "Full"

filter "system:windows"
    toolset (iif(_ACTION == "vs2019", "v142", "v141"))
    systemversion (os.winSdkVersion() .. ".0")
    debugdir "../workdir/"

project "Noctis"
    files { "src/**.cpp", 
            "src/**.hpp", 
            "src/3rdparty/**.hxx",
            "**.natvis" }
    location "build/Noctis"