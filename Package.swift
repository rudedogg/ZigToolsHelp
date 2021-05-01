// swift-tools-version:5.3
// The swift-tools-version declares the minimum version of Swift required to build this package.

import PackageDescription

let package = Package(
  name: "ZigTools",
  products: [
    .library(
      name: "ZigTools",
      targets: ["ZigSyntaxChecker", "ZigTools"]),
  ],
  dependencies: [],
  targets: [
    .target(
      name: "ZigTools",
      dependencies: ["ZigSyntaxChecker"],
      path: "Sources/ZigTools")
    ,
    .target(
      name: "ZigSyntaxChecker",
      dependencies: [],
      path: "Sources/ZigSyntaxChecker",
      exclude: [
        "src/",
        "README.md",
        "build.zig"
      ],
      resources: [
        Resource.copy("libZigSyntaxChecker.dylib"),
      ],
      publicHeadersPath: "include",
      cSettings: [
        .headerSearchPath("include/ZigSyntaxChecker.h")
      ]
    ),
    .testTarget(
      name: "ZigToolsTests",
      dependencies: ["ZigTools"]
    ),
  ]
)
